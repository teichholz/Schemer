{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- This module uses llvm-hs-pure to contstruct an AST representing an LLVM module for which llvm-hs will generate the llvm-ir code.
-- By design the given IRBuilder is not used, instead a similar is designed, with functions acting over state.

module Phases.Codegen where

import RIO hiding (local, const, mod)
import GHC.Float (float2Double)
import RIO.State
import qualified RIO.Map as Map
import Types.Types hiding (Name)
import Types.Constructors hiding (toName)
import Types.Pprint
import Utils.NameResolver (primsAndAritys)

import Data.Monoid ((<>))
import Data.Char (ord)
import Control.Monad
import Data.String
import Data.Word
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Global
import LLVM.AST.Float as F
import qualified LLVM.AST.Linkage as L
import LLVM.AST.Type as TY
import LLVM.AST.Name as LN
import LLVM.AST.Typed (typeOf)
import qualified RIO.Text as T
import RIO.List (sortBy)
import Data.List (repeat)
import Control.Monad.Fix (MonadFix)
import LLVM.Module (withModuleFromAST)
import LLVM.Context (withContext, Context)
import LLVM (moduleLLVMAssembly, moduleAST)
import Prelude (print)
import RIO.ByteString (putStr)
import LLVM.Target (withHostTargetMachine)
import LLVM.Relocation as Relocation (Model(PIC))
import LLVM.CodeModel as Model (Model(Default, Large))
import LLVM.CodeGenOpt as CodeOpt (Level(Default))
import LLVM.Module (File(File))
import LLVM.Module (writeObjectToFile)
import LLVM.Linking (loadLibraryPermanently)
import LLVM.PassManager (withPassManager, runPassManager, PassSetSpec, optLevel, defaultCuratedPassSetSpec)
import qualified LLVM.ExecutionEngine as EE

import GHC.Ptr (FunPtr)
import Foreign (castFunPtr)
import System.IO (putStrLn)

transform :: ScEnv ()
transform = do
  logInfo "Generating code for the ast"
  procsref <- asks _procs
  procs <- readSomeRef procsref

  SourceFile{_fname} <- asks _file
  _ <- case _fname of
    "<repl>" -> runJIT procs
    _ -> compile procs

  return ()


-------------------------------------------------------------------------------
-- Codegen
-------------------------------------------------------------------------------
passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

foreign import ccall "dynamic" haskFun :: FunPtr (IO ()) -> IO ()

run :: FunPtr a -> IO ()
run fn = haskFun (castFunPtr fn :: FunPtr (IO ()))

runJIT :: [Proc UniqName] -> ScEnv Module
runJIT procs = do
  liftIO $ withContext $ \context ->
    withModuleFromAST context newast $ \m -> do
      withPassManager passes $ \pm -> do
        runPassManager pm m
        jit context $ \executionEngine -> do
          EE.withModuleInEngine executionEngine m $ \ee -> do
            optmod <- moduleAST m
            s <- moduleLLVMAssembly m
            putStr s
            mainfn <- EE.getFunction ee (AST.Name "main")
            mapM_ run mainfn
            return optmod
  where
    modn = initModule >> mapM codegen procs
    newast = runLLVM modn


compile :: [Proc UniqName] -> ScEnv Module
compile procs = do
  objectFilePath <- asks _outputFile
  liftIO $ withContext $ \context ->
    withModuleFromAST context newast $ \m -> do
      withHostTargetMachine Relocation.PIC Model.Default CodeOpt.Default $ \target -> do
        llstr <- moduleLLVMAssembly m
        putStr $ llstr <> "\n"
        writeObjectToFile target objectFilePath m
        return newast
  where
    modn = initModule >> mapM codegen procs
    newast = runLLVM modn

codegen :: Proc UniqName -> LLVM ()
codegen (Proc (name, ELam (Lam ps (Body [body])))) = do
  let formals :: [(Type, Name)] = zip (repeat sobjPtr) (fmap toName ps)
  -- let rettype = case name of
  --                 UName "main" _ -> TY.IntegerType 32
  --                 _     -> sobjPtr

  define (decl sobjPtr name formals) $ \operands -> do
    -- Register the arguments, so they can be accessed as variables
    let names :: [Name] = fmap toName ps
        args :: [(Name, Operand)] = zip names operands
    mapM_ (uncurry assign) args

    -- Codegen the body
    body' <- codegenExpr body

    -- Unregister the arguments after the body
    mapM_ unassign names
    ret body'

codegen _ = error "wrong Proc"

codegenExpr :: ToExpr e UniqName => e -> Codegen Operand
codegenExpr e = case toExpr e of
  ELit lit -> literal lit
  ELet lt -> letbind lt
  EVar n -> ident n
  EIf tst thn els -> cond tst thn els
  EApp (AppPrim pn es) -> primCall pn es
  EApp (AppLam e es) -> closureCall e es
  e -> trace (fromString $ show e) (error "wrong Expr")
  where
    literal :: Literal -> Codegen Operand
    literal (LitInt int) = do
      fn <- callableFnPtr "const_init_int"
      call fn [intC int]
    literal (LitFloat f) = do
      fn <- callableFnPtr "const_init_float"
      call fn [floatC f]
    literal (LitChar c) = do
      fn <- callableFnPtr "const_init_char"
      call fn [charC c]
    literal (LitString s) = do
      strptr <- globalStringPtr "str" s
      fn <- callableFnPtr "const_init_string"
      call fn [strptr]
    literal (LitSymbol s) = do
      strptr <- globalStringPtr "sym" s
      fn <- callableFnPtr "const_init_symbol"
      call fn [strptr]
    literal (LitList lits) = do
      let list' = makeConsList' (fmap ELit lits)
      codegenExpr list'
    literal (LitVector lits) = do
      let list' = makeVectorFromList (fmap ELit lits)
      codegenExpr list'
    literal (LitBool b) = do
      fn <- callableFnPtr (if b then "get_true" else "get_false")
      call fn []
    literal LitUnspecified = do
      fn <- callableFnPtr "get_unspecified"
      call fn []
    literal LitNil = do
      fn <- callableFnPtr "get_nil"
      call fn []

    letbind :: Let UniqName -> Codegen Operand
    letbind (Let [(n, expr)] (Body [body])) = do
      letval <- codegenExpr expr
      assign n letval
      bodyval <- codegenExpr body
      unassign n
      return bodyval

    ident :: ToLLVMName n => n -> Codegen Operand
    ident un = do
      let name = toName un
      maybeFnPtr <- fnPtr name
      case maybeFnPtr of
        Nothing -> getvar un
        Just _ -> do
          cfptr <- callableFnPtr name
          ui <- ptrToInt i64 cfptr
          fn <- callableFnPtr "const_init_int"
          call fn [ui]

    cond :: Expr UniqName -> Expr UniqName -> Expr UniqName -> Codegen Operand
    cond tst thn els = do
      thenBlock <- addBlock "then"
      elseBlock <- addBlock "else"
      mergeBlock <- addBlock "merge"

      tst' <- codegenExpr tst
      coerceFn <- callableFnPtr "coerce_c"
      i8tst <- call coerceFn [tst']
      i1tst <- trunc i1 i8tst
      cbr i1tst thenBlock elseBlock

      setBlock thenBlock
      thn' <- codegenExpr thn
      br mergeBlock
      thenBlock <- getBlock

      setBlock elseBlock
      els' <- codegenExpr els
      br mergeBlock
      elseBlock <- getBlock

      setBlock mergeBlock
      phi [(thn', thenBlock), (els', elseBlock)]

    primCall :: PrimName -> [Expr UniqName] -> Codegen Operand
    primCall pn es = do
      fn <- callableFnPtr pn
      es <- mapM codegenExpr es
      call fn es

    closureCall :: Expr UniqName -> [Expr UniqName] -> Codegen Operand
    closureCall (EVar name) es = do
      vec <- getvar name
      callClosureWithArgs vec es

    closureCall e es = do
      vec <- codegenExpr e
      callClosureWithArgs vec es

    callClosureWithArgs :: Operand -> [Expr UniqName] -> Codegen Operand
    callClosureWithArgs vec args = do
      createClosure <- callableFnPtr "closure_create"
      clo <- call createClosure [vec]

      getIPtr <- callableFnPtr "closure_get_iptr"
      iptr <- call getIPtr [clo]
      fptr <- intToPtr iptr

      args <- mapM codegenExpr args
      call fptr (clo:args)

initModule :: LLVM ()
initModule = do
  declarePrims
  declareConstInits
  declareHelper

-------------------------------------------------------------------------------
-- LLVM-IR Types and Utils
-------------------------------------------------------------------------------

-- opaque structure which the RT defines
sobj :: Type
sobj = TY.NamedTypeReference "SObj"

-- Pointer to the opaque structure
sobjPtr :: Type
sobjPtr = ptr sobj

funTy :: Type
funTy = FunctionType sobjPtr [sobjPtr, sobjPtr] False

funTyPtr :: Type
funTyPtr = PointerType funTy (AddrSpace 0)

unnamedArgList :: [Name]
unnamedArgList = fmap argNameRT [0..]

singletonArg :: Type -> [(Type, Name)]
singletonArg ty = [(ty, argNameRT 0)]

primArgList :: Int -> [(Type, Name)]
primArgList argcount = zip (replicate argcount sobjPtr) unnamedArgList

strType :: Type
strType = ptr i8

consts :: [(ShortByteString, Type)]
consts = [("const_init_int", i64), ("const_init_float", FloatingPointType DoubleFP),
          ("const_init_string", strType), ("const_init_char", i8),
          ("const_init_symbol", strType), ("const_init_bool", i1)]

helper :: [(Type ,ShortByteString, [NamedArg])]
helper = [(i8 ,"coerce_c", singletonArg sobjPtr), (sobjPtr, "get_nil", []),
          (sobjPtr, "get_unspecified", []), (sobjPtr, "get_false", []),
          (sobjPtr, "get_true", []), (sobjPtr, "halt", singletonArg sobjPtr),
          (sobjPtr, "apply_halt", singletonArg sobjPtr), (sobjPtr, "closure_create", singletonArg sobjPtr),
          (i64, "closure_get_iptr", singletonArg sobjPtr)]

class ToLLVMName a where
  toName :: a -> Name

instance ToLLVMName Name where
  toName = id

instance ToLLVMName String where
  toName = toName . fromString @ShortByteString

instance ToLLVMName ByteString where
  toName = toName . toShort

instance ToLLVMName ShortByteString where
  toName = Name

instance ToLLVMName UniqName where
  toName (UName n i) = toName (n <> if i == 0 then "" else fromString (show i))

instance ToLLVMName PrimName where
  toName (PName (_, rtName)) = toName rtName
-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type NamedArg = (Type, Name)

type SymbolTable = [(Name, Operand)]

type Names = Map.Map ShortByteString Int

data LLVMState
  = LLVMState {
    mod :: AST.Module
  , globalNames        :: Names
  } deriving Show

data CodegenState
  = CodegenState {
    currentBlock :: LN.Name                     -- Name of the active block to append to
  , blocks       :: Map.Map LN.Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

newtype LLVM a = LLVM (State LLVMState a)
  deriving (Functor, Applicative, Monad, MonadState LLVMState, MonadFix)

newtype Codegen a = Codegen { runCodegen :: StateT CodegenState LLVM a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadFix)

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

runLLVM :: LLVM a -> AST.Module
runLLVM (LLVM m) = mod $ execState m emptyLLVMState

emptyLLVMState :: LLVMState
emptyLLVMState = LLVMState {
  mod = emptyModule
  , globalNames = Map.empty
                           }
emptyModule :: AST.Module
emptyModule = defaultModule { moduleName = "Scheme", moduleDefinitions = [TypeDefinition "SObj" Nothing] }

decl :: ToLLVMName name => Type -> name -> [NamedArg] -> Global
decl ret label argtys =
  functionDefaults {
    name        = toName label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = ret
  , LLVM.AST.Global.callingConvention = CC.Fast
  }

getModule :: MonadState LLVMState m => m AST.Module
getModule = gets mod

updateModule :: MonadState LLVMState m => (AST.Module -> AST.Module) -> m ()
updateModule mf = do
  mod <- getModule
  modify $ \s -> s { mod = mf mod  }

addDefn :: Definition -> LLVM ()
addDefn d = do
  mod <- getModule
  let defs = moduleDefinitions mod
  updateModule $ \m -> m { moduleDefinitions = defs ++ [d] }

define ::  Global -> ([Operand] -> Codegen a) -> LLVM ()
define decl body = do
  let paramnames = [ name  | (Parameter _ name _)  <- fst $ parameters decl]

  codegenState <- execCodegen $ do
      enter <- addBlock entryBlockName
      setBlock enter
      body $ map (LocalReference sobjPtr) paramnames
  let bls = createBlocks codegenState

  addDefn $ GlobalDefinition $ decl { basicBlocks = bls }

fnPtr :: Name -> Codegen (Maybe Type)
fnPtr nm = do
  fn <- findFnByName nm
  return $ fmap (\fn' -> PointerType (typeOf fn') (AddrSpace 0)) fn

findFnByName :: Name -> Codegen (Maybe Global)
findFnByName nm = Codegen $ lift $ fmap (findType . moduleDefinitions) getModule
  where
    findType defs =
      case fnDefByName of
        [fn] -> Just fn
        _ -> Nothing
      where
        globalDefs = [g | GlobalDefinition g <- defs]
        fnDefByName = [f | f@Function {name = nm'} <- globalDefs, nm' == nm]

callableFnPtr :: ToLLVMName n => n -> Codegen Operand
callableFnPtr nm' = do
  let nm = toName nm'
  ptr <- fnPtr nm
  let ptr' = case ptr of
        Nothing -> error ("Function not defined: " <> show nm)
        Just ptr -> ptr
  return $ const (global ptr' nm)

globalStringPtr ::
  ShortByteString         -- ^ Variable name of the pointer
  -> String       -- ^ The string to generate
  -> Codegen Operand
globalStringPtr nm str = do
  let asciiVals = map (fromIntegral . ord) str
      llvmVals  = map (C.Int 8) (asciiVals ++ [0]) -- append null terminator
      char      = IntegerType 8
      charArray = C.Array char llvmVals
      ty        = LLVM.AST.Typed.typeOf charArray

  name <- Codegen $ lift $ uniqueGlobalName nm

  Codegen $ lift $ addDefn $ GlobalDefinition globalVariableDefaults
    { name                  = Name name
    , LLVM.AST.Global.type' = ty
    , linkage               = L.External
    , isConstant            = True
    , initializer           = Just charArray
    , unnamedAddr           = Just GlobalAddr
    }

  return $ ConstantOperand $ C.GetElementPtr True
                           (C.GlobalReference (ptr ty) (Name name))
                           [C.Int 32 0, C.Int 32 0]

declare ::  Global -> LLVM ()
declare = addDefn . GlobalDefinition

declarePrims :: LLVM ()
declarePrims = mapM_ go primsAndAritys
  where
    go :: (ByteString, [Int]) -> LLVM ()
    go (pn, [arity]) =
      forM_ [pn, "apply_" <> pn] $ \pn' ->
        declare $ decl sobjPtr pn' (primArgList arity)
    go (pn, aritys@[_, _]) =
      forM_ aritys $ \arity ->
      forM_ [pn, "apply_" <> pn] $ \pn' ->
        declare $ decl sobjPtr (pn' <> fromString (show arity))
                       (primArgList arity)
    go _ = error "invalid aritys of primitve function"

declareConstInits :: LLVM ()
declareConstInits = forM_ consts $ \(n, argtype) ->
  declare $ decl sobjPtr n (singletonArg argtype)

declareHelper :: LLVM ()
declareHelper = forM_ helper $ \(ret, name, args) ->
  declare $ decl ret name args
-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

llvmName :: UniqName -> Name
llvmName = toName

llvmNameAst :: Expr UniqName -> Expr Name
llvmNameAst = fmap llvmName

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm <> fromString (show ix), Map.insert nm (ix + 1) ns)

uniqueGlobalName :: MonadState LLVMState m => ShortByteString -> m ShortByteString
uniqueGlobalName nm = do
  ns <- gets globalNames
  let (nm', ns') = uniqueName nm ns
  modify $ \s -> s { globalNames = ns' }
  return nm'

argNameRT :: Word -> Name
argNameRT = UnName

-------------------------------------------------------------------------------
-- Codegen operations
-------------------------------------------------------------------------------

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` idx . snd)

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = fmap makeBlock $ sortBlocks $ Map.toList $ blocks m

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: ShortByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> LLVM CodegenState
execCodegen m = let t = execStateT (runCodegen m) emptyCodegen in t

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s {count = 1 + i}
  return $ i + 1

instr :: Type -> Instruction -> Codegen Operand
instr ty ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = (ref := ins) : i})
  return $ local ty ref

unnminstr :: Instruction -> Codegen ()
unnminstr ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = Do ins : i})

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk {term = Just trm})
  return trm
-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: ShortByteString -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s {currentBlock = bname}
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s {blocks = Map.insert active new (blocks s)}

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Sybol Table
-------------------------------------------------------------------------------

assign :: ToLLVMName name => name -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s {symtab = (toName var, x) : lcls}

unassign :: ToLLVMName name => name -> Codegen ()
unassign var = do
  lcls <- gets symtab
  let lcls' = [(n, e) | (n, e) <- lcls, n /= toName var ]
  modify $ \s -> s {symtab = lcls'}

getvar :: ToLLVMName name => name -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup (toName var) syms of
    Just x -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show (toName var)

-------------------------------------------------------------------------------
-- References
-------------------------------------------------------------------------------

local :: Type -> Name -> Operand
local = LocalReference

global :: Type -> Name -> C.Constant
global = C.GlobalReference

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (,[])

const :: C.Constant -> Operand
const = ConstantOperand

intC :: Int -> Operand
intC i = const $ C.Int 64 (toInteger i)

floatC :: Float -> Operand
floatC f = const $ C.Float $ F.Double (float2Double f)

charC :: Char -> Operand
charC c = const $ C.Int 8 (toInteger $ ord c)


-------------------------------------------------------------------------------
-- Instructions
-------------------------------------------------------------------------------

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr sobjPtr $ Call Nothing CC.Fast [] (Right fn) (toArgs args) [] []

intToPtr ::  Operand -> Codegen Operand
intToPtr a = instr funTyPtr $ IntToPtr a funTyPtr []

ptrToInt :: Type -> Operand -> Codegen Operand
ptrToInt ty a = instr i64 $ PtrToInt a ty []

-------------------------------------------------------------------------------
-- Control Flow Instructions
-------------------------------------------------------------------------------

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

trunc :: Type -> Operand -> Codegen Operand
trunc ty op = instr i1 $ Trunc op ty []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: [(Operand, Name)] -> Codegen Operand
phi incoming = instr sobjPtr $ Phi sobjPtr incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
