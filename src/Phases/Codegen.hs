{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude #-}

-- This module uses llvm-hs-pure to contstruct an AST representing an LLVM module for which llvm-hs will generate the llvm-ir code.
-- By design the given IRBuilder is not used, instead a similar is designed, with functions acting over state.

module Phases.Codegen where

import RIO hiding (local, const)
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

transform :: ScEnv ()
transform = do
  logInfo "Generating code for the ast"
  procsref <- asks _procs
  procs <- readSomeRef procsref

  return ()


-------------------------------------------------------------------------------
-- Codegen
-------------------------------------------------------------------------------

codegen :: Proc UniqName -> LLVM ()
codegen (Proc (name, ELam (Lam ps body))) = do
  let args = zip (repeat sobjPtr) (fmap toName ps)
  define (decl sobjPtr name args) $ \fptr ->
    return ()


-- codegenBody :: Expr UniqName -> Codegen ()
-- codegenBody e = case e  of
--   ELit lit -> literal lit
--   where
--     literal :: Literal -> Codegen Operand
--     literal (LitInt int) = do
--       let fn = callableFnPtr "const_init_int"
--       call fn [intC int]


-------------------------------------------------------------------------------
-- LLVM-IR Types and Utils
-------------------------------------------------------------------------------

sobj :: Type -- opaque structure which the RT defines
sobj = TY.NamedTypeReference "SObj"

sobjPtr :: Type
sobjPtr = ptr sobj

unnamedArgList :: [Name]
unnamedArgList = fmap argNameRT [0..]

singletonArg :: Type -> [(Type, Name)]
singletonArg ty = [(ty, argNameRT 0)]

primArgList :: Int -> [(Type, Name)]
primArgList argcount = zip (replicate argcount sobj) unnamedArgList

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
          (sobjPtr, "apply_halt", singletonArg sobjPtr), (sobjPtr, "closure_create", singletonArg sobjPtr)]

class ToLLVMName a where
  toName :: a -> Name

instance ToLLVMName ByteString where
  toName = toName . toShort

instance ToLLVMName ShortByteString where
  toName = Name

instance ToLLVMName UniqName where
  toName (UName n i) = toName (n <> fromString (show i))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type NamedArg = (Type, Name)

type SymbolTable = [(ShortByteString, Operand)]

type Names = Map.Map ShortByteString Int

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

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

newtype Codegen a = Codegen { runCodegen :: StateT CodegenState LLVM a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

runLLVM :: LLVM a -> AST.Module
runLLVM (LLVM m) = execState m emptyModule

emptyModule :: AST.Module
emptyModule = defaultModule { moduleName = "Scheme" }

decl :: ToLLVMName name => Type -> name -> [NamedArg] -> Global
decl ret label argtys =
  functionDefaults {
    name        = toName label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = ret
  }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Global -> (Type -> Codegen a) -> LLVM ()
define decl body = do

  codegenState <- execCodegen $ do
      enter <- addBlock entryBlockName
      _ <- setBlock enter
      body ptrThisType
  let bls = createBlocks codegenState

  addDefn $ GlobalDefinition $ decl { basicBlocks = bls }
  where
    ptrThisType = PointerType
      { pointerReferent = FunctionType
          { resultType = returnType decl,
            argumentTypes = fmap (\(Parameter ty _ _) -> ty) (fst $ parameters decl),
            isVarArg = False
          },
        pointerAddrSpace = AddrSpace 0
      }

fnPtr :: Name -> LLVM Type
fnPtr nm = findType <$> gets moduleDefinitions
  where
    findType defs =
      case fnDefByName of
        [] -> error $ "Undefined function: " ++ show nm
        [fn] -> PointerType (typeOf fn) (AddrSpace 0)
        _ -> error $ "Ambiguous function name: " ++ show nm
      where
        globalDefs = [g | GlobalDefinition g <- defs]
        fnDefByName = [f | f@Function {name = nm'} <- globalDefs, nm' == nm]

callableFnPtr :: Name -> LLVM Operand
callableFnPtr nm = do
  ptr <- fnPtr nm
  return $ ConstantOperand (global ptr nm)

globalStringPtr ::
  Name         -- ^ Variable name of the pointer
  -> String       -- ^ The string to generate
  -> LLVM C.Constant
globalStringPtr nm str = do
  let asciiVals = map (fromIntegral . ord) str
      llvmVals  = map (C.Int 8) (asciiVals ++ [0]) -- append null terminator
      char      = IntegerType 8
      charArray = C.Array char llvmVals
      ty        = LLVM.AST.Typed.typeOf charArray
  addDefn $ GlobalDefinition globalVariableDefaults
    { name                  = nm
    , LLVM.AST.Global.type' = ty
    , linkage               = L.External
    , isConstant            = True
    , initializer           = Just charArray
    , unnamedAddr           = Just GlobalAddr
    }
  return $ C.GetElementPtr True
                           (C.GlobalReference (ptr ty) nm)
                           [C.Int 32 0, C.Int 32 0]

declare ::  Global -> LLVM ()
declare = addDefn . GlobalDefinition

declarePrims :: LLVM ()
declarePrims = mapM_ go primsAndAritys
  where
    go :: (ByteString, [Int]) -> LLVM ()
    go (pn, [arity]) =
      forM_ [pn, "apply_" <> pn] $ \pn' ->
        declare $ decl sobj pn' (primArgList arity)
    go (pn, aritys@[_, _]) =
      forM_ aritys $ \arity ->
      forM_ [pn, "apply_" <> pn] $ \pn' ->
        declare $ decl sobj (pn' <> fromString (show arity)) (primArgList arity)
    go _ = error "invalid aritys of primitve function"

declareConstInits :: LLVM ()
declareConstInits = mapM_ (\(n, argtype) -> declare $ decl sobj n (singletonArg argtype)) consts

declareHelper :: LLVM ()
declareHelper = mapM_ (\(ret, name, args) -> declare $ decl ret name args) helper
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

assign :: ShortByteString -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s {symtab = (var, x) : lcls}

getvar :: ShortByteString -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

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

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr float $ UIToFP a ty []

const :: C.Constant -> Operand
const = ConstantOperand

intC :: Int -> Operand
intC i = const $ C.Int 64 (toInteger i)

floatC :: Float -> Operand
floatC f = const $ C.Float $ F.Double (float2Double f)

charC :: Char -> Operand
charC c = const $ C.Int 8 (toInteger $ ord c)


-------------------------------------------------------------------------------
-- Effects
-------------------------------------------------------------------------------

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr sobjPtr $ Call Nothing CC.Fast [] (Right fn) (toArgs args) [] []


-------------------------------------------------------------------------------
-- Control Flow
-------------------------------------------------------------------------------

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr float $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retvoid :: Codegen (Named Terminator)
retvoid = terminator $ Do $ Ret Nothing []
