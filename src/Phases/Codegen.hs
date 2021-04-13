{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This module uses llvm-hs-pure to contstruct an AST representing an LLVM module for which llvm-hs will generate the llvm-ir code.
-- By design the given IRBuilder is not used, instead a similar is designed, with functions acting over state.

module Phases.Codegen where

import RIO hiding (local)
import RIO.State
import qualified RIO.Map as Map
import Types.Types hiding (Name)
import Types.Constructors
import Types.Pprint
import Utils.NameResolver (primsAndAritys)

import Data.Monoid ((<>))
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
import qualified LLVM.AST.Linkage as L
import LLVM.AST.Type as TY
import LLVM.AST.Name as LN
import LLVM.AST.Typed (typeOf)
import qualified RIO.Text as T
import RIO.List (sortBy)

transform :: ScEnv ()
transform = do
  logInfo "Generating code for the ast"
  procsref <- asks _procs
  procs <- readSomeRef procsref


  return ()


-------------------------------------------------------------------------------
-- Codegen
-------------------------------------------------------------------------------
-- Sobj Typ definieren
-- Deklarationen erzeugen
-- singleton objecte Funktionsaufrufe (call_fn)

codegen :: Proc Name -> Codegen ()
codegen proc = do
  return ()

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

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: AST.Module
emptyModule = defaultModule { moduleName = "Scheme" }


decl :: Type -> ShortByteString -> [(Type, Name)] -> Global
decl ret label argtys =
  functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = ret
  }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Global -> [BasicBlock] -> LLVM ()
define decl body = addDefn $ GlobalDefinition $ decl { basicBlocks = body }

declare ::  Global -> LLVM ()
declare = addDefn . GlobalDefinition

declarePrims :: LLVM ()
declarePrims = mapM_ go primsAndAritys
  where
    go :: (ByteString, [Int]) -> LLVM ()
    go (pn, [arity]) =
      forM_ [pn, "apply_" <> pn] $ \pn ->
        declare $ decl sobj (toShort pn) (primArgList arity)
    go (pn, aritys@[_, _]) =
      forM_ aritys $ \arity ->
      forM_ [pn, "apply_" <> pn] $ \pn ->
        declare $ decl sobj (toShort $ pn <> fromString (show arity)) (primArgList arity)
    go x = error "invalid aritys of primitve function"

declareConstInits :: LLVM ()
declareConstInits = mapM_ (\(n, argtype) -> declare $ decl sobj n (singletonArg argtype)) consts

declareHelper :: LLVM ()
declareHelper = mapM_ (\(ret, name, args) -> declare $ decl ret name args) helper
-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

llvmName :: UniqName -> Name
llvmName (UName n i) = Name (toShort $ n <> toName (show i))

nameAst :: Expr UniqName -> Expr Name
nameAst = fmap llvmName

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
createBlocks m = fmap makeBlock $ sortBlocks $ Map.toList (blocks m)

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

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

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

-------------------------------------------------------------------------------
-- Effects
-------------------------------------------------------------------------------

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr float $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

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
