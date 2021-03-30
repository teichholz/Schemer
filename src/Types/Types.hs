{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}
-- | Compiler types

module Types.Types  where

import RIO hiding (void)
import qualified Control.Monad.Cont as C
import Data.Text.Prettyprint.Doc
import qualified RIO.Set as S
import qualified Data.Text as T


-- Main datatype as a: ReaderT CompilerState IO a
type ScEnv a = RIO Env a

data Env = Env
  { _file :: SourceFile
  , _ast      :: SomeRef ScSyn  -- Scheme syntax
  , _toplevel :: [ScSyn] -- Top level Scheme syntax
  , _options :: Options -- CLI options / arguments
  , _name :: String -- Name of this awesome compiler
  , _logF :: LogFunc -- Logger (RIO)
  }

data SourceFile = SourceFile
  { _fname :: FilePath
  , _fsrc :: Text
  } deriving (Show)

data Options = Options
  { _optionsVerbose :: !Bool
  , _fileName :: !FilePath
  } deriving (Show)

instance HasLogFunc Env where
  logFuncL = lens _logF (\x y -> x {_logF = y})

-------------------------------------------------------------------------------
-- AST
-------------------------------------------------------------------------------

-- Pre-Desugar

-- dummy AST
dummy :: ScSyn
dummy = ScExpr $ ELit LitUnspecified

data ScSyn
  = ScDecl Decl
  | ScExpr Expr
  deriving (Show, Generic, Typeable)

data Decl
  = FunDecl Name Params Body
  | FunDotDecl Name Params Param Body
  | FunListDecl Name Param Body
  | VarDecl Name Expr
  deriving (Show, Generic, Typeable)

type Param = Name
type Params = [Name]

data Expr
  = EApp Application    -- (+ 1 2)
  | EVar Name           -- +
  | ELam Lambda         -- (lambda xs (apply + xs))
  | ELet Let           -- (let ((x 21)) (+ x x))
  | EIf Expr Expr Expr  -- (if test then else)
  | ESet Name Expr
  | EApply Apply
  | ECallCC Expr
  | ELit Literal -- '(1 2 3 4), ...
  | ESynExt SynExtension -- makros
  deriving (Show, Generic, Typeable)

type Name = Text

data Apply
  = ApplyPrim PrimName Expr
  | ApplyLam Expr Expr
  deriving (Show, Generic, Typeable)

data Application
  = AppPrim PrimName [Expr]
  | AppLam Expr [Expr]
  deriving (Show, Generic, Typeable)

-- Scheme name and RT (Runtime) name
-- Example: + (Scheme) and plus (RT)
newtype PrimName = PName {unPName :: (String, String)}
  deriving (Show, Generic)

-- These represent methods needed by the runtime itself.
-- These don't have an actual Scheme counterpart which is used for pretty printing.
-- Example: halt (RT), no Scheme counterpart
newtype PrimName' = PName' {unPName' :: String}
  deriving (Show, Generic)

data SynExtension
  = ECond CondBody      -- (cond ((#t) (io) 'true) (else 'false))
  | ECase Expr CaseBody -- (case (+ 2 2) ((4) 'true) (else 'false))
  | EOr (Maybe [Expr])  -- (or)
  | EAnd (Maybe [Expr]) -- (and)
  | EBegin [Expr]       -- (begin (io) 'true)
  | LetStar [(Name, Expr)] Body
  | LetRec [(Name, Expr)] Body
  deriving (Show, Generic, Typeable)


type CondBody
  = [(Expr, Body)] -- (test expr1 expr2 ...)

type CaseBody
  = [([Literal], Body)] -- (test expr1 expr2 ...)

data Lambda
  = Lam [Name] Body
  | LamDot ([Name], Name) Body
  | LamList Name Body
  deriving (Show, Generic, Typeable)

type Binding = [(Name, Expr)]

data Let = Let [(Name, Expr)] Body
  deriving (Show, Generic, Typeable)

newtype Body = Body { unBody :: [ScSyn]}
  deriving (Show, Generic, Typeable)

data Literal
  = LitString String
  | LitSymbol String
  | LitInt Int
  | LitFloat Float
  | LitChar Char
  | LitBool Bool
  | LitList [Literal]
  | LitNil
  | LitVector [Literal]
  | LitUnspecified
  deriving (Show, Generic, Typeable)

-- Post-Desugar

data Core
  = Void


-------------------------------------------------------------------------------
-- Traversal
-------------------------------------------------------------------------------

-- Mapper gives acces to the function needed to transform declaration or expressions
data (a :*: b) = D a b
type Mapper m = (Expr -> m Expr) :*: (Decl -> m Decl)

-- Alias for MakeMapExpr
makeMap ::(Monad m)  => (Expr -> m Expr) -> Mapper m
makeMap = makeMapExpr

makeMapExpr :: (Monad m)  => (Expr -> m Expr) -> Mapper m
makeMapExpr f = D f return

makeMapDecl :: Monad m => (Decl -> m Decl) -> Mapper m
makeMapDecl = D return

makeMap' :: (Expr -> m Expr) -> (Decl -> m Decl) -> Mapper m
makeMap' = D

mapDecl :: Mapper m -> Decl -> m Decl
mapDecl (D _ f) = f

mapExpr :: Mapper m -> Expr -> m Expr
mapExpr (D f _) = f

descend :: (Expr -> Expr) -> ScSyn -> ScSyn
descend f exp = runIdentity (descendM (makeMap (return . f)) exp)

runDescendM :: Monad m => (m ScSyn -> ScSyn) -> (Expr -> m Expr)  -> ScSyn -> ScSyn
runDescendM run f syn = run (descendM (makeMap f) syn)

descendM :: (Monad m) => Mapper m -> ScSyn -> m ScSyn
descendM f syn = case syn of
  ScExpr e -> ScExpr <$> descendExprM f e
  ScDecl d -> ScDecl <$> descendDeclM f d

descendDeclM :: (Monad m) => Mapper m -> Decl -> m Decl
descendDeclM f d = mapDecl f =<< case d of
  VarDecl n e -> VarDecl n <$> descendExprM f e
  FunDecl n p b -> FunDecl n p <$> descendBodyM f b
  FunDotDecl n ps p b -> FunDotDecl n ps p <$> descendBodyM f b
  FunListDecl n p b -> FunListDecl n p <$> descendBodyM f b

descendExprM :: (Monad m) => Mapper m -> Expr -> m Expr
descendExprM f e = mapExpr f =<< case e of
  EApp app -> EApp <$> descendApplicationM f app
  EVar name -> return $ EVar name
  ELam lambda -> ELam <$> descendLambdaM f lambda
  ELet letb -> ELet <$> descendLetM f letb
  EIf tst thn els -> EIf <$> descendExprM f tst <*> descendExprM f thn <*> descendExprM f els
  ESet name expr -> ESet name <$> descendExprM f expr
  EApply apply -> EApply <$> descendApplyM f apply
  ECallCC expr -> ECallCC <$> descendExprM f expr
  ELit lit -> return $ ELit lit
   -- ESynExt ext -> descendSynExt ext

descendApplyM :: (Monad m) => Mapper m -> Apply -> m Apply
descendApplyM f e = case e of
  ApplyPrim primName expr -> ApplyPrim primName <$> descendExprM f expr
  ApplyLam exprhd expr -> ApplyLam <$> descendExprM f exprhd <*> descendExprM f expr

descendApplicationM :: (Monad m) => Mapper m -> Application -> m Application
descendApplicationM f e = case e of
  AppPrim primName expr -> AppPrim primName <$> mapM (descendExprM f) expr
  AppLam exprhd expr -> AppLam <$> descendExprM f exprhd <*> mapM (descendExprM f) expr


descendLambdaM :: (Monad m) => Mapper m -> Lambda -> m Lambda
descendLambdaM f e = case e of
  Lam pats body ->
    Lam pats <$> descendBodyM f body
  LamDot (pats, pat) body ->
    LamDot (pats, pat) <$> descendBodyM f body
  LamList pat body ->
    LamList pat <$> descendBodyM f body

descendBodyM :: (Monad m) => Mapper m -> Body -> m Body
descendBodyM f b = Body <$> mapM (descendM f) (unBody b)


descendLetM :: (Monad m) => Mapper m -> Let -> m Let
descendLetM f e = case e of
  Let patlist bodyKind -> do
    let newpatlist = (mapM . mapM) (descendExprM f) patlist
    let newbodyKind = descendBodyM f bodyKind
    liftA2 Let newpatlist newbodyKind


makeList' :: Int -> ([Int] -> [Int]) -> [Int]
makeList' n k =
  if 0 == n
  then k []
  else makeList' (n-1) (\l -> k (n:l))


makeList'' :: Int -> C.Cont [Int] [Int]
makeList'' n =
  if 0 == n
  then return []
  else do
    l <- makeList'' (n - 1)
    return (n:l)



-------------------------------------------------------------------------------
-- Free vars calculation
-------------------------------------------------------------------------------

class FreeVars e where
  fv :: e -> S.Set Name

instance FreeVars a => FreeVars [a] where
  fv = S.unions . fmap fv

instance FreeVars Name where
  fv = S.singleton

instance FreeVars ScSyn where
  fv (ScDecl d) = fv d
  fv (ScExpr e) = fv e

instance FreeVars Body where
  fv (Body b) = fv b

instance FreeVars Decl where
  fv (FunDecl _ ps b) = fv ps  S.\\ fv b
  fv (FunDotDecl _ ps p b) = fv (p:ps) S.\\ fv b
  fv (FunListDecl _ p b) = fv p S.\\ fv b
  fv (VarDecl _ e) = fv e

instance FreeVars Expr where
  fv (EApp app) = fv app
  fv (EVar name) = fv name
  fv (ELam lam) = fv lam
  fv (ELet lt) = fv lt
  fv (EIf tst thn els) = S.unions [fv tst, fv thn, fv els]
  fv (ESet _ e) = fv e
  fv (EApply apply) = fv apply
  fv (ECallCC e) = fv e
  fv (ELit _) = S.empty

instance FreeVars Apply where
  fv (ApplyPrim _ e) = fv e
  fv (ApplyLam e1 e2) = fv e1 `S.union` fv e2

instance FreeVars Application where
  fv (AppPrim _ es) = fv es
  fv (AppLam e es) = fv (e:es)

instance FreeVars Lambda where
  fv (Lam ps b) = fv ps  S.\\ fv b
  fv (LamDot (ps, p) b) = fv (p:ps) S.\\ fv b
  fv (LamList p b) = fv p S.\\ fv b

instance FreeVars Let where
  fv (Let bind body) = fv (fst <$> bind) S.\\ fv body
