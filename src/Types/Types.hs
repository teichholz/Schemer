{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}
-- | Compiler types

module Types.Types  where

import RIO hiding (void)
import qualified Unbound.Generics.LocallyNameless as Un
import qualified Control.Monad.Cont as C


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

instance Un.Alpha ScSyn

data Decl
  = FunDecl Name Params Body
  | FunDotDecl Name Params Param Body
  | FunListDecl Name Param Body
  | VarDecl Name Expr
  deriving (Show, Generic, Typeable)

instance Un.Alpha Decl

type Param = Name
type Params = [Name]

data Expr
  = EApp Application    -- (+ 1 2)
  | EVar Name           -- +
  | ELam Lambda         -- (lambda xs (apply + xs))
  | ELet Let           -- (let ((x 21)) (+ x x))
  | EIf Expr Expr Expr  -- (if test then else)
  | ESet Name Expr
  | EApply Expr Expr
  | ECallCC Expr
  | ELit Literal -- '(1 2 3 4), ...
  | ESynExt SynExtension -- makros
  deriving (Show, Generic, Typeable)

type Name = Un.Name Expr

instance Un.Alpha Expr

data Application
  = AppPrim PrimName [Expr]
  | AppLam Expr [Expr]
  deriving (Show, Generic, Typeable)

instance Un.Alpha Application

-- Scheme name and RT (Runtime) name
newtype PrimName = PName {unPName :: (String, String)}
  deriving (Show, Generic)
instance Un.Alpha PrimName

data SynExtension
  = ECond CondBody      -- (cond ((#t) (io) 'true) (else 'false))
  | ECase Expr CaseBody -- (case (+ 2 2) ((4) 'true) (else 'false))
  | EOr (Maybe [Expr])  -- (or)
  | EAnd (Maybe [Expr]) -- (and)
  | EBegin [Expr]       -- (begin (io) 'true)
  | LetStar [(Name, Expr)] Body
  | LetRec [(Name, Expr)] Body
  deriving (Show, Generic, Typeable)


instance Un.Alpha SynExtension

type CondBody
  = [(Expr, Body)] -- (test expr1 expr2 ...)

type CaseBody
  = [([Literal], Body)] -- (test expr1 expr2 ...)

data Lambda
  = Lam (Un.Bind [Name] Body)
  | LamDot (Un.Bind ([Name], Name) Body)
  | LamList (Un.Bind Name Body)
  deriving (Show, Generic, Typeable)

instance Un.Alpha Lambda

newtype Let
  = Let (Un.Bind [(Name, Un.Embed Expr)] Body)
  deriving (Show, Generic, Typeable)

type Binding = [(Name, Un.Embed Expr)]

instance Un.Alpha Let

newtype Body = Body { unBody :: [ScSyn]}
  deriving (Show, Generic, Typeable)

instance Un.Alpha Body

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

instance Un.Alpha Literal
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
makeMap ::(Un.Fresh m)  => (Expr -> m Expr) -> Mapper m
makeMap = makeMapExpr

makeMapExpr :: (Un.Fresh m)  => (Expr -> m Expr) -> Mapper m
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
descend f exp = Un.runFreshM (descendM (makeMap (return . f)) exp)

runDescendM :: (Expr -> Un.FreshM Expr) -> ScSyn -> ScSyn
runDescendM f syn = Un.runFreshM (descendM (makeMap f) syn)

descendM :: (Un.Fresh m) => Mapper m -> ScSyn -> m ScSyn
descendM f syn = case syn of
  ScExpr e -> ScExpr <$> descendExprM f e
  ScDecl d -> ScDecl <$> descendDeclM f d

descendDeclM :: (Un.Fresh m) => Mapper m -> Decl -> m Decl
descendDeclM f d = mapDecl f =<< case d of
  VarDecl n e -> VarDecl n <$> descendExprM f e
  FunDecl n p b -> FunDecl n p <$> descendBodyM f b
  FunDotDecl n ps p b -> FunDotDecl n ps p <$> descendBodyM f b
  FunListDecl n p b -> FunListDecl n p <$> descendBodyM f b

descendExprM :: (Un.Fresh m) => Mapper m -> Expr -> m Expr
descendExprM f e = mapExpr f =<< case e of
  EApp app -> EApp <$> descendApplicationM f app
  EVar name -> return $ EVar name
  ELam lambda -> ELam <$> descendLambdaM f lambda
  ELet letb -> ELet <$> descendLetM f letb
  EIf tst thn els -> EIf <$> descendExprM f tst <*> descendExprM f thn <*> descendExprM f els
  ESet name expr -> ESet name <$> descendExprM f expr
  EApply expr1 expr2 -> liftA2 EApply (descendExprM f expr1) (descendExprM f expr2)
  ECallCC expr -> ECallCC <$> descendExprM f expr
  ELit lit -> return $ ELit lit
   -- ESynExt ext -> descendSynExt ext

descendApplicationM :: (Un.Fresh m) => Mapper m -> Application -> m Application
descendApplicationM f e = case e of
  AppPrim primName expr -> AppPrim primName <$> mapM (descendExprM f) expr
  AppLam exprhd expr -> AppLam <$> descendExprM f exprhd <*> mapM (descendExprM f) expr


descendLambdaM :: (Un.Fresh m) => Mapper m -> Lambda -> m Lambda
descendLambdaM f e = case e of
  Lam bnd -> do
    (pats, body) <- Un.unbind bnd
    Lam . Un.bind pats <$> descendBodyM f body
  LamDot bnd -> do
    ((pats, pat), body) <- Un.unbind bnd
    LamDot . Un.bind (pats, pat) <$> descendBodyM f body
  LamList bnd -> do
    (pat, body) <- Un.unbind bnd
    LamList . Un.bind pat <$> descendBodyM f body

descendBodyM :: (Un.Fresh m) => Mapper m -> Body -> m Body
descendBodyM f b = Body <$> mapM (descendM f) (unBody b)


descendLetM :: (Un.Fresh m) => Mapper m -> Let -> m Let
descendLetM f e = case e of
  Let bnd -> do
    (patlist, bodyKind) <- Un.unbind bnd
    let newpatlist =
         (fmap . fmap) Un.Embed <$>
         (mapM . mapM) (descendExprM f) (fmap (\ (n, Un.Embed e) -> (n, e)) patlist)
    let newbodyKind = descendBodyM f bodyKind
    Let <$> liftA2 Un.bind newpatlist newbodyKind





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
