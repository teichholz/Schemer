{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}
-- | Compiler types

module Types where

import RIO hiding (void)
import qualified Unbound.Generics.LocallyNameless as Un

-- Main datatype as a: ReaderT CompilerState IO a
type ScEnv a = RIO CompilerState a

data CompilerState = CompilerState
  { _file :: Maybe SourceFile
  , _ast      :: ScSyn  -- Scheme Syntax
  , _options :: Options
  , _name :: String -- Name of this awesome compiler
  , _logF :: LogFunc
  }

data SourceFile = SourceFile
  { _fname :: FilePath
  , _fsrc :: Text
  } deriving (Show)

data Options = Options
  { _optionsVerbose :: !Bool
  , _fileName :: !FilePath
  } deriving (Show)

instance HasLogFunc CompilerState where
  logFuncL = lens _logF (\x y -> x {_logF = y})

-------------------------------------------------------------------------------
-- AST
-------------------------------------------------------------------------------

-- Pre-Desugar

-- Functions will be converted to lambdas, so there are no bindings just parameters
data ScSyn
  = ScDecl Decl
  | ScExpr Expr
  deriving (Show, Generic, Typeable)

instance Un.Alpha ScSyn

data Decl
  = FunDecl Params BodyKind
  | FunDotDecl Params BodyKind
  | FunListDecl Params BodyKind
  | VarDecl Name Expr
  deriving (Show, Generic, Typeable)

instance Un.Alpha Decl


type Params = [Name]

data Expr
  = EApp Application    -- (+ 1 2)
  | EVar Name           -- +
  | ELam Lambda         -- (lambda xs (apply + xs))
  | ELet Let           -- (let ((x 21)) (+ x x))
  | EIf Expr Expr Expr  -- (if test then else)
  | ESet Name Expr
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

type PrimName = String

data SynExtension
  = ECond CondBody      -- (cond ((#t) (io) 'true) (else 'false))
  | ECase Expr CaseBody -- (case (+ 2 2) ((4) 'true) (else 'false))
  | EOr (Maybe [Expr])  -- (or)
  | EAnd (Maybe [Expr]) -- (and)
  | EBegin [Expr]       -- (begin (io) 'true)
  | LetStar [(Name, Expr)] BodyKind
  | LetRec [(Name, Expr)] BodyKind
  deriving (Show, Generic, Typeable)


instance Un.Alpha SynExtension

type CondBody
  = [(Expr, BodyKind)] -- (test expr1 expr2 ...)

type CaseBody
  = [([Literal], BodyKind)] -- (test expr1 expr2 ...)

data Lambda
  = Lam (Un.Bind [Name] BodyKind)
  | LamDot (Un.Bind ([Name], Name) BodyKind)
  | LamList (Un.Bind Name BodyKind)
  deriving (Show, Generic, Typeable)

instance Un.Alpha Lambda

newtype Let
  = Let (Un.Bind [(Name, Un.Embed Expr)] BodyKind)
  deriving (Show, Generic, Typeable)

instance Un.Alpha Let

data BodyKind
  = BSingle Expr
  | BMultiple [Expr]
  deriving (Show, Generic, Typeable)

instance Un.Alpha BodyKind


data Literal
  = LitString String
  | LitSymbol String
  | LitInt Integer
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

descend :: (Expr -> Expr) -> Expr -> Expr
descend f exp = Un.runFreshM (descendM (return . f) exp)

descendM :: (Monad m, Un.Fresh m) => (Expr -> m Expr) -> Expr -> m Expr
descendM f e = case e of
   EApp app -> EApp <$> descendApplicationM f app
   EVar name -> pure (EVar name)
   ELam lambda -> ELam <$> descendLambdaM f lambda
   ELet letb -> ELet <$> descendLetM f letb
   EIf tst thn els -> EIf <$> descendM f tst <*> descendM f thn <*> descendM f els
   ELit lit -> f $ ELit lit
   -- ESynExt ext -> descendSynExt ext

descendApplicationM :: (Monad m, Un.Fresh m) => (Expr -> m Expr) -> Application -> m Application
descendApplicationM f e = case e of
  AppPrim primName expr -> AppPrim primName <$> mapM (descendM f) expr
  AppLam exprhd expr -> AppLam <$> descendM f exprhd <*> mapM (descendM f) expr


descendLambdaM :: (Monad m, Un.Fresh m) => (Expr -> m Expr) -> Lambda -> m Lambda
descendLambdaM f e = case e of
  Lam bnd -> do
    (pats, body) <- Un.unbind bnd
    Lam . Un.bind pats <$> descendBodyKindM f body
  LamDot bnd -> do
    ((pats, pat), body) <- Un.unbind bnd
    LamDot . Un.bind (pats, pat) <$> descendBodyKindM f body
  LamList bnd -> do
    (pat, body) <- Un.unbind bnd
    LamList . Un.bind pat <$> descendBodyKindM f body

descendBodyKindM :: (Monad m, Un.Fresh m) => (Expr -> m Expr) -> BodyKind -> m BodyKind
descendBodyKindM f e = case e of
  BSingle e -> BSingle <$> descendM f e
  BMultiple es -> BMultiple <$> sequence (descendM f <$> es)


descendLetM :: (Monad m, Un.Fresh m) => (Expr -> m Expr) -> Let -> m Let
descendLetM f e = case e of
  Let bnd -> do
    (patlist, bodyKind) <- Un.unbind bnd
    let newpatlist =
          (fmap . fmap . fmap) Un.Embed $ (mapM . mapM) f (fmap (\(n, Un.Embed e) -> (n, e)) patlist)
    let newbodyKind = descendBodyKindM f bodyKind
    Let <$> (Un.bind <$> newpatlist <*> newbodyKind)


descendSynExtensionM :: (Monad m, Un.Fresh m) => (Expr -> m Expr) -> SynExtension -> m SynExtension
descendSynExtensionM f e = case e of
  EOr mes -> EOr <$> (mapM . mapM) (descendM f) mes
  EAnd mes -> EAnd <$> (mapM . mapM) (descendM f) mes
  EBegin es -> EBegin <$> mapM (descendM f) es
  ECond condBody ->
    do
      let ml = sequence $ do
            (e, bodyKind) <- condBody
            return $ (,) <$> descendM f e <*> descendBodyKindM f bodyKind
      ECond <$> ml
  ECase cse caseBody ->
    do
      let ml = sequence $ do
            (e, bodyKind) <- caseBody
            return $ (e,) <$> descendBodyKindM f bodyKind
      ECase <$> descendM f cse <*> ml
  LetStar bindings body -> do
    let newpatlist = (mapM . mapM) f bindings
    let newbodyKind = descendBodyKindM f body
    LetStar <$> newpatlist <*> newbodyKind

  LetRec bindings body -> do
    let newpatlist = (mapM . mapM) f bindings
    let newbodyKind = descendBodyKindM f body
    LetRec <$> newpatlist <*> newbodyKind

compose
  :: (Expr -> Expr)
  -> (Expr -> Expr)
  -> (Expr -> Expr)
compose f g = descend (f . g)

