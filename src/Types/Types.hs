{-# LANGUAGE DeriveTraversable #-}


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}
-- | Compiler types

module Types.Types  where

import RIO hiding (void)
import RIO.State
import qualified RIO.Map as M
import qualified RIO.Map.Partial as MP
import qualified RIO.Set as S
import qualified Data.Text as T
import Data.Data (cast)


-- Main datatype as a: ReaderT Env IO a
type ScEnv a = RIO Env a

data Env = Env
  { _file :: SourceFile
  , _ast      :: SomeRef (ScSyn Name)  -- Scheme syntax
  , _toplevel :: [ScSyn Name] -- Top level Scheme syntax
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
dummy :: ScSyn Name
dummy = ScExpr $ ELit LitUnspecified

type Name = Text

type SynN = ScSyn Name
type ExprN = Expr Name
type BodyN = Body Name
type BindN = Binding Name
type AppN = Application Name
type LamN = Lambda Name
type LetN = Let Name

data ScSyn a
  = ScDecl (Decl a)
  | ScExpr (Expr a)
  deriving (Show, Generic, Typeable, Functor, Foldable, Traversable)

data Decl a
  = FunDecl a (Params a) (Body a)
  | FunDotDecl a (Params a) (Param a) (Body a)
  | FunListDecl a (Param a) (Body a)
  | VarDecl a (Expr a)
  deriving (Show, Generic, Typeable, Functor, Foldable, Traversable)

type Param a = a
type Params a = [a]

data Expr a
  = EApp (Application a)    -- (+ 1 2)
  | EVar a           -- +
  | ELam (Lambda a)         -- (lambda xs (apply + xs))
  | ELet (Let a)           -- (let ((x 21)) (+ x x))
  | EIf (Expr a) (Expr a) (Expr a)  -- (if test then else)
  | ESet a (Expr a)
  | EApply (Apply a)
  | ECallCC (Expr a)
  | ELit Literal -- '(1 2 3 4), ...
  deriving (Show, Generic, Typeable, Functor, Foldable, Traversable)

data Apply a
  = ApplyPrim PrimName (Expr a)
  | ApplyLam (Expr a) (Expr a)
  deriving (Show, Generic, Typeable, Functor, Foldable, Traversable)

data Application a
  = AppPrim PrimName [Expr a]
  | AppLam (Expr a) [Expr a]
  deriving (Show, Generic, Typeable, Functor, Foldable, Traversable)

-- Scheme name and RT (Runtime) name
-- Example: + (Scheme) and plus (RT)
newtype PrimName = PName {unPName :: (String, String)}
  deriving (Show, Generic)

-- These represent methods needed by the runtime itself.
-- These don't have an actual Scheme counterpart which is used for pretty printing.
-- Example: halt (RT), no Scheme counterpart
newtype PrimName' = PName' {unPName' :: String}
  deriving (Show, Generic)

data Lambda a
  = Lam [a] (Body a)
  | LamDot ([a], a) (Body a)
  | LamList a (Body a)
  deriving (Show, Generic, Typeable, Functor, Foldable, Traversable)

type Binding a = [(a, Expr a)]

data Let a = Let [(a, Expr a)] (Body a)
  deriving (Show, Generic, Typeable, Functor, Foldable, Traversable)

newtype Body a = Body { unBody :: [ScSyn a]}
  deriving (Show, Generic, Typeable, Functor, Foldable, Traversable)

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

-- data SynExtension
--   = ECond CondBody      -- (cond ((#t) (io) 'true) (else 'false))
--   | ECase Expr CaseBody -- (case (+ 2 2) ((4) 'true) (else 'false))
--   | EOr (Maybe [Expr])  -- (or)
--   | EAnd (Maybe [Expr]) -- (and)
--   | EBegin [Expr]       -- (begin (io) 'true)
--   | LetStar [(Name, Expr)] Body
--   | LetRec [(Name, Expr)] Body
--   deriving (Show, Generic, Typeable)


-- type CondBody
--   = [(Expr, Body)] -- (test expr1 expr2 ...)

-- type CaseBody
--   = [([Literal], Body)] -- (test expr1 expr2 ...)

-- Post-Desugar

data Core
  = Void


-------------------------------------------------------------------------------
-- Traversal
-------------------------------------------------------------------------------

-- Mapper gives acces to the function needed to transform declaration or expressions
data (a :*: b) = D a b
type Mapper m a = (Expr a -> m (Expr a)) :*: (Decl a -> m (Decl a))

-- Alias for MakeMapExpr
makeMap ::(Monad m)  => (Expr a -> m (Expr a)) -> Mapper m a
makeMap = makeMapExpr

makeMapExpr :: (Monad m)  => (Expr a -> m (Expr a)) -> Mapper m a
makeMapExpr f = D f return

makeMapDecl :: Monad m => (Decl a -> m (Decl a)) -> Mapper m a
makeMapDecl = D return

makeMap' :: (Expr a -> m (Expr a)) -> (Decl a -> m (Decl a)) -> Mapper m a
makeMap' = D

mapDecl :: Mapper m a -> Decl a -> m (Decl a)
mapDecl (D _ f) = f

mapExpr :: Mapper m a -> Expr a -> m (Expr a)
mapExpr (D f _) = f

descend :: (Expr a -> Expr a) -> ScSyn a -> ScSyn a
descend f exp = runIdentity (descendM (makeMap (return . f)) exp)

runDescendM :: Monad m => (m (ScSyn a) -> ScSyn a) -> (Expr a -> m (Expr a))  -> ScSyn a -> ScSyn a
runDescendM run f syn = run (descendM (makeMap f) syn)

descendM :: (Monad m) => Mapper m a -> ScSyn a -> m (ScSyn a)
descendM f syn = case syn of
  ScExpr e -> ScExpr <$> descendExprM f e
  ScDecl d -> ScDecl <$> descendDeclM f d

descendDeclM :: (Monad m) => Mapper m a -> Decl a -> m (Decl a)
descendDeclM f d = mapDecl f =<< case d of
  VarDecl n e -> VarDecl n <$> descendExprM f e
  FunDecl n p b -> FunDecl n p <$> descendBodyM f b
  FunDotDecl n ps p b -> FunDotDecl n ps p <$> descendBodyM f b
  FunListDecl n p b -> FunListDecl n p <$> descendBodyM f b

descendExprM :: (Monad m) => Mapper m a -> Expr a -> m (Expr a)
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

descendApplyM :: (Monad m) => Mapper m a -> Apply a -> m (Apply a)
descendApplyM f e = case e of
  ApplyPrim primName expr -> ApplyPrim primName <$> descendExprM f expr
  ApplyLam exprhd expr -> ApplyLam <$> descendExprM f exprhd <*> descendExprM f expr

descendApplicationM :: (Monad m) => Mapper m a -> Application a -> m (Application a)
descendApplicationM f e = case e of
  AppPrim primName expr -> AppPrim primName <$> mapM (descendExprM f) expr
  AppLam exprhd expr -> AppLam <$> descendExprM f exprhd <*> mapM (descendExprM f) expr


descendLambdaM :: (Monad m) => Mapper m a -> Lambda a -> m (Lambda a)
descendLambdaM f e = case e of
  Lam pats body ->
    Lam pats <$> descendBodyM f body
  LamDot (pats, pat) body ->
    LamDot (pats, pat) <$> descendBodyM f body
  LamList pat body ->
    LamList pat <$> descendBodyM f body

descendBodyM :: (Monad m) => Mapper m a -> Body a -> m (Body a)
descendBodyM f b = Body <$> mapM (descendM f) (unBody b)


descendLetM :: (Monad m) => Mapper m a -> Let a -> m (Let a)
descendLetM f e = case e of
  Let patlist bodyKind -> do
    let newpatlist = (mapM . mapM) (descendExprM f) patlist
    let newbodyKind = descendBodyM f bodyKind
    liftA2 Let newpatlist newbodyKind



-------------------------------------------------------------------------------
-- Free vars calculation
-------------------------------------------------------------------------------
class FreeVars e where
  fv :: e -> S.Set Name

instance FreeVars a => FreeVars [a] where
  fv = S.unions . fmap fv

instance FreeVars Name where
  fv = S.singleton

instance FreeVars a => FreeVars (ScSyn a) where
  fv (ScDecl d) = fv d
  fv (ScExpr e) = fv e

instance FreeVars a => FreeVars (Body a) where
  fv (Body b) = fv b

instance FreeVars a => FreeVars (Decl a) where
  fv (FunDecl _ ps b) = fv ps  S.\\ fv b
  fv (FunDotDecl _ ps p b) = fv (p:ps) S.\\ fv b
  fv (FunListDecl _ p b) = fv p S.\\ fv b
  fv (VarDecl _ e) = fv e

instance FreeVars a => FreeVars (Expr a) where
  fv (EApp app) = fv app
  fv (EVar name) = fv name
  fv (ELam lam) = fv lam
  fv (ELet lt) = fv lt
  fv (EIf tst thn els) = S.unions [fv tst, fv thn, fv els]
  fv (ESet _ e) = fv e
  fv (EApply apply) = fv apply
  fv (ECallCC e) = fv e
  fv (ELit _) = S.empty

instance FreeVars a => FreeVars (Apply a) where
  fv (ApplyPrim _ e) = fv e
  fv (ApplyLam e1 e2) = fv e1 `S.union` fv e2

instance FreeVars a => FreeVars (Application a) where
  fv (AppPrim _ es) = fv es
  fv (AppLam e es) = fv (e:es)

instance FreeVars a => FreeVars (Lambda a) where
  fv (Lam ps b) = fv ps  S.\\ fv b
  fv (LamDot (ps, p) b) = fv (p:ps) S.\\ fv b
  fv (LamList p b) = fv p S.\\ fv b

instance FreeVars a => FreeVars (Let a) where
  fv (Let bind body) = fv (fst <$> bind) S.\\ fv body

-------------------------------------------------------------------------------
-- Alphatization
-------------------------------------------------------------------------------

-- fst = unique name, snd = scheme name
data UniqName = UName Name Int
  deriving (Show, Eq)
type NameMap = M.Map Name UniqName
type Counter = Int

makeUniqName :: Name -> Int -> UniqName
makeUniqName = UName

addUniqName :: Name -> UniqName -> NameMap -> NameMap
addUniqName = M.insert

runAlpha :: ScSyn Name -> ScSyn UniqName
runAlpha syn = evalState (alpha syn) (0, M.empty)

unAlpha :: ScSyn UniqName -> ScSyn Name
unAlpha = fmap (\(UName n _) -> n)

class Alphatization e where
  alpha :: e Name -> State (Counter, NameMap) (e UniqName)

instance Alphatization ScSyn where
  alpha (ScExpr e) = ScExpr <$> alpha e

instance Alphatization Expr where
  alpha (EApp app) = EApp <$> alpha app
  alpha (EIf tst thn els) = liftA3 EIf (alpha tst) (alpha thn) (alpha els)
  alpha (EVar n) = do
    map <- gets snd
    let uniq = map MP.! n
    return $ EVar uniq
  alpha (ELam lam) = ELam <$> alpha lam
  alpha (ELet lt) = ELet <$> alpha lt
  alpha (ESet n e) = do
    map <- gets snd
    let uniq = map MP.! n
    ESet uniq <$> alpha e
  alpha (EApply apply) = EApply <$> alpha apply
  alpha (ECallCC e) = ECallCC <$> alpha e
  alpha (ELit l) = return $ ELit l

instance Alphatization Application where
  alpha (AppPrim n es) = AppPrim n <$> mapM alpha es
  alpha (AppLam e es) = liftA2 AppLam (alpha e) (mapM alpha es)

instance Alphatization Apply where
  alpha (ApplyPrim n e) = ApplyPrim n <$> alpha e
  alpha (ApplyLam e1 e2) = liftA2 ApplyLam (alpha e1) (alpha e2)

instance Alphatization Body where
  alpha (Body es) = Body <$> mapM alpha es

instance Alphatization Let where
  alpha (Let [(n, e)] b) = do
    (cnt, map) <- get
    let uniq = makeUniqName n cnt
        newmap = addUniqName n uniq map
    let newbind = (mapM . mapM) alpha [(uniq, e)]
        newbody = alpha b
    put (cnt + 1, newmap)
    liftA2 Let newbind newbody

instance Alphatization Lambda where
  alpha (Lam ps b) = do
    ps' <- mapM f ps
    Lam ps' <$> alpha b
    where
      f :: Name -> State (Counter, NameMap) UniqName
      f p = do
        (cnt, map) <- get
        let uniq = makeUniqName p cnt
            newmap = addUniqName p uniq map
        put (cnt + 1, newmap)
        return uniq

  alpha (LamList p b) = do
    (cnt, map) <- get
    let uniq = makeUniqName p cnt
        newmap = addUniqName p uniq map
    put (cnt + 1, newmap)
    LamList uniq <$> alpha b
