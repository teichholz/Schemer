{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Foldable
import LLVM (File(File))
import Data.Monoid

-- Main datatype as a: ReaderT Env IO a
type ScEnv a = RIO Env a

data Env = Env
  { _file :: SourceFile
  , _sexps :: SomeRef [Sexp]
  , _toplevel :: SomeRef [ScSyn Name] -- Top level Scheme syntax
  , _ast      :: SomeRef (ScSyn Name)  -- Scheme syntax
  , _procs :: SomeRef [Proc UniqName]
  , _options :: Options -- CLI options / arguments
  , _name :: String -- Name of this awesome compiler
  , _outputFile :: File
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
-- Sexp
-------------------------------------------------------------------------------
data Sexp =
    Atom Text
  | List [Sexp]
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- AST
-------------------------------------------------------------------------------

-- dummy AST
dummy :: ScSyn Name
dummy = ScExpr $ ELit LitUnspecified

type Name = ByteString

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

newtype Proc a = Proc { unProc :: (a, Expr a) }
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
newtype PrimName = PName {unPName :: (ByteString, ByteString)}
  deriving stock (Show, Generic)
  deriving (Semigroup) via ((,) ByteString ByteString)

instance Eq PrimName where
  PName (n, _) == PName (n', _) = n == n'


-- These represent methods needed by the runtime itself.
-- These don't have an actual Scheme counterpart which is used for pretty printing.
-- Example: halt (RT), no Scheme counterpart
newtype PrimName' = PName' {unPName' :: ByteString}
  deriving (Show)


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
  deriving (Show, Eq, Generic, Typeable)

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
  fv :: Ord a => e a -> S.Set a

instance FreeVars ScSyn where
  fv (ScDecl d) = fv d
  fv (ScExpr e) = fv e

instance FreeVars Body where
  fv (Body b) = S.unions $ fmap fv b

instance FreeVars Decl where
  fv (FunDecl _ ps b) = fv b S.\\ S.unions (fmap S.singleton ps)
  fv (FunDotDecl _ ps p b) = fv b S.\\ S.unions (fmap S.singleton (p:ps))
  fv (FunListDecl _ p b) = fv b S.\\ S.singleton p
  fv (VarDecl _ e) = fv e

instance FreeVars Expr where
  fv (EApp app) = fv app
  fv (EVar name) = S.singleton name
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
  fv (AppPrim _ es) = S.unions (fmap fv es)
  fv (AppLam e es) = fv e `S.union` S.unions (fmap fv es)

instance FreeVars Lambda where
  fv (Lam ps b) =  fv b S.\\ S.unions (fmap S.singleton ps)
  fv (LamDot (ps, p) b) =  fv b S.\\ S.unions (fmap S.singleton (p:ps))
  fv (LamList p b) = fv b S.\\ S.singleton p

instance FreeVars Let where
  fv (Let [(var, exp)] body) = (fv body S.\\ S.singleton var) `S.union` fv exp
  fv (Let bs body) = (fv body S.\\ S.unions (fmap (S.singleton . fst) bs)) `S.union` S.unions (fmap (fv . snd) bs)

-------------------------------------------------------------------------------
-- All vars calculation
-------------------------------------------------------------------------------

-- The AST is parameterized over it's type of variables. This means Foldable can be used to get all of them.
av :: (Foldable e, Ord a) => e a -> S.Set a
av = foldl' go S.empty
  where
    go :: Ord a => S.Set a -> a -> S.Set a
    go sa a = S.union sa (S.singleton a)

-------------------------------------------------------------------------------
-- Alphatization
-------------------------------------------------------------------------------

data UniqName = UName Name Int
  deriving (Show, Eq, Ord)

type NameMap = M.Map Name UniqName
type Counter = Int

makeUniqName :: Name -> Int -> UniqName
makeUniqName = UName

unUniqName :: UniqName -> Name
unUniqName (UName n _) = n

indexOfUniqName :: UniqName -> Int
indexOfUniqName (UName _ i) = i

addUniqName :: Name -> UniqName -> NameMap -> NameMap
addUniqName = M.insert

runAlpha :: Alphatization e =>  e Name -> e UniqName
runAlpha e = evalState (alpha e) (0, M.empty)

unAlpha :: Functor e => e UniqName -> e Name
unAlpha = fmap (\(UName n _) -> n)

callWithAlpha :: (Alphatization e, Functor e) => (e UniqName -> e UniqName) -> e Name -> e Name
callWithAlpha f = unAlpha . f . runAlpha

callWithAlphaM :: (Alphatization e, Functor e, Monad m) => (e UniqName -> m (e UniqName)) -> e Name -> m (e Name)
callWithAlphaM f = fmap unAlpha . f . runAlpha


class Alphatization e where
  alpha :: e Name -> State (Counter, NameMap) (e UniqName)

-- Use -1 as part of the UniqName, if the name is free in the whole ast. This should only happen for primnames in identifier positions.
getUN :: Name -> State (Counter, NameMap) UniqName
getUN n = do
  map <- gets snd
  let muniq = n `M.lookup` map
      uniq = fromMaybe (makeUniqName n (-1)) muniq
  return uniq

instance Alphatization ScSyn where
  alpha (ScExpr e) = ScExpr <$> alpha e

instance Alphatization Expr where
  alpha (EApp app) = EApp <$> alpha app
  alpha (EIf tst thn els) = do
    (_, map) <- get
    tst' <- withState (fmap $ const map) (alpha tst)
    thn' <- withState (fmap $ const map) (alpha thn)
    els' <- withState (fmap $ const map) (alpha els)
    return $ EIf tst' thn' els'

  alpha (EVar n) = do
    uniq <- getUN n
    return $ EVar uniq
  alpha (ELam lam) = ELam <$> alpha lam
  alpha (ELet lt) = ELet <$> alpha lt
  alpha (ESet n e) = do
    uniq <- getUN n
    ESet uniq <$> alpha e
  alpha (EApply apply) = EApply <$> alpha apply
  alpha (ECallCC e) = ECallCC <$> alpha e
  alpha (ELit l) = return $ ELit l

instance Alphatization Application where
  alpha (AppPrim n es) = do
    (_, map) <- get
    es' <- forM es $ \e -> withState (fmap $ const map) (alpha e)
    return $ AppPrim n es'
  alpha (AppLam e es) = do
    (_, map) <- get
    hd <- withState (fmap $ const map) (alpha e)
    tl <- forM es $ \e -> withState (fmap $ const map) (alpha e)
    return $ AppLam hd tl

instance Alphatization Apply where
  alpha (ApplyPrim n e) = ApplyPrim n <$> alpha e
  alpha (ApplyLam e1 e2) = do
    (_, map) <- get
    e1' <- withState (fmap $ const map) (alpha e1)
    e2' <- withState (fmap $ const map) (alpha e2)
    return $ ApplyLam e1' e2'

instance Alphatization Body where
  alpha (Body es) = do
    (_, map) <- get
    es' <- forM es $ \e -> withState (fmap $ const map) (alpha e)
    return $ Body es'

instance Alphatization Let where
  alpha (Let [(n, e)] b) = do
    (cnt, map) <- get
    e' <- alpha e
    modify (fmap $ const map)
    let uniq = makeUniqName n cnt
        newmap = addUniqName n uniq map
        newbind = [(uniq, e')]
    put (cnt + 1, newmap)
    b' <- alpha b
    return $ Let newbind b'

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
