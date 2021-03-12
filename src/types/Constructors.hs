-- |

module Constructors where

import RIO
import Types
import qualified Unbound.Generics.LocallyNameless as Un
import qualified NameResolver as NR

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

makePrimApp :: PrimName -> [Expr] -> Expr
makePrimApp name exprs = EApp $ AppPrim (NR.getCname name) exprs

makeLamApp :: Expr -> [Expr] -> Expr
makeLamApp expr exprs = EApp $ AppLam expr exprs

makeVar :: Name -> Expr
makeVar = EVar

makeLam :: [Name] -> Expr -> Expr
makeLam names expr = ELam $ Lam $ Un.bind names (BSingle expr)

makeLamMultBods :: [Name] -> [Expr] -> Expr
makeLamMultBods names exprs = ELam $ Lam $ Un.bind names (BMultiple exprs)

makeLamDot :: [Name] -> Name -> Expr -> Expr
makeLamDot names name expr = ELam $ LamDot $ Un.bind (names, name) (BSingle expr)

makeLamDotMultBods :: [Name] -> Name -> [Expr] -> Expr
makeLamDotMultBods names name exprs = ELam $ LamDot $ Un.bind (names, name) (BMultiple exprs)

makeLamList :: Name -> Expr -> Expr
makeLamList name expr = ELam $ LamList $ Un.bind name (BSingle expr)

makeLamListMultBods :: Name -> [Expr] -> Expr
makeLamListMultBods name exprs = ELam $ LamList $ Un.bind name (BMultiple exprs)

makeLetT :: [(Name, Expr)] -> BodyKind -> Expr
makeLetT bindings body =
  ELet $ Let $ Un.bind ((fmap . fmap) Un.Embed bindings) body

makeLet :: [(Name, Expr)] -> Expr -> Expr
makeLet bindings expr = makeLetT bindings (BSingle expr)

makeLetN :: [(Name, Expr)] -> [Expr] -> Expr
makeLetN bindings exprs = makeLetT bindings (BMultiple exprs)

makeLetStarT :: [(Name, Expr)] -> BodyKind -> Expr
makeLetStarT bindings body =
  ESynExt $ LetStar bindings body

makeLetStar :: [(Name, Expr)] -> Expr -> Expr
makeLetStar bindings expr = makeLetStarT bindings (BSingle expr)

makeLetStarN :: [(Name, Expr)] -> [Expr] -> Expr
makeLetStarN bindings exprs = makeLetStarT bindings (BMultiple exprs)

makeLetRecT :: [(Name, Expr)] -> BodyKind -> Expr
makeLetRecT bindings body =
  ESynExt $ LetRec bindings body

makeLetRec :: [(Name, Expr)] -> Expr -> Expr
makeLetRec bindings expr = makeLetRecT bindings (BSingle expr)

makeLetRecN :: [(Name, Expr)] -> [Expr] -> Expr
makeLetRecN bindings exprs = makeLetRecT bindings (BMultiple exprs)

makeIf3 :: Expr -> Expr -> Expr -> Expr
makeIf3 = EIf

makeIf2 :: Expr -> Expr -> Expr
makeIf2 tst thn = EIf tst thn (ELit LitUnspecified)

makeLiteral :: Literal -> Expr
makeLiteral = ELit
