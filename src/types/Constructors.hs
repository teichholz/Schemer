-- |

module Constructors where

import RIO
import RIO.Text (unpack, pack)
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

makeVar :: String -> Expr
makeVar = EVar . makeName . pack

makeName :: Text -> Name
makeName = Un.s2n . unpack

makeLam :: [Name] -> ScSyn -> Expr
makeLam names expr = ELam $ Lam $ Un.bind names (BSingle expr)

makeLamMultBods :: [Name] -> [ScSyn] -> Expr
makeLamMultBods names exprs = ELam $ Lam $ Un.bind names (BMultiple exprs)

makeLamDot :: [Name] -> Name -> ScSyn -> Expr
makeLamDot names name expr = ELam $ LamDot $ Un.bind (names, name) (BSingle expr)

makeLamDotMultBods :: [Name] -> Name -> [ScSyn] -> Expr
makeLamDotMultBods names name exprs = ELam $ LamDot $ Un.bind (names, name) (BMultiple exprs)

makeLamList :: Name -> ScSyn -> Expr
makeLamList name expr = ELam $ LamList $ Un.bind name (BSingle expr)

makeLamListMultBods :: Name -> [ScSyn] -> Expr
makeLamListMultBods name exprs = ELam $ LamList $ Un.bind name (BMultiple exprs)

makeLetT :: [(Name, Expr)] -> BodyKind -> Expr
makeLetT bindings body =
  ELet $ Let $ Un.bind ((fmap . fmap) Un.Embed bindings) body

makeMultBody :: [ScSyn] -> BodyKind
makeMultBody = BMultiple

makeBody :: ScSyn -> BodyKind
makeBody = BSingle

makeLet :: [(Name, Expr)] -> ScSyn -> Expr
makeLet bindings expr = makeLetT bindings (BSingle expr)

makeLetN :: [(Name, Expr)] -> [ScSyn] -> Expr
makeLetN bindings exprs = makeLetT bindings (BMultiple exprs)

makeLetStarT :: [(Name, Expr)] -> BodyKind -> Expr
makeLetStarT bindings body =
  ESynExt $ LetStar bindings body

makeLetStar :: [(Name, Expr)] -> ScSyn -> Expr
makeLetStar bindings expr = makeLetStarT bindings (BSingle expr)

makeLetStarN :: [(Name, Expr)] -> [ScSyn] -> Expr
makeLetStarN bindings exprs = makeLetStarT bindings (BMultiple exprs)

makeLetRecT :: [(Name, Expr)] -> BodyKind -> Expr
makeLetRecT bindings body =
  ESynExt $ LetRec bindings body

makeLetRec :: [(Name, Expr)] -> ScSyn -> Expr
makeLetRec bindings expr = makeLetRecT bindings (BSingle expr)

makeLetRecN :: [(Name, Expr)] -> [ScSyn] -> Expr
makeLetRecN bindings exprs = makeLetRecT bindings (BMultiple exprs)

makeCond :: CondBody -> Expr
makeCond condBody = ESynExt $ ECond condBody

makeIf3 :: Expr -> Expr -> Expr -> Expr
makeIf3 = EIf

makeIf2 :: Expr -> Expr -> Expr
makeIf2 tst thn = EIf tst thn (ELit LitUnspecified)

makeSet :: Text -> Expr -> Expr
makeSet n = ESet (makeName n)

makeApply :: Expr -> Expr -> Expr
makeApply = EApply

makeCallCC :: Expr -> Expr
makeCallCC = ECallCC

makeLiteral :: Literal -> Expr
makeLiteral = ELit

makeString :: String -> Literal
makeString = LitString

makeSymbol :: String -> Literal
makeSymbol = LitSymbol

makeInt :: Int -> Literal
makeInt = LitInt

makeFloat :: Float -> Literal
makeFloat = LitFloat

makeChar :: Char -> Literal
makeChar = LitChar

makeBool :: Bool -> Literal
makeBool = LitBool

makeList :: [Literal] -> Literal
makeList = LitList

makeVector :: [Literal] -> Literal
makeVector = LitVector

makeNil :: Literal
makeNil =  LitNil

makeUnspecified :: Literal
makeUnspecified =  LitUnspecified

makeExp :: Expr -> ScSyn
makeExp = ScExpr

makeDecl :: Decl -> ScSyn
makeDecl = ScDecl

makeVarDecl :: Text -> Expr -> Decl
makeVarDecl t = VarDecl (makeName t)

makeFunDecl :: Name -> [Name] -> BodyKind -> Decl
makeFunDecl = FunDecl

makeFunDotDecl :: Name -> [Name] -> Name -> BodyKind -> Decl
makeFunDotDecl = FunDotDecl

makeFunListDecl :: Name -> Name -> BodyKind -> Decl
makeFunListDecl = FunListDecl

makeOr :: Maybe [Expr] -> Expr
makeOr = ESynExt . EOr

makeAnd :: Maybe [Expr] -> Expr
makeAnd = ESynExt . EAnd

makeBegin :: [Expr] -> Expr
makeBegin = ESynExt . EBegin
