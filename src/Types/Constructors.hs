-- |

module Types.Constructors where

import RIO
import RIO.Text (unpack, pack)
import Types.Types
import qualified Unbound.Generics.LocallyNameless as Un
import qualified Utils.NameResolver as NR

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

makePrimApp :: PrimName -> [Expr] -> Expr
makePrimApp name exprs = toExpr $ AppPrim (NR.getCname name) exprs

makeLamApp :: Expr -> [Expr] -> Expr
makeLamApp expr exprs = toExpr $ AppLam expr exprs

makeVar :: String -> Expr
makeVar = EVar . makeName . pack

makeName :: Text -> Name
makeName = Un.s2n . unpack

makeLam :: ToBody b => [Name] -> b -> Expr
makeLam names b = toExpr $ Lam $ Un.bind names $ toBody b

makeLamDot :: ToBody b => [Name] -> Name -> b -> Expr
makeLamDot names name b = toExpr $ LamDot $ Un.bind (names, name) $ toBody b

makeLamList :: ToBody b => Name -> b -> Expr
makeLamList name b = toExpr $ LamList $ Un.bind name $ toBody b

makeLetT :: [(Name, Expr)] -> Body -> Expr
makeLetT bindings body =
  ELet $ Let $ Un.bind ((fmap . fmap) Un.Embed bindings) body

makeLet :: ToBody b =>  [(Name, Expr)] -> b -> Expr
makeLet bindings b = makeLetT bindings $ toBody b

makeIf3 :: Expr -> Expr -> Expr -> Expr
makeIf3 = EIf

makeIf2 :: Expr -> Expr -> Expr
makeIf2 tst thn = EIf tst thn (toExpr makeUnspecified)

makeSet :: Name -> Expr -> Expr
makeSet = ESet

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

makeFunDecl :: Name -> [Name] -> Body -> Decl
makeFunDecl = FunDecl

makeFunDotDecl :: Name -> [Name] -> Name -> Body -> Decl
makeFunDotDecl = FunDotDecl

makeFunListDecl :: Name -> Name -> Body -> Decl
makeFunListDecl = FunListDecl

makeOr :: Maybe [Expr] -> Expr
makeOr = ESynExt . EOr

makeAnd :: Maybe [Expr] -> Expr
makeAnd = ESynExt . EAnd

makeBegin :: [Expr] -> Expr
makeBegin = ESynExt . EBegin
