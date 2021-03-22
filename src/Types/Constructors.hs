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

makeLam :: (ToBody b, ToName n) => [n] -> b -> Expr
makeLam names b = toExpr $ Lam $ Un.bind (toName <$> names) $ toBody b

makeLamDot :: (ToBody b, ToName n, ToName n2) => [n] -> n2 -> b -> Expr
makeLamDot names name b = toExpr $ LamDot $ Un.bind (toName <$> names, toName name) $ toBody b

makeLamList :: (ToBody b, ToName n) => n -> b -> Expr
makeLamList name b = toExpr $ LamList $ Un.bind (toName name) $ toBody b

makeFunDecl :: (ToBody b, ToName n, ToName n2) => n -> [n2] -> b -> Decl
makeFunDecl n ps b = FunDecl (toName n) (toName <$> ps) $ toBody b

makeFunDotDecl :: (ToBody b, ToName n, ToName n2, ToName n3) => n -> [n2] -> n3 -> b -> Decl
makeFunDotDecl n ps p b = FunDotDecl (toName n) (toName <$> ps) (toName p) $ toBody b

makeFunListDecl :: (ToBody b, ToName n, ToName n2) => n -> n2 -> b -> Decl
makeFunListDecl n p b = FunListDecl (toName n) (toName p) $ toBody b

makeLet :: (ToBinding b, ToBody b2) => b -> b2 -> Expr
makeLet bindings body =
  let bindings' = toBinding bindings in
    ELet $ Let $ Un.bind bindings' (toBody body)

makeIf3 :: Expr -> Expr -> Expr -> Expr
makeIf3 = EIf

makeIf2 :: Expr -> Expr -> Expr
makeIf2 tst thn = EIf tst thn (toExpr makeUnspecified)

makeSet :: (ToName n) => n -> Expr -> Expr
makeSet n = ESet (toName n)

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

makeVarDecl :: (ToName n) => n -> Expr -> Decl
makeVarDecl t = VarDecl (toName t)

makeOr :: Maybe [Expr] -> Expr
makeOr = ESynExt . EOr

makeAnd :: Maybe [Expr] -> Expr
makeAnd = ESynExt . EAnd

makeBegin :: [Expr] -> Expr
makeBegin = ESynExt . EBegin

car :: Expr -> Expr
car e = makePrimApp "car" [e]

cdr :: Expr -> Expr
cdr e = makePrimApp "cdr" [e]

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
