-- |
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}

module Types.Constructors where

import RIO
import RIO.Text (unpack, pack)
import Types.Types
import qualified Unbound.Generics.LocallyNameless as Un
import qualified Utils.NameResolver as NR
import RIO.Lens as L
import qualified RIO.Set as S
import qualified RIO.Text



-------------------------------------------------------------------------------
-- Helper classes
-------------------------------------------------------------------------------

class ToSyn t where
  toSyn :: t -> ScSyn

class ToExpr t where
  toExpr :: t -> Expr

class ToDecl t where
  toDecl :: t -> Decl

class ToBody t where
  toBody :: t -> Body

class ToEmbed t where
  toEmbed :: t -> Un.Embed Expr

class ToBinding t where
  toBinding :: t -> Binding

class ToBind t where
  toBind :: t -> Un.Bind Binding Body

class ToName t where
  toName :: t -> Name

class ToPrimName t where
  toPrimName :: t -> PrimName

-- ToSyn

instance ToSyn Decl where
  toSyn = ScDecl

instance ToSyn Expr where
  toSyn = ScExpr

instance ToSyn Application where
  toSyn = toSyn . toExpr

instance ToSyn Name where
  toSyn = toSyn . toExpr

instance ToSyn Lambda where
  toSyn = toSyn . toExpr

instance ToSyn Let where
  toSyn = toSyn . toExpr

instance ToSyn Literal where
  toSyn = toSyn . toExpr

-- ToExpr
instance ToExpr Expr where
  toExpr = id

instance ToExpr ScSyn where
  toExpr (ScExpr e) = e

instance ToExpr [ScSyn] where
  toExpr (ScExpr e:_) = e

instance ToExpr Body where
  toExpr (Body ss) = toExpr ss

instance ToExpr (Un.Embed Expr) where
  toExpr (Un.Embed e) = e

instance ToExpr Application where
  toExpr = EApp

instance ToExpr Lambda where
  toExpr = ELam

instance ToExpr Let where
  toExpr = ELet

instance ToExpr Literal where
  toExpr = ELit

instance ToExpr Name where
  toExpr = EVar

-- ToDecl
instance ToDecl ScSyn where
  toDecl (ScDecl d) = d

-- ToBody
instance ToBody Body where
  toBody = id

instance ToBody Expr where
  toBody e = Body $ toSyn <$> [e]

instance ToBody [Expr] where
  toBody e = Body $ toSyn <$> e

instance ToBody Literal where
  toBody = toBody . toExpr

instance ToBody [Literal] where
  toBody = toBody . fmap toExpr

instance ToBody ScSyn where
  toBody e = Body [e]

instance ToBody [ScSyn] where
  toBody = Body

-- ToEmbed
instance ToEmbed Expr where
  toEmbed = Un.embed

instance ToEmbed [Expr] where
  toEmbed [e] = Un.embed e

instance ToEmbed (Un.Embed Expr) where
  toEmbed = id

-- ToBinding
instance (ToName n, ToEmbed e) => ToBinding (n, e) where
  toBinding (n, e) = [(toName n, toEmbed e)]

instance (ToName n, ToEmbed e) => ToBinding [(n, e)] where
  toBinding = fmap (bimap toName toEmbed)

-- instance ToBind ()

-- ToName
instance ToName String where
  toName = Un.s2n

instance ToName Text where
  toName = toName . RIO.Text.unpack

instance ToName Name where
  toName = id

-- ToPrimName
instance ToPrimName PrimName where
  toPrimName = id

instance ToPrimName String where
  toPrimName s = PName (s, NR.getCname s)

instance ToPrimName Text where
  toPrimName = toPrimName . unpack

-- Functor
instance Functor Un.Embed where
  fmap f (Un.Embed t) = Un.Embed $ f t


-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

makePrimApp :: ToPrimName n => n -> [Expr] -> Expr
makePrimApp n exprs = toExpr $ AppPrim (toPrimName n) exprs

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
car e = makePrimApp ("car" :: String) [e]

cdr :: Expr -> Expr
cdr e = makePrimApp ("cdr" :: String) [e]

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

isDecl :: ScSyn -> Bool
isDecl (ScDecl _) = True
isDecl _ = False

isExpr :: ScSyn -> Bool
isExpr (ScExpr _) = True
isExpr _ = False

mapBind f g b = Un.runFreshM $ do
        (binding, body) <- Un.unbind b
        let body' = g body
            binding' = f binding
        return $ Un.bind binding' body'

getFreeVars :: (Un.Alpha s) => s -> [Name]
getFreeVars = L.toListOf Un.fv

makeUniqueName :: (Un.Alpha s, ToName n) => n -> s -> Name
makeUniqueName n s =
  let frees = S.fromList $ Un.AnyName <$> getFreeVars s in
  Un.contLFreshM (Un.lfresh $ toName n) frees
