-- |
{-# LANGUAGE FlexibleInstances #-}


{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Constructors where

import RIO
import RIO.Text (unpack, pack)
import Types.Types
import qualified Utils.NameResolver as NR
import RIO.List.Partial (head)
import RIO.Lens as L
import qualified RIO.Set as S
import qualified RIO.Text
import qualified Data.Text as T



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

class ToBinding t where
  toBinding :: t -> Binding

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

instance ToExpr [Expr] where
  toExpr [e] = e

instance ToExpr ScSyn where
  toExpr (ScExpr e) = e

instance ToExpr [ScSyn] where
  toExpr (ScExpr e:_) = e

instance ToExpr Body where
  toExpr (Body ss) = toExpr ss

instance ToExpr Application where
  toExpr = EApp

instance ToExpr Apply where
  toExpr = EApply

instance ToExpr Lambda where
  toExpr = ELam

instance ToExpr Let where
  toExpr = ELet

instance ToExpr Literal where
  toExpr = ELit

instance ToExpr Name where
  toExpr = EVar

instance ToExpr String where
  toExpr = EVar . toName

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

-- ToBinding
instance (ToName n, ToExpr e) => ToBinding (n, e) where
  toBinding (n, e) = [(toName n, toExpr e)]

instance (ToName n, ToExpr e) => ToBinding [(n, e)] where
  toBinding = fmap (bimap toName toExpr)

-- instance ToBind ()

-- ToName
instance ToName String where
  toName = T.pack

-- instance ToName Text where
--   toName = toName . RIO.Text.unpack

instance ToName Name where
  toName = id

instance ToName Utf8Builder  where
  toName = toName . textDisplay

-- IsString for OverloadedStrings pragma
-- instance IsString Name where
--   fromString = toName

instance IsString Expr where
  fromString = EVar . toName

instance IsString PrimName where
  fromString = toPrimName

instance IsString PrimName' where
  fromString = PName'

-- ToPrimName
instance ToPrimName PrimName where
  toPrimName = id

instance ToPrimName PrimName' where
  toPrimName (PName' n) = PName (n,n)

instance ToPrimName String where
  toPrimName s = PName (s, NR.getCname s)

instance ToPrimName Text where
  toPrimName = toPrimName . unpack


-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

makePrimApp :: ToPrimName n => n -> [Expr] -> Expr
makePrimApp n exprs = toExpr $ AppPrim (toPrimName n) exprs

makeLamApp :: Expr -> [Expr] -> Expr
makeLamApp expr exprs = toExpr $ AppLam expr exprs

makePrimApply :: ToPrimName n => n -> Expr -> Expr
makePrimApply n expr = toExpr $ ApplyPrim (toPrimName n) expr

makeLamApply :: Expr -> Expr -> Expr
makeLamApply expr1 expr2 = toExpr $ ApplyLam expr1 expr2

makeVar :: String -> Expr
makeVar = EVar . makeName . pack

makeName :: Text -> Name
makeName = id

makeLam :: (ToBody b, ToName n, IsString n) => [n] -> b -> Expr
makeLam names b = toExpr $ Lam  (toName <$> names) (toBody b)

makeLamDot :: (ToBody b, ToName n, ToName n2) => [n] -> n2 -> b -> Expr
makeLamDot names name b = toExpr $ LamDot (toName <$> names, toName name) (toBody b)

makeLamList :: (ToBody b, ToName n) => n -> b -> Expr
makeLamList name b = toExpr $ LamList (toName name) (toBody b)

-- prepends the newarg variable to the argument list
extendLamList :: (ToName n, ToExpr n, ToBody b) => n -> n -> b -> Expr
extendLamList newarg oldarg body =
  makeLamList oldarg
    (makeLet (newarg, car $ toExpr oldarg)
      (makeLet (oldarg, cdr $ toExpr oldarg) (toBody body)))

makeFunDecl :: (ToBody b, ToName n, ToName n2) => n -> [n2] -> b -> Decl
makeFunDecl n ps b = FunDecl (toName n) (toName <$> ps) $ toBody b

makeFunDotDecl :: (ToBody b, ToName n, ToName n2, ToName n3) => n -> [n2] -> n3 -> b -> Decl
makeFunDotDecl n ps p b = FunDotDecl (toName n) (toName <$> ps) (toName p) $ toBody b

makeFunListDecl :: (ToBody b, ToName n, ToName n2) => n -> n2 -> b -> Decl
makeFunListDecl n p b = FunListDecl (toName n) (toName p) $ toBody b

makeLet :: (ToBinding b, ToBody b2) => b -> b2 -> Expr
makeLet bindings body =
  let bindings' = toBinding bindings in
    ELet $ Let bindings' (toBody body)

makeIf3 :: Expr -> Expr -> Expr -> Expr
makeIf3 = EIf

makeIf2 :: Expr -> Expr -> Expr
makeIf2 tst thn = EIf tst thn (toExpr makeUnspecified)

makeSet :: (ToName n) => n -> Expr -> Expr
makeSet n = ESet (toName n)


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

isLamApp :: Expr -> Bool
isLamApp (EApp (AppLam _ _)) = True
isLamApp _ = False

makeName' :: String -> Int -> Name
makeName' s i = toName $ s <> show i

makeUniqueName :: (FreeVars e) => String -> e -> Name
makeUniqueName n e =
  let frees =  fv e in
    toName $ head $ filter (\n -> not $ S.member n frees)
                           (fmap (makeName' n) [0..])
