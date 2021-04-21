{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- |
{-# LANGUAGE FlexibleInstances #-}



module Types.Constructors where

import RIO
import Types.Types
import qualified Utils.NameResolver as NR
import RIO.List.Partial ( head, foldr1 )
import RIO.Lens as L
import qualified RIO.Set as S
import qualified RIO.Text
import qualified Data.Text as T
import Data.Foldable (maximum)
import GHC.Enum (succ)



-------------------------------------------------------------------------------
-- Helper classes
-------------------------------------------------------------------------------

-- To understand these, check out:
-- - https://wiki.haskell.org/Functional_dependencies
-- - https://www.fpcomplete.com/haskell/tutorial/fundeps/ (extra)
class ToSyn t a | t -> a where
  toSyn :: t -> ScSyn a

class ToExpr t a | t -> a where
  toExpr :: t -> Expr a

class ToDecl t a | t -> a where
  toDecl :: t -> Decl a

class ToBody t a | t -> a where
  toBody :: t -> Body a

class ToBinding t a | t -> a where
  toBinding :: t -> Binding a

class ToName t where
  toName :: t -> Name

class ToPrimName t where
  toPrimName :: t -> PrimName

-- ToSyn

instance ToSyn (Decl a) a where
  toSyn = ScDecl

instance ToSyn (Expr a) a where
  toSyn = ScExpr

instance ToSyn (Application a) a where
  toSyn = toSyn . toExpr

instance ToSyn Name Name where
  toSyn = toSyn . toExpr

instance ToSyn (Lambda a) a where
  toSyn = toSyn . toExpr

instance ToSyn (Let a) a where
  toSyn = toSyn . toExpr

instance ToSyn Literal Name where
  toSyn = toSyn . toExpr

-- ToExpr
instance ToExpr (Expr a) a where
  toExpr = id

instance ToExpr [Expr a] a where
  toExpr [e] = e

instance ToExpr (ScSyn a) a where
  toExpr (ScExpr e) = e

instance ToExpr [ScSyn a] a where
  toExpr (ScExpr e:_) = e

instance ToExpr (Body a) a where
  toExpr (Body ss) = toExpr ss

instance ToExpr (Application a) a where
  toExpr = EApp

instance ToExpr (Apply a) a where
  toExpr = EApply

instance ToExpr (Lambda a) a where
  toExpr = ELam

instance ToExpr (Let a) a where
  toExpr = ELet

instance ToExpr Literal Name where
  toExpr = ELit

instance ToExpr Name Name where
  toExpr = EVar

instance ToExpr UniqName UniqName where
  toExpr = EVar

instance ToExpr String Name where
  toExpr = EVar . toName

-- -- ToDecl
instance ToDecl (ScSyn a) a where
  toDecl (ScDecl d) = d

-- -- ToBody
instance ToBody (Body a) a where
  toBody = id

instance ToBody (Expr a) a where
  toBody e = Body $ toSyn <$> [e]

instance ToBody [Expr a] a where
  toBody e = Body $ toSyn <$> e

instance ToBody Literal Name where
  toBody = toBody . toExpr

instance ToBody [Literal] Name where
  toBody = toBody . fmap toExpr

instance ToBody (ScSyn a) a where
  toBody e = Body [e]

instance ToBody [ScSyn a] a where
  toBody = Body

-- -- ToBinding
instance (ToExpr e a) => ToBinding (a, e) a where
  toBinding (n, e) = [(n, toExpr e)]

-- instance (ToExpr e UniqName) => ToBinding (n, e) UniqName where
--   toBinding (n, e) = [(n, toExpr e)]

instance (ToName n, ToExpr e Name) => ToBinding [(n, e)] Name where
  toBinding = fmap (bimap toName toExpr)


instance ToName Name where
  toName = id

instance ToName Int where
  toName = fromString . show

instance ToName String  where
  toName = fromString

instance ToName Text where
  toName = encodeUtf8

instance IsString (Expr Name) where
  fromString = toExpr . toName

instance IsString UniqName where
  fromString s = makeUniqName (toName s) (-1)

instance IsString PrimName where
  fromString s = toPrimName (fromString s :: ByteString)

instance IsString PrimName' where
  fromString = PName' . fromString

-- -- ToPrimName
instance ToPrimName PrimName where
  toPrimName = id

instance ToPrimName PrimName' where
  toPrimName (PName' n) = PName (n, n)

instance ToPrimName Name where
  toPrimName s = PName (s, NR.getCname s)

instance ToPrimName UniqName where
  toPrimName (UName s _) = PName (s, NR.getCname s)

instance ToPrimName Text where
  toPrimName = toPrimName . toName

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

makePrimApp :: ToPrimName n => n -> [Expr a] -> Expr a
makePrimApp n exprs = toExpr $ AppPrim (toPrimName n) exprs

makeLamApp :: Expr a -> [Expr a] -> Expr a
makeLamApp expr exprs = toExpr $ AppLam expr exprs

makePrimApply :: ToPrimName n => n -> Expr a -> Expr a
makePrimApply n expr = toExpr $ ApplyPrim (toPrimName n) expr

makeLamApply :: Expr a -> Expr a -> Expr a
makeLamApply expr1 expr2 = toExpr $ ApplyLam expr1 expr2

makeVar :: String -> Expr Name
makeVar = EVar . makeName . toName

makeName :: ByteString -> Name
makeName = id

makeLam :: (ToBody b n) => [n] -> b -> Expr n
makeLam names b = toExpr $ Lam  names (toBody b)

makeLamDot :: (ToBody b Name, ToName n, ToName n2) => [n] -> n2 -> b -> Expr Name
makeLamDot names name b = toExpr $ LamDot (toName <$> names, toName name) (toBody b)

makeLamList :: ToBody b n => n -> b -> Expr n
makeLamList name b = toExpr $ LamList name (toBody b)

-- -- prepends the newarg variable to the argument list
extendLamList :: (ToExpr n n, ToBody b n) => n -> n -> b -> Expr n
extendLamList newarg oldarg body =
  makeLamList oldarg
    (makeLet (newarg, car $ toExpr oldarg)
      (makeLet (oldarg, cdr $ toExpr oldarg) (toBody body)))

makeFunDecl :: (ToBody b Name, ToName n, ToName n2) => n -> [n2] -> b -> Decl Name
makeFunDecl n ps b = FunDecl (toName n) (toName <$> ps) $ toBody b

makeFunDotDecl :: (ToBody b Name, ToName n, ToName n2, ToName n3) => n -> [n2] -> n3 -> b -> Decl Name
makeFunDotDecl n ps p b = FunDotDecl (toName n) (toName <$> ps) (toName p) $ toBody b

makeFunListDecl :: (ToBody b Name, ToName n, ToName n2) => n -> n2 -> b -> Decl Name
makeFunListDecl n p b = FunListDecl (toName n) (toName p) $ toBody b

makeLet :: (ToBinding b a, ToBody b2 a) => b -> b2 -> Expr a
makeLet bindings body =
  let bindings' = toBinding bindings in
    ELet $ Let bindings' (toBody body)

makeIf3 :: Expr a -> Expr a -> Expr a -> Expr a
makeIf3 = EIf

makeIf2 :: Expr Name -> Expr Name -> Expr Name
makeIf2 tst thn = EIf tst thn (toExpr makeUnspecified)

makeSet :: (ToName n) => n -> Expr Name -> Expr Name
makeSet n = ESet (toName n)

makeCallCC :: Expr a -> Expr a
makeCallCC = ECallCC

makeLiteral :: Literal -> Expr Name
makeLiteral = ELit

makeString :: String -> Expr Name
makeString = toExpr . LitString

makeSymbol :: String -> Expr Name
makeSymbol = toExpr . LitSymbol

makeInt :: Int -> Expr Name
makeInt = toExpr . LitInt

makeFloat :: Float -> Expr Name
makeFloat = toExpr . LitFloat

makeChar :: Char -> Expr Name
makeChar = toExpr . LitChar

makeBool :: Bool -> Expr Name
makeBool = toExpr . LitBool

makeList :: [Literal] -> Expr Name
makeList = toExpr . LitList

makeVector :: [Literal] -> Expr Name
makeVector = toExpr . LitVector

makeNil :: Expr Name
makeNil =  toExpr LitNil

makeUnspecified :: Expr Name
makeUnspecified =  toExpr LitUnspecified

makeExp :: Expr a -> ScSyn a
makeExp = ScExpr

makeDecl :: Decl a -> ScSyn a
makeDecl = ScDecl

makeVarDecl :: (ToName n) => n -> Expr Name -> Decl Name
makeVarDecl t = VarDecl (toName t)

cons :: Expr a -> Expr a -> Expr a
cons car cdr = makePrimApp ("cons" :: PrimName) [car, cdr]

car :: Expr a -> Expr a
car e = makePrimApp ("car" :: PrimName) [e]

cdr :: Expr a -> Expr a
cdr e = makePrimApp ("cdr" :: PrimName) [e]

cadr :: Int -> Expr a -> Expr a
cadr cnt e = car $ fix (\rec n -> if n < 1 then e else cdr (rec (n - 1))) cnt

makeConsList :: [Expr a] -> Expr a
makeConsList = foldr cons (ELit LitNil)

makeConsList' :: [Expr a] -> Expr a
makeConsList' = foldr1 cons

makeVectorFromList :: [Expr a] -> Expr a
makeVectorFromList es = makePrimApp ("list2vector" :: PrimName) [makeConsList es]

vectorRef :: Expr a -> Int -> Expr a
vectorRef e i = makePrimApp ("vector-ref" :: PrimName) [e, ELit $ LitInt i]

vectorSet :: Expr a -> Int -> Expr a -> Expr a
vectorSet lhs i rhs   = makePrimApp ("vector-set!" :: PrimName) [lhs, ELit $ LitInt i, rhs]




-- if' :: Bool -> a -> a -> a
-- if' True  x _ = x
-- if' False _ y = y

-- -------------------------------------------------------------------------------
-- -- Utils
-- -------------------------------------------------------------------------------

isDecl :: ScSyn a -> Bool
isDecl (ScDecl _) = True
isDecl _ = False

isExpr :: ScSyn a -> Bool
isExpr (ScExpr _) = True
isExpr _ = False

isLamApp :: Expr a -> Bool
isLamApp (EApp (AppLam _ _)) = True
isLamApp _ = False

makeName' :: ByteString -> Int -> Name
makeName' s i = toName $ s <> fromString (show i)


makeUniqueName :: Foldable e => ByteString -> e Name -> Name
makeUniqueName n e =
  let vars = av e
      notTakenP = \n -> not $ S.member n vars in
    if notTakenP n then
      toName n
    else
      toName $ head $ filter notTakenP (fmap (makeName' n) [0..])

makeUniqueName' :: Foldable e => ByteString -> [e Name] -> Name
makeUniqueName' n e =
  let vars = S.unions (fmap av e) in
    if S.null vars then
      toName n
    else
      toName $ head $ filter (\n -> not $ S.member n vars)
                             (fmap (makeName' n) [0..])

makeGloballyUniqueName :: (Foldable e, Functor e) => Name -> e UniqName -> UniqName
makeGloballyUniqueName n e =
  let vars =  av e
      n' = makeUniqueName n (unAlpha e)
      low = succ $ indexOfUniqName (maximum e)
  in
     head $ filter (\n -> not $ S.member n vars)
                   (fmap (makeUniqName n') [low..])
