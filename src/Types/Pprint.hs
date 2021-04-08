{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Pretty Printer

module Types.Pprint (pretty, display) where

import RIO
import Types.Types
import Types.Constructors
import RIO.Text as T
import Data.Text.Prettyprint.Doc


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------
ind :: Int
ind = 2

inParens :: Doc ann -> Doc ann
inParens d = surround d "(" ")"

inQuotes :: Doc ann -> Doc ann
inQuotes = dquotes

quoted :: Doc ann -> Doc ann
quoted  = (squote<>)

mkList :: [Doc ann] -> Doc ann
mkList = inParens . sep

vertDocs :: [Doc ann] -> Doc ann
vertDocs = align . vsep

mkVertList :: [Doc ann] -> Doc ann
mkVertList = inParens . vertDocs

doIndent :: Doc ann -> Doc ann
doIndent d = nest ind (line <> d)

mkLet :: Pretty a => [(a, Expr a)] -> Body a -> Doc ann
mkLet bindings body =
  inParens $ "let" <+> mkBinding bindings <> pretty body
  where
    mkBind :: Pretty a => (,) a (Expr a) -> Doc ann
    mkBind (n, e) = mkList [pretty n, pretty e]
    mkBinding :: Pretty a => [(a, Expr a)] -> Doc ann
    mkBinding bnds = mkVertList $ fmap mkBind bnds

mkParamList :: Pretty a => [a] -> Doc ann
mkParamList names = mkList $ pretty <$> names

mkParamListDot :: Pretty a => [a] -> a -> Doc ann
mkParamListDot ns n = mkList $
  (pretty <$> ns) ++ [".", pretty n]

-------------------------------------------------------------------------------
-- Type class and instances
-------------------------------------------------------------------------------

instance Pretty Literal where
  pretty (LitBool True) =  "#t"
  pretty (LitBool False) = "#t"
  pretty (LitInt x) = unsafeViaShow x
  pretty (LitChar c) =
    pretty $ getChar c
    where
      getChar :: Char -> Text
      getChar ' ' = "#\\" <> "space"
      getChar '\n' = "#\\" <> "newline"
      getChar c = snoc "#\\" c
  pretty (LitString str) = inQuotes $ unsafeViaShow str
  pretty (LitSymbol sym) = pretty sym
  pretty (LitList list) = quoted $ mkList $ pretty <$> list
  pretty LitNil = quoted $ inParens emptyDoc
  pretty (LitVector vec) = quoted $ "#" <> pretty (LitList vec)
  pretty LitUnspecified  = "unspecified"

instance Pretty a => Pretty (Body a) where
  pretty b =  doIndent $ align (vsep $ pretty <$> unBody b)

instance Pretty PrimName where
  pretty (PName (schemeName, rtName)) = pretty (schemeName, rtName)

instance Pretty UniqName where
  pretty (UName n i) = pretty n <> "@" <> pretty i

instance Pretty a => Pretty (Let a) where
  pretty (Let bnds body) = mkLet bnds body

instance Pretty a => Pretty (Lambda a) where
  pretty (Lam ps body) = inParens ("lambda" <+> mkParamList ps <> pretty body)

  pretty (LamDot (ps, p) body) = inParens ("lambda" <+> mkParamListDot ps p <> pretty body)

  pretty (LamList p body) = inParens ("lambda" <+> pretty p <> pretty body)

instance Pretty a => Pretty (Application a) where
  pretty (AppPrim "car" [EApp (AppPrim "cdr" [EApp (AppPrim "cdr" [EApp (AppPrim "cdr" [EApp (AppPrim "cdr" [e])])])])]) =
    inParens (pretty ("caddddr" :: String) <+> doIndent (pretty e))
  pretty (AppPrim "car" [EApp (AppPrim "cdr" [EApp (AppPrim "cdr" [EApp (AppPrim "cdr" [e])])])]) =
    inParens (pretty ("cadddr" :: String) <+> doIndent (pretty e))
  pretty (AppPrim "car" [EApp (AppPrim "cdr" [EApp (AppPrim "cdr" [e])])]) =
    inParens (pretty ("caddr" :: String) <+> doIndent (pretty e))
  pretty (AppPrim "car" [EApp (AppPrim "cdr" [e])]) =
    inParens (pretty ("cadr" :: String) <+> doIndent (pretty e))
  pretty (AppPrim "cons" [car, EApp (AppPrim "cons" [cadr, EApp (AppPrim "cons" [caddr, EApp (AppPrim "cons" [cadddr, nil])])])]) =
    inParens $ pretty ("cons'" :: String) <> doIndent (vertDocs $ pretty <$> [car, cadr, caddr, caddr])
  pretty (AppPrim "cons" [car, EApp (AppPrim "cons" [cadr, EApp (AppPrim "cons" [caddr, nil])])]) =
    inParens $ pretty ("cons'" :: String) <> doIndent (vertDocs $ pretty <$> [car, cadr, caddr])
  pretty (AppPrim "cons" [car, EApp (AppPrim "cons" [cadr, nil])]) =
    inParens $ pretty ("cons'" :: String) <> doIndent (vertDocs $ pretty <$> [car, cadr])
  pretty (AppPrim "cons" [car, nil]) =
    inParens $ pretty ("cons'" :: String) <> doIndent (vertDocs $ pretty <$> [car])
  pretty (AppPrim n es) =
    inParens (pretty n <> doIndent (vertDocs $ pretty <$> es))
  pretty (AppLam e es) =
    inParens (pretty e <> doIndent (vertDocs $ pretty <$> es))

instance Pretty a => Pretty (Apply a) where
  pretty (ApplyPrim n e) =
    inParens (pretty n <> doIndent (vertDocs [pretty e]))
  pretty (ApplyLam e e') =
    inParens (pretty e <> doIndent (vertDocs [pretty e']))

instance Pretty a => Pretty (ScSyn a) where
  pretty (ScDecl d) = pretty d
  pretty (ScExpr e) = pretty e

instance Pretty a => Pretty (Expr a) where
  pretty (EApp app) = pretty app
  pretty (EVar name) = pretty name
  pretty (ELam lam) = pretty lam
  pretty (ELet lt) = pretty lt
  pretty (EIf tst thn els) =
    inParens ("if" <> doIndent (pretty tst) <> doIndent (pretty thn) <> doIndent (pretty els))
  pretty (ESet n e) =
    inParens ("set!" <> doIndent (pretty n) <> doIndent (pretty e))
  pretty (ELit lit) = pretty lit
  pretty (EApply app) = pretty app


instance Pretty a => Pretty (Decl a) where
  pretty (VarDecl n e) =
    inParens ("define" <+> vertDocs [pretty n, pretty e])
  pretty (FunDecl n ps b) =
    inParens ("define" <+> mkParamList (n:ps) <> pretty b)
  pretty (FunListDecl n p b) =
    inParens ("define" <+> pretty n <+> pretty p <> pretty b)
  pretty (FunDotDecl n ps p b) =
    inParens ("define" <+> mkParamListDot (n:ps) p <> pretty b)


-- Type class from RIO to display human readable text

instance Display (Doc a) where
  textDisplay = pack . show

instance Pretty a => Display (ScSyn a) where
  textDisplay = textDisplay . pretty

instance Display String where
  textDisplay = textDisplay . pretty
