{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Pretty Printer

module Types.Pprint (pretty, display) where

import RIO
import Types.Types
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

mkLet :: [(Name, Expr)] -> Body -> Doc ann
mkLet bindings body =
  inParens $ "let" <+> mkBinding bindings <> pretty body
  where
    mkBind :: (,) Name Expr -> Doc ann
    mkBind (n, e) = mkList [pretty n, pretty e]
    mkBinding ::  [(Name, Expr)] -> Doc ann
    mkBinding bnds = mkVertList $ fmap mkBind bnds

mkParamList :: [Name] -> Doc ann
mkParamList names = mkList $ pretty <$> names

mkParamListDot :: [Name] -> Name -> Doc ann
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
  pretty (LitSymbol sym) = unsafeViaShow sym
  pretty (LitList list) = mkList $ pretty <$> list
  pretty LitNil = inParens emptyDoc
  pretty (LitVector vec) = "#" <> pretty (LitList vec)
  pretty LitUnspecified  = "unspecified"

instance Pretty Body where
  pretty b =  doIndent $ align (vsep $ pretty <$> unBody b)

instance Pretty PrimName where
  pretty (PName (schemeName, _)) = pretty schemeName

instance Pretty Let where
  pretty (Let bnds body) = mkLet bnds body

instance Pretty Lambda where
  pretty (Lam ps body) = inParens ("lambda" <+> mkParamList ps <> pretty body)

  pretty (LamDot (ps, p) body) = inParens ("lambda" <+> mkParamListDot ps p <> pretty body)

  pretty (LamList p body) = inParens ("lambda" <+> pretty p <> pretty body)

instance Pretty Application where
  pretty (AppPrim n es) =
    inParens (pretty n <> doIndent (vertDocs $ pretty <$> es))
  pretty (AppLam e es) =
    inParens (pretty e <> doIndent (vertDocs $ pretty <$> es))

instance Pretty ScSyn where
  pretty (ScDecl d) = pretty d
  pretty (ScExpr e) = pretty e

instance Pretty Expr where
  pretty (EApp app) = pretty app
  pretty (EVar name) = pretty name
  pretty (ELam lam) = pretty lam
  pretty (ELet lt) = pretty lt
  pretty (EIf tst thn els) =
    inParens ("if" <> doIndent (pretty tst) <> doIndent (pretty thn) <> doIndent (pretty els))
  pretty (ESet n e) =
    inParens ("set!" <> doIndent (pretty n) <> doIndent (pretty e))
  pretty (ELit lit) = pretty lit
  pretty (ESynExt _) = "Syntactic Extension"


instance Pretty Decl where
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

instance Display ScSyn where
  textDisplay = textDisplay . pretty

instance Display String where
  textDisplay = textDisplay . pretty
