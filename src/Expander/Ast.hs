{-# LANGUAGE PatternSynonyms #-}
-- |

module Expander.Ast where

import RIO

data Stree
  = LitString String
  | LitSymbol Symbol
  | LitInt Int
  | LitFloat Float
  | LitChar Char
  | LitBool Bool
  | MStree MStree
  | LitList [Stree]
  | LitVec [Stree]
  | LitNil
  | Deleted
  deriving (Show, Eq)

data MStree
  = SynExtId Symbol
  | SynExtApp Symbol [Stree]
  deriving (Show, Eq)


type SchemeList = [Stree]
type Symbol = String


isConst :: Stree -> Bool
isConst (LitString _) = True
isConst (LitSymbol _) = True
isConst (LitInt _) = True
isConst (LitChar _) = True
isConst (LitBool _) = True
isConst (LitVec _) = True
isConst _ = False

pattern Sym s = LitSymbol s
pattern Sxp l = LitList l
pattern DefineSyntax name transformerSpec = Sxp [Sym "define-syntax", Sym name, transformerSpec]
pattern Define args body  = Sxp [Sym "define", args, body]
pattern Lambda args body = Sxp [Sym "lambda", args, body]
pattern Let binds body = Sxp [Sym "let", Sxp binds, body]
pattern Letrec binds body = Sxp [Sym "letrec", Sxp binds, body]
pattern Bind var expr = Sxp [Sym var, expr]
pattern App hd tl = Sxp (hd:tl)
pattern Or es = Sxp (Sym "or":es)
pattern And es = Sxp (Sym "and":es)
pattern Begin es = Sxp (Sym "begin":es)
pattern If tst thn els = Sxp [Sym "if", tst, thn, els]
pattern T = Sym "#t"
pattern F = Sym "#f"
