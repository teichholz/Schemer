{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | Parses S-expressions into its own datatype

module SParser (SParser.parse, Sexp, Parser) where

import Text.Megaparsec as M
import Data.Void as V
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative as A


type Parser = Parsec Void String

data SexpA d =
    AtomA (Xa d) String
  | ListA (Xl d) [SexpA d]

type Sexp = SexpA UD

-- extension descriptors and type families
data UD
type family Xa d
type family Xl d
type instance Xa UD = Void
type instance Xl UD = Void

pattern Atom str <- AtomA _ str
  where Atom str = AtomA UD str

pattern List list <- ListA _ list
  where List list = ListA UD list


sc :: Parser ()
sc = L.space
  space1
  (choice [L.skipLineComment ";", L.skipLineComment ";;"])
  A.empty

symbol = L.symbol sc
lexeme = L.lexeme sc

is_atom :: Char -> Bool
is_atom char = not $ elem char ['\n', '\t', '(', ')', ' ']

raw_atom :: Parser String
raw_atom = takeWhile1P (Just "S-expression atom character") is_atom

atom :: Parser Sexp
atom = Atom <$> lexeme raw_atom

list :: Parser Sexp
list = (List <$> parens (M.some list)) <|> atom

parens :: Parser a -> Parser a
parens = between lpar rpar

lpar = symbol "("
rpar = symbol ")"

parse :: Parser Sexp
parse = list


-- Predicates for datatypes

