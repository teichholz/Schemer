{-# LANGUAGE OverloadedStrings #-}
-- | Parses S-expressions into its own datatype

module SParser where

import Text.Megaparsec as M
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative as A


type Parser = Parsec Void String

data Sexp =
    Atom String
  | List [Sexp]
  deriving (Show, Eq)

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




-- Predicates for datatypes

string_literal :: Parser String
string_literal = char '"' >> manyTill L.charLiteral (char '"')

char_literal :: Parser Char
char_literal = string "#\\" >> (asciiChar <|> newline <|> space)
  where
    newline = string "newline" >> return '\n'
    space = string "space" >> return ' '

int_literal :: Parser Int
int_literal = L.signed A.empty L.decimal

float_literal :: Parser Float
float_literal = L.signed A.empty L.float

bool_literal :: Parser Bool
bool_literal = true <|> false
  where
    true = string "#\t" >> return True
    false = string "#\f" >> return False
