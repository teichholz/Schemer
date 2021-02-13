-- |

module Literals where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative as A
import SParser

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
