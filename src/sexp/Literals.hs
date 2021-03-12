-- |
{-# LANGUAGE OverloadedStrings  #-}
module Literals where

import RIO
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative as A
import SParser

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

charLiteral :: Parser Char
charLiteral = string "#\\" >> (asciiChar <|> newline <|> space)
  where
    newline = string "newline" >> return '\n'
    space = string "space" >> return ' '

intLiteral :: Parser Int
intLiteral = L.signed A.empty L.decimal

floatLiteral :: Parser Float
floatLiteral = L.signed A.empty L.float

boolLiteral :: Parser Bool
boolLiteral = true <|> false
  where
    true = string "#\t" >> return True
    false = string "#\f" >> return False
