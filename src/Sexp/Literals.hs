-- | Literals
{-# LANGUAGE OverloadedStrings  #-}
module Sexp.Literals where

import RIO

import Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative as A
import Sexp.Parser (Parser)
import RIO.Partial (fromJust)

import Text.Regex.TDFA as Re

-- Not R5RS conforming
isIdent :: Text -> Bool
isIdent = (Re.=~ ("^[a-zA-Z][a-zA-Z0-9]*$" :: String))

isName :: Text -> Bool
isName t = not (isInt t) && not (isFloat t) && not (isString t) && not (isChar t) && not (isBool t)

isInt :: Text -> Bool
isInt = (Re.=~ ("^-?[0-9]+$" :: String))

isFloat :: Text -> Bool
isFloat = A.liftA2 (||)
          (Re.=~ ("^-?[0-9]+\\.[0-9]*$" :: String))
          (Re.=~ ("^-?\\.[0-9]+$" :: String))

isString :: Text -> Bool
isString = (Re.=~ ("^\\\"[a-zA-Z0-9 ]*\\\"$" :: String))

isChar :: Text -> Bool
isChar = A.liftA3 or3
         (Re.=~ ("^#\\\\.$" :: String))
         (Re.=~ ("^#\\\\newline$" :: String))
         (Re.=~ ("^#\\\\space$" :: String))
  where
    or3 :: Bool -> Bool -> Bool -> Bool
    or3 o1 o2 o3 = o1 || o2 || o3

isBool :: Text -> Bool
isBool = A.liftA2 (||)
          (Re.=~ ("^#t$" :: String))
          (Re.=~ ("^#f$" :: String))



-- Partial functions are bad
parseLiteral :: Parser a -> Text -> a
parseLiteral p t = fromJust $ parseMaybe p t

isLiteral :: Parser a -> Text -> Bool
isLiteral p t = isJust $ parseMaybe p t

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

varLiteral :: Parser String
varLiteral =  A.many L.charLiteral

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
    true = string "#t" >> return True
    false = string "#f" >> return False
