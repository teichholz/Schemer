{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
-- | Parses S-expressions into its own datatype

module SParser (Sexp(..), SParser.runParser, Parser) where

import RIO
import Text.Megaparsec as M
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative as A


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
type Parser = Parsec Void Text

data Sexp =
    Atom Text
  | List [Sexp]
  deriving (Show)

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------
sc :: Parser ()
sc = L.space
  space1
  (choice [L.skipLineComment ";", L.skipLineComment ";;"])
  A.empty

symbol = L.symbol sc
lexeme = L.lexeme sc

isAtom :: Char -> Bool
isAtom = (`notElem` ['\n', '\t', '(', ')', ' '])

rawAtom :: Parser Text
rawAtom = takeWhile1P (Just "S-expression atom character") isAtom

atom :: Parser Sexp
atom = Atom <$> lexeme rawAtom

list :: Parser Sexp
list = (List <$> parens (M.some list)) <|> atom

parens :: Parser a -> Parser a
parens = between lpar rpar

lpar :: Parser Text
lpar = symbol "("
rpar :: Parser Text
rpar = symbol ")"

sexpParser :: Parser Sexp
sexpParser = list

runParser :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Sexp
runParser = M.parse sexpParser
