{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings, QuasiQuotes #-}
-- | Parses S-expressions into its own datatype

module Sexp.Parser (Sexp(..), Sexp.Parser.runParser, Parser) where

import RIO
import Text.Megaparsec as M
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative as A
import Text.RawString.QQ

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

symbol :: Text -> Parser Text
symbol = L.symbol sc
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

whites :: Parser ()
whites = sc

atom :: Parser Sexp
atom = Atom <$> lexeme rawAtom

rawAtom :: Parser Text
rawAtom = takeWhile1P (Just "S-expression atom character") isAtom

isAtom :: Char -> Bool
isAtom = (`notElem` ['\n', '\t', '(', ')', ' '])

sexp :: Parser Sexp
sexp = sc *> ((List <$> parens (M.some sexp)) <|> atom) <* sc

parens :: Parser a -> Parser a
parens = between lpar rpar

lpar :: Parser Text
lpar = symbol "("
rpar :: Parser Text
rpar = symbol ")"

sexps :: Parser [Sexp]
sexps = sepBy sexp sc <* eof

runParser :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Sexp]
runParser = M.parse sexps

txt :: Text
txt = [r|
(define   x   (+ 2   4  )   )
(+ x 2)

|]

test = parseTest sexps txt
