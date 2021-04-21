{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings, QuasiQuotes #-}
-- | Parses S-expressions into its own datatype

module Sexp.Parser (Sexp(..), Sexp.Parser.runParser, Parser) where

import RIO hiding (try)
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

atom :: Parser Sexp
atom = Atom <$> lexeme (choice [stringAtom, rawAtom]) -- stringAtom has higher precedence than rawAtom

rawAtom :: Parser Text
rawAtom = takeWhile1P (Just "S-expression atom character") isAtom

stringAtom :: Parser Text
stringAtom = char '\"' *> fmap (("\"" <>) . (<> "\"")) (takeWhileP (Just "S-expression string character") isStringAtom) <* char '\"'

isAtom :: Char -> Bool
isAtom = (`notElem` ['\n', '\t', '(', ')', ' ', '#', '\''])

isStringAtom :: Char -> Bool
isStringAtom = (`notElem` ['\n', '\"'])

list :: Parser Sexp -> Parser Sexp
list ps = List <$> parens (M.some ps)

sexp :: Parser Sexp
sexp = choice $ fmap try [nil, quoted vec, quoted' atom, quoted' (list sexp)]

quoted :: Parser Sexp -> Parser Sexp
quoted p = fmap (\sxp -> List[Atom "quote", sxp]) (char '\'' *> p)

quoted' :: Parser Sexp -> Parser Sexp
quoted' p = try (quoted p) <|> p

vec :: Parser Sexp
vec = lexeme $ fmap (\(List l) -> List $ Atom"vec":l) (char '#' *> list sexp)

nil :: Parser Sexp
nil = lexeme $ fmap (const $ List[]) (string "'()")

parens :: Parser a -> Parser a
parens = between lpar rpar

lpar :: Parser Text
lpar = symbol "("
rpar :: Parser Text
rpar = symbol ")"

sexp' :: Parser Sexp
sexp' = sc *> sexp <* sc

sexps :: Parser [Sexp]
sexps = sepBy sexp' sc <* eof

runParser :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Sexp]
runParser = M.parse sexps



-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

txt :: Text
txt = [r|
(define   x   (+ 2   4  )   )
(+ x 2)

|]

nilT :: Text
nilT = [r|'()|]

listT :: Text
listT = [r|'(1 2 3 4)|]

vecT :: Text
vecT = [r|'#(1 2 3 4)|]

defT :: Text
defT =[r|
  (foldl rcons '() lst)
|]

symT :: Text
symT = [r|'hey|]

letT :: Text
letT = [r|(let ((x '()))
             x)|]

test = parseTest sexps
