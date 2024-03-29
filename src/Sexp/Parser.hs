{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings, QuasiQuotes #-}
-- | Parses S-expressions into its own datatype

module Sexp.Parser (Sexp.Parser.parse, Parser) where

import RIO hiding (try)

import Text.Megaparsec as M hiding (runParser)
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative as A
import Text.RawString.QQ
import Types.Types (ScEnv (), Sexp(..), Env(..), SourceFile(..))
import Types.Pprint
import Prelude (putStrLn, print)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Parser = Parsec Void Text

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
rawAtom = takeWhile1P (Just "Atom") isAtom
  where
    isAtom = (`notElem` ['\n', '\t', '(', ')', ' '])

stringAtom :: Parser Text
stringAtom =  inQuotes $ fmap enquote (takeWhileP (Just "SAtom") isStringAtom)
  where
    inQuotes = between (char '\"') (char '\"')
    enquote = ("\"" <>) . (<> "\"")
    isStringAtom = (`notElem` ['\n', '\"'])

list :: Parser Sexp -> Parser Sexp
list ps = List <$> parens (M.many ps) -- many encount for empty list () (not nil)

modifier :: Parser (Sexp -> Sexp)
modifier = do
  fs <- M.many $ choice shortcuts
  return $ foldr (.) id fs
  where
    shortcuts :: [Parser (Sexp -> Sexp)]
    shortcuts = [quote, qq, uqs, uq]

sexp :: Parser Sexp
sexp = try modifier <*> choice [vec, list sexp, atom]

vec :: Parser Sexp
vec = try $ lexeme $ fmap (\(List l) -> List $ Atom"vec":l) (char '#' *> list sexp)

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
-- Quotes
-------------------------------------------------------------------------------
quote :: Parser (Sexp -> Sexp)
quote = shortcut "'" "quote"

-------------------------------------------------------------------------------
-- Quasiquotes
-------------------------------------------------------------------------------
-- ` => quasiquote
qq :: Parser (Sexp -> Sexp)
qq = shortcut "`" "quasiquote"

-- , => unquote
uq :: Parser (Sexp -> Sexp)
uq = shortcut "," "unquote"

-- @ => unquote-splicing
uqs :: Parser (Sexp -> Sexp)
uqs = shortcut ",@" "unquote-splicing"

-------------------------------------------------------------------------------
-- Reader helper
-------------------------------------------------------------------------------
shortcut :: Text -> Text -> Parser (Sexp -> Sexp)
shortcut short long =  string short $> (\sxp -> List[Atom long, sxp])

-------------------------------------------------------------------------------
-- Reading and transforming the source file into symbolic expressions
-------------------------------------------------------------------------------

parse :: ScEnv ()
parse = do
  logInfo "Parsing raw text into symbolic expressions"
  sexps <- asks _sexps
  SourceFile{_fname, _fsrc} <- asks _file


  liftIO $ case runParser _fname _fsrc of
    Left err -> do
      print err
      exitFailure
    Right sxps -> do
      writeSomeRef sexps sxps

  sxps <- readSomeRef sexps
  logDebug $ "Parsed symbolic expressions:\n" <> display sxps

  return ()

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

appT :: Text
appT = [r| ((lambda (x) (+ x 42))) |]

qqT :: Text
qqT = [r| `(1 2 3 4) |]

qq2T :: Text
qq2T = [r| `(1 2 3 ,(+ 1 2)) |]

qq3T :: Text
qq3T = [r| `(1 ,@(0)) |]

qq4T :: Text
qq4T = [r| `(1 2 3 ,@(1 2) ,(+ 1 2) ',name) |]

test = parseTest sexps

df :: Text
df = [r|
(define f #f)
(define (fak n)
    (if (< n 2 )
    (call-with-current-continuation
      (lambda (c) (set! f c) 1))
    (let ((decn (dec n)))
        (let ((fakdecn (fak decn)))
        (* n facdekn)))))
(fak 5)
|]
