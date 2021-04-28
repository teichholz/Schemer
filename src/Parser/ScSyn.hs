{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms #-}
-- | Parsing Sexp into Expr

module Parser.ScSyn (parse) where

import RIO
import RIO.Text (unpack)
import Utils.NameResolver (isPrim)
import RIO.List.Partial (head, tail)
import RIO.State
import Types.Types
import Types.Constructors
import Types.Exceptions
import Sexp.Literals

-------------------------------------------------------------------------------
-- Parse action
-------------------------------------------------------------------------------

parse :: ScEnv ()
parse = do
  logInfo "Parsing symbolic expressions into Scheme toplevel syntax"
  sexpsref <- asks _sexps
  sexps <- readSomeRef sexpsref

  top <- liftIO $ forM sexps runParser

  toplevel <- asks _toplevel
  writeSomeRef toplevel top

  return ()

-------------------------------------------------------------------------------
-- Parse Sexps into toplevel Scheme expressions
-------------------------------------------------------------------------------

data Args
  = Normal [Name]
  | Dotted [Name] Name
  deriving (Show)

runParser :: Sexp -> IO (ScSyn Name)
runParser = \case
  List(Atom "define":tl) -> makeDecl <$> parseDecl tl
  e -> makeExp <$> parseExpr e

parseL :: [Sexp] -> IO [ScSyn Name]
parseL = mapM runParser

parseDecl :: [Sexp] -> IO (Decl Name)
parseDecl = \case
  [Atom name,expr] -> makeVarDecl name <$> parseExpr expr
  List[Atom name, Atom ".", Atom arg]:body -> makeFunListDecl name arg <$> parseL body
  List(Atom name:argl):body -> do
    args <- parseArgs argl
    case args of
      Normal args ->  makeFunDecl name args <$> parseL body
      Dotted args arg -> makeFunDotDecl (toName name) args arg <$> parseL body
  _ -> throwM $ ParseException "Wrong define"

parseExprs :: [Sexp] -> IO [Expr Name]
parseExprs = mapM parseExpr

parseExpr :: Sexp -> IO (Expr Name)
parseExpr = \case
  l@(List(UQ:_)) -> parseExpr $ parseQQ l

  List(Atom "let":tl) -> (case tl of
    List bindings:body -> liftA2 makeLet (parseBindings bindings) (parseL body)
    _ -> throwM $ ParseException "Wrong let")

  List(Atom "if":tl) -> (case tl of
    [e1, e2, e3] -> liftA3 makeIf3 (parseExpr e1) (parseExpr e2) (parseExpr e3)
    [e1, e2    ] -> liftA2 makeIf2 (parseExpr e1) (parseExpr e2)
    _ -> throwM $ ParseException "Wrong if")

  List(Atom "set!":tl) -> (case tl of
    [Atom id, e] ->  makeSet id <$> parseExpr e
    _ -> throwM $ ParseException "wrong set!")

  List[Atom "apply", lhs, rhs] -> (case lhs of
   Atom hd | isPrim hd -> makePrimApply hd <$> parseExpr lhs
   Atom hd | isIdent hd -> liftA2 makeLamApply (parseIdent hd) (parseExpr rhs)
   List _ -> liftA2 makeLamApply (parseExpr lhs) (parseExpr rhs)
   _ -> throwM $ ParseException "wrong apply")

  List(Atom "lambda":tl) -> parseLambda tl

  List[Atom "quote", sxp] -> ELit <$> parseQuote sxp

  List[Atom "call\\cc", fn] -> makeCallCC <$> case fn of
    Atom x | isIdent x -> parseIdent x
    List(Atom "lambda":lam) -> parseLambda lam
    _ -> throwM $ ParseException "Wrong call\\cc"

  List(hd:tl) -> case hd of
    Atom hd | isPrim hd -> makePrimApp hd <$> parseExprs tl
    Atom hd | isIdent hd -> liftA2 makeLamApp (parseIdent hd) (parseExprs tl)
    List _ -> liftA2 makeLamApp (parseExpr hd) (parseExprs tl)
    _ -> throwM $ ParseException "Wrong application"

  Atom x -> parseAtom (Atom x)
  List[] -> return makeNil


  _ -> throwM $ ParseException "Wrong expression"

parseAtom :: Sexp -> IO (Expr Name)
parseAtom = \case
  Atom x | isIdent x -> parseIdent x
  x -> ELit <$> parseLit x

-- This is eta-abstraction over identifier which actually are primitives. Consider: (let ((x display)) (x 2))
parseIdent :: Text -> IO (Expr Name)
parseIdent x = do
  return $
    if isPrim x then
      makeLamList ("l" :: Name) (makePrimApply x (toExpr ("l" :: Name)))
    else
      makeVar (parseLiteral varLiteral x)

parseLit :: Sexp -> IO Literal
parseLit = \case
  Atom x | isString x -> return $ LitString (parseLiteral stringLiteral x)
  Atom x | isChar x -> return $ LitChar (parseLiteral charLiteral x)
  Atom x | isInt x -> return $ LitInt (parseLiteral intLiteral x)
  Atom x | isFloat x -> return $ LitFloat (parseLiteral floatLiteral x)
  Atom x | isBool x -> return $ LitBool (parseLiteral boolLiteral x)
  _ -> throwM $ ParseException "Wrong literal"


parseBindings :: [Sexp] -> IO [(Name, Expr Name)]
parseBindings = sequence . go
  where
    go :: [Sexp] -> [IO (Name, Expr Name)]
    go = \case
      List[Atom id, expr]:tl -> let t =  mapM parseExpr (toName id, expr) in t:go tl
      [] -> []
      _ -> throwM $ ParseException "Wrong binding form"


parseLambda :: [Sexp] -> IO (Expr Name)
parseLambda = \case
  List argl:body -> do
    args <- parseArgs argl
    case args of
      Normal args ->  makeLam args <$> parseL body
      Dotted args arg -> makeLamDot args arg <$> parseL body
  Atom arg:body -> makeLamList (toName arg) <$> parseL body


parseArgs :: [Sexp] -> IO Args
parseArgs args = evalStateT (go args) []
  where
    go :: [Sexp] -> StateT [Name] IO Args
    go s = case s of
      Atom ".":tl -> do
        case tl of
          [Atom id] -> do
            lst <- get
            return $ Dotted (reverse lst) (toName id)
          _ -> throwM $ ParseException "Wrong arg list for dotted lambda"
      Atom id:tl -> do
        modify (toName id:)
        go tl
      [] -> Normal . reverse <$> get
      _ -> throwM $ ParseException "Wrong args"

parseQuote :: Sexp -> IO Literal
parseQuote sxp = case sxp of
  List(Atom "vec":tl) -> LitVector <$> parseList tl
  List es -> LitList <$> parseList es
  Atom _ -> case sxp of
    Atom x | isIdent x -> return $ LitSymbol (parseLiteral varLiteral x)
    Atom _ ->  parseLit sxp
  where
    parseList :: [Sexp] -> IO [Literal]
    parseList sxps = case sxps of
      [tl] -> do
        tl <- parseQuote tl
        return [tl, LitNil]

      [Atom ".", tl] -> do
        tl <- parseQuote tl
        return [tl]

      hd:tl -> do
        hd <- parseQuote hd
        tl <- parseList tl
        return $ hd:tl

-------------------------------------------------------------------------------
-- Reader
-------------------------------------------------------------------------------
-- This code is executed before the actual parser

pattern QQ :: Sexp
pattern QQ = Atom "quasiquote"
pattern UQ :: Sexp
pattern UQ = Atom "unquote"
pattern UQS :: Sexp
pattern UQS = Atom "unquote-splicing"

quote :: Sexp -> Sexp
quote sxp = List[Atom "quote", sxp]

unquote :: Sexp -> Sexp
unquote (List [UQ, sxp]) = sxp
unquote x = x

listApp :: [Sexp] -> Sexp
listApp args = List $ Atom "list" : args

containsUQ :: Sexp -> Bool
containsUQ (Atom _) = False
containsUQ (List l) = not $ null [ x | x@(List [UQS, _]) <- l ]


type Nesting = Int
type QQ a = State Nesting a

incN :: QQ ()
incN = modify (+1)

decN :: QQ ()
decN = modify ((-)1)

getN :: QQ Nesting
getN = get

parseQQ :: Sexp -> Sexp
parseQQ (List[QQ, sexp'@(Atom _)]) = quote sexp'
parseQQ (List [QQ, List[UQ, sexp'@(Atom _)]]) = sexp'
parseQQ l = evalState (qqList l) 0
  where
    qqList :: Sexp -> QQ Sexp
    qqList (List[QQ, sexp@(List l)])
      | containsUQ sexp = do
          l' <- forM l \case
                  l@(List [UQ, x]) -> do
                    decN
                    nest <- getN
                    let sexp = if nest == 0 then listApp [x] else listApp [quote l]
                    return sexp

                  l@(List [UQS, x]) -> do
                    decN
                    nest <- getN
                    let sexp = if nest == 0 then x else listApp [quote l]
                    return sexp

                  l@(List [QQ, List _]) -> do
                    incN
                    qqList l

                  s@(List [QQ, Atom _]) -> return $ listApp [quote s]

                  a@(Atom _) -> return $ listApp [quote a]

          return $ listApp l'


      | otherwise = do
          l' <- forM l \case
                  l@(List [UQ, a@(Atom _)]) -> do
                    nest <- getN
                    let sexp = if nest == 1 then a else l
                    return sexp

                  l@(List [UQ, l'@(List _)]) -> do
                    nest <- getN
                    sexp <- if nest == 1 then return l' else decN >> qqList l
                    put nest
                    return sexp

                  l@(List [QQ, List _]) -> do
                    nest <- getN
                    sexp <- incN >> qqList l
                    put nest
                    return sexp

                  l@(List [QQ, Atom _]) -> return $ quote l

                  a@(Atom _) -> return $ quote a

          return $ listApp l'





-- parseCond :: [Sexp] -> IO Expr
-- parseCond clauses = makeCond <$> parseCondBody clauses

-- parseCondBody :: [Sexp] -> IO CondBody
-- parseCondBody = sequence . go
--   where
--     go :: [Sexp] -> [IO (Expr, BodyKind)]
--     go sxp =
--       case sxp of
--         [] -> [return (makeLiteral $ makeBool True, makeMultBody [makeLiteral makeUnspecified])]
--         [List(Atom "else":tl)] -> [liftA2 (,) (return $ makeLiteral $ makeBool True) (makeMultBody <$> parseExprs tl)]
--         clause:tl -> parseCondClause clause:go tl

-- parseCondClause ::  Sexp -> IO (Expr, BodyKind)
-- parseCondClause = \case
--   List(tst:bdy) ->
--     let test = parseExpr tst
--         body = makeMultBody <$> parseExprs bdy in
--       liftA2 (,) test body
--   _ -> throwM $ ParseException "Wrong cond clause"

-- parseCaseBody :: [Sexp] -> IO CaseBody
-- parseCaseBody = sequence . go
--   where
--     go :: [Sexp] -> [IO ([Literal], BodyKind)]
--     go sxp =
--       case sxp of
--         [] -> [return (makeLiteral $ makeBool True, makeMultBody [makeLiteral makeUnspecified])]
--         [List(Atom "else":tl)] -> [liftA2 (,) (return [makeLiteral $ makeBool True]) (makeMultBody <$> parseExprs tl)]
--         clause:tl -> parseCaseClause clause:go tl

-- parseCaseClause :: Sexp -> IO ([Literal], BodyKind)
-- parseCaseClause = \case
--   List(List(datums):bdy) ->
--     let lits = sequence (parseLit <$> datums)
--         body = makeMultBody <$> parseExprs bdy in
--       liftA2 (,) lits body
