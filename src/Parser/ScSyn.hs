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
import Prelude (print)
import RIO.State
import Types.Types
import Types.Constructors
import Types.Exceptions
import Sexp.Literals
import System.IO (putStrLn)

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
  l@(List(QQ:_)) -> do
    qq <- parseQQ l

    print "QUASIQUOTE DESUGAR: \n"
    print qq
    parseExpr qq

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
  s -> throwM $ ParseException $ "Wrong literal: " <> show s


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
    Atom x | isName x -> return $ LitSymbol (parseLiteral varLiteral x)
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

-------------------------------------------------------------------------------
-- Quasiquotation
-------------------------------------------------------------------------------

pattern QQ :: Sexp
pattern QQ = Atom "quasiquote"
pattern UQ :: Sexp
pattern UQ = Atom "unquote"
pattern UQS :: Sexp
pattern UQS = Atom "unquote-splicing"

mods :: [Sexp]
mods = [QQ, UQ, UQS]

isMod :: Sexp -> Bool
isMod (List (hd:_)) = elem hd mods
isMod _ = False

listFun :: Sexp
listFun = Atom "list"

appendFun :: Sexp
appendFun = Atom "append"

quote :: Sexp -> Sexp
quote sxp = List[Atom "quote", sxp]

listApp :: Sexp -> Sexp
listApp (List args) = List $ Atom "list" : args

wrapInListApp :: Sexp -> Sexp
wrapInListApp sxp = listApp $ List [sxp]

appendApp :: Sexp -> Sexp
appendApp (List args) = List $ Atom "append" : args

containsUQS :: Sexp -> Bool
containsUQS (Atom _) = False
containsUQS (List [UQS, _]) = True
containsUQS (List l) = not $ null [ x | x@(List [UQS, _]) <- l ]

type Nesting = Int
type UQSP = Bool
-- IO for debugging, trace does not do its job well
type QQ a = StateT (Nesting, UQSP) IO a

emptyS :: (Int, Bool)
emptyS = (1, False)

incN :: QQ ()
incN = modify $ bimap (+1) id

decN :: QQ ()
decN = modify $ bimap (\n -> n - 1) id

getN :: QQ Nesting
getN = gets fst

modifyUQS :: Bool -> QQ ()
modifyUQS b = modify $ bimap id (const b)

getUQSP :: QQ UQSP
getUQSP = gets snd

setUQS :: QQ ()
setUQS = modifyUQS True

unsetUQS :: QQ ()
unsetUQS = modifyUQS False

useUQS :: QQ a -> QQ a
useUQS m = do
  setUQS
  a <- m
  unsetUQS
  return a


-- We don't wont changes of nesting to 'leak' over to other symbolic expressions in a list
-- So this is used whenever we change the nesting and recur over sub symbolic expressions of a list.
freezeN :: QQ a -> QQ a
freezeN m = do
  n <- getN
  a <- m
  modify $ bimap (const n) id
  return a


parseQQ :: Sexp -> IO Sexp
parseQQ (List[QQ, sexp'@(Atom _)]) = return $ quote sexp' -- Atom case is simple Quote
parseQQ (List[QQ, List [Atom "vec", l@(List _)]]) = do
  list <- parseQQ l
  return $ List [Atom "list2vector", list]
parseQQ (List[QQ, l@(List _)])
  | isMod l = evalStateT (qqSexp l) emptyS -- need to consider cases with nested modifiers, e.g: `,2
  | otherwise = evalStateT (qqList l) emptyS
  where
    qqList :: Sexp -> QQ Sexp
    qqList sexp@(List l) = do
          nest <- getN
          lift $ putStrLn $ "NEST IS:" <> show nest
          lift $ putStrLn $ "UQS?" <> show (containsUQS sexp)
          -- These correspond with unquote-splicing (,@)
          let uqsp = containsUQS sexp && nest == 1
              finalWrap = if uqsp then appendApp else listApp

          -- qqList and qqSexp are mutually recursive
          l' <- if uqsp then forM l (useUQS . qqSexp) else forM l qqSexp

          -- we either need make a `list` call or a `append` call out of the list l', depends whether or not UQS got used
          return $ finalWrap $ List l'

    qqList _ = error "unreachable"

    qqSexp :: Sexp -> QQ Sexp
    qqSexp sexp = do
      let _ = List [Atom "quasiquote",List [Atom "0",List [Atom "unquote",List [Atom "+",Atom "1",Atom "2"]],List [Atom "unquote-splicing",List [Atom "list",Atom "0"]]]]
      nest <- getN
      uqsp <- getUQSP
      unsetUQS -- unset in case of recursive calls
      lift $ putStrLn ("uqsp ist: " <> show uqsp)
      let wrap = if uqsp then wrapInListApp else id
      case sexp of
        List [UQS, a@(Atom _)] -> do
          lift $ putStrLn "UQSA"
          if nest == 1 then -- check if 1 since decN is not needed, we can just return the sexp
            return a
          else
            return $ quote $ List [UQS, a]

        List [UQS, l@(List _)]  -> do
          lift $ putStrLn "UQSL"
          if nest == 1 then do -- check if 1 since decN is not needed, we can just return the sexp
            return l
          else do
            -- We need to consider cases where a modifier follows a modifier, e.g: ,,2.
            sexp <- freezeN $ if isMod l then decN >> qqSexp l else decN >> qqList l
            if containsUQS l && nest == 2 then
              -- We need to consider cases like: ,,@(list 0) which becomes:
              -- (append (list 'unquote) (list 0)) => ,0
              return $ List [appendFun, wrapInListApp $ quote UQS, sexp]
            else
              return $ List [listFun, quote UQS, sexp]

        List [UQ, a@(Atom _)] -> do
          lift $ putStrLn "UQA"
          if nest == 1 then -- check if 1 since decN is not needed, we can just return the sexp
            return $ wrap a
          else
            return $ quote $ List [UQ, a]

        List [UQ, l@(List _)] -> do
          lift $ putStrLn "UQL"
          if nest == 1 then  -- check if 1 since decN is not needed, we can just return the sexp
            return $ wrap l
          else do
            -- We need to consider cases where a modifier follows a modifier, e.g: ,,2.
            sexp <- freezeN $ if isMod l then decN >> qqSexp l else decN >> qqList l
            if containsUQS l && nest == 2 then
              -- We need to consider cases like: ,,@(list 0) which becomes:
              -- (append (list 'unquote) (list 0)) => ,0
              return $ List [appendFun, wrapInListApp $ quote UQ, sexp]
            else
              return $ List [listFun, quote UQ, sexp]

        List [QQ, l@(List _)] -> do
          lift $ putStrLn "QQL"
          if nest == 0 then -- Start new QQ if nesting is 0 e.g: `,`2
            lift $ parseQQ sexp
          else do
            -- We need to consider cases where a modifier follows a modifier, e.g: ,,2.
            sexp <- freezeN $ if isMod l then incN >> qqSexp l else incN >> qqList l
            if containsUQS l && nest == 2 then
              -- We need to consider cases like: ,,@(list 0) which becomes:
              -- (append (list 'unquote) (list 0)) => ,0
              return $ List [appendFun, wrapInListApp $ quote QQ, sexp]
            else
              return $ wrap $ List [listFun, quote QQ, sexp]

        List [QQ, Atom _] -> do
          lift $ putStrLn "QQA"
          if nest == 0 then
            lift $ parseQQ sexp -- Start new QQ if nesting is 0 e.g: `,`2
          else do
            return $ wrap $ quote sexp

        List _ -> do
          lift $ putStrLn "L"
          wrap <$> freezeN (qqList sexp) -- Loop over the list

        Atom _ -> do
          lift $ putStrLn "A"
          return $ wrap $ quote sexp -- Quote the atom, consider that UQS is used

parseQQ _ = error "unreachable"
