{-# LANGUAGE PatternSynonyms #-}
-- |

module Expander.Expander where

import RIO hiding (ST, Seq, seq)
import RIO.State
import RIO.List (initMaybe, lastMaybe)
import RIO.List.Partial (head)
import RIO.Partial (fromJust)
import Prelude (print)
import Types.Types (Sexp(..), ScEnv, Env(..))
import Types.Exceptions
import Sexp.Literals
import qualified RIO.Map as Map
import RIO.State (State)
import RIO.List (stripPrefix)
import Data.Foldable (maximum, foldrM)
import Utils.NameResolver (isPrim)


import Expander.Ast
import qualified Expander.Matcher as Matcher
import qualified Expander.Constructor as Constructor
import RIO.Partial (succ)


-- Syntax Table
data ST = ST { bindings :: Bindings, vars :: [Symbol], timestamp :: Timestamp  }

-- MStree -> Stree
-- Syntax transformer function
type STF = Stree -> Stree
type Bindings = Map Symbol STF

-- The main driver used for transcription of syntactic extensions
newtype Expander a = Expander { runExpander :: StateT ST IO a }
  deriving (Functor, Applicative, Monad, MonadState ST, MonadIO, MonadThrow)

data SyntaxRules = SyntaxRules { rules :: [SyntaxRule] }
  deriving (Show, Eq)

data SyntaxRule = SyntaxRule { pat :: Matcher.Pattern, template :: Constructor.Template }
  deriving (Show, Eq)
-------------------------------------------------------------------------------
-- ST Setup
-------------------------------------------------------------------------------
defaultBindings :: Bindings
defaultBindings = Map.fromList [("or", or), ("and", and), ("begin", begin)]
  where
    or :: Stree -> Stree
    or (Sxp []) = F
    or (Sxp [e]) = e
    or (Sxp (e:es)) = Let [Bind (Sym "e") e] (If (Sym "e") (Sym "e") (Sxp (Sym "or": es)))
    or _ = error "invalid synext"

    and :: Stree -> Stree
    and (Sxp []) = T
    and (Sxp [e]) = e
    and (Sxp (e:es)) = If e (Sxp (Sym "and": es)) F
    and _ = error "invalid synext"

    begin :: Stree -> Stree
    begin (Sxp []) = error "invalid synext"
    begin (Sxp [e]) = e
    begin (Sxp (e:es)) = Let [Bind (Sym "l") e] (Sxp (Sym "let": es))
    begin _ = error "invalid synext"

defaultST :: ST
defaultST = ST { bindings = defaultBindings, vars = [], timestamp = 0 }
-------------------------------------------------------------------------------
-- ST functions
-------------------------------------------------------------------------------

getBinding :: Symbol -> Expander (Maybe STF)
getBinding sym = do
  bs <- gets bindings
  return $ bs Map.!? sym

-- Adds or overwrites a binding
addBinding :: Symbol -> STF -> Expander ()
addBinding sym stf = do
  bindings <- gets bindings
  modify $ \s -> s { bindings = Map.insert sym stf bindings }

addVar :: Stree -> Expander ()
addVar (Sym sym) = do
  vars' <- gets vars
  modify $ \s -> s { vars = sym:vars' }
addVar e = error ("Error: addVar: illegal expression found: " <> show e)

rmVar :: Stree -> Expander ()
rmVar (Sym sym) = do
  vars' <- gets vars
  let Just vars'' = stripPrefix [sym] vars'
  modify $ \s -> s { vars = vars'' }
rmVar e = error ("Error: rmVar: illegal expression found: " <> show e)

withVars :: [Stree] -> Expander b -> Expander b
withVars vars f = do
  forM_ vars addVar
  b <- f
  forM_ vars rmVar
  return b

incTimestamp :: Expander ()
incTimestamp = do
  ts <- gets timestamp
  modify $ \s -> s { timestamp = succ ts }


-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------
parseLit :: Sexp -> IO Stree
parseLit = \case
  Atom x | isString x -> return $ LitString (parseLiteral stringLiteral x)
  Atom x | isChar x -> return $ LitChar (parseLiteral charLiteral x)
  Atom x | isInt x -> return $ LitInt (parseLiteral intLiteral x)
  Atom x | isFloat x -> return $ LitFloat (parseLiteral floatLiteral x)
  Atom x | isBool x -> return $ LitBool (parseLiteral boolLiteral x)
  s -> throwM $ ParseException $ "Wrong literal: " <> show s

-- Most primitive Scheme parser which can be used for interpreters
parse :: Sexp -> IO Stree
parse sxp = case sxp of
  List(Atom "vec":tl) -> LitVec . (LitSymbol "vec":) <$> parseList tl
  List [] -> return $ LitList []
  List es -> LitList <$> parseList es
  Atom _ -> case sxp of
    Atom x | isName x-> return $ LitSymbol (parseLiteral varLiteral x)
    Atom _ ->  parseLit sxp
  where
    parseList :: [Sexp] -> IO [Stree]
    parseList sxps = case sxps of
      [] -> return []

      -- TODO handle these right
      -- [Atom ".", tl] -> do
      --   tl <- parse tl
      --   return [tl]

      hd:tl -> do
        hd <- parse hd
        tl <- parseList tl
        return $ hd:tl


parseSyntaxRules :: Stree -> SyntaxRules
parseSyntaxRules (Sxp (Sym "syntax-rules":(Sxp literals):rulessxp)) =
  let
    literals' = getLiterals literals
    rules = fmap (parseSyntaxRule literals') rulessxp in
    SyntaxRules { rules }
  where
    getLiterals :: [Stree] -> [Symbol]
    getLiterals [] = []
    getLiterals (Sym s:tl) = s:getLiterals tl

    parseSyntaxRule :: [Symbol] -> Stree -> SyntaxRule
    parseSyntaxRule _ (Sxp [Sym _, template]) = SyntaxRule { pat = Matcher.PatEmpty, template = Constructor.parse [] template }
    parseSyntaxRule lits (Sxp [Sxp (_:tl), template]) = SyntaxRule { pat = Matcher.parse lits tl, template = Constructor.parse lits template }
    parseSyntaxRule _ _ = error "Wrong syntax rule"
parseSyntaxRules _ = error "Wrong syntax-rules"
-------------------------------------------------------------------------------
-- Predicates
-------------------------------------------------------------------------------
isBound :: Bindings -> Symbol ->  Bool
isBound = flip Map.member

isQuoted :: Stree -> Expander Bool
isQuoted (Quote _) = return True
isQuoted (QuasiQuote _) = return True
isQuoted _ = return False

isVar :: Stree -> Expander Bool
isVar (Sym sym) = elem sym <$> gets vars
isVar _ = return False

isStamped :: Stree -> Expander Bool
isStamped (TSVar _)  = return True
isStamped _ = return False

atomicNonVar :: Stree -> Expander Bool
atomicNonVar stree = do
  isVar <- isVar stree
  case stree of
    _ | isVar -> return False
    _ | isConst stree -> return True
    TSVar _ -> return True
    _ -> return False

atomicNotStamped :: Stree -> Expander Bool
atomicNotStamped stree = case stree of
  _ | isConst stree -> return True
  _ -> return False

-------------------------------------------------------------------------------
-- Helper
-------------------------------------------------------------------------------

getVarsFromBind :: [Stree] -> [Stree]
getVarsFromBind [] = []
getVarsFromBind (Bind v _:tl) = v:getVarsFromBind tl
getVarsFromBind e = error ("Error: getVarsFromBind: found illegal expression:" <> show e)

getExprsFromBind :: [Stree] -> [Stree]
getExprsFromBind [] = []
getExprsFromBind (Bind _ e:tl) = e:getExprsFromBind tl
getExprsFromBind e = error ("Error: getExprsFromBind: found illegal expression:" <> show e)

freshName :: TSVar -> Symbol
freshName (var, timestamp) = var <> show timestamp

-------------------------------------------------------------------------------
-- Hygiene
-------------------------------------------------------------------------------

-- Converts vars to timestamped vars. We need to differ between vars and symbols.
t :: Stree -> (Symbol -> Expander TSVar) -> Expander Stree
t stree f = do
  stree' <- t stree f
  incTimestamp
  return stree'
  where
    t :: Stree -> (Symbol -> Expander TSVar) -> Expander Stree
    t stree f = do
      isAnv <- atomicNonVar stree
      isVar <- isVar stree
      case stree of
        _ | isAnv -> return stree
        _ | isVar  -> let Sym var = stree in TSVar <$> f var
        _ | isConst stree -> return stree

        -- Binding forms
        -- Define
        Define (Sxp (name:args)) (Sxp es) -> do
          (args', es') <- withVars args $ do
            liftA2 (,) (mapM (`t` f) args) (mapM (`t` f) es)

          return $ Define (Sxp (name:args')) (Sxp es')

        -- Normal lambda and dotted lambda
        Lambda (Sxp args) (Sxp es) -> do
          (args', es') <- withVars args $ do
            liftA2 (,) (mapM (`t` f) args) (mapM (`t` f) es)

          return $ Lambda (Sxp args') (Sxp es')
        -- List lambda
        Lambda arg (Sxp es) -> do
          (arg', es') <- withVars [arg] $ do
            liftA2 (,) (t arg f) (forM es (`t` f))

          return $ Lambda arg' (Sxp es')
        -- Let
        Let bnd (Sxp es) -> do
          let vars = getVarsFromBind bnd
              exprs = getExprsFromBind bnd

          (vars', es') <- withVars vars $ do
            liftA2 (,) (mapM (`t` f) vars) (forM es (`t` f))

          exprs' <- mapM (`t` f) exprs

          let bnd' = zipWith Bind vars' exprs'
          return $ Let bnd' (Sxp es')

        -- recur everything else
        (Sxp es) -> Sxp <$> mapM (`t` f) es

s :: Symbol -> Expander TSVar
s var = do
  nat <- gets timestamp
  return (var, nat)

-- Substitutes timestamped variablenames with variablenames
subst :: (Symbol, TSVar) -> Stree -> Expander Stree
subst p@(sym, tsvar) stree = do
  ans <- atomicNotStamped stree
  quote <- isQuoted stree
  stamped <- isStamped stree
  case stree of
    TSVar v | v == tsvar -> return $ Sym sym
    _ | ans || quote || stamped -> return stree

    -- recur everything else
    Sxp es -> Sxp <$> mapM (subst p) es

    _ -> error ("Error: subst: Found illegal expression: " <> show stree)


-- Creates fresh names from timestamped variables
-- We need to only substitute bound variables
-- We do not consider define, since we can not generate a fresh name for it
a :: Stree -> Expander Stree
a stree = do
  var <- isVar stree
  anv <- atomicNonVar stree
  quote <- isQuoted stree
  case stree of
    _ | var || anv || quote -> return stree

    Lambda (Sxp args) body -> do
      args' <- forM args $ \case
        TSVar arg -> do
          let gen = freshName arg
          return (gen, arg)
        e -> error ("Error: a: Expected TSVar in argument list, but got: " <> show e)
      let newargs = Sym <$> fmap fst args'
      newbody <- foldrM subst body args'
      Lambda (Sxp newargs) <$> a newbody

    Define (Sxp (name:args)) body -> do
      args' <- forM args $ \case
        TSVar arg -> do
          let gen = freshName arg
          return (gen, arg)
        e -> error ("Error: a: Expected TSVar in argument list, but got: " <> show e)
      let newargs = Sym <$> fmap fst args'
      newbody <- foldrM subst body args'
      Define (Sxp (name:newargs)) <$> a newbody

    Lambda (TSVar arg) body -> do
      let newarg = freshName arg
      newbody <- subst (newarg, arg) body
      Lambda (Sym newarg) <$> a newbody

    Let binding body -> do
      let vars = getVarsFromBind binding
          exprs = getExprsFromBind binding
      args' <- forM vars $ \case
        TSVar arg -> do
          let gen = freshName arg
          return (gen, arg)
        e -> error ("Error: a: Expected TSVar in variable list, but got: " <> show e)
      let newargs = fmap (Sym . fst) args'
      newbody <- foldrM subst body args'
      Let (zipWith Bind newargs exprs) <$> a newbody

    -- recur everything else
    Sxp es -> Sxp <$> mapM a es

    _ -> error  ("Error: a: Found illegal Expression:" <> show stree)




u :: Stree -> Expander Stree
u stree = do
  ans <- atomicNotStamped stree
  case stree of
    _ | ans -> return stree
    TSVar (v, _) -> return $ Sym v
    Sxp stree -> Sxp <$> mapM u stree

-------------------------------------------------------------------------------
-- Expander
-------------------------------------------------------------------------------

-- TODO: quote, set
eHyg :: Stree -> Expander Stree
eHyg stree = do
  timestamped <- t stree s
  expanded <- e timestamped
  case ((`t` s) >=> e) stree of
    _ | timestamped == expanded -> return stree
    _ -> (a >=> u) expanded

e :: Stree -> Expander Stree
e stree = do
  bindings <- gets bindings
  let isMacro = isBound bindings
      const = isConst stree
  quote <- isQuoted stree
  stamped <- isStamped stree

  case stree of
    -- Constants
    _ | const || quote || stamped -> return stree

    -- define-syntax
    DefineSyntax name transformerSpec -> do
      addBinding name (makeSTF $ parseSyntaxRules transformerSpec)
      return Deleted

    -- Macros
    App (Sym synext) tl | isMacro synext -> do
      sf <- getBinding synext
      -- if sf is Nothing then error else sf mstree
      let transcription = fromMaybe (error $ "Error: e: Macro not found: " <> synext) (sf <*> Just (Sxp tl))
      ((`t` s) >=> e) transcription

    Sxp es -> Sxp <$> mapM e es

    _ -> error $ "Error: e: Found: " <> show stree



makeSTF :: SyntaxRules -> STF
makeSTF SyntaxRules { rules } = \sxp ->
  let maybematch = findMatchingRule sxp rules in
    fromMaybe (error "No Pattern matched") maybematch
  where
    findMatchingRule :: Stree -> [SyntaxRule] -> Maybe Stree
    findMatchingRule _ [] = error "Error: makeSTF: No rules specified"
    findMatchingRule sxp (SyntaxRule { pat, template }:tl) =
      let pvs = Matcher.tryMatch pat sxp in
      Constructor.construct template <$> pvs <|> findMatchingRule sxp tl



runExpand :: Expander a -> IO a
runExpand expander = evalStateT (runExpander expander) defaultST

-------------------------------------------------------------------------------
-- Stree -> Sexp
-------------------------------------------------------------------------------

streeToSexp :: Stree -> Sexp
streeToSexp (LitString str) = Atom (fromString $ "\"" <> str <> "\"")
streeToSexp (LitSymbol str) = Atom (fromString str)
streeToSexp (LitInt int) = Atom (fromString $ show int)
streeToSexp (LitFloat float) = Atom (fromString $ show float)
streeToSexp (LitChar char) = Atom (fromString $ "#\\" <> [char])
streeToSexp (LitBool b) = Atom (fromString $ if b then "#t" else "#f")
streeToSexp (LitList l) = List $ fmap streeToSexp l
streeToSexp (LitVec l) = List $ fmap streeToSexp l

-------------------------------------------------------------------------------
-- RIO Action
-------------------------------------------------------------------------------

expand :: ScEnv ()
expand = do
  logInfo "Expanding Syntactic Extensions"
  sexpsref <- asks _sexps
  sexps <- readSomeRef sexpsref

  strees <- liftIO $ forM sexps parse
  logInfo "Parsed Expressions:"
  liftIO $ forM_ strees print
  strees' <- liftIO $ runExpand $ forM strees eHyg
  let strees'' = filter (/= Deleted) strees'
  let sexps = fmap streeToSexp strees''
  logDebug $ "Sexps after macro expansion:\n" <> mconcat (display <$> sexps)

  writeSomeRef sexpsref sexps
