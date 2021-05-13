{-# LANGUAGE PatternSynonyms #-}
-- |

module Expander.Naive where

import RIO hiding (ST)
import RIO.State
import RIO.List (initMaybe, lastMaybe)
import RIO.Partial (fromJust)
import Prelude (print)
import Types.Types (Sexp(..), ScEnv, Env(..))
import Types.Exceptions
import Sexp.Literals
import qualified RIO.Map as Map
import RIO.State (State)
import RIO.List (stripPrefix)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
-- Scheme Syntax
data Stree
  = LitString String
  | LitSymbol Symbol
  | LitInt Int
  | LitFloat Float
  | LitChar Char
  | LitBool Bool
  | MStree MStree
  | LitList [Stree]
  | LitVec [Stree]
  | LitNil
  | Deleted
  deriving (Show, Eq)

data MStree
  = SynExtId Symbol
  | SynExtApp Symbol [Stree]
  deriving (Show, Eq)

type SchemeList = [Stree]
type Symbol = String



-- Syntax Table
data ST = ST { bindings :: Bindings, vars :: [Symbol] }
-- MStree -> Stree
-- Syntax transformer function
type STF = MStree -> Stree
type Bindings = Map Symbol STF

newtype Expander a = Expander { runExpander :: StateT ST IO a }
  deriving (Functor, Applicative, Monad, MonadState ST, MonadIO, MonadThrow)

data PVValue
  = PVStree Stree
  | PVSome [Stree]
  deriving (Show, Eq)

data PV = PV { patternVars :: Map Symbol PVValue }

newtype Matcher a = Matcher { runMatcher :: State PV a }
  deriving (Functor, Applicative, Monad, MonadState PV)

data SyntaxRules = SyntaxRules { literals :: [Stree], rules :: [SyntaxRule] }
  deriving (Show, Eq)

data Pattern
  = PatLiteral Stree -- 2, sym
  | PatIdentifier Symbol --  id
  | PatListSome [Pattern] Pattern -- (x y ...)
  | PatList [Pattern]
  | PatVec [Pattern]
  | PatEmpty
  deriving (Show, Eq)

type PatternVariable = (Symbol, Stree)

data SyntaxRule = SyntaxRule { pat :: Pattern, template :: Stree }
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Patterns
-------------------------------------------------------------------------------

pattern Sym :: Symbol -> Stree
pattern Sym s = LitSymbol s
pattern Sxp :: [Stree] -> Stree
pattern Sxp l = LitList l
pattern DefineSyntax :: Symbol -> Stree -> Stree
pattern DefineSyntax name transformerSpec = Sxp [Sym "define-syntax", Sym name, transformerSpec]
pattern Define :: Stree -> Stree -> Stree
pattern Define args body  = Sxp [Sym "define", args, body]
pattern Lambda :: Stree -> Stree -> Stree
pattern Lambda args body = Sxp [Sym "lambda", args, body]
pattern Let :: [Stree] -> Stree -> Stree
pattern Let binds body = Sxp [Sym "let", Sxp binds, body]
pattern Letrec :: [Stree] -> Stree -> Stree
pattern Letrec binds body = Sxp [Sym "letrec", Sxp binds, body]
pattern Bind :: Symbol -> Stree -> Stree
pattern Bind var expr = Sxp [Sym var, expr]
pattern App hd tl = Sxp (hd:tl)
pattern Or es = Sxp (Sym "or":es)
pattern And es = Sxp (Sym "and":es)
pattern Begin es = Sxp (Sym "begin":es)
pattern If tst thn els = Sxp [Sym "if", tst, thn, els]
pattern T = Sym "#t"
pattern F = Sym "#f"

-------------------------------------------------------------------------------
-- ST Setup
-------------------------------------------------------------------------------
defaultBindings :: Bindings
defaultBindings = Map.fromList [("or", or), ("and", and), ("begin", begin), ("define", define)]
  where
    or :: MStree -> Stree
    or (SynExtId _) = error "invalid synext"
    or (SynExtApp _ []) = F
    or (SynExtApp _ [e]) = e
    or (SynExtApp _ (e:es)) = If e e (MStree $ SynExtApp "or" es)

    and :: MStree -> Stree
    and (SynExtId _) = error "invalid synext"
    and (SynExtApp _ []) = T
    and (SynExtApp _ [e]) = e
    and (SynExtApp _ (e:es)) = If e (MStree $ SynExtApp "and" es) F

    begin :: MStree -> Stree
    begin (SynExtId _) = error "invalid synext"
    begin (SynExtApp _ []) = error "invalid synext"
    begin (SynExtApp _ [e]) = e
    begin (SynExtApp _ (e:es)) = Let [Bind "l" e] (MStree $ SynExtApp "let" es)

    define :: MStree -> Stree
    define (SynExtId _) = error "invalid synext"
    define (SynExtApp _ [d@(Define (Sym _) _)]) = d
    define (SynExtApp _ [Define (Sxp (name:args)) expr]) = Define name (Lambda (Sxp args) expr)
    define (SynExtApp _ _) = error "invalid synext"

defaultST :: ST
defaultST = ST { bindings = defaultBindings, vars = [] }
-------------------------------------------------------------------------------
-- ST functions
-------------------------------------------------------------------------------
getBinding :: Symbol -> Expander (Maybe STF)
getBinding sym = do
  bs <- gets bindings
  return $ bs Map.!? sym

addBinding :: Symbol -> STF -> Expander ()
addBinding sym stf = do
  bindings <- gets bindings
  modify $ \s -> s { bindings = Map.insert sym stf bindings }

addVar :: Stree -> Expander ()
addVar (LitSymbol sym) = do
  vars' <- gets vars
  modify $ \s -> s { vars = sym:vars' }

rmVar :: Stree -> Expander ()
rmVar (LitSymbol sym) = do
  vars' <- gets vars
  let Just vars'' = stripPrefix [sym] vars'
  modify $ \s -> s { vars = vars'' }

withVars :: [Stree] -> Expander b -> Expander b
withVars vars f = do
  forM_ vars addVar
  b <- f
  forM_ vars rmVar
  return b

-------------------------------------------------------------------------------
-- Exceptions

syntaxError :: MonadThrow m => String -> m a
syntaxError str = throwM $ ExpandException str

-------------------------------------------------------------------------------
-- Predicates
-------------------------------------------------------------------------------
isMactok :: Text -> Bool
isMactok = flip elem mactok
  where
    mactok :: [Text]
    mactok = ["or", "begin", "and", "define"]

isConst :: Stree -> Bool
isConst (LitString _) = True
isConst (LitSymbol _) = True
isConst (LitInt _) = True
isConst (LitChar _) = True
isConst (LitBool _) = True
isConst (LitVec _) = True
isConst _ = False

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
  List (Atom e:es) | isMactok e -> MStree . SynExtApp (parseLiteral varLiteral e) <$> parseList es
  List(Atom "vec":tl) -> LitVec . (LitSymbol "vec":) <$> parseList tl
  List [] -> return LitNil
  List es -> LitList <$> parseList es
  Atom _ -> case sxp of
    Atom x | isMactok x -> return $ MStree . SynExtId $ parseLiteral varLiteral x
    Atom x | isName x-> return $ LitSymbol (parseLiteral varLiteral x)
    Atom _ ->  parseLit sxp
  where
    parseList :: [Sexp] -> IO [Stree]
    parseList sxps = case sxps of
      [tl] -> do
        tl <- parse tl
        return [tl]

      -- TODO handle these right
      [Atom ".", tl] -> do
        tl <- parse tl
        return [tl]

      hd:tl -> do
        hd <- parse hd
        tl <- parseList tl
        return $ hd:tl

-------------------------------------------------------------------------------
-- Pattern Matching
-------------------------------------------------------------------------------
testRule = Sxp [Sym "syntax-rules", Sxp [Sym "x"], Sxp [Sym "or", Sym "and"]]
testRule2 = Sxp [Sym "syntax-rules", Sxp [Sym "x"], Sxp [Sxp [Sym "or", Sym "x"], Sym "and"]]
testRule3 = Sxp [Sym "syntax-rules", Sxp [Sym "x"], Sxp [Sxp [Sym "or", Sym "x", Sym "y", Sym "..."], Sym "and"]]
testRule4 = Sxp [Sym "syntax-rules", Sxp [Sym "x"], Sxp [Sxp [Sym "or", Sxp [Sym "a", Sym "b"], Sym "..."], Sym "and"]]
testRule5 = Sxp [Sym "syntax-rules", Sxp [Sym "x"], Sxp [Sxp [Sym "or", Sym "c", Sxp [Sym "a", Sym "b"], Sym "..."], Sym "and"]]

-- We ignore the head of each rule, since they always match
parseSyntaxRule :: [Stree] -> Stree -> SyntaxRule
parseSyntaxRule _ (Sxp [Sym _, template]) = SyntaxRule { pat = PatEmpty, template }
parseSyntaxRule lits (Sxp [Sxp (_:tl), template]) = SyntaxRule { pat = evalState (parsePatternList tl) [], template }
  where
    parsePatternList :: [Stree] -> State [Pattern] Pattern
    parsePatternList (pat:[Sym "..."]) = do
      pat' <- parsePattern pat
      lst <- get
      return $ PatListSome (reverse lst) pat'

    parsePatternList [end] = do
      pat <- parsePattern end
      lst <- get
      return $ PatList (reverse $ pat:lst)

    parsePatternList (pat:tl) = do
      pat' <- parsePattern pat
      modify (pat':)
      parsePatternList tl

    parsePatternList [] = return PatEmpty

    parsePattern :: Stree -> State [Pattern] Pattern
    parsePattern pat = case pat of
        _ | elem pat lits -> return $ PatLiteral pat
        Sym sym ->  return $ PatIdentifier sym
        Sxp pats -> do
          s <- get
          l <- put [] >> parsePatternList pats
          put s >> return l
        _ -> error "Wrong pattern"


parseSyntaxRules :: Stree -> SyntaxRules
parseSyntaxRules (Sxp (Sym "syntax-rules":(Sxp literals):rulessxp)) =
  let rules = fmap (parseSyntaxRule literals) rulessxp in
    SyntaxRules { literals, rules }

-- Pattern = P, Stree = F
matches :: Pattern -> Stree -> Matcher Bool
-- Non Literal Identifier always match
matches (PatIdentifier sym) stree = return True
-- Literals must be equal
matches (PatLiteral lit) stree = return $ lit == stree
-- Pn must match Fn
matches (PatList list) (LitList l) =
  if length list == length l then
    and <$> zipWithM matches list l
  else return False

-- Pn and Fn must match,  Pn+1 must match with Fn+1...Fn+m
matches (PatListSome list some) (LitList l) =
  let inits = take (length list) l
      rest = drop (length list) l in
    if length inits == length list then
      liftA2 (&&) (and <$> zipWithM matches list inits) (and <$> mapM (matches some) rest)
    else
      return False

-- The empty pattern always matches
matches PatEmpty _ = return True


-------------------------------------------------------------------------------
-- Expander
-------------------------------------------------------------------------------

getVarsFromBind :: [Stree] -> [Stree]
getVarsFromBind [] = []
getVarsFromBind (Bind v _:tl) = Sym v:getVarsFromBind tl

getMacro :: MStree -> Expander (Maybe STF)
getMacro (SynExtId sym) = getBinding sym
getMacro (SynExtApp sym _) = getBinding sym

e :: Stree -> Expander Stree
e s = do
  vars' <- gets vars
  case s of
    -- Constants
    LitSymbol v | v `elem` vars' -> return s
    c | isConst c -> return c
    LitList (LitSymbol "quote":_) -> return s
    LitVec _ -> return s

    -- define-syntax
    DefineSyntax name transformerSpec -> do
      addBinding name (makeSTF transformerSpec)
      return Deleted

    -- Macros
    MStree mstree -> do
      sf <- getMacro mstree
      -- if sf is Nothing then error else e (sf mstree)
      maybe (syntaxError "Macro not found") e (sf <*> Just mstree)

    -- Binding forms
    -- Define
    Define arg expr -> do
      addVar arg
      expr' <- e expr
      return $ Define arg expr'
    -- Normal lambda and dotted lambda
    Lambda (Sxp args) (Sxp es) -> do
      es' <- withVars (filter (/= LitNil) args) $ forM es e
      return $ Lambda (Sxp args) (Sxp es')
    -- List lambda
    Lambda arg (Sxp es) -> do
      es' <- withVars [arg] $ forM es e
      return $ Lambda arg (Sxp es')
    -- Let
    Let bnd (Sxp es) -> do
      es' <- withVars (getVarsFromBind bnd) $ forM es e
      return $ Let bnd (Sxp es')

    -- Application
    App hd tl -> liftA2 App (e hd) (mapM e tl)
    -- If
    If tst thn els -> liftA3 If (e tst) (e thn) (e els)



makeSTF :: Stree -> STF
makeSTF = error "not implemented"

runExpand :: Expander a -> IO a
runExpand expander = evalStateT (runExpander expander) defaultST

-------------------------------------------------------------------------------
-- Stree -> Sexp
-------------------------------------------------------------------------------
streeToSexp :: Stree -> Sexp
streeToSexp (LitString str) = Atom (fromString str)
streeToSexp (LitSymbol str) = Atom (fromString str)
streeToSexp (LitInt int) = Atom (fromString $ show int)
streeToSexp (LitFloat float) = Atom (fromString $ show float)
streeToSexp (LitChar char) = Atom (fromString $ "#\\" <> show char)
streeToSexp (LitBool b) = Atom (fromString $ if b then "#t" else "#f")
streeToSexp (LitList l) = List $ fmap streeToSexp l
streeToSexp (LitVec l) = List $ fmap streeToSexp l
streeToSexp (MStree _) = error "MStree found while converting to sexp"
streeToSexp LitNil = List[]

-------------------------------------------------------------------------------
-- RIO Action
-------------------------------------------------------------------------------

expand :: ScEnv ()
expand = do
  logInfo "Expanding Syntactic Extensions"
  sexpsref <- asks _sexps
  sexps <- readSomeRef sexpsref

  strees <- liftIO $ forM sexps parse
  liftIO $ forM_ strees print
  strees' <- liftIO $ runExpand $ forM strees e
  let strees'' = filter (/= Deleted) strees'
  let sexps = fmap streeToSexp strees''
  logDebug $ "Sexps after macro expansion:\n" <> mconcat (display <$> sexps)

  writeSomeRef sexpsref sexps
