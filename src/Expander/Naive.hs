{-# LANGUAGE PatternSynonyms #-}
-- |

module Expander.Naive where

import RIO hiding (ST)
import RIO.State
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

-------------------------------------------------------------------------------
-- Patterns
-------------------------------------------------------------------------------

pattern Sym :: Symbol -> Stree
pattern Sym s = LitSymbol s
pattern Sxp :: [Stree] -> Stree
pattern Sxp l = LitList l
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
defaultBindings = Map.fromList [("or", or), ("and", and), ("begin", begin)]
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

defaultST :: ST
defaultST = ST { bindings = defaultBindings, vars = [] }
-------------------------------------------------------------------------------
-- ST functions
-------------------------------------------------------------------------------
getBinding :: Symbol -> Expander (Maybe STF)
getBinding sym = do
  bs <- gets bindings
  return $ bs Map.!? sym

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
    mactok = ["or", "begin", "and"]

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

    -- Macros
    MStree mstree -> do
      sf <- getMacro mstree
      -- if sf is Nothing then error else e (sf mstree)
      maybe (syntaxError "Macro not found") e (sf <*> Just mstree)

    -- Binding forms
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
    App hd tl -> do
      hd' <- e hd
      tl' <- mapM e tl
      return $ App hd' tl'
    -- If
    If tst thn els -> do
      tst' <- e tst
      thn' <- e thn
      els' <- e els
      return $ If tst' thn' els'

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
  let sexps = fmap streeToSexp strees'
  logDebug $ "Sexps after macro expansion:\n" <> mconcat (display <$> sexps)

  writeSomeRef sexpsref sexps
