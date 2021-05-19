{-# LANGUAGE PatternSynonyms #-}
-- |

module Expander.Naive where

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
import Data.Foldable (maximum)

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

-- The main driver used for transcription of syntactic extensions
newtype Expander a = Expander { runExpander :: StateT ST IO a }
  deriving (Functor, Applicative, Monad, MonadState ST, MonadIO, MonadThrow)

-- Types for pattern P
data Pattern
  = PatLiteral Stree -- 2, sym
  | PatIdentifier Symbol --  id
  | PatListSome [Pattern] Pattern -- (x y ...)
  | PatList [Pattern]
  | PatVec [Pattern]
  | PatEmpty
  deriving (Show, Eq)

type PatternVariable = (Symbol, Stree)
type Seq = Int
data PVValue
  = PVStree Stree
  | PVSome (Map [Seq] Stree)
  deriving (Show, Eq)

type PVS = Map Symbol PVValue

data SeqState = SeqState { seq :: Seq, seqs :: [Seq] }
defaultSeqState = SeqState { seq = -1, seqs = [] }

class Monad m => HasSeqState m where
  getSeqState :: m SeqState
  putSeqState :: SeqState -> m ()

data PV = PV { patternVars :: PVS, someSwitch :: Bool, matSeqState :: SeqState }

newtype Matcher a = Matcher { runMatcher :: State PV a }
  deriving (Functor, Applicative, Monad, MonadState PV)

instance HasSeqState Matcher where
  getSeqState = gets matSeqState
  putSeqState seqs = modify $ \s -> s { matSeqState = seqs }

-- Types for template F
data Template
  = TempLiteral Stree
  | TempVar Symbol
  | TempSome Template
  | TempList [Template]
  deriving (Show, Eq)

data ConstructorState = ConstructorState { cpatternVars :: PVS, spliceSwitch :: Bool, conSeqState :: SeqState }

newtype Constructor a = Constructor { runConstructor :: State ConstructorState a }
  deriving (Functor, Applicative, Monad, MonadState ConstructorState)

instance HasSeqState Constructor where
  getSeqState = gets conSeqState
  putSeqState seqs = modify $ \s -> s { conSeqState = seqs }

-- Types for syntax-rules
data SyntaxRules = SyntaxRules { literals :: [Symbol], rules :: [SyntaxRule] }
  deriving (Show, Eq)

data SyntaxRule = SyntaxRule { pat :: Pattern, template :: Stree }
  deriving (Show, Eq)

instance MonadFail Constructor
-------------------------------------------------------------------------------
-- Haskell Pattern Synonyms
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
parseSyntaxRule :: [Symbol] -> Stree -> SyntaxRule
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
        Sym sym | elem sym lits -> return $ PatLiteral pat
        Sym sym ->  return $ PatIdentifier sym
        _ | isConst pat -> return $ PatLiteral pat
        Sxp pats -> do
          s <- get
          l <- put [] >> parsePatternList pats
          put s >> return l
        _ -> error "Wrong pattern"


parseSyntaxRules :: Stree -> SyntaxRules
parseSyntaxRules (Sxp (Sym "syntax-rules":(Sxp literals):rulessxp)) =
  let
    literals' = getLiterals literals
    rules = fmap (parseSyntaxRule literals') rulessxp in
    SyntaxRules { literals=literals', rules }
  where
    getLiterals :: [Stree] -> [Symbol]
    getLiterals [] = []
    getLiterals (Sym s:tl) = s:getLiterals tl
parseSyntaxRules _ = error "Wrong syntax-rules"

-- (_ x y ...)
testPattern = PatListSome [PatIdentifier "x"] (PatIdentifier "y")
testSxp = Sxp [ Sym "x", Sym "y", Sym "z"]
-- (_ x (y z) ...)
testPattern2 = PatListSome [PatIdentifier "x"] (PatList [PatIdentifier "y", PatIdentifier "z"])
testSxp2 = Sxp [ Sym "x", Sxp[Sym "y", Sym "z"], Sxp[Sym "y", Sym "z"]]
-- (_ a (b ...) ...)
-- (_ a ((b) (b)) ((b b)))
testPattern5 = PatListSome [PatIdentifier "a"] (PatListSome [] (PatIdentifier "b"))
testSxp5 = Sxp [ Sym "a", Sxp[Sxp[Sym "b"], Sxp[Sym "b"]], Sxp[Sxp[Sym "b", Sym "b"]]]
-- (_ x (y (z)) ...)
testPattern3 = PatListSome [PatIdentifier "x"] (PatList [PatIdentifier "y", PatList[PatIdentifier "z"]])
testSxp3 = Sxp [ Sym "x", Sxp[Sym "y", Sxp[Sym "z"]], Sxp[Sym "y", Sxp[Sym "z"]]]
-- (_ a (b (c d ...)) ...)
testPattern4 = PatListSome [PatIdentifier "a"]
               (PatList [PatIdentifier "b", PatListSome [PatIdentifier "c"]
                                            (PatIdentifier "d")])
testSxp4 = Sxp [ Sym "a", Sxp[Sym "b", Sxp[Sym "c"]], Sxp[Sym "b", Sxp[Sym "c", Sym "d", Sym "d"]]]

useSome :: Matcher a -> Matcher a
useSome pva = do
  modify $ \s -> s { someSwitch = True }
  a <- pva
  modify $ \s -> s { someSwitch = False }
  return a

getSeq :: HasSeqState m => m Seq
getSeq = seq <$> getSeqState

getSeqs :: HasSeqState m => m [Seq]
getSeqs = seqs <$> getSeqState

putSeqs :: HasSeqState m => [Seq] -> m ()
putSeqs seqs' = do
  seqs <- getSeqState
  putSeqState (seqs { seqs = seqs' })

resetSeq :: HasSeqState m => m ()
resetSeq = do
  seqs <- getSeqState
  putSeqState (seqs { seq = -1 })

incSeq :: HasSeqState m => m ()
incSeq = do
  seqs <- getSeqState
  putSeqState (seqs { seq = seq seqs + 1 })

addPatternVar :: Symbol -> Stree -> Matcher ()
addPatternVar sym stree = do
  somep <- gets someSwitch
  pvs <- gets patternVars
  if not somep then
    modify $ \s -> s { patternVars = Map.insert sym (PVStree stree) pvs }
  else do
    seqs <- getSeqs
    let f = \case
               Nothing -> Just $ PVSome $ Map.singleton seqs stree
               Just (PVSome map) -> Just $ PVSome $ Map.insert seqs stree map
               _ -> error "Pattern identifier are not allowed to share names"
    modify $ \s -> s { patternVars = Map.alter f sym pvs }



tryMatch :: Pattern -> Stree -> Maybe PVS
tryMatch p s = let (b, PV { patternVars }) = runState (runMatcher $ matches p s) (PV { patternVars = Map.empty, someSwitch = False, matSeqState = defaultSeqState}) in
  if b then Just patternVars else Nothing
  where
    -- Pattern = P, Stree = F
    matches :: Pattern -> Stree -> Matcher Bool
    -- Non Literal Identifier always match
    matches (PatIdentifier sym) stree = do
      addPatternVar sym stree
      return True

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
        if length inits == length list then do
          mp <- and <$> zipWithM matches list inits
          resetSeq
          mp' <- useSome $ forM rest $ \r -> do
            incSeq
            seq <- getSeq
            seqs <- getSeqs
            b <- putSeqs (seqs ++ [seq]) >> matches some r
            putSeqs seqs
            return b


          return $ mp && and mp'
        else
          return False

    -- The empty pattern always matches
    matches PatEmpty _ = return True

-------------------------------------------------------------------------------
-- Constructor
-------------------------------------------------------------------------------
testTemplate = Sxp [ Sym "a", Sym "...", Sym "b", Sym "..." ]
testTemplate2 = Sxp [ Sxp[Sym "a", Sym "..."], Sxp[Sym "b", Sym "..."] ]
testTemplate3 = Sxp [ Sxp[Sym "a", Sym "b"], Sym "..." ]

parseTemplate :: [Symbol] -> Stree -> Template
parseTemplate lits stree = case stree of
  Sym _ | isNoVar stree -> TempLiteral stree
  Sym s -> TempVar s
  _ | isConst stree -> TempLiteral stree

  Sxp l -> TempList $ parseTemplateList l
  where
    parseTemplateList :: [Stree] -> [Template]
    parseTemplateList strees = case strees of
      [] -> []
      v@(Sym s):Sym "...":tl | isVar v -> TempSome (TempVar s):parseTemplateList tl
      s@(Sxp _):Sym "...":tl -> TempSome (parseTemplate lits s):parseTemplateList tl
      hd:tl -> parseTemplate lits hd:parseTemplateList tl

    isNoVar :: Stree -> Bool
    isNoVar (Sym s) = elem s lits
    isNoVar _ = True

    isVar = not . isNoVar


setSplice :: Constructor ()
setSplice = do
  modify $ \s -> s { spliceSwitch = True }

unsetSplice :: Constructor ()
unsetSplice = do
  modify $ \s -> s { spliceSwitch = False }

getVar :: MonadState ConstructorState m => Symbol -> m PVValue
getVar sym = do
  cpvs <- gets cpatternVars
  let maybestree = cpvs Map.!? sym
  maybe (error "Didn't find var while constructing the template") return maybestree

-- TODO to properly construct sexps, I need to store in what sequence a pattern var matched something in the input.
-- Consider:
-- (define-syntax test2
--   (syntax-rules ()
--     ((test (a b ...) ...) ((a b ...) ...))))
-- (test2 (a b b b) (a b b)) -> ((a b b b) (a b b))


getCountOfTemplate :: Int -> [[Seq]] -> Int
getCountOfTemplate _ [] = 0
getCountOfTemplate depth l@(hd:_)
  | depth > length hd = error "invalid depth" -- the depth of a var doesn't change, so we just check the head
  | otherwise = maximum $ fmap (head . drop depth) l

getVarInList :: Template -> Maybe Symbol
getVarInList (TempVar v) = Just v
getVarInList (TempList []) = Nothing
getVarInList (TempList (hd:tl)) = getVarInList hd <|> fix (\rec -> \case
                                                              [] -> Nothing
                                                              (hd:tl)-> getVarInList hd <|> rec tl) tl
getVarInList (TempSome t) = getVarInList t
getVarInList (TempLiteral _) = Nothing

construct :: Template -> Constructor Stree
construct (TempLiteral lit) = return lit
construct (TempVar v) = do
  var <- getVar v
  case var of
    PVStree stree -> return stree
    PVSome _ -> error "Error ... can only be used in lists"
construct (TempSome (TempVar _)) = error "Variables followed by ... (Ellipses) must be inside a list"
construct (TempSome (TempLiteral _)) = error "Literal can't be used with ... (Ellipses)"

construct (TempList l) = Sxp <$>  constructList l


constructList :: [Template] -> Constructor [Stree]
constructList (TempVar v:tl) = do
  seqs <- getSeqs
  PVSome var <- getVar v
  let var' = var Map.!? seqs
      var'' = maybe (error "Var not found") return var'

  liftA2 (:) var'' (constructList tl)

constructList (TempSome (TempVar v):tl) = do
  seq <- getSeq
  seqs <- getSeqs
  PVSome var <- getVar v
  let vars = Map.assocs var -- [([Seq], Stree)]
      count = getCountOfTemplate seq (fmap fst vars) -- how many vars do we take?
      seqs' = fmap (\i -> seqs++[i]) [0..(count-1)]  -- with which [Seq] do we take vars?
      vars' = filter (\(seq, _) -> elem seq seqs') vars -- take only the vars at the right position
      vars'' = fmap snd vars' -- remove the [Seq], getting only the Stree

  liftA2 (++) (return vars'') (constructList tl)

constructList (TempSome t@(TempList l):tl) = do
  let var = maybe (error "No var in template") id (getVarInList t)
  PVSome var' <- getVar var
  seq <- getSeq
  seqs <- getSeqs
  let vars = Map.assocs var'
      count = getCountOfTemplate seq (fmap fst vars)
  incSeq
  lists <- forM [0..count-1] $ \pos -> do
    putSeqs $ seqs++[pos]
    constructList l
  putSeqs seqs
  resetSeq
  let lists' = concat lists

  liftA2 (++) (return lists') (constructList tl)

constructList (TempSome (TempLiteral _):_) = error "Literal can't be used with ... (Ellipses)"

constructList (hd:tl) = do
  hd' <- construct hd
  liftA2 (:) (return hd') (constructList tl)


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
