-- |

module Expander.Expander where
import RIO
import Types.Types (Sexp(..))
import Types.Exceptions
import Sexp.Literals
import qualified RIO.Map as Map
import RIO.State (State)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
-- Scheme Syntax
data SchemeObject
  = LitString String
  | LitSymbol String
  | LitInt Int
  | LitFloat Float
  | LitChar Char
  | LitBool Bool
  | LitList [SchemeObject]
  | LitSynList [SyntaxObject]
  | LitNil
  deriving (Show, Eq)

data SyntaxObject = SyntaxObject { schemeObject :: SchemeObject, sobjWrap:: Wrap }
  deriving (Show, Eq)

type Wrap = [ Either Mark Subst ]

type Symbol = String
data Mark
  = TopMark
  | Mark Int
  deriving (Show, Eq)
type Label = Int

data Subst = Subst { sym :: Symbol, marks :: [Mark], label :: Label }
  deriving (Show, Eq)

-- Binding
data BindType = Core | Lexical | Macro
data Binding = Binding { typ :: BindType, val :: Void }
type Bindings =  [Binding]

-- Env
data Env = Env { bindings :: Bindings,  wrap :: Wrap, names :: Names}
newtype Expander a = Expander { runExpander :: State Env a }

-- Fresh Names
type Name = ByteString
type Names = Map.Map Name Int

freshName :: Name -> Names -> (Name, Names)
freshName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm <> fromString (show ix), Map.insert nm (ix + 1) ns)

-------------------------------------------------------------------------------
-- Accessors
-------------------------------------------------------------------------------
wrapMarks :: Wrap -> [Mark]
wrapMarks (Left m:tl) = m : wrapMarks tl
wrapMarks (_:tl) = wrapMarks tl
wrapMarks [] = []

wrapSubsts :: Wrap -> [Subst]
wrapSubsts (Right s:tl) = s : wrapSubsts tl
wrapSubsts (_:tl) = wrapSubsts tl
wrapSubsts [] = []
-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------
parseLit :: Sexp -> IO SchemeObject
parseLit = \case
  Atom x | isString x -> return $ LitString (parseLiteral stringLiteral x)
  Atom x | isChar x -> return $ LitChar (parseLiteral charLiteral x)
  Atom x | isInt x -> return $ LitInt (parseLiteral intLiteral x)
  Atom x | isFloat x -> return $ LitFloat (parseLiteral floatLiteral x)
  Atom x | isBool x -> return $ LitBool (parseLiteral boolLiteral x)
  s -> throwM $ ParseException $ "Wrong literal: " <> show s

-- Most primitive Scheme parser which can be used for interpreters
parse :: Sexp -> IO SchemeObject
parse sxp = case sxp of
  List es -> LitList <$> parseList es
  Atom _ -> case sxp of
    Atom x | isName x -> return $ LitSymbol (parseLiteral varLiteral x)
    Atom _ ->  parseLit sxp
  where
    parseList :: [Sexp] -> IO [SchemeObject]
    parseList sxps = case sxps of
      [tl] -> do
        tl <- parse tl
        return [tl, LitNil]

      [Atom ".", tl] -> do
        tl <- parse tl
        return [tl]

      hd:tl -> do
        hd <- parse hd
        tl <- parseList tl
        return $ hd:tl

-------------------------------------------------------------------------------
-- Stripping Syntax Objects
-------------------------------------------------------------------------------

isTopMarked :: Wrap -> Bool
isTopMarked = any
  (\case
    Left TopMark -> True
    _ -> False)

strip :: Either SyntaxObject SchemeObject -> SchemeObject
strip (Left synobj) =
  if isTopMarked $ sobjWrap synobj
  then schemeObject synobj
  else strip $ Right $ schemeObject synobj

strip (Right x@(LitList (hd:tl))) =
  let a = strip $ Right hd
      d = strip $ Right $ LitList tl in
    if a == hd && d == LitList tl
    then x
    else LitList $ hd:tl

strip (Right x) = x

-------------------------------------------------------------------------------
-- Syntax Error while expanding
-------------------------------------------------------------------------------
syntaxError :: MonadThrow m => SyntaxObject -> String -> m a
syntaxError synobj str = throwM $ ExpandException $ str <> show (schemeObject synobj)

-------------------------------------------------------------------------------
-- Structural Predicates
-------------------------------------------------------------------------------
isIdentifier :: SyntaxObject -> Bool
isIdentifier SyntaxObject { schemeObject = (LitSymbol _) } = True
isIdentifier _ = False

isSelfEvaluating :: SchemeObject -> Bool
isSelfEvaluating (LitList _ ) = False
isSelfEvaluating (LitSymbol _) = False
isSelfEvaluating _ = True

-------------------------------------------------------------------------------
-- Creating Wraps
-------------------------------------------------------------------------------

-- TODO don't use Either
addMark :: Mark -> SyntaxObject -> SyntaxObject
addMark m synobj = extendWrap [Left m] synobj

addSubst :: Symbol -> Label -> SyntaxObject -> SyntaxObject
addSubst sym' l synobj =
  let subst = Subst { sym = sym', marks = wrapMarks $ sobjWrap synobj, label = l } in
  extendWrap [Right subst] synobj

-- TODO use class to extend Syn and Scheme objcets
extendWrap :: Wrap -> SyntaxObject -> SyntaxObject
extendWrap = error "not implemented"


-- TODO marks need to cancel each other out
joinWraps :: Wrap -> Wrap -> Wrap
joinWraps w1 w2 = error "not implemented"


-------------------------------------------------------------------------------
-- Manipulating Environments
-------------------------------------------------------------------------------

extendEnv :: Label -> Binding -> Env -> Env
extendEnv l b e = error "not implemented"

-------------------------------------------------------------------------------
-- Identifier Resolution
-------------------------------------------------------------------------------

idBinding :: SyntaxObject -> Env -> Void
idBinding sym env = error "not implemented"

idLabel :: SyntaxObject -> Maybe Label
idLabel syn = error "not implemented"

labelBinding :: Label -> Env -> Void
labelBinding l env = error "not implemented"

-------------------------------------------------------------------------------
-- The Expander
-------------------------------------------------------------------------------
