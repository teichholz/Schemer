-- |

module Expander.Matcher where

import RIO hiding (Seq, seq)
import RIO.State
import qualified RIO.Map as Map
import Expander.Ast
import Expander.Seq

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

data PVValue
  = PVStree Stree
  | PVSome (Map [Seq] Stree)
  deriving (Show, Eq)

type PVS = Map Symbol PVValue
data PV = PV { patternVars :: PVS, someSwitch :: Bool, seqState :: SeqState }

newtype Matcher a = Matcher { runMatcher :: State PV a }
  deriving (Functor, Applicative, Monad, MonadState PV)


instance HasSeqState Matcher where
  getSeqState = gets seqState
  putSeqState seqs = modify $ \s -> s { seqState = seqs }

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

parse :: [Symbol] -> [Stree] -> Pattern
parse lits l = evalState (parsePatternList l) []
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
        Sym sym | sym `elem` lits -> return $ PatLiteral pat
        Sym sym ->  return $ PatIdentifier sym
        _ | isConst pat -> return $ PatLiteral pat
        Sxp pats -> do
          s <- get
          l <- put [] >> parsePatternList pats
          put s >> return l
        _ -> error "Wrong pattern"

-------------------------------------------------------------------------------
-- Matcher
-------------------------------------------------------------------------------

useSome :: Matcher a -> Matcher a
useSome pva = do
  modify $ \s -> s { someSwitch = True }
  a <- pva
  modify $ \s -> s { someSwitch = False }
  return a


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
tryMatch p s = let (b, PV { patternVars }) = runState (runMatcher $ matches p s) (PV { patternVars = Map.empty, someSwitch = False, seqState = seqStateWith (-1)}) in
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
-- Tests, ... sort of
-------------------------------------------------------------------------------

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
