-- |

module Expander.Constructor where

import RIO hiding (Seq, seq)
import Data.Foldable (maximum)
import RIO.List.Partial (head, (!!))
import RIO.State
import qualified RIO.Map as Map
import Expander.Matcher (PVS, PVValue(..))
import Expander.Ast
import Expander.Seq
import Utils.NameResolver (isPrim)


data Template
  = TempLiteral Stree
  | TempVar Symbol
  | TempSome Template
  | TempList [Template]
  deriving (Show, Eq)


data ConstructorState = ConstructorState { cpatternVars :: PVS, seqState :: SeqState }

newtype Constructor a = Constructor { runConstructor :: State ConstructorState a }
  deriving (Functor, Applicative, Monad, MonadState ConstructorState)

instance HasSeqState Constructor where
  getSeqState = gets seqState
  putSeqState seqs = modify $ \s -> s { seqState = seqs }

instance MonadFail Constructor

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

parse :: [Symbol] -> Stree -> Template
parse lits stree = case stree of
  Sym _ | isNoVar stree -> TempLiteral stree
  Sym s -> TempVar s
  _ | isConst stree -> TempLiteral stree

  Sxp l -> TempList $ parseList l
  where
    parseList :: [Stree] -> [Template]
    parseList strees = case strees of
      [] -> []
      (Sym "quote"):_ -> fmap TempLiteral strees
      v@(Sym s):Sym "...":tl | isVar v -> TempSome (TempVar s):parseList tl
      s@(Sxp _):Sym "...":tl -> TempSome (parse lits s):parseList tl
      hd:tl -> parse lits hd:parseList tl

    isNoVar :: Stree -> Bool
    isNoVar (Sym s) = s `elem` lits || isPrim s
    isNoVar _ = True

    isVar = not . isNoVar

-------------------------------------------------------------------------------
-- Constructor
-------------------------------------------------------------------------------

getVar :: Symbol -> Constructor PVValue
getVar sym = do
  cpvs <- gets cpatternVars
  let maybestree = cpvs Map.!? sym
  maybe
    (error $ "Didn't find var while constructing the template: " <> "Searched for: " <> show sym <> " Vars are: " <> show cpvs)
    return maybestree


getCountOfTemplate :: Int -> [[Seq]] -> Int
getCountOfTemplate _ [] = 0
getCountOfTemplate depth l@(hd:_)
  | depth > length hd = error "invalid depth" -- the depth of a var doesn't change, so we just check the head
  | otherwise = maximum $ fmap (!! depth) l

getVarInList :: Template -> Maybe Symbol
getVarInList (TempVar v) = Just v
getVarInList (TempList []) = Nothing
getVarInList (TempList (hd:tl)) = getVarInList hd <|> fix (\rec -> \case
                                                              [] -> Nothing
                                                              (hd:tl)-> getVarInList hd <|> rec tl) tl
getVarInList (TempSome t) = getVarInList t
getVarInList (TempLiteral _) = Nothing

construct :: Template -> PVS -> Stree
construct t pvs =
  let c = construct t in
    evalState (runConstructor c) (ConstructorState { cpatternVars = pvs, seqState = seqStateWith 0})
  where
    construct :: Template -> Constructor Stree
    construct (TempLiteral lit) = return lit
    construct (TempVar v) = do
      var <- getVar v
      case var of
        PVStree stree -> return stree
        PVSome _ -> error "Error ... can only be used in lists"
    construct (TempSome (TempVar _)) = error "Variables followed by ... (Ellipses) must be inside a list"
    construct (TempSome (TempLiteral _)) = error "Literal can't be used with ... (Ellipses)"

    construct (TempList l) = Sxp <$> constructList l


    constructList :: [Template] -> Constructor [Stree]
    constructList [] = return []
    constructList (TempVar v:tl) = do
      var <- getVar v
      let var' = case var of
            PVStree stree -> return stree
            PVSome var -> do
              seqs <- getSeqs
              let var' = var Map.!? seqs
              maybe (error "Var not found") return var'

      liftA2 (:) var' (constructList tl)

    constructList (TempSome (TempVar v):tl) = do
      seq <- getSeq
      seqs <- getSeqs
      PVSome var <- getVar v
      let vars = Map.assocs var -- [([Seq], Stree)]
          count = getCountOfTemplate seq (fmap fst vars) -- how many vars do we take?
          seqs' = fmap (\i -> seqs++[i]) [0..count]  -- with which [Seq] do we take vars?
          vars' = filter (\(seq, _) -> seq `elem` seqs') vars -- take only the vars at the right position
          vars'' = fmap snd vars' -- remove the [Seq], getting only the Stree

      fmap (vars''++) (constructList tl)

    constructList (TempSome t@(TempList l):tl) = do
      let var = fromMaybe (error "No var in template") (getVarInList t)
      PVSome var' <- getVar var
      seq <- getSeq
      seqs <- getSeqs
      let vars = Map.assocs var'
          count = getCountOfTemplate seq (fmap fst vars)
      incSeq
      lists <- forM [0..count] $ \pos -> do
        putSeqs $ seqs++[pos]
        Sxp <$> constructList l
      putSeqs seqs
      resetSeq

      fmap (lists++) (constructList tl)

    constructList (TempSome (TempLiteral _):_) = error "Literal can't be used with ... (Ellipses)"

    constructList (hd:tl) = do
      hd' <- construct hd
      fmap (hd':) (constructList tl)

-------------------------------------------------------------------------------
-- Tests, ... sort of
-------------------------------------------------------------------------------

-- (b ...)
testTemplate = parse [] (Sxp [ Sym "b", Sym "..." ])
testPVS = Map.fromList [("b", b)]
  where
    b = PVSome $ Map.fromList [([0], Sym "b"), ([1], Sym "b"), ([2], Sym "b"), ([3], Sym "b")]

-- ((a b) ...)
testTemplate2 = parse [] (Sxp [ Sxp [ Sym "a", Sym "b" ], Sym "..." ])
testPVS2 = Map.fromList [("a", a), ("b", b)]
  where
    a = PVSome $ Map.fromList [([0], Sym "a"), ([1], Sym "a"), ([2], Sym "a"), ([3], Sym "a")]
    b = PVSome $ Map.fromList [([0], Sym "b"), ([1], Sym "b"), ([2], Sym "b"), ([3], Sym "b")]

-- (a ... b ...)
testTemplate3 = parse [] (Sxp [ Sym "a", Sym "...", Sym "b", Sym "..." ])
testPVS3 = Map.fromList [("a", a), ("b", b)]
  where
    a = PVSome $ Map.fromList [([0], Sym "a"), ([1], Sym "a"), ([2], Sym "a"), ([3], Sym "a")]
    b = PVSome $ Map.fromList [([0], Sym "b"), ([1], Sym "b"), ([2], Sym "b"), ([3], Sym "b")]

-- ((b ...) ...)
-- ((b b) (b))
-- b: [00=b, 01=b, 10=b]
testTemplate4 = parse [] (Sxp [ Sxp [Sym "b", Sym "..."], Sym "..."])
testPVS4 = Map.fromList [("b", b)]
  where
    b = PVSome $ Map.fromList [([0,0], Sym "b"), ([0,1], Sym "b"), ([1,0], Sym "b")]
