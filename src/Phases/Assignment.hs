{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 5. Phase Removes set! by wrapping local variables and parameters in one-element vectors called boxes.

-- ((lambda (x)
--   (let ((c 2))
--    (set! x 2)
--    (set! c 4))) 4)
-- =>
-- ((lambda (x)
--   (let ((c (vec 2)))
--    (vector-set! x 0 2)
--    (voctor-set! c 0 4))) (vec 4))

module Phases.Assignment where
import RIO
import RIO.State
import qualified RIO.Map as M
import RIO.Set as S
import Types.Types
import Types.Constructors
import Types.Pprint
import Prelude (print)


transform :: ScEnv ()
transform = do
  logInfo "Performing assignment transformation"
  astref <- asks _ast
  ast <- readSomeRef astref

  let (ast', frees) = go ast
  logDebug $ "Free variables in AST:\n" <> display (show $ S.map unUniqName frees)
  logDebug $ "AST after assignment transformation:\n" <> display ast'

  writeSomeRef astref ast'
  return ()

-- All mutated Variables
type Mutated = S.Set UniqName
type SM = State Mutated

go :: ScSyn Name -> (ScSyn Name, Mutated)
go syn = runState (callWithAlphaM go' syn) S.empty
  where
    go' :: ScSyn UniqName -> SM (ScSyn UniqName)
    go'= descendM (makeMap removeSet)

-- Wraps an expression in a vector, called Box
makeBox :: Expr UniqName -> Expr UniqName
makeBox e = makePrimApp ("make-vector" :: PrimName) [ELit $ LitInt 1, e]

-- Wraps the Expr in a Binding in a Box
makeBoxBinding :: Binding UniqName -> Binding UniqName
makeBoxBinding [(n, e)] = [(n, makeBox e)]

-- Sets the value of a Box
makeBoxSet :: UniqName -> Expr UniqName -> Int -> Expr UniqName
makeBoxSet name e i = makePrimApp ("vector-set!" :: PrimName) [EVar name, ELit $ LitInt i, e]

-- Gets the value of a Box
makeBoxGet :: UniqName -> Int -> Expr UniqName
makeBoxGet name i = makePrimApp ("vector-ref" :: PrimName) [EVar name, ELit $ LitInt i]

isMutated' :: Mutated -> UniqName -> Bool
isMutated' m n = S.member n m

add :: UniqName -> Mutated -> Mutated
add = S.union . S.singleton

-- Calls makeBox* on Vars, respecting their index in the Vector
callBox :: [(UniqName, Int)]
  -> UniqName
  -> Body UniqName
  -> Body UniqName
callBox al newname b =
  runIdentity $ runReaderT (descendBodyM (makeMap f) b) (M.fromList al)
  where
    f :: Expr UniqName -> ReaderT (M.Map UniqName Int) Identity  (Expr UniqName)
    f e = do
      map <- ask
      case e of
        EVar n -> do
          let pair = M.lookup n map
          if isJust pair then do
            let (Just i) = pair
            return $ makeBoxGet newname i
          else do
            return e
        ESet n rhs -> do
          let pair = M.lookup n map
          if isJust pair then do
            let (Just i) = pair
            return $ makeBoxSet newname rhs i
          else do
            return e

        x -> return x

removeSet :: Expr UniqName -> SM (Expr UniqName)
removeSet e = do
  ms <- get -- mutated vars
  let isMutated = isMutated' ms

  case e of
    ESet n _ -> do
      modify (add n)
      return e

    ELet (Let pat@[(n, _)] b) | isMutated n -> do
      let b' = callBox [(n, 0)] n b
      return $ ELet $ Let (makeBoxBinding pat) b'

    EApp (AppLam e es) -> return $ makeLamApp e [makeVectorFromList es]

    ELam (Lam ns@(n:_) b) -> do
      let ns' = makeGloballyUniqueName n b
      let b' = callBox (zip ns [0..]) ns' b
      return $ ELam $ Lam [ns'] b'

    ELam (LamList n b) -> do
      let n' = makeGloballyUniqueName n b
      let b' = callBox [(n, 0)] n' b
      return $ ELam $ LamList n' b'

    x -> return x
