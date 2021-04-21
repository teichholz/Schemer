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
import Phases.Simplify (flattenLet)


transform :: ScEnv ()
transform = do
  logInfo "Performing assignment transformation"
  astref <- asks _ast
  ast <- readSomeRef astref

  logDebug $ "AST with unique names before assignment transformation:\n" <> display (runAlpha ast)
  let (ast', frees) = go ast
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
    go' syn = descendM (makeMap mvs) syn >> descendM (makeMap removeSet) syn --This prepares the state with all mutated vars

-- Wraps an expression in a vector, called Box
makeBox :: Expr UniqName -> Expr UniqName
makeBox e = makePrimApp ("make-vector" :: PrimName) [ELit $ LitInt 1, e]

-- Wraps the Expr in a Binding in a Box
makeBoxBinding :: (UniqName, Expr UniqName) -> Binding UniqName
makeBoxBinding (n, e) = [(n, makeBox e)]

-- Sets the value of a Box
makeBoxSet :: Expr UniqName ->  Expr UniqName
makeBoxSet (ESet n e) = vectorSet (toExpr n) 0 e

-- Gets the value of a Box
makeBoxGet :: Expr UniqName -> Expr UniqName
makeBoxGet var = vectorRef var 0

isMutated' :: Mutated -> UniqName -> Bool
isMutated' m n = S.member n m

add :: UniqName -> Mutated -> Mutated
add = S.union . S.singleton

mvs :: Expr UniqName -> SM (Expr UniqName)
mvs e = case e of
    ESet n _ -> do
      modify (add n)
      return e
    _ -> return e

removeSet :: Expr UniqName -> SM (Expr UniqName)
removeSet e = do
  ms <- get -- mutated vars
  let isMutated = isMutated' ms

  case e of
    ESet _ _ -> do
      return $ makeBoxSet e

    ELet (Let [pat@(n, _)] b) | isMutated n -> do
      return $ ELet $ Let (makeBoxBinding pat) b

    EVar n | isMutated n -> return $ makeBoxGet e

    ELam (Lam ns b) -> do
      let f  = \n -> do
            (ns', bs) <- get
            if isMutated n then do
              let n' = makeGloballyUniqueName "mutatedlamarg" b
              put (ns' ++ [n'], bs ++ makeBoxBinding (n, toExpr n'))
            else
              put (ns' ++ [n], bs)

      let (ns', bs) = runIdentity $ execStateT (mapM f ns) ([], [])
          b' = if ns == ns' then b else toBody (flattenLet $ ELet $ Let bs b)

      return $ ELam $ Lam ns' b'

    ELam (LamList n b) | isMutated n -> do
      let n' = makeGloballyUniqueName "mutatedlamlarg" b
      return $ ELam $ LamList n' (toBody $ ELet $ Let (makeBoxBinding (n, toExpr n')) b)

    x -> return x
