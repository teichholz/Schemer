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

  let ast' = ast
  logDebug $ "AST after assignment transformation:\n" <> display ast

  -- logDebug $ display $ show $ execState (go ast') S.empty

  writeSomeRef astref ast'
  return ()

go :: SynN -> SynN
go = runDescendM (`evalState` S.empty) removeSet

-- All mutated Variables
type Mutated = S.Set Name
type SM = State Mutated

-- Wraps an expression in a vector, called Box
makeBox :: ExprN -> ExprN
makeBox e = makePrimApp ("make-vector" :: PrimName') [makeInt 0, e]

-- Wraps the Expr in a Binding in a Box
makeBoxBinding :: BindN -> BindN
makeBoxBinding [(n, e)] = [(n, makeBox e)]

-- Sets the value of a Box
makeBoxSet :: ExprN -> ExprN
makeBoxSet (ESet n e) = makePrimApp ("vector-set!" :: PrimName') [toExpr n, makeInt 0, e]

-- Gets the value of a Box
makeBoxGet :: ExprN -> ExprN
makeBoxGet (EVar n) = makePrimApp ("vector-ref" :: PrimName') [toExpr n, makeInt 0]

isMutated' :: Mutated -> Name -> Bool
isMutated' m n = S.member n m

add :: Name -> Mutated -> Mutated
add = S.union . S.singleton

removeSet :: ExprN -> SM ExprN
removeSet e = do
  ms <- get -- mutated vars
  let isMutated = isMutated' ms

  case e of
    ESet n _ -> do
      modify $ add n
      return $ makeBoxSet e

    EVar n | isMutated n ->
      return $ makeBoxGet e

    ELet (Let pat@[(n, _)] body) ->
      if isMutated n then
        return $ makeLet (makeBoxBinding pat) body
      else
        return e

    -- ELam (Lam ns b) ->

    x -> return x
