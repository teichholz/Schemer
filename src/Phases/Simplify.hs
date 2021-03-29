{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 2. Phase: Simplifying scheme syntax in three ways

-- 1. We remove lambda with a list as the parameter:
-- ((lambda (x y . z) (x y z)) 1 2 '(1 2 3))
-- =>
-- ((lambdal zgen
--    (let ((x (car zgen))
--          (y (cadr zgen))
--          (z (cddr zgen))))
--      (x y z))
--  1 2 '(1 2 3))

-- 2. We sequence the Binding of a let
-- (let ((x 2)
--        y 3)
--   (+ x y))
-- =>
-- (let ((x 3))
--   (let ((y 2)))
--     (+ x y))

-- 3. We sequence the body of a let
-- (let ((_ _))
--   (io)
--   (io))
-- =>
-- (let ((_ _))
--   (let ((freshvar (io)))
--     (io)))

-- Step 1 and 3 make use of undbound-generics to get locally fresh variables.
-- Even though the variable in 3 is never used, we need to ensure it is by either hardcoding it, like for example using an illegal scheme identifier. Or we use a fresh variable.

module Phases.Simplify where

import RIO
import Types.Types
import Types.Pprint
import RIO.State
import Prelude (print)
import Types.Constructors


transform :: ScEnv ()
transform = do
  logInfo "Performing simplify transformation"
  astref <- asks _ast
  ast <- readSomeRef astref

  let frees = getFreeVars ast
  logDebug $ "Free Vars:\n" <> mconcat (display <$> frees)

  logDebug $ "Uniq name:\n" <> display (makeUniqueName "seqbody" ast)

  let ast' = go ast

  logDebug $ "Simplified AST:\n" <> display ast'
  logDebug $ display $ show ast'


  writeSomeRef astref ast'
  return ()

go :: ScSyn -> ScSyn
go = runIdentity sequenceLet . runIdentity flattenLet . runIdentity lambdad2lambdal

type IE = Identity Expr

lambdad2lambdal :: Expr -> IE
lambdad2lambdal = \case
  ELam lam -> go lam
  x -> return x
  where
    go :: Lambda -> IE Expr
    go = \case
      LamDot (args, dotarg) body -> do
        lamlarg <- return dotarg -- todo make unique
        let binding = toBinding $ evalState (go' (toExpr lamlarg) (args, dotarg)) (id, [])
        return $ makeLamList lamlarg (makeLet binding body)
      x -> return $ toExpr x

    go' ::  Expr -> ([Name], Name) -> State (Expr -> Expr, [(Name, Expr)]) [(Name, Expr)]
    go' lamlarg (ns, dotn) = do
      (app, _) <- get
      case ns of
        [] -> do
            modify (second ((dotn, app lamlarg):))
            gets snd
        n:ns -> do
            modify (bimap (cdr .) ((n, car $ app lamlarg):))
            go' lamlarg (ns, dotn)

flattenLet :: Expr -> IE Expr
flattenLet = \case
  ELet (Let p t) -> return $ go p t
  x -> return x
  where
    go :: Binding -> Body -> Expr
    go bind body = case bind of
      [b] -> makeLet (toBinding b) body
      b:tl -> makeLet (toBinding b) (toBody $ go tl body)


sequenceLet :: Expr -> IE Expr
sequenceLet = \case
  lt@(ELet (Let binding body)) -> do
    let ss@(_:bs) = unBody body
    if null bs then -- Body has one expression
      return lt
    else -- Body has >1 expressions
      makeLet binding <$> go ss
  x -> return x
  where
    go :: [ScSyn] -> IE Expr
    go [s, send] = do
      let uniqName = makeUniqueName "seqbody" send
      return $ makeLet (uniqName, toExpr s) send
    go (s:ss) = do
      let uniqName = makeUniqueName "seqbody" ss
      makeLet (uniqName, toExpr s) <$> go ss
