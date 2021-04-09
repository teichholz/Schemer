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

-- 3. We sequence all bodies
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

  let ast' = go ast

  logDebug $ "Simplified AST:\n" <> display ast'

  writeSomeRef astref ast'
  return ()

run =  descend

type EN = Expr Name
type SN = ScSyn Name
type IE = Identity (Expr Name)

go :: SN -> SN
go = run sequenceBody . run flattenLet . run lambdad2lambdal



lambdad2lambdal :: EN -> EN
lambdad2lambdal = \case
  ELam lam -> go lam
  x -> x
  where
    go :: Lambda Name -> EN
    go = \case
      LamDot (args, dotarg) body -> do
        let lamlarg = makeUniqueName (dotarg <> "'") dotarg
        let binding = toBinding $ evalState (go' (toExpr lamlarg) (args, dotarg)) (id, [])
        makeLamList lamlarg (makeLet binding body)
      x -> toExpr x

    go' :: EN -> ([Name], Name) -> State (EN -> EN, [(Name, EN)]) [(Name, EN)]
    go' lamlarg (ns, dotn) = do
      (app, _) <- get
      case ns of
        [] -> do
            modify (second ((dotn, app lamlarg):))
            gets snd
        n:ns -> do
            modify (bimap (cdr .) ((n, car $ app lamlarg):))
            go' lamlarg (ns, dotn)

flattenLet :: Expr a  -> Expr a
flattenLet = \case
  ELet (Let p t) -> go p t
  x -> x
  where
    go :: Binding a -> Body a -> Expr a
    go bind body = case bind of
      [b] -> makeLet b body
      b:tl -> makeLet b (toBody $ go tl body)


sequenceBody :: Expr Name -> Expr Name
sequenceBody = \case
  ELet (Let binding body) -> makeLet binding (seq body)
  ELam (Lam ns b) -> makeLam ns (seq b)
  ELam (LamList n b) -> makeLamList n (seq b)
  x -> x
  where
    go :: [SN] -> EN
    go [s, send] = do
      let uniqName = makeUniqueName "seqbody" send in
        makeLet (uniqName, toExpr s) send
    go (s:ss) = do
      let uniqName = makeUniqueName "seqbody" ss in
        makeLet (uniqName, toExpr s) $ go ss
    seq :: Body Name -> Body Name
    seq b =
      let ss@(_:bs) = unBody b in
        if null bs then -- Body has one expression
          b
        else -- Body has >1 expressions
          toBody $ go ss
