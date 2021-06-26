{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE OverloadedStrings #-}
-- | 3. Phase: Transformation to administrative normal form

-- This phase ensures that non-atomic values will be letbound. The value function decdies which values are atomic and which not.
-- By definition atomic values are literals, lambdas and variables. ANF will cause all applications and applys in a body to be letbound, except the ones that return.
-- This associates with scheme's call-by-value as well as continuation passing style, which is the next transformation.

module Phases.ANF where

import RIO
import RIO.List.Partial (head)
import RIO.State
import Control.Monad.Cont hiding (Cont)
import qualified Control.Monad.Cont as C (Cont, ContT)
import Types.Types
import Types.Constructors
import Types.Pprint

instance MonadFail Identity


transform :: ScEnv ()
transform = do
  logInfo "Performing transformation to ANF"
  astref <- asks _ast
  ast <- readSomeRef astref
  let ast' = go ast

  logDebug $ "AST in ANF:\n" <> display ast'

  writeSomeRef astref ast'
  return ()

-- The Cont monad represents computations in continuation passing style.
-- That is, we call a continuation with [Expr] as argument, which will itself produce an [Expr] based on the [Expr] argument.
type CTR = State Int
type Cont = C.ContT (Expr Name) CTR (Expr Name)


-- Exprs that instantly return a value, thus they are atomic.
isValue :: Expr Name -> Bool
isValue (ELit _) = True
isValue (ELam _) = True
isValue (EVar _) = True
isValue _ = False

go :: ScSyn Name -> ScSyn Name
go = toSyn . normalizeTerm . toExpr

normalizeTermWith :: ToExpr e Name => e -> (Expr Name -> CTR (Expr Name)) -> Expr Name
normalizeTermWith e f = let state = runContT (normalize e) f in evalState state 0

normalizeTerm :: ToExpr e Name => e -> Expr Name
normalizeTerm e = normalizeTermWith e return

normalize :: ToExpr e Name => e -> Cont
normalize e = case toExpr e of
  ELam (Lam pat body) -> do
      let lam =  makeLam pat (normalizeTerm body)
      return lam

  ELam (LamList pat body) -> do
      let lam =  makeLamList pat (normalizeTerm body)
      return lam

  ELet (Let [(n, eexpr)] body) -> do
      n1 <- normalize eexpr
      ContT $ \k -> return $ makeLet (n, n1) (normalizeTermWith body k)

  EIf tst thn els -> do
      tst' <- normalizeName tst
      return $ makeIf3 tst' (normalizeTerm thn) (normalizeTerm els)

  EApp (AppPrim n es) ->
    makePrimApp n <$> mapM normalizeName es
  EApp (AppLam e es) ->
    liftA2 makeLamApp (normalizeName e) (mapM normalizeName es)

  EApply (ApplyPrim n e) ->
    makePrimApply n <$> normalizeName e
  EApply (ApplyLam e e2) ->
    liftA2 makeLamApply (normalizeName e) (normalizeName e2)

  ECallCC expr ->
    makeCallCC <$> normalizeName expr

  e -> return e

normalizeName :: Expr Name -> Cont
normalizeName e = do
  n <- normalize e
  if isValue n then
    return n
  else do
    var@(EVar n') <- getFreshName e
    ContT $ \k -> do
      e <- k var
      return $ makeLet (n', n) e

getFreshName :: Expr Name -> Cont
getFreshName e = do
    ctr <- get
    modify (+1)
    let n' = makeUniqueName ("anf" <> toName ctr) e
        var = toExpr n'
    return var

