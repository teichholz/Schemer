
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
import qualified Control.Monad.Cont as C (Cont)
import Types.Types
import Types.Constructors
import Types.Pprint



transform :: ScEnv ()
transform = do
  logInfo "Performing transformation to ANF"
  astref <- asks _ast
  ast <- readSomeRef astref
  let ast' = go ast

  logDebug $ "AST in ANF:\n" <> display ast'
  logDebug $ display $ show ast'

  writeSomeRef astref ast'
  return ()

-- The Cont monad represents computations in continuation passing style.
-- That is, we call a continuation with [Expr] as argument, which will itself produce an [Expr] based on the [Expr] argument.
type Cont = C.Cont [Expr] [Expr]

-- Exprs that instantly return a value, thus they are atomic.
isValue :: Expr -> Bool
isValue (ELit _) = True
isValue (ELam _) = True
isValue (EVar _) = True
isValue _ = False

go :: ScSyn -> ScSyn
go = toSyn . normalizeTerm . toExpr

normalizeTerm :: ToExpr e => e -> Expr
normalizeTerm e = head $ runCont (normalize e) id

normalize :: ToExpr e => e -> Cont
normalize e = case toExpr e of
  ELam (Lam pat body) -> do
    let lam =  makeLam pat (normalizeTerm body)
    return [lam]

  ELam (LamList pat body) -> do
      let lam =  makeLamList pat (normalizeTerm body)
      return [lam]

  ELet (Let [(n, eexpr)] body) -> do
      n1 <- normalize eexpr
      cont $ \k -> [makeLet (n, n1) (runCont (normalize body) k)]

  EIf tst thn els -> do
      tst' <- normalizeName [tst]
      return [makeIf3 (head tst') (normalizeTerm thn) (normalizeTerm els)]

  EApp app -> case app of
     AppPrim n es -> do
       es' <- normalizeNames es
       return [makePrimApp n es']
     AppLam e es -> do
       e' <- normalizeName [e]
       es' <- normalizeNames es
       return [makeLamApp (head e') es']

  ESet n e -> do
       return [makeSet n (normalizeTerm e)]

  EApply app -> case app of
     ApplyPrim n e -> do
       e' <- normalizeName [e]
       return [makePrimApply n (head e')]
     ApplyLam e e2 -> do
       e1' <- normalizeName [e]
       e2' <- normalizeName [e2]
       return [makeLamApply (head e1') (head e2')]

  e -> return [e]

normalizeName :: [Expr] -> Cont
normalizeName [e] = do
  let n' = makeUniqueName "anf" e
  n <- normalize e
  if isValue $ head n then
    return n
  else do
    let var = toExpr n'
    cont $ \k -> [makeLet (n', n) (k [var])]

normalizeNames :: [Expr] -> Cont
normalizeNames es =
  if null es then
    return []
  else do
    let (e:es') = es
    n <- normalizeName [e]
    ns <- normalizeNames es'
    return $ n++ns
