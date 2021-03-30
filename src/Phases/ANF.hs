
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

instance MonadFail Identity where


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
data K = Es [Expr] | E Expr
type Cont = C.Cont K K

getE :: K -> Expr
getE (E e) = e

-- Exprs that instantly return a value, thus they are atomic.
isValue :: Expr -> Bool
isValue (ELit _) = True
isValue (ELam _) = True
isValue (EVar _) = True
isValue _ = False

go :: ScSyn -> ScSyn
go = toSyn . normalizeTerm . toExpr

normalizeTermWith :: ToExpr e => e -> (K -> K) -> Expr
normalizeTermWith e f = getE $ runCont (normalize e) f

normalizeTerm :: ToExpr e => e -> Expr
normalizeTerm e = normalizeTermWith e id

normalize :: ToExpr e => e -> Cont
normalize e = case toExpr e of
  ELam (Lam pat body) -> do
      let lam =  makeLam pat (normalizeTerm body)
      return $ E lam

  ELam (LamList pat body) -> do
      let lam =  makeLamList pat (normalizeTerm body)
      return $ E lam

  ELet (Let [(n, eexpr)] body) -> do
      E n1 <- normalize eexpr
      cont $ \k -> E $ makeLet (n, n1) (normalizeTermWith body k)

  EIf tst thn els -> do
      E tst' <- normalizeName tst
      return $ E $ makeIf3 tst' (normalizeTerm thn) (normalizeTerm els)

  EApp app -> case app of
     AppPrim n es -> do
       Es es' <- normalizeNames es
       return $ E $ makePrimApp n es'
     AppLam e es -> do
       E e' <- normalizeName e
       Es es' <- normalizeNames es
       return $ E $ makeLamApp e' es'

  ESet n e -> do
       return $ E $ makeSet n (normalizeTerm e)

  EApply app -> case app of
     ApplyPrim n e -> do
       E e' <- normalizeName e
       return $ E $ makePrimApply n e'
     ApplyLam e e2 -> do
       E e1' <- normalizeName e
       E e2' <- normalizeName e2
       return $ E $ makeLamApply e1' e2'

  e -> return $ E e

normalizeName :: Expr -> Cont
normalizeName e = do
  let n' = makeUniqueName "anf" e
  E n <- normalize e
  if isValue n then
    return $ E n
  else do
    let var = toExpr n'
    cont $ \k -> E $ makeLet (n', n) (getE $ k $ E var)

normalizeNames :: [Expr] -> Cont
normalizeNames es =
  if null es then
    return $ Es []
  else do
    let (e:es') = es
    E n <- normalizeName e
    Es ns <- normalizeNames es'
    return $ Es $ n:ns
