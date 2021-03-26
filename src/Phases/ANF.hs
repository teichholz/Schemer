
{-# LANGUAGE OverloadedStrings #-}
-- | 3. Phase: Transformation to administrative normal form

module Phases.ANF where

import RIO
import RIO.List.Partial (head)
import RIO.State
import Control.Monad.Cont hiding (Cont)
import qualified Control.Monad.Cont as C (Cont)
import Types.Types
import Types.Constructors
import qualified Unbound.Generics.LocallyNameless as Un
import Types.Pprint



transform :: ScEnv ()
transform = do
  logInfo "Performing toplevel transformation"
  astref <- asks _ast
  ast <- readSomeRef astref
  let ast' = go ast

  logDebug $ "AST in ANF:\n" <> display ast'

  writeSomeRef astref ast'
  return ()

type Cont = Un.FreshMT (C.Cont [Expr]) [Expr]

-- Exprs that instantly return a value
isValue :: Expr -> Bool
isValue (ELit _) = True
isValue (ELam _) = True
isValue (EVar _) = True
isValue _ = False

go :: ScSyn -> ScSyn
go = toSyn . normalizeTerm . toExpr

normalizeTerm :: ToExpr e => e -> Expr
normalizeTerm e = head $  runCont (Un.runFreshMT $ normalize e) id

runCont' :: Cont -> ([Expr] -> [Expr]) -> [Expr]
runCont' cont = runCont (Un.runFreshMT $ cont)

normalize :: ToExpr e => e -> Cont
normalize e = case toExpr e of
  ELam (Lam bnd) -> do
    (pat, body) <- Un.unbind bnd
    let lam =  makeLam pat (normalizeTerm body)
    return [lam]

  ELam (LamList bnd) -> do
      (pat, body) <- Un.unbind bnd
      let lam =  makeLamList pat (normalizeTerm body)
      return [lam]

  ELet (Let bnd) -> do
      bnd <- Un.unbind bnd
      let ([(n, eexpr)], body) = bnd
      n1 <- normalize $ eexpr
      lift $ cont $ \k -> [makeLet (n, n1) (runCont' (normalize body) k)]

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

  e -> return [e]

normalizeName :: [Expr] -> Cont
normalizeName [e] = do
  let n' = makeUniqueName (makeName "anf") e
  n <- normalize e
  if isValue $ head n then
    return n
  else do
    let var = toExpr n'
    lift $ cont $ \k -> [makeLet (n', n) (k [var])]

normalizeNames :: [Expr] -> Cont
normalizeNames es =
  if null es then
    return []
  else do
    let (e:es') = es
    n <- normalizeName [e]
    ns <- normalizeNames es'
    return $ n++ns
