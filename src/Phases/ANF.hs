{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 3. Phase: Transformation to administrative normal form

module Phases.ANF where

import RIO
import Types.Types
import Types.Constructors
import qualified Unbound.Generics.LocallyNameless as Un


transform :: ScEnv ()
transform = do
  logInfo "Performing toplevel transformation"
  ast <- asks _ast

  return ()

-- first transformation to sequence lets
flattenLet :: Expr -> Expr
flattenLet = \case
  ELet (Let bind) -> do
    (p, t) <- Un.unbind bind
    go p t
  x -> x
  where
    go :: Binding -> Body -> Expr
    go bind body = case bind of
      [b] -> makeLet (toBinding b) body
      b:tl -> makeLet (toBinding b) (toBody $ go tl body)

valuep :: Expr -> Bool
valuep = \case
  ELit _ -> True
  ELam _ -> True
  _ -> False

normalizeTerm :: Expr -> Expr
normalizeTerm = \case
