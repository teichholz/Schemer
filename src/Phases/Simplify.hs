{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Transformation to administrative normal form

-- ((lambda (x y . z) (x y z)) 1 2 '(1 2 3))
-- =>
-- ((lambda (zgen)
--    (let ((x (car zgen))
--          (y (cadr zgen))
--          (z (cddr zgen))))
--      (x y z))
--  1 2 '(1 2 3))



module Phases.Simplify where

import RIO
import Types.Types
import RIO.State
import Types.Constructors
import qualified Unbound.Generics.LocallyNameless as Un


transform :: ScEnv ()
transform = do
  logInfo "Performing toplevel transformation"
  ast <- asks _ast

  return ()

-- first transformation to sequence lets
flattenLet :: (Un.Fresh m) => Expr -> m Expr
flattenLet = \case
  ELet (Let bind) -> do
    (p, t) <- Un.unbind bind
    return $ go p t
  x -> return x
  where
    go :: Binding -> Body -> Expr
    go bind body = case bind of
      [b] -> makeLet (toBinding b) body
      b:tl -> makeLet (toBinding b) (toBody $ go tl body)

lambdad2lambdal :: (Un.Fresh m) => Expr -> m Expr
lambdad2lambdal = \case
  ELam lam -> go lam
  x -> return x
  where
    go ::(Un.Fresh m) => Lambda -> m Expr
    go = \case
      LamDot bind -> do
        ((args, dotarg), body) <- Un.unbind bind
        lamlarg <- Un.fresh dotarg
        let binding = toBinding $ evalState (go' (toExpr lamlarg) (args, dotarg)) (id, [])
        return $ makeLamList lamlarg (makeLet binding body)
      x -> return $ toExpr x

    go' ::  Expr -> ([Name], Name) -> State (Expr -> Expr, [(Name, Expr)]) [(Name, Expr)]
    go' lamlarg (ns, dotn) = do
      (app, _) <- get
      case ns of
        [] -> do
            modify (second ((dotn, car $ app lamlarg):))
            gets snd
        n:ns -> do
            modify (bimap (cdr .) ((n, car $ app lamlarg):))
            go' lamlarg (ns, dotn)
