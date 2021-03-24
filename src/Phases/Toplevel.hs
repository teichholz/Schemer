{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 1. Phase: Toplevel transformation into a single Expr, removing all declarations.

-- This phase removes alle declarations (define) inside of bodies (let, lambda, define, Toplevel)
-- and transforms them into letrec with the expressions of the body as expressions in the letrec.
-- At this point bodies only contain Expressions or letrec with expressions inside.

-- For simplicity letrec is the default let binding for this compiler, called "let"

module Phases.Toplevel where

import RIO
import Types.Pprint (display)
import RIO.List
import RIO.Text (pack)
import Types.Types
import Types.Constructors
import qualified Unbound.Generics.LocallyNameless as Un


transform :: ScEnv ()
transform = do
  logInfo "Performing toplevel transformation"
  top <- asks _toplevel
  let top' = toBody top
      topLet = toSyn $ body2Rec top'
      top'' =  go topLet

  logDebug $ "Created AST from toplevel:\n" <> display top''

  ast <- asks _ast
  writeSomeRef ast top''


-- getDecls -> map decl2Bind -> makeRecBindings
body2Rec :: Body -> Expr
body2Rec oldBody = makeRecBindings  (decl2bind <$> getDecls oldBody) (toBody $ getExprs oldBody)

makeRecBindings :: [(Name, Expr)] -> Body -> Expr
makeRecBindings = makeLet

decl2bind :: Decl -> (Name, Expr)
decl2bind = \case
  VarDecl n e -> (n, e)
  FunDecl n p b -> (n, makeLam p b)
  FunListDecl n p b -> (n, makeLamList p b)
  FunDotDecl n ps p b -> (n, makeLamDot ps p b)

getDecls :: Body -> [Decl]
getDecls = fmap toDecl . filter isDecl . unBody

getExprs :: Body -> [Expr]
getExprs = fmap toExpr . filter isExpr . unBody

hasDecls :: Un.Alpha p => Un.Bind p Body -> Bool
hasDecls b = Un.runFreshM $ do
        (_, body) <- Un.unbind b
        let decl = getDecls body
        return $ not $ null decl

go :: ScSyn -> ScSyn
go = descend go'
  where
    go' :: Expr -> Expr
    go' = \case
      ELet (Let bind) -> toExpr $ Let $ body2RecIfHasDecls bind
      ELam lam -> case lam of
        Lam bind -> toExpr $ Lam $ body2RecIfHasDecls bind
        LamDot bind -> toExpr $ LamDot $ body2RecIfHasDecls bind
        LamList bind -> toExpr $ LamList $ body2RecIfHasDecls bind
      x -> x
    body2RecIfHasDecls b =
      if hasDecls b then  mapBind id (toBody . body2Rec) b else b

