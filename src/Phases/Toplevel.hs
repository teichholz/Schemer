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

transform :: ScEnv ()
transform = do
  logInfo "Performing toplevel transformation"
  top <- asks _toplevel
  logDebug $ "Input Toplevel is:\n" <>  mconcat (display <$> top)
  let top' = toBody top
      topLet = toSyn $ body2Rec top'
      top'' =  go topLet

  logDebug $ "Created AST from toplevel:\n" <> display top''

  ast <- asks _ast
  writeSomeRef ast top''


-- getDecls -> map decl2Bind -> makeRecBindings
body2Rec :: Body -> Expr
body2Rec oldBody = makeRecBindings (decl2bind <$> getDecls oldBody) (toBody $ getExprs oldBody)

makeRecBindings :: [(Name, Expr)] -> Body -> Expr
makeRecBindings bindings body =
  let bindings' =
        if null bindings
        then [(makeUniqueName "toplevel" body, toExpr makeUnspecified)]
        else bindings in
    makeLet bindings' body

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

hasDecls :: Body -> Bool
hasDecls b = let decl = getDecls b in not $ null decl

go :: ScSyn -> ScSyn
go = descend go'
  where
    go' :: Expr -> Expr
    go' = \case
      ELet (Let pat body) -> makeLet pat $ body2RecIfHasDecls body
      ELam lam -> case lam of
        Lam pats body -> makeLam pats $ body2RecIfHasDecls body
        LamDot (pats, pat) body -> makeLamDot pats pat $ body2RecIfHasDecls body
        LamList pat body -> makeLamList pat $ body2RecIfHasDecls body
      x -> x
    body2RecIfHasDecls b = if hasDecls b then toBody $ body2Rec b else b
