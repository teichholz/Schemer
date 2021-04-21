{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
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
  logDebug $ "Created raw AST from toplevel:\n" <> display (show top'')

  ast <- asks _ast
  writeSomeRef ast top''


-- getDecls -> map decl2Bind -> makeRecBindings
body2Rec :: Body Name -> Expr Name
body2Rec oldBody = makeRecBinding (decl2bind <$> getDecls oldBody) (toBody $ getExprs oldBody)

makeRecBinding :: [(Name, Expr Name)] -> Body Name -> Expr Name
makeRecBinding bindings body =
  if null bindings
  then makeLet [(makeUniqueName "toplevel" body, toExpr makeUnspecified)] body
  else makeRecLet bindings body

makeRecLet :: [(Name, Expr Name)] -> Body Name -> Expr Name
makeRecLet binds (Body bes) =
  let (ns, es) = unzip binds
      unspecs = fmap (, makeUnspecified) ns
      sets = zipWith makeSet ns es in
    makeLet unspecs (toBody $ sets ++ fmap toExpr bes)

decl2bind :: Decl Name -> (Name, Expr Name)
decl2bind = \case
  VarDecl n e -> (n, e)
  FunDecl n p b -> (n, makeLam p b)
  FunListDecl n p b -> (n, makeLamList p b)
  FunDotDecl n ps p b -> (n, makeLamDot ps p b)

getDecls :: Body Name -> [Decl Name]
getDecls = fmap toDecl . filter isDecl . unBody

getExprs :: Body Name -> [Expr Name]
getExprs = fmap toExpr . filter isExpr . unBody

hasDecls :: Body Name -> Bool
hasDecls b = let decl = getDecls b in not $ null decl

go :: ScSyn Name -> ScSyn Name
go = descend go'
  where
    go' :: Expr Name -> Expr Name
    go' = \case
      ELet (Let pat body) -> makeLet pat $ body2RecIfHasDecls body
      ELam lam -> case lam of
        Lam pats body -> makeLam pats $ body2RecIfHasDecls body
        LamDot (pats, pat) body -> makeLamDot pats pat $ body2RecIfHasDecls body
        LamList pat body -> makeLamList pat $ body2RecIfHasDecls body
      x -> x
    body2RecIfHasDecls b = if hasDecls b then toBody $ body2Rec b else b
