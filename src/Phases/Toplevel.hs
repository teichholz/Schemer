{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | Toplevel transformation into a single Expr, removing all declarations.

module Phases.Toplevel where

import RIO
import RIO.List
import Types.Pprint
import Types.Types
import Types.Constructors
import qualified Unbound.Generics.LocallyNameless as Un


transform :: ScEnv ()
transform = do
  top <-  view toplevelL
  let top' = makeMultBody top
  logInfo "Performing toplevel transformation"
  return ()

-- removeDecls :: Body -> ScSyn
-- removeDecls b = runIdentity $ do
--   let decls = getDecls b

decl2bind :: Decl -> (Name, Expr)
decl2bind = \case
  VarDecl n e -> (n, e)
  FunDecl n p b -> (n, makeLamMultBods p $ unBody b)
  FunListDecl n p b -> (n, makeLamListMultBods p $ unBody b)
  FunDotDecl n ps p b -> (n, makeLamDotMultBods ps p $ unBody b)

getDecls :: [ScSyn] -> [Decl]
getDecls = fmap (\(ScDecl d) -> d) . filter isDecl

makeRecBindings :: [(Name, Expr)] -> ScSyn
makeRecBindings bind =
  let (ns, es) = unzip bind
      bind' = zip ns (repeat $ toExpr makeUnspecified)
      body = toSyn <$> zipWith makeSet ns es in
    toSyn $ makeLetN bind' body
