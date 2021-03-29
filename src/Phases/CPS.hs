{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 4. Phase CPS transformation

module Phases.CPS where
import RIO
import Types.Types
import Types.Constructors
import Types.Pprint


transform :: ScEnv ()
transform = do
  logInfo "Performing transformation to CPS"
  astref <- asks _ast
  ast <- readSomeRef astref

  logDebug $ "AST in CPS:\n" <> display ast

  writeSomeRef astref ast
  return ()


callcc :: Expr
callcc = makeLam ["cc" :: Name, "f"]
           (makeLamApp ("f" :: Expr)
              ["cc" :: Expr,
               makeLam ["_" :: Name, "x"]
                (makeLamApp ("cc" :: Expr) ["x" :: Expr])])

-- Adds cont parameter to lambda, trasforms the body
tAe :: Expr -> Expr
tAe e = case e of
  ELam (Lam pat body) -> do
    let cont = makeUniqueName "cont" e
    makeLam (cont:pat) body

  ELam (LamList pat body) -> do
    let cont = makeUniqueName "cont" e
    extendLamList cont pat body

  x -> x

-- Checks wether a let binds a lambda application
appLamBind :: Let -> Bool
appLamBind (Let [(_, e)] _) = isLamApp e
appLamBind _ = False

-- Traverses ast, adds cont (lambda) to non-primitive application calls. Gets rid of call/cc. Calls cont on return values (last expr).
t :: Expr -- ^ Expr to transform into CPS
  -> Expr -- ^ Continuation argument to normalize with
  -> Expr
t e c = case e of
  -- Primtive calls in return positions will be letbound and the cont called on the body of the let. Thus they return they pass their value to the continuation and "return"
  EApp (AppPrim _ _) -> t (ret e) c
  EApply (ApplyPrim _ _ ) -> t (ret e) c

  -- Propagate tAe, remember that ANF forces non-atomic expressions to applications/applys to be letbound
  ELet l@(Let pat body) | not $ appLamBind l -> do
    let [(lhs, rhs)] = pat
    let rhs' = case rhs of
                  EApp (AppPrim n es) -> makePrimApp n (tAe <$> es)
                  EApply (ApplyPrim n e) -> makePrimApply n (tAe e)
                  ELam _ -> tAe rhs
                  x -> x

    makeLet (lhs, rhs') (t (toExpr body) c)

  -- tst expression will be normalized, thus atomic. Call cont on return expr in thn and els.
  EIf tst thn els -> makeIf3 (tAe tst) (t thn c) (t els c)

  -- if a LamApp is letbound, the body of the let represents its continuation
  ELet (Let pat body) -> do
    let [(arg, lamApp)] = pat
    let cont = makeLam [arg] (t (toExpr body) c)
    t lamApp cont

  -- Expressions in return posititon
  -- add cont to the argument list
  EApply (ApplyLam e1 e2) -> do
    let lst = makeUniqueName "applylistwithcont" [e1, e2]
    makeLet
      (lst, makePrimApp ("cons" :: PrimName) [c, tAe e2])
      (makeLamApply (tAe e1) (toExpr lst))


  -- Normalize application and add cont as first argument. Causes the application to pass its result to cont.
  EApp (AppLam e es) -> makeLamApp (tAe e) (c : (tAe <$> es))

  -- Call cont with lambda as argument, if lambdas will be returned
  ELam _ -> makeLamApp c [tAe e]

  -- Call cont on literals and variables, if they are returned
  x -> makeLamApp c [x]


ret :: Expr -> Expr
ret e =
  let ret = makeUniqueName "ret" e in
    makeLet (ret, e) (toExpr ret)
