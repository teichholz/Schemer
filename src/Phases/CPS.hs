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

  let ast' = go ast
  logDebug $ "AST in CPS:\n" <> display ast'

  writeSomeRef astref ast'
  return ()

callcc :: Expr Name
callcc = makeLam ["cc" :: Name, "f"]
           (makeLamApp ("f" :: Expr Name)
              ["cc" :: Expr Name,
               makeLam ["_" :: Name, "x"]
                (makeLamApp ("cc" :: Expr Name) ["x" :: Expr Name])])

-- Adds cont parameter to lambda, trasforms the body
tAe :: Expr Name -> Expr Name
tAe e = case e of
  ELam (Lam pat body) -> do
    let cont = makeUniqueName "cont" e
    makeLam (cont:pat) (t (toExpr body) (toExpr cont))

  ELam (LamList pat body) -> do
    let cont = makeUniqueName "cont" e
    extendLamList cont pat (t (toExpr body) (toExpr cont)) -- Extends the list parameter with cont

  x -> x


-- Traverses ast, adds cont (lambda) to non-primitive application calls. Gets rid of call/cc. Calls cont on return values (last expr).
t :: Expr Name -- ^ Expr to transform into CPS
  -> Expr Name -- ^ Continuation argument to normalize with
  -> Expr Name -- ^ Expr in CPS
t e c = case e of
  -- Primtive calls in return positions will be letbound and the cont called on the body of the let. Thus they return they pass their value to the continuation and "return"
  EApp (AppPrim _ _) -> t (ret e) c
  EApply (ApplyPrim _ _ ) -> t (ret e) c

  -- Propagate tAe, remember that ANF forces non-atomic expressions to applications/applys to be letbound
  ELet (Let [(lhs, rhs)] body) | not $ isLamApp rhs || isCallCC rhs -> do
    let rhs' = case rhs of
                  EApp (AppPrim n es) -> makePrimApp n (tAe <$> es)
                  EApply (ApplyPrim n e) -> makePrimApply n (tAe e)
                  ELam _ -> tAe rhs
                  x -> x

    makeLet (lhs, rhs') (t (toExpr body) c)

  -- tst expression will be normalized, thus atomic. Call cont on return expr in thn and els.
  EIf tst thn els -> makeIf3 (tAe tst) (t thn c) (t els c)

  -- if a LamApp is letbound, the body of the let represents its continuation
  ELet (Let [(arg, lamApp)] body) -> do
    let cont = makeLam [arg] (t (toExpr body) c)
    t lamApp cont
  -- ELet (Let [(arg, lamApp)] body) -> do
  --   let unused = makeUniqueName "usused" body
  --       cont = makeLam [unused, arg] (t (toExpr body) c)
  --       n = makeUniqueName "cont" body
  --   makeLet (n, cont) (t lamApp (toExpr n))

  ECallCC e -> do
    let name = makeUniqueName "callcc" e
    makeLet (name, callcc)
      (makeLamApp name [c, tAe e])
  -- ECallCC e -> do
  --   makeLamApp (tAe e) [c, c]

  -- Expressions in return posititon
  -- add cont to the argument list
  EApply (ApplyLam e1 e2) -> do
    let lst = makeUniqueName "applylistwithcont" (toBody [e1, e2])
    makeLet
      (lst, makePrimApp ("cons" :: PrimName) [c, tAe e2])
      (makeLamApply (tAe e1) (toExpr lst))


  -- Normalize application and add cont as first argument. Causes the application to pass its result to cont.
  EApp (AppLam e es) -> makeLamApp (tAe e) (c : (tAe <$> es))

  -- Call cont with lambda as argument, if lambdas will be returned
  ELam _ -> makeLamApp c [tAe e]
  -- ELam _ -> do
  --   let lam = makeLam ["un", "used"] [makeLamApp ("un" :: Name) ["used" :: Expr Name, "used"]]
  --   makeLamApp c [lam, tAe e]

  -- Call cont on literals and variables, if they are returned
  x -> makeLamApp c [x]


ret :: Expr Name -> Expr Name
ret e =
  let ret = makeUniqueName "ret" e in
    makeLet (ret, e) (toExpr ret)


go :: ScSyn Name -> ScSyn Name
go = toSyn . go' . toExpr
  where
    go' :: Expr Name -> Expr Name
    go' e = do
      let finalCont = makeLam ["k", "v"] [makeLet ("_", makePrimApp ("halt" :: PrimName') ["v" :: Expr Name]) (makeLamApp ("k" :: Expr Name) ["v"])]
          finalCont' = makeLam ["final"] [makePrimApp ("halt" :: PrimName') ["final" :: Expr Name]]
      makeLet
        ("halt-and-display" :: Name, finalCont')
        (t e ("halt-and-display" :: Expr Name))
