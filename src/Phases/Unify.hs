
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 6. Phase: Unification of procedures

-- This phase takes care of overloading the procedures: +, *, make-string, make-vector
-- It also eliminates lambdal: i.e. (lambda x (apply + x))

-- By convention, proceduces will expect lists as parameters. This enables the apply primitive.
-- Apply with lambda procedurs is simple application (since by convention we now use lists as single arg).
-- Apply with primitives need to call a special primitive. I.e: apply make-string => apply apply_make-string. These are implemented by the runtime and forward the list elements.
-- Apply with variadic function (i.e. +) is a special case. "apply" is appended, thought the runtime just forwards the list to the primitive function.

-- We can remove lambdal ((lambda x (...))), since by convention all lambda proceduces have now one argument.

-- Examples:
-- Overloading:
-- (make-string 4) => (make-string1 4)
-- (make-string 4 \#c) => (make-string2 4 \#c)

-- Apply with prims:
-- (apply (+, plus) '(1 2 3)) => ((+, apply_plus) '(1 2 3))

-- Apply with lambdas:
-- (apply myf '(1 2 3)) => (myf '(1 2 3))

-- Lambdal:
-- (lambda x (apply + x)) => (lambda (x) (apply + x))

-- Lambda:
-- ((lambda (x y) (+ x y)) 1 2) => ((lambda (x') (+ (car x') (cadr x'))) '(1 2))

module Phases.Unify where

import RIO
import RIO.State
import RIO.List as L
import qualified RIO.Map as M
import RIO.Set as S
import Types.Types
import Control.Monad.Reader
import Data.Semigroup ((<>))
import Types.Constructors
import Types.Pprint
import qualified Utils.NameResolver as NR
import Prelude (print)

transform :: ScEnv ()
transform = do
  logInfo "Performing unification of procedures"
  astref <- asks _ast
  ast <- readSomeRef astref

  let ast' = go ast

  logDebug $ "AST after unifiction:\n" <> display ast'

  logDebug $ "AST with unique names after unifiction:\n" <> display (runAlpha ast')

  writeSomeRef astref ast'
  return ()

go :: ScSyn Name -> ScSyn Name
go = callWithAlpha (descend overload . descend unify)

overload :: Expr a -> Expr a
overload e = case e of
  EApp (AppPrim pn es) | NR.isOverloaded pn -> EApp $ AppPrim (overload' pn es) es
  -- Should not exist anymore
  -- EApply (ApplyPrim pn e) | NR.isOverloaded pn -> EApply $ ApplyPrim (overload' pn [e]) e
  e -> e
  where
    overload' :: PrimName -> [a] -> PrimName
    overload' pn args =
      let len = fromString $ show $ L.length args
          overloaded = ["+", "*", "make-string", "make-vector"]
       in if
              -- len will either be 0 or 1 for +/*, since we unify before overloading
              | L.elem pn overloaded -> pn <> PName ("", len)
              | otherwise -> pn

unify :: Expr UniqName -> Expr UniqName
unify e = case e of
  -- Variadic proceduces will use apply
  EApp (AppPrim pn es) | NR.isVariadic pn -> EApp $ AppPrim (PName ("", "apply_") <> pn) [makeConsList es]
  -- All lambda call sites will use lists as parameters
  EApp (AppLam e es) -> EApp (AppLam e [makeConsList es])

  -- Apply with primitive proceduces will use normall application special apply_ variant of the procedure, taking a list as arg
  EApply (ApplyPrim pn e) -> EApp $ AppPrim (PName ("", "apply_") <> pn) [e]
  -- Apply with lambda becomes normal application, since all call sites will use lists
  EApply (ApplyLam n e) -> EApp $ AppLam n [e]

  -- All lambdas use lists at call sites, so (lambda _ ...) can become (lambda [_] ...)
  ELam (LamList p b) -> ELam (Lam [p] b)
  -- (lambda [_ ...] ...) will become (lambda [_] ...). It will now need to properly (car)/(cdr) a specific parameter from it's only parameter, which is a list
  -- The name "variadicn" is introduced for its parameter, where n is a natural number, which keeps the name unique in respect to the whole ast
  ELam (Lam ps b) ->
    let newname = makeUniqueName ("variadic" :: Name) b
        al = zip ps [0..]
        b' = getVarsFromList al newname b in
      ELam  (Lam [newname] b')

  e -> e

getVarsFromList :: [(UniqName, Int)]
  -> UniqName
  -> Body UniqName
  -> Body UniqName
getVarsFromList al newname b =
  runReader (descendBodyM (makeMap f) b) (M.fromList al)
  where
    f :: Expr UniqName -> Reader (M.Map UniqName Int) (Expr UniqName)
    f e = do
      map <- ask
      case e of
        EVar n -> do
          let pair = M.lookup n map
          if isJust pair then do
            let (Just i) = pair
            return $ cadr i (toExpr newname)
          else do
            return e

        x -> return x
