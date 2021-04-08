{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 6. Phase: Closure transformation


-- Closure transformation returns a list of toplevel functions, for which code can be generated with the help off LLVM.
-- This means all free variables need to be eliminated, expclicitly capturing the references in a closure environment.

module Phases.Closure where
import RIO
import RIO.State
import qualified RIO.Map as M
import RIO.Set as S
import Types.Types
import Control.Monad.Reader
import Types.Constructors
import Types.Pprint
import Prelude (print)

transform :: ScEnv ()
transform = do
  logInfo "Performing closure transformation"
  astref <- asks _ast
  ast <- readSomeRef astref

  let ast' = go ast

  logDebug $ "AST after assignment transformation:\n" <> display ast'

  writeSomeRef astref ast'
  return ()


go :: ScSyn Name -> ScSyn Name
go = callWithAlpha (descend closureConversion)

envAccess :: Body UniqName -> UniqName -> [(UniqName, Int)] -> Body UniqName
envAccess b newname al =
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
            return $ vectorRef (toExpr newname) i
          else do
            return e

        x -> return x

makeClosure :: Lambda UniqName -> [UniqName] -> Expr UniqName
makeClosure lam ns = makeVectorFromList (ELam lam:fmap EVar ns)


closureConversion :: Expr UniqName -> Expr UniqName
closureConversion e = case e of
  ELam lam@(Lam ps b) ->
    let env = makeGloballyUniqueName "env" b
        fvs = toAscList (fv b)
        fvs' = zip fvs [1..]
        b' = envAccess b env fvs' in
      makeClosure (Lam (env:ps) b') fvs
  e -> e
