{-# LANGUAGE OverloadedStrings #-}
-- | 6. Phase: Closure transformation


-- Closure transformation returns a list of toplevel functions, for which code can be generated with the help off LLVM.
-- This means all free variables need to be eliminated, expclicitly capturing the references in a closure environment.

module Phases.Closure where
import RIO hiding (trace)
import RIO.State
import qualified RIO.Map as M
import RIO.Set as S
import Types.Types
import Control.Monad.Reader
import Types.Constructors
import Types.Pprint
import Debug.Trace (trace)
import Prelude (print)

transform :: ScEnv ()
transform = do
  logInfo "Performing closure transformation"
  astref <- asks _ast
  procsref <- asks _procs
  ast <- readSomeRef astref

  let ast' = go ast
  let procs = hoist $ runAlpha ast'

  logDebug $ "AST after closure transformation:\n" <> display ast'
  logDebug $ "All Procs after closure transformation:\n" <> display procs

  writeSomeRef astref ast'
  writeSomeRef procsref procs
  return ()


go :: ScSyn Name -> ScSyn Name
go = callWithAlpha (descend closureConversion)

hoist :: ScSyn UniqName -> [Proc UniqName]
hoist e =
  let (maine, (_, procs)) = runState (descendM (makeMap go) e) (0, [])
   in procs ++ [Proc (makeUniqName "main" 0, makeLam ([] :: [UniqName]) (toBody maine))]
  where
    go :: Expr UniqName -> State (Counter, [Proc UniqName]) (Expr UniqName)
    go e = case e of
      ELam (Lam _ _) -> do
        (cntr, _) <- get
        let procname = makeUniqName "proc" cntr
            proc = Proc (procname, e)
        modify $ bimap (+ 1) (++ [proc])
        return $ toExpr procname
      e -> return e

envAccess :: Body UniqName -> UniqName -> [(UniqName, Int)] -> Body UniqName
envAccess b newname al =
  runReader (descendBodyM (makeMap f) b) (M.fromList al)
  where
    f :: Expr UniqName -> Reader (M.Map UniqName Int) (Expr UniqName)
    f e = do
      case e of
        EVar n -> do
          map <- ask
          let pair = M.lookup n map
          if isJust pair then do
            let (Just i) = pair
            return $ vectorRef (toExpr newname) i
          else do
            return e

        x -> return x

makeClosure :: Lambda UniqName -> [UniqName] -> Expr UniqName
makeClosure lam ns = makeVectorFromList (ELam lam:fmap EVar ns++[ELit LitNil])


closureConversion :: Expr UniqName -> Expr UniqName
closureConversion e = case e of
  ELam lam@(Lam ps b) ->
    let env = makeGloballyUniqueName "env" b
        fvs = toAscList (fv lam)
        fvs' = trace (show fvs <> show lam) (zip fvs [1..])
        b' = envAccess b env fvs' in
      makeClosure (Lam (env:ps) b') fvs
  e -> e


