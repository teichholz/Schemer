{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 6. Phase: Closure transformation

module Phases.Closure where
import RIO
import RIO.State
import qualified RIO.Map as M
import RIO.Set as S
import Types.Types
import Types.Constructors
import Types.Pprint
import Prelude (print)

transform :: ScEnv ()
transform = do
  logInfo "Performing assignment transformation"
  astref <- asks _ast
  ast <- readSomeRef astref

  -- logDebug $ "Free variables in AST:\n" <> display (show $ S.map unUniqName frees)
  -- logDebug $ "AST after assignment transformation:\n" <> display ast'

  writeSomeRef astref ast
  return ()
