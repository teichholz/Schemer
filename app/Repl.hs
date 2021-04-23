{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- | Repl

module Repl where

import RIO
import RIO.Directory
import Control.Monad.Trans
import Data.List (isPrefixOf, drop, take, length)
import System.Console.Repline
import System.Process (callCommand)
import Prelude (putStrLn, print)
import Utils.NameResolver (schemePrims)
import Types.Pprint (pretty)

type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: (String -> IO ()) -> String -> Repl ()
cmd process input = liftIO $ process input

-- Tab Completion: return a completion for partial words entered
keywords :: [String]
keywords = ["define", "let", "lambda", "do"]

prims :: [String]
prims = fmap toString schemePrims
  where
    toString :: ByteString -> String
    toString name = let str = show name
                        len = length str in take (len - 2) $ drop 1 str

files :: IO [String]
files = listDirectory "./programs"

completer :: WordCompleter IO
completer n = do
  files' <- files
  let names = keywords ++ prims ++ files'
  return $ filter (isPrefixOf n) names

-- Commands
help :: String -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

abrt :: String -> Repl ()
abrt _ = abort

load :: (String -> IO ()) -> String -> Repl ()
load = cmd

repl :: (String -> IO ()) -> IO ()
repl process =
  evalReplOpts $
    ReplOpts
      { banner = \case SingleLine -> pure ">>> "
                       MultiLine -> pure ">>>> ",
        command = cmd process ,
        options =  [ ("help", help), ("q", abrt), ("load", load process) ],
        prefix = Just ':',
        multilineCommand = Just "do",
        tabComplete = Word completer,
        initialiser = ini,
        finaliser = fin
      }

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome and happy Schemeing!"

fin :: Repl ExitDecision
fin = liftIO $ return Exit <* putStrLn "Have a good day!"
