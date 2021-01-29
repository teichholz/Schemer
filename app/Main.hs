module Main where

import Lib
import Data.IORef
import Control.Monad.Reader

data Env = Env
  {  counter :: !(IORef Int)
  }

main :: IO ()
main = do
  ref <- newIORef 0
  let env = Env {
        counter = ref
                }
  return ()
