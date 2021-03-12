module Main where


import Types (CompilerState, SourceFile, ScEnv, Options, _fileName, SourceFile(..))
import Cli (getCLIInput)
import RIO
import RIO.Directory
import SParser
import qualified RIO.Text as T
import Prelude (print)
import qualified RIO as R

phases :: [ScEnv a]
phases = []

loadFileIFExists :: MonadIO m => FilePath -> m (Maybe SourceFile)
loadFileIFExists fpath = do
  exists <- doesFileExist fpath
  if exists
    then do
      text <- readFileUtf8 fpath
      return $ Just $ SourceFile { _fname = fpath, _fsrc = text }
    else return Nothing
  
main :: IO ()
main = do
  opts <- getCLIInput
  file <- loadFileIFExists $ _fileName opts
  let srcfile@SourceFile{..} = fromMaybe (SourceFile { _fname = "stdin", _fsrc = "stdin" }) file
      result = runParser _fname _fsrc

  print result


-- Main driver to execute the compiler
-- runApp :: MonadUnliftIO m => Options -> RIO App b -> m b
-- runApp opts inner = do
--   logOptions' <- logOptionsHandle stderr (optionsVerbose opts)
--   let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
--   withLogFunc logOptions $ \logFunc -> do
--     let app =
--           App
--             { appLogFunc = logFunc,
--               appName = "Alice"
--             }
--     runRIO app inner

-- sayHello :: RIO App ()
-- sayHello = do
--   name <- view $ to appName
--   logInfo $ "Hello, " <> name
