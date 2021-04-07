module Main where

import Types.Types (Env(..), dummy, SourceFile, ScEnv, Options(..), _fileName, SourceFile(..), ScSyn, SynN)
import Cli (getCLIInput)
import RIO
import Data.Foldable (foldl1)
import RIO.Directory
import Types.Pprint (pretty)

import Sexp.Parser as SexpParser
import Parser.ScSyn as ScSynParser

import qualified Phases.Toplevel as Top
import qualified Phases.Simplify as Sim
import qualified Phases.ANF as ANF
import qualified Phases.CPS as CPS
import qualified Phases.Assignment as Ass

import Prelude (print)


phases :: [ScEnv ()]
phases = [Top.transform, Sim.transform, ANF.transform, CPS.transform, Ass.transform]

compileAction :: ScEnv ()
compileAction = foldl1 (>>) phases

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
  case SexpParser.runParser _fname _fsrc of
    Left err ->
      print err
    Right sxps -> do
      syns <- mapM ScSynParser.runParser sxps
      runApp srcfile syns opts compileAction
      -- forM_ syns (print . pretty)

runApp :: SourceFile -- ^ Source
  -> [SynN] -- ^ Toplevel Scheme syntax made of declarations and expressions
  -> Options -- ^ CLI options
  -> ScEnv () -- ^ Action to execute
  -> IO ()
runApp sf top opts action = do
  logOptions' <- logOptionsHandle stderr (_optionsVerbose opts)
  let logOptions = setLogUseTime False $ setLogUseLoc False logOptions'
  withLogFunc logOptions $ \logFunc -> do
    astRef <- newSomeRef dummy
    let state =
          Env {
            _file = sf
            , _ast = astRef
            , _toplevel = top
            , _options = opts
            , _name = "Schemer"
            , _logF = logFunc }
    runRIO state action

