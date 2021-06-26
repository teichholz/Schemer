module Main where

import Types.Types (Env(..), dummy, SourceFile, ScEnv, Options(..), _fileName, SourceFile(..), ScSyn, SynN)
import RIO
import RIO.Directory
import Data.Text (pack)
import Data.Foldable (foldl1)
import Types.Pprint (pretty)

import Cli (getCLIInput)
import Repl (repl)

import qualified Sexp.Parser as Sexp
import qualified Expander.Expander as Expander
import qualified Parser.ScSyn as ScSyn
import qualified Phases.Toplevel as Top
import qualified Phases.Simplify as Sim
import qualified Phases.ANF as ANF
import qualified Phases.CPS as CPS
import qualified Phases.Assignment as Ass
import qualified Phases.Unify as Uni
import qualified Phases.Closure as Clo
import qualified Phases.Codegen as Cod

import Prelude (print)
import LLVM (File(File))
import System.IO (putStrLn)


phases :: [ScEnv ()]
phases = [Sexp.parse,  Expander.expand,
          ScSyn.parse, Top.transform, Sim.transform, Ass.transform, ANF.transform, CPS.transform, Uni.transform, Clo.transform, Cod.transform]

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
  case file of
    Just srcfile -> runApp opts srcfile
    Nothing -> do
      let f = \s ->  runApp opts (SourceFile { _fname = "<repl>", _fsrc = pack s })
      repl f

runApp :: Options -- ^ CLI options
  -> SourceFile -- ^ Source
  -> IO ()
runApp opts sf = do
  logOptions' <- logOptionsHandle stderr (_optionsVerbose opts)
  let logOptions = setLogUseTime False $ setLogUseLoc False logOptions'
  withLogFunc logOptions $ \logFunc -> do
    astRef <- newSomeRef dummy
    toplevelRef <- newSomeRef []
    sexpsRef <- newSomeRef []
    procsRef <- newSomeRef []
    let state =
          Env {
            _file = sf
            , _sexps = sexpsRef
            , _toplevel = toplevelRef
            , _ast = astRef
            , _procs = procsRef
            , _options = opts
            , _name = "Schemer"
            , _outputFile = File "../code.o"
            , _logF = logFunc }
    runRIO state compileAction
