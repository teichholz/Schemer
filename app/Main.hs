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
import qualified Phases.Unify as Uni
import qualified Phases.Closure as Clo
import qualified Phases.Codegen as Cod

import Prelude (print)
import LLVM (File(File))
import System.IO (putStrLn)


phases :: [ScEnv ()]
phases = [Top.transform, Sim.transform, ANF.transform, CPS.transform, Ass.transform, Uni.transform, Clo.transform, Cod.transform]

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
      forM_ sxps (putStrLn . show)
      syns <- mapM ScSynParser.runParser sxps
      runApp srcfile syns opts

runApp :: SourceFile -- ^ Source
  -> [SynN] -- ^ Toplevel Scheme syntax made of declarations and expressions
  -> Options -- ^ CLI options
  -> IO ()
runApp sf top opts = do
  logOptions' <- logOptionsHandle stderr (_optionsVerbose opts)
  let logOptions = setLogUseTime False $ setLogUseLoc False logOptions'
  withLogFunc logOptions $ \logFunc -> do
    astRef <- newSomeRef dummy
    procsRef <- newSomeRef []
    let state =
          Env {
            _file = sf
            , _ast = astRef
            , _toplevel = top
            , _procs = procsRef
            , _options = opts
            , _name = "Schemer"
            , _outputFile = File "./../code.o"
            , _logF = logFunc }
    runRIO state compileAction


