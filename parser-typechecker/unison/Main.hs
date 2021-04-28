{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

#if defined(mingw32_HOST_OS)
import qualified GHC.ConsoleHandler as WinSig
#else
import qualified System.Posix.Signals as Sig
#endif

import Control.Concurrent (mkWeakThreadId, myThreadId)
import Control.Error.Safe (rightMay)
import Control.Exception (AsyncException (UserInterrupt), throwTo)
import Data.ByteString.Char8 (unpack)
import qualified Data.Configurator as Config
import Data.Configurator.Types (Config)
import qualified Data.Text as Text
import qualified Network.URI.Encode as URI
import System.Directory (getCurrentDirectory, removeDirectoryRecursive)
import System.Environment (getArgs, getProgName)
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.IO.Error (catchIOError)
import qualified System.IO.Temp as Temp
import System.Mem.Weak (deRefWeak)
import qualified System.Path as Path
import Text.Megaparsec (runParser)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Init as Codebase
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Editor.RemoteRepo (RemoteNamespace)
import qualified Unison.Codebase.Editor.VersionParser as VP
import Unison.Codebase.Execute (execute)
import qualified Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.CommandLine (watchConfig)
import qualified Unison.CommandLine.Main as CommandLine
import Unison.Parser (Ann)
import Unison.Prelude
import qualified Unison.PrettyTerminal as PT
import qualified Unison.Runtime.Interface as RTI
import qualified Unison.Runtime.Rt1IO as Rt1
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import qualified Version
import qualified Unison.Codebase.Conversion.Upgrade12 as Upgrade12

usage :: String -> P.Pretty P.ColorText
usage executableStr = P.callout "🌻" $ P.lines [
  P.bold "Usage instructions for the Unison Codebase Manager",
  "You are running version: " <> P.string Version.gitDescribe,
  "",
  P.bold executable,
  P.wrap "Starts Unison interactively, using the codebase in the home directory.",
  "",
  P.bold $ executable <> " -codebase path/to/codebase",
  P.wrap "Starts Unison interactively, using the specified codebase. This flag can also be set for any of the below commands.",
  "",
  P.bold $ executable <> " run .mylib.mymain",
  P.wrap "Executes the definition `.mylib.mymain` from the codebase, then exits.",
  "",
  P.bold $ executable <> " run.file foo.u mymain",
  P.wrap "Executes the definition called `mymain` in `foo.u`, then exits.",
  "",
  P.bold $ executable <> " run.pipe mymain",
  P.wrap "Executes the definition called `mymain` from a `.u` file read from the standard input, then exits.",
  "",
  P.bold $ executable <> " transcript mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript and creates"
        <> "`mytranscript.output.md` if successful. Exits after completion, and deletes"
        <> "the temporary directory created."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold $ executable <> " transcript -save-codebase mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript and creates"
        <> "`mytranscript.output.md` if successful. Exits after completion, and saves"
        <> "the resulting codebase to a new directory on disk."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold $ executable <> " transcript.fork mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript in a copy of the current codebase"
        <> "and creates `mytranscript.output.md` if successful. Exits after completion."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold $ executable <> " transcript.fork -save-codebase mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript in a copy of the current codebase"
        <> "and creates `mytranscript.output.md` if successful. Exits after completion,"
        <> "and saves the resulting codebase to a new directory on disk."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold $ executable <> " version",
  "Prints version of Unison then quits.",
  "",
  P.bold $ executable <> " help",
  "Prints this help."]
      where executable = (P.text . Text.pack) executableStr

installSignalHandlers :: IO ()
installSignalHandlers = do
  main_thread <- myThreadId
  wtid <- mkWeakThreadId main_thread

  let interrupt = do
        r <- deRefWeak wtid
        case r of
          Nothing -> return ()
          Just t  -> throwTo t UserInterrupt

#if defined(mingw32_HOST_OS)
  let sig_handler WinSig.ControlC = interrupt
      sig_handler WinSig.Break    = interrupt
      sig_handler _               = return ()
  _ <- WinSig.installHandler (WinSig.Catch sig_handler)
#else
  _ <- Sig.installHandler Sig.sigQUIT  (Sig.Catch interrupt) Nothing
  _ <- Sig.installHandler Sig.sigINT   (Sig.Catch interrupt) Nothing
#endif

  return ()

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  -- hSetBuffering stdout NoBuffering -- cool

  _ <- installSignalHandlers
  -- We need to know whether the program was invoked with -codebase for
  -- certain messages. Therefore we keep a Maybe FilePath - mcodepath
  -- rather than just deciding on whether to use the supplied path or
  -- the home directory here and throwing away that bit of information
  let (mcodepath, restargs0) = case args of
           "-codebase" : codepath : restargs -> (Just codepath, restargs)
           _                                 -> (Nothing, args)
      (mNewRun, restargs1) = case restargs0 of
           "--new-runtime" : rest -> (Just True, rest)
           _ -> (Nothing, restargs0)
      (fromMaybe True -> newCodebase, restargs) = case restargs1 of
           "--new-codebase" : rest -> (Just True, rest)
           "--old-codebase" : rest -> (Just False, rest)
           _ -> (Nothing, restargs1)
      cbInit = if newCodebase then SC.init else FC.init
  currentDir <- getCurrentDirectory
  configFilePath <- getConfigFilePath mcodepath
  config <-
    catchIOError (watchConfig configFilePath) $ \_ ->
      Exit.die "Your .unisonConfig could not be loaded. Check that it's correct!"
  case restargs of
    [] -> do
      theCodebase <- Codebase.getCodebaseOrExit cbInit mcodepath
      Server.start theCodebase $ \token port -> do
        PT.putPrettyLn . P.string $ "I've started a codebase API server at "
        PT.putPrettyLn . P.string $ "http://127.0.0.1:"
          <> show port <> "?" <> URI.encode (unpack token)
        PT.putPrettyLn' . P.string $ "Now starting the Unison Codebase Manager..."
        launch currentDir mNewRun config theCodebase []
    [version] | isFlag "version" version ->
      putStrLn $ progName ++ " version: " ++ Version.gitDescribe
    [help] | isFlag "help" help -> PT.putPrettyLn (usage progName)
    ["init"] -> Codebase.initCodebaseAndExit cbInit mcodepath
    "run" : [mainName] -> do
      theCodebase <- Codebase.getCodebaseOrExit cbInit mcodepath
      runtime <- join . getStartRuntime mNewRun $ fst config
      execute theCodebase runtime mainName
    "run.file" : file : [mainName] | isDotU file -> do
      e <- safeReadUtf8 file
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
        Right contents -> do
          theCodebase <- Codebase.getCodebaseOrExit cbInit mcodepath
          let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
          launch currentDir mNewRun config theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
    "run.pipe" : [mainName] -> do
      e <- safeReadUtf8StdIn
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I had trouble reading this input."
        Right contents -> do
          theCodebase <- Codebase.getCodebaseOrExit cbInit mcodepath
          let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
          launch
            currentDir mNewRun config theCodebase
            [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
    "transcript" : args' ->
      case args' of
      "-save-codebase" : transcripts -> runTranscripts mNewRun cbInit False True mcodepath transcripts
      _                              -> runTranscripts mNewRun cbInit False False mcodepath args'
    "transcript.fork" : args' ->
      case args' of
      "-save-codebase" : transcripts -> runTranscripts mNewRun cbInit True True mcodepath transcripts
      _                              -> runTranscripts mNewRun cbInit True False mcodepath args'
    ["upgrade-codebase"] -> upgradeCodebase mcodepath
    _ -> do
      PT.putPrettyLn (usage progName)
      Exit.exitWith (Exit.ExitFailure 1)

upgradeCodebase :: Maybe Codebase.CodebasePath -> IO ()
upgradeCodebase mcodepath =
  Codebase.getCodebaseDir mcodepath >>= \root -> do
    putStrLn $ "I'm upgrading the codebase in '" ++ root ++ "', but it will take a while."
    Upgrade12.upgradeCodebase root
    putStrLn $ "\nTry it out and once you're satisfied, you may delete the old version from\n\n\t'"
      ++ Codebase.codebasePath (FC.init @IO) root ++ "';\n\nbut there's no rush."

prepareTranscriptDir :: Codebase.Init IO Symbol Ann -> Bool -> Maybe FilePath -> IO FilePath
prepareTranscriptDir cbInit inFork mcodepath = do
  tmp <- Temp.getCanonicalTemporaryDirectory >>= (`Temp.createTempDirectory` "transcript")

  if inFork then
    Codebase.getCodebaseOrExit cbInit mcodepath >> do
    path <- Codebase.getCodebaseDir mcodepath
    PT.putPrettyLn $ P.lines [
      P.wrap "Transcript will be run on a copy of the codebase at: ", "",
      P.indentN 2 (P.string path)
      ]
    Path.copyDir (Codebase.codebasePath cbInit path) (Codebase.codebasePath cbInit tmp)
  else do
    PT.putPrettyLn . P.wrap $ "Transcript will be run on a new, empty codebase."
    void $ Codebase.openNewUcmCodebaseOrExit cbInit tmp
  pure tmp

runTranscripts'
  :: Maybe Bool
  -> Codebase.Init IO Symbol Ann
  -> Maybe FilePath
  -> FilePath
  -> [String]
  -> IO Bool
runTranscripts' mNewRun cbInit mcodepath transcriptDir args = do
  currentDir <- getCurrentDirectory
  case args of
    args@(_:_) -> do
      for_ args $ \arg -> case arg of
        md | isMarkdown md -> do
          parsed <- TR.parseFile arg
          case parsed of
            Left err ->
              PT.putPrettyLn $ P.callout "❓" (
                P.lines [
                  P.indentN 2 "A parsing error occurred while reading a file:", "",
                  P.indentN 2 $ P.string err])
            Right stanzas -> do
              configFilePath <- getConfigFilePath mcodepath
              theCodebase <- Codebase.getCodebaseOrExit cbInit $ Just transcriptDir
              mdOut <- TR.run mNewRun transcriptDir configFilePath stanzas theCodebase
              let out = currentDir FP.</>
                         FP.addExtension (FP.dropExtension arg ++ ".output")
                                         (FP.takeExtension md)
              writeUtf8 out mdOut
              putStrLn $ "💾  Wrote " <> out
        wat ->
              PT.putPrettyLn $ P.callout "❓" (
                P.lines [
                  P.indentN 2 "Unrecognized command, skipping:", "",
                  P.indentN 2 $ P.string wat])
      pure True
    [] ->
      pure False

runTranscripts
  :: Maybe Bool
  -> Codebase.Init IO Symbol Ann
  -> Bool
  -> Bool
  -> Maybe FilePath
  -> [String]
  -> IO ()
runTranscripts mNewRun cbInit inFork keepTemp mcodepath args = do
  progName <- getProgName
  transcriptDir <- prepareTranscriptDir cbInit inFork mcodepath
  completed <-
    runTranscripts' mNewRun cbInit (Just transcriptDir) transcriptDir args
  when completed $ do
    unless keepTemp $ removeDirectoryRecursive transcriptDir
    when keepTemp $ PT.putPrettyLn $
        P.callout "🌸" (
          P.lines [
            "I've finished running the transcript(s) in this codebase:", "",
            P.indentN 2 (P.string transcriptDir), "",
            P.wrap $ "You can run"
                  <> P.backticked (P.string progName <> " -codebase " <> P.string transcriptDir)
                  <> "to do more work with it."])

  unless completed $ do
      unless keepTemp $ removeDirectoryRecursive transcriptDir
      PT.putPrettyLn (usage progName)
      Exit.exitWith (Exit.ExitFailure 1)

initialPath :: Path.Absolute
initialPath = Path.absoluteEmpty

getStartRuntime :: Maybe Bool -> Config -> IO (IO (Runtime Symbol))
getStartRuntime newRun config = do
  b <- maybe (Config.lookupDefault False config "new-runtime") pure newRun
  pure $ if b then RTI.startRuntime else pure Rt1.runtime

launch
  :: FilePath
  -> Maybe Bool
  -> (Config, IO ())
  -> _
  -> [Either Input.Event Input.Input]
  -> IO ()
launch dir newRun config code inputs = do
  startRuntime <- getStartRuntime newRun $ fst config
  CommandLine.main dir defaultBaseLib initialPath config inputs startRuntime code Version.gitDescribe

isMarkdown :: String -> Bool
isMarkdown md = case FP.takeExtension md of
  ".md" -> True
  ".markdown" -> True
  _ -> False

isDotU :: String -> Bool
isDotU file = FP.takeExtension file == ".u"

-- so we can do `ucm --help`, `ucm -help` or `ucm help` (I hate
-- having to remember which one is supported)
isFlag :: String -> String -> Bool
isFlag f arg = arg == f || arg == "-" ++ f || arg == "--" ++ f

getConfigFilePath :: Maybe FilePath -> IO FilePath
getConfigFilePath mcodepath = (FP.</> ".unisonConfig") <$> Codebase.getCodebaseDir mcodepath

defaultBaseLib :: Maybe RemoteNamespace
defaultBaseLib = rightMay $
  runParser VP.defaultBaseLib "version" (Text.pack Version.gitDescribe)
