{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}
{-# Language PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Unison.Prelude
import           Control.Concurrent             ( mkWeakThreadId
                                                , myThreadId
                                                --, forkIO
                                                )
import           Control.Error.Safe             (rightMay)
import           Control.Exception              ( throwTo
                                                , AsyncException(UserInterrupt)
                                                )
import           Data.ByteString.Char8          ( unpack )
import           Data.Configurator.Types        ( Config )
import qualified Network.URI.Encode            as URI
import           System.Directory               ( getCurrentDirectory
                                                , removeDirectoryRecursive
                                                )
import           System.Environment             ( getArgs, getProgName )
import           System.Mem.Weak                ( deRefWeak )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.Editor.VersionParser as VP
import           Unison.Codebase.Execute        ( execute )
import qualified Unison.Codebase.FileCodebase  as FileCodebase
import           Unison.Codebase.FileCodebase.Common ( codebasePath )
import           Unison.Codebase.Editor.RemoteRepo (RemoteNamespace)
import           Unison.Codebase.Runtime        ( Runtime )
import           Unison.CommandLine             ( watchConfig )
import qualified Unison.CommandLine.Main       as CommandLine
import qualified Unison.Runtime.Rt1IO          as Rt1
import qualified Unison.Runtime.Interface      as RTI
import           Unison.Symbol                  ( Symbol )
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Util.Cache             as Cache
import qualified Unison.Server.CodebaseServer as Server
import qualified Version
import qualified Unison.Codebase.TranscriptParser as TR
import qualified System.Path as Path
import qualified System.FilePath as FP
import qualified System.IO.Temp as Temp
import qualified System.Exit as Exit
import System.IO.Error (catchIOError)
import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Util.Pretty as P
import qualified Unison.PrettyTerminal as PT
import qualified Data.Text as Text
import qualified Data.Configurator as Config
import Text.Megaparsec (runParser)

#if defined(mingw32_HOST_OS)
import qualified GHC.ConsoleHandler as WinSig
#else
import qualified System.Posix.Signals as Sig
#endif

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

data UcmConfig = UcmConfig
  { codepath :: Maybe String
  , useNewRuntime :: Maybe Bool
  , args :: [String]
  }

mkConfig :: [String] -> UcmConfig
mkConfig str =
  let
    -- We need to know whether the program was invoked with -codebase for
    -- certain messages. Therefore we keep a Maybe FilePath - mcodepath
    -- rather than just deciding on whether to use the supplied path or
    -- the home directory here and throwing away that bit of information
    (codepath, restargs0) = case str of
      "-codebase" : codepath : restargs -> (Just codepath, restargs)
      _                                 -> (Nothing, str)
    (useNewRuntime, restargs) = case restargs0 of
      "--new-runtime" : rest -> (Just True, rest)
      _                      -> (Nothing, restargs0)
  in
    UcmConfig { codepath=codepath, useNewRuntime=useNewRuntime, args=restargs }

main :: IO ()
main = do
  ucmConfig@UcmConfig{ codepath } <- mkConfig <$> getArgs
  progName <- getProgName
  -- hSetBuffering stdout NoBuffering -- cool

  _ <- installSignalHandlers
  currentDir <- getCurrentDirectory
  configFilePath <- getConfigFilePath codepath

  config@(config_, _cancelConfig) <-
    catchIOError (watchConfig configFilePath) $ \_ ->
      Exit.die "Your .unisonConfig could not be loaded. Check that it's correct!"

  branchCacheSize :: Word <- Config.lookupDefault 4096 config_ "NamespaceCacheSize"
  branchCache <- Cache.semispaceCache branchCacheSize
  case args ucmConfig of
    [] -> do
      theCodebase <- FileCodebase.getCodebaseOrExit branchCache codepath
      Server.start theCodebase $ \token port -> do
        PT.putPrettyLn . P.string $ "I've started a codebase API server at "
        PT.putPrettyLn . P.string $ "http://127.0.0.1:"
          <> show port <> "?" <> URI.encode (unpack token)
        launch ucmConfig currentDir config theCodebase branchCache []
    [version] | isFlag "version" version ->
      putStrLn $ progName ++ " version: " ++ Version.gitDescribe
    [help] | isFlag "help" help -> PT.putPrettyLn (usage progName)
    ["init"] -> FileCodebase.initCodebaseAndExit codepath
    "run" : [mainName] -> do
      theCodebase <- FileCodebase.getCodebaseOrExit branchCache codepath
      runtime <- join . getStartRuntime (useNewRuntime ucmConfig) $ fst config
      execute theCodebase runtime mainName
    "run.file" : file : [mainName] | isDotU file -> do
      e <- safeReadUtf8 file
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
        Right contents -> do
          theCodebase <- FileCodebase.getCodebaseOrExit branchCache codepath
          let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
          launch ucmConfig currentDir config theCodebase branchCache [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
    "run.pipe" : [mainName] -> do
      e <- safeReadUtf8StdIn
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I had trouble reading this input."
        Right contents -> do
          theCodebase <- FileCodebase.getCodebaseOrExit branchCache codepath
          let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
          launch
            ucmConfig currentDir config theCodebase branchCache
            [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
    "transcript" : args' ->
      case args' of
      "-save-codebase" : transcripts -> runTranscripts ucmConfig branchCache False True transcripts
      _                              -> runTranscripts ucmConfig branchCache False False args'
    "transcript.fork" : args' ->
      case args' of
      "-save-codebase" : transcripts -> runTranscripts ucmConfig branchCache True True transcripts
      _                              -> runTranscripts ucmConfig branchCache True False args'
    _ -> do
      PT.putPrettyLn (usage progName)
      Exit.exitWith (Exit.ExitFailure 1)

prepareTranscriptDir :: Branch.Cache IO -> Bool -> Maybe FilePath -> IO FilePath
prepareTranscriptDir branchCache inFork mcodepath = do
  tmp <- Temp.getCanonicalTemporaryDirectory >>= (`Temp.createTempDirectory` "transcript")
  unless inFork $ do
    PT.putPrettyLn . P.wrap $ "Transcript will be run on a new, empty codebase."
    _ <- FileCodebase.initCodebase branchCache tmp
    pure()

  when inFork $ FileCodebase.getCodebaseOrExit branchCache mcodepath >> do
    path <- FileCodebase.getCodebaseDir mcodepath
    PT.putPrettyLn $ P.lines [
      P.wrap "Transcript will be run on a copy of the codebase at: ", "",
      P.indentN 2 (P.string path)
      ]
    Path.copyDir (path FP.</> codebasePath) (tmp FP.</> codebasePath)

  pure tmp

runTranscripts'
  :: Maybe Bool
  -> Branch.Cache IO
  -> Maybe FilePath
  -> FilePath
  -> [String]
  -> IO Bool
runTranscripts' mNewRun branchCache mcodepath transcriptDir args = do
  currentDir <- getCurrentDirectory
  theCodebase <- FileCodebase.getCodebaseOrExit branchCache $ Just transcriptDir
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
              mdOut <- TR.run mNewRun transcriptDir configFilePath stanzas theCodebase branchCache
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
  :: UcmConfig
  -> Branch.Cache IO
  -> Bool
  -> Bool
  -> [String]
  -> IO ()
runTranscripts config branchCache inFork keepTemp args = do
  progName <- getProgName
  transcriptDir <- prepareTranscriptDir branchCache inFork (codepath config)
  completed <-
    runTranscripts' (useNewRuntime config) branchCache (Just transcriptDir) transcriptDir args
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
  :: UcmConfig
  -> FilePath
  -> (Config, IO ())
  -> _
  -> Branch.Cache IO
  -> [Either Input.Event Input.Input]
  -> IO ()
launch ucmConfig dir config code branchCache inputs = do
  startRuntime <- getStartRuntime (useNewRuntime ucmConfig) $ fst config
  CommandLine.main dir defaultBaseLib initialPath config inputs startRuntime code branchCache Version.gitDescribe

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
getConfigFilePath mcodepath = (FP.</> ".unisonConfig") <$> FileCodebase.getCodebaseDir mcodepath

defaultBaseLib :: Maybe RemoteNamespace
defaultBaseLib = rightMay $
  runParser VP.defaultBaseLib "version" (Text.pack Version.gitDescribe)
