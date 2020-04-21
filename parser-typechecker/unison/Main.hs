{-# Language OverloadedStrings #-}
{-# Language PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Unison.Prelude
import           Control.Concurrent             ( mkWeakThreadId, myThreadId )
import           Control.Exception              ( throwTo, AsyncException(UserInterrupt) )
import           System.Directory               ( getCurrentDirectory, removeDirectoryRecursive )
import           System.Environment             ( getArgs )
import           System.Mem.Weak                ( deRefWeak )
import           Unison.Codebase.Execute        ( execute )
import qualified Unison.Codebase.FileCodebase  as FileCodebase
import qualified Unison.CommandLine.Main       as CommandLine
import qualified Unison.Runtime.Rt1IO          as Rt1
import qualified Unison.Codebase.Path          as Path
import qualified Version
import qualified Unison.Codebase.TranscriptParser as TR
import qualified System.Path as Path
import qualified System.Posix.Signals as Sig
import qualified System.FilePath as FP
import qualified System.IO.Temp as Temp
import qualified System.Exit as Exit
import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Util.Pretty as P
import qualified Unison.PrettyTerminal as PT
import qualified Data.Text as Text

usage :: P.Pretty P.ColorText
usage = P.callout "🌻" $ P.lines [
  P.bold "Usage instructions for the Unison Codebase Manager",
  "You are running version: " <> P.string Version.gitDescribe,
  "",
  P.bold "ucm",
  P.wrap "Starts Unison interactively, using the codebase in the home directory.",
  "",
  P.bold "ucm -codebase path/to/codebase",
  P.wrap "Starts Unison interactively, using the specified codebase. This flag can also be set for any of the below commands.",
  "",
  P.bold "ucm run .mylib.mymain",
  P.wrap "Executes the definition `.mylib.mymain` from the codebase, then exits.",
  "",
  P.bold "ucm run.file foo.u mymain",
  P.wrap "Executes the definition called `mymain` in `foo.u`, then exits.",
  "",
  P.bold "ucm run.pipe mymain",
  P.wrap "Executes the definition called `mymain` from a `.u` file read from the standard input, then exits.",
  "",
  P.bold "ucm transcript mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript and creates"
        <> "`mytranscript.output.md` if successful. Exits after completion, and deletes"
        <> "the temporary directory created."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold "ucm transcript -save-codebase mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript and creates"
        <> "`mytranscript.output.md` if successful. Exits after completion, and saves"
        <> "the resulting codebase to a new directory on disk."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold "ucm transcript.fork mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript in a copy of the current codebase"
        <> "and creates `mytranscript.output.md` if successful. Exits after completion."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold "ucm transcript.fork -save-codebase mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript in a copy of the current codebase"
        <> "and creates `mytranscript.output.md` if successful. Exits after completion,"
        <> "and saves the resulting codebase to a new directory on disk."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold "ucm version",
  "Prints version of Unison then quits.",
  "",
  P.bold "ucm help",
  "Prints this help."
  ]

installSignalHandlers :: IO ()
installSignalHandlers = do
  main_thread <- myThreadId
  wtid <- mkWeakThreadId main_thread

  let interrupt = do
        r <- deRefWeak wtid
        case r of
          Nothing -> return ()
          Just t  -> throwTo t UserInterrupt
  _ <- Sig.installHandler Sig.sigQUIT  (Sig.Catch interrupt) Nothing
  _ <- Sig.installHandler Sig.sigINT   (Sig.Catch interrupt) Nothing
  return ()

main :: IO ()
main = do
  args <- getArgs
  -- hSetBuffering stdout NoBuffering -- cool

  _ <- installSignalHandlers
  -- We need to know whether the program was invoked with -codebase for
  -- certain messages. Therefore we keep a Maybe FilePath - mcodepath
  -- rather than just deciding on whether to use the supplied path or
  -- the home directory here and throwing away that bit of information
  let (mcodepath, restargs) = case args of
           "-codebase" : codepath : restargs -> (Just codepath, restargs)
           _                                 -> (Nothing, args)
  currentDir <- getCurrentDirectory
  configFilePath <- getConfigFilePath mcodepath
  case restargs of
    [] -> do
      theCodebase <- FileCodebase.getCodebaseOrExit mcodepath
      launch currentDir configFilePath theCodebase []
    [version] | isFlag "version" version ->
      putStrLn $ "ucm version: " ++ Version.gitDescribe
    [help] | isFlag "help" help -> PT.putPrettyLn usage
    ["init"] -> FileCodebase.initCodebaseAndExit mcodepath
    "run" : [mainName] -> do
      theCodebase <- FileCodebase.getCodebaseOrExit mcodepath
      execute theCodebase Rt1.runtime mainName
    "run.file" : file : [mainName] | isDotU file -> do
      e <- safeReadUtf8 file
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
        Right contents -> do
          theCodebase <- FileCodebase.getCodebaseOrExit mcodepath
          let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
          launch currentDir configFilePath theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
    "run.pipe" : [mainName] -> do
      e <- safeReadUtf8StdIn
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I had trouble reading this input."
        Right contents -> do
          theCodebase <- FileCodebase.getCodebaseOrExit mcodepath
          let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
          launch currentDir configFilePath theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
    "transcript" : args' ->
      case args' of
      "-save-codebase" : transcripts -> runTranscripts False True mcodepath transcripts
      _                              -> runTranscripts False False mcodepath args'
    "transcript.fork" : args' ->
      case args' of
      "-save-codebase" : transcripts -> runTranscripts True True mcodepath transcripts
      _                              -> runTranscripts True False mcodepath args'
    _ -> do
      PT.putPrettyLn usage
      Exit.exitWith (Exit.ExitFailure 1)

prepareTranscriptDir :: Bool -> Maybe FilePath -> IO FilePath
prepareTranscriptDir inFork mcodepath = do
  currentDir <- getCurrentDirectory
  tmp <- Temp.createTempDirectory currentDir "transcript"

  unless inFork $ do
    PT.putPrettyLn . P.wrap $ "Transcript will be run on a new, empty codebase."
    _ <- FileCodebase.initCodebase tmp
    pure()

  when inFork $ FileCodebase.getCodebaseOrExit mcodepath >> do
    path <- FileCodebase.getCodebaseDir mcodepath
    PT.putPrettyLn $ P.lines [
      P.wrap "Transcript will be run on a copy of the codebase at: ", "",
      P.indentN 2 (P.string path)
      ]
    Path.copyDir path tmp

  pure tmp

runTranscripts' :: Maybe FilePath -> FilePath -> [String] -> IO Bool
runTranscripts' mcodepath transcriptDir args = do
  currentDir <- getCurrentDirectory
  theCodebase <- FileCodebase.getCodebaseOrExit $ Just transcriptDir
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
              mdOut <- TR.run transcriptDir configFilePath stanzas theCodebase
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

runTranscripts :: Bool -> Bool -> Maybe FilePath -> [String] -> IO ()
runTranscripts inFork keepTemp mcodepath args = do
  transcriptDir <- prepareTranscriptDir inFork mcodepath
  completed <- runTranscripts' (Just transcriptDir) transcriptDir args
  when completed $ do
    unless keepTemp $ removeDirectoryRecursive transcriptDir
    when keepTemp $ PT.putPrettyLn $
        P.callout "🌸" (
          P.lines [
            "I've finished running the transcript(s) in this codebase:", "",
            P.indentN 2 (P.string transcriptDir), "",
            P.wrap $ "You can run"
                  <> P.backticked ("ucm -codebase " <> P.string transcriptDir)
                  <> "to do more work with it."])

  unless completed $ do
      unless keepTemp $ removeDirectoryRecursive transcriptDir
      PT.putPrettyLn usage
      Exit.exitWith (Exit.ExitFailure 1)

initialPath :: Path.Absolute
initialPath = Path.absoluteEmpty

launch :: FilePath -> FilePath -> _ -> [Either Input.Event Input.Input] -> IO ()
launch dir configFile code inputs =
  CommandLine.main dir initialPath configFile inputs (pure Rt1.runtime) code

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
