{-# Language OverloadedStrings #-}
{-# Language PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Unison.Prelude
import           System.Directory               ( getCurrentDirectory, getHomeDirectory )
import           System.Environment             ( getArgs )
import qualified Unison.Codebase.FileCodebase  as FileCodebase
import qualified Unison.CommandLine.Main       as CommandLine
import qualified Unison.Runtime.Rt1IO          as Rt1
import qualified Unison.Codebase.Path          as Path
import qualified Version as Version
import qualified Unison.Codebase.TranscriptParser as TR
import qualified System.Path as Path
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
  P.wrap $ "Executes the definition `.mylib.mymain` from the codebase, then exits.",
  "",
  P.bold "ucm run.file foo.u mymain",
  P.wrap $ "Executes the definition called `mymain` in `foo.u`, then exits.",
  "",
  P.bold "ucm run.pipe mymain",
  P.wrap $ "Executes the definition called `mymain` from a `.u` file read from the standard input, then exits.",
  "",
  P.bold "ucm transcript mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript and creates"
        <> "`mytranscript.output.md` if successful. Exits after completion."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold "ucm transcript.fork mytranscript.md",
  P.wrap $ "Executes the `mytranscript.md` transcript in a copy of the current codebase"
        <> "and creates `mytranscript.output.md` if successful. Exits after completion."
        <> "Multiple transcript files may be provided; they are processed in sequence"
        <> "starting from the same codebase.",
  "",
  P.bold "ucm version",
  "Prints version of Unison then quits.",
  "",
  P.bold "ucm help",
  "Prints this help."
  ]

main :: IO ()
main = do
  args <- getArgs
  -- hSetBuffering stdout NoBuffering -- cool

  -- We need to know whether the program was invoked with -codebase for
  -- certain messages. Therefore we keep a Maybe FilePath - mcodepath
  -- rather than just deciding on whether to use the supplied path or
  -- the home directory here and throwing away that bit of information
  let (mcodepath, restargs) = case args of
           "-codebase" : codepath : restargs -> (Just codepath, restargs)
           _                                 -> (Nothing, args)
  currentDir <- getCurrentDirectory
  case restargs of
    [] -> do
      theCodebase <- FileCodebase.getCodebaseOrExit mcodepath
      launch currentDir theCodebase []
    [version] | isFlag "version" version ->
      putStrLn $ "ucm version: " ++ Version.gitDescribe
    [help] | isFlag "help" help -> PT.putPrettyLn usage
    ["init"] -> FileCodebase.initCodebaseAndExit mcodepath
    "run" : [mainName] -> do
      theCodebase <- FileCodebase.getCodebaseOrExit mcodepath
      launch currentDir theCodebase [Right $ Input.ExecuteI mainName, Right Input.QuitI]
    "run.file" : file : [mainName] | isDotU file -> do
      e <- safeReadUtf8 file
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
        Right contents -> do
          theCodebase <- FileCodebase.getCodebaseOrExit mcodepath
          let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
          launch currentDir theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
    "run.pipe" : [mainName] -> do
      e <- safeReadUtf8StdIn
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I had trouble reading this input."
        Right contents -> do
          theCodebase <- FileCodebase.getCodebaseOrExit mcodepath
          let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
          launch currentDir theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
    "transcript" : args -> runTranscripts False mcodepath args
    "transcript.fork" : args -> runTranscripts True mcodepath args
    _ -> do
      PT.putPrettyLn usage
      Exit.exitWith (Exit.ExitFailure 1)

runTranscripts :: Bool -> Maybe FilePath -> [String] -> IO ()
runTranscripts inFork mcodepath args = do
  currentDir <- getCurrentDirectory
  transcriptDir <- do
    tmp <- Temp.createTempDirectory currentDir "transcript"
    when (not inFork) $ do
      PT.putPrettyLn . P.wrap $ "Transcript will be run on a new, empty codebase."
      _ <- FileCodebase.initCodebase tmp
      pure ()
    when inFork $ FileCodebase.getCodebaseOrExit mcodepath >> do
      origCodePath <- case mcodepath of
        Just codepath -> pure codepath
        Nothing       -> getHomeDirectory
      let path = (origCodePath FP.</> FileCodebase.codebasePath)
      PT.putPrettyLn $ P.lines [
        P.wrap "Transcript will be run on a copy of the codebase at: ", "",
        P.indentN 2 (P.string path)
        ]
      Path.copyDir path (tmp FP.</> FileCodebase.codebasePath)
    pure tmp
  theCodebase <- FileCodebase.getCodebaseOrExit $ Just transcriptDir
  case args of
    args@(_:_) -> do
      for_ args $ \arg -> case arg of
        md | isMarkdown md -> do
          parsed <- TR.parseFile arg
          case parsed of
            Left err -> putStrLn $ "Parse error: \n" <> show err
            Right stanzas -> do
              mdOut <- TR.run currentDir stanzas theCodebase
              let out = currentDir FP.</>
                         FP.addExtension (FP.dropExtension arg ++ ".output")
                                         (FP.takeExtension md)
              writeUtf8 out mdOut
              putStrLn $ "💾  Wrote " <> out
        wat -> putStrLn $ "Unrecognized command, skipping: " <> wat
      PT.putPrettyLn $
        P.callout "🌸" (
          P.lines [
            "I've finished running the transcript(s) in this codebase:", "",
            P.indentN 2 (P.string transcriptDir), "",
            P.wrap $ "You can run"
                  <> P.backticked ("ucm -codebase " <> P.string transcriptDir)
                  <> "to do more work with it."])
    [] -> do
      PT.putPrettyLn usage
      Exit.exitWith (Exit.ExitFailure 1)

initialPath :: Path.Absolute
initialPath = Path.absoluteEmpty

launch :: FilePath -> _ -> [Either Input.Event Input.Input] -> IO ()
launch dir code inputs =
  CommandLine.main dir initialPath inputs (pure Rt1.runtime) code

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
