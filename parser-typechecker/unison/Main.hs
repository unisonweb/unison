{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Control.Concurrent (mkWeakThreadId, myThreadId)
import Control.Exception (AsyncException (UserInterrupt), throwTo)
import qualified Data.Text as Text
import Options (Command (..), Fork (..), SaveCodebase (..), Stdin (..))
import qualified Options
import Options.Applicative (customExecParser, prefs, showHelpOnError)
import System.Directory (getCurrentDirectory, removeDirectoryRecursive)
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import qualified System.IO.Temp as Temp
import System.Mem.Weak (deRefWeak)
import qualified System.Path as Path
import qualified System.Posix.Signals as Sig
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Execute (execute)
import qualified Unison.Codebase.FileCodebase as FileCodebase
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.TranscriptParser as TR
import qualified Unison.CommandLine.Main as CommandLine
import Unison.Prelude
import qualified Unison.PrettyTerminal as PT
import qualified Unison.Runtime.Rt1IO as Rt1
import qualified Unison.Util.Pretty as P
import qualified Version

installSignalHandlers :: IO ()
installSignalHandlers = do
  main_thread <- myThreadId
  wtid <- mkWeakThreadId main_thread
  let interrupt = do
        r <- deRefWeak wtid
        case r of
          Nothing -> pure ()
          Just t -> throwTo t UserInterrupt
  void $ Sig.installHandler Sig.sigQUIT (Sig.Catch interrupt) Nothing
  void $ Sig.installHandler Sig.sigINT (Sig.Catch interrupt) Nothing

main :: IO ()
main = do
  void installSignalHandlers
  option <- customExecParser (prefs showHelpOnError) Options.options
  currentDir <- getCurrentDirectory
  case option of
    Launch codepath -> do
      configFilePath <- getConfigFilePath codepath
      theCodebase <- FileCodebase.getCodebaseOrExit codepath
      launch currentDir configFilePath theCodebase []
    Version -> putStrLn $ "ucm version: " <> Version.gitDescribe
    Init codepath -> FileCodebase.initCodebaseAndExit codepath
    Run codepath _ (Stdin True) mainName -> do
      e <- safeReadUtf8StdIn
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I had trouble reading this input."
        Right contents -> do
          theCodebase <- FileCodebase.getCodebaseOrExit codepath
          let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
          configFilePath <- getConfigFilePath codepath
          launch currentDir configFilePath theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
    Run codepath Nothing (Stdin False) mainName -> do
      theCodebase <- FileCodebase.getCodebaseOrExit codepath
      execute theCodebase Rt1.runtime mainName
    Run codepath (Just file) (Stdin False) mainName | isDotU file -> do
      e <- safeReadUtf8 file
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
        Right contents -> do
          theCodebase <- FileCodebase.getCodebaseOrExit codepath
          let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
          configFilePath <- getConfigFilePath codepath
          launch currentDir configFilePath theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
    Run _ _ _ _ -> Exit.die "Expected a unison file with extension .u"
    Transcript codepath fork save transcripts -> runTranscripts fork save currentDir codepath transcripts

prepareTranscriptDir :: Fork -> FilePath -> Maybe FilePath -> IO FilePath
prepareTranscriptDir (Fork False) currentDir _ = do
  tmp <- Temp.createTempDirectory currentDir "transcript"
  PT.putPrettyLn . P.wrap $ "Transcript will be run on a new, empty codebase."
  void $ FileCodebase.initCodebase tmp
  pure tmp
prepareTranscriptDir (Fork True) currentDir mcodepath = do
  void $ FileCodebase.getCodebaseOrExit mcodepath
  tmp <- Temp.createTempDirectory currentDir "transcript"
  path <- FileCodebase.getCodebaseDir mcodepath
  PT.putPrettyLn $
    P.lines
      [ P.wrap "Transcript will be run on a copy of the codebase at: ",
        "",
        P.indentN 2 (P.string path)
      ]
  Path.copyDir path tmp
  pure tmp

runTranscripts' :: FilePath -> Maybe FilePath -> FilePath -> [String] -> IO ()
runTranscripts' currentDir mcodepath transcriptDir args = do
  theCodebase <- FileCodebase.getCodebaseOrExit $ Just transcriptDir
  for_ args $ \arg -> case arg of
    md | isMarkdown md -> do
      parsed <- TR.parseFile arg
      case parsed of
        Left err ->
          PT.putPrettyLn $
            P.callout
              "❓"
              ( P.lines
                  [ P.indentN 2 "A parsing error occurred while reading a file:",
                    "",
                    P.indentN 2 $ P.string err
                  ]
              )
        Right stanzas -> do
          configFilePath <- getConfigFilePath mcodepath
          mdOut <- TR.run transcriptDir configFilePath stanzas theCodebase
          let out =
                currentDir
                  FP.</> FP.addExtension
                    (FP.dropExtension arg ++ ".output")
                    (FP.takeExtension md)
          writeUtf8 out mdOut
          putStrLn $ "💾  Wrote " <> out
    wat ->
      PT.putPrettyLn $
        P.callout
          "❓"
          ( P.lines
              [ P.indentN 2 "Unrecognized command, skipping:",
                "",
                P.indentN 2 $ P.string wat
              ]
          )

runTranscripts :: Fork -> SaveCodebase -> FilePath -> Maybe FilePath -> [String] -> IO ()
runTranscripts inFork keepTemp currentDir mcodepath args = do
  transcriptDir <- prepareTranscriptDir inFork currentDir mcodepath
  runTranscripts' currentDir (Just transcriptDir) transcriptDir args
  case keepTemp of
    (SaveCodebase False) -> removeDirectoryRecursive transcriptDir
    (SaveCodebase True) ->
      PT.putPrettyLn $
        P.callout
          "🌸"
          ( P.lines
              [ "I've finished running the transcript(s) in this codebase:",
                "",
                P.indentN 2 (P.string transcriptDir),
                "",
                P.wrap $
                  "You can run"
                    <> P.backticked ("ucm --codebase " <> P.string transcriptDir)
                    <> "to do more work with it."
              ]
          )

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

getConfigFilePath :: Maybe FilePath -> IO FilePath
getConfigFilePath mcodepath = (FP.</> ".unisonConfig") <$> FileCodebase.getCodebaseDir mcodepath
