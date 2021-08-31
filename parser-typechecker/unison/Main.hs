{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Concurrent (newEmptyMVar, takeMVar)
import Control.Error.Safe (rightMay)
import Data.Configurator.Types (Config)
import qualified Data.Text as Text
import qualified GHC.Conc
import System.Directory (canonicalizePath, getCurrentDirectory, removeDirectoryRecursive)
import System.Environment (getProgName)
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.IO.Error (catchIOError)
import qualified System.IO.Temp as Temp
import qualified System.Path as Path
import Text.Megaparsec (runParser)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Init as Codebase
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace)
import qualified Unison.Codebase.Editor.VersionParser as VP
import Unison.Codebase.Execute (execute)
import Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.CommandLine (plural', watchConfig)
import qualified Unison.CommandLine.Main as CommandLine
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Codebase.Runtime as Rt
import qualified Unison.PrettyTerminal as PT
import qualified Unison.Runtime.Interface as RTI
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import qualified Version
import Compat ( installSignalHandlers )
import ArgParse
    ( UsageRenderer,
      GlobalOptions(GlobalOptions, codebasePath),
      Command(Launch, PrintVersion, Init, Run, Transcript),
      IsHeadless(WithCLI, Headless),
      ShouldSaveCodebase(..),
      ShouldForkCodebase(..),
      RunSource(RunFromPipe, RunFromSymbol, RunFromFile),
      parseCLIArgs )
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

main :: IO ()
main = do
  progName <- getProgName
  -- hSetBuffering stdout NoBuffering -- cool

  void installSignalHandlers
  (renderUsageInfo, globalOptions, command) <- parseCLIArgs progName Version.gitDescribe
  let GlobalOptions{codebasePath=mcodepath} = globalOptions
  let cbInit = SC.init
  currentDir <- getCurrentDirectory
  configFilePath <- getConfigFilePath mcodepath
  config <-
    catchIOError (watchConfig configFilePath) $ \_ ->
      Exit.die "Your .unisonConfig could not be loaded. Check that it's correct!"
  case command of
     PrintVersion ->
       putStrLn $ progName ++ " version: " ++ Version.gitDescribe
     Init ->
       Codebase.initCodebaseAndExit cbInit "main.init" mcodepath
     Run (RunFromSymbol mainName) -> do
      (closeCodebase, theCodebase) <- getCodebaseOrExit mcodepath
      runtime <- RTI.startRuntime
      execute theCodebase runtime mainName
      closeCodebase
     Run (RunFromFile file mainName)
       | not (isDotU file) -> PT.putPrettyLn $ P.callout "⚠️" "Files must have a .u extension."
       | otherwise -> do
            e <- safeReadUtf8 file
            case e of
              Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
              Right contents -> do
                (closeCodebase, theCodebase) <- getCodebaseOrExit mcodepath
                rt <- RTI.startRuntime
                let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
                launch currentDir config rt theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI] Nothing
                closeCodebase
     Run (RunFromPipe mainName) -> do
      e <- safeReadUtf8StdIn
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I had trouble reading this input."
        Right contents -> do
          (closeCodebase, theCodebase) <- getCodebaseOrExit mcodepath
          rt <- RTI.startRuntime
          let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
          launch
            currentDir config rt theCodebase
            [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
            Nothing
          closeCodebase
     Transcript shouldFork shouldSaveCodebase transcriptFiles ->
       runTranscripts renderUsageInfo shouldFork shouldSaveCodebase mcodepath transcriptFiles
     Launch isHeadless codebaseServerOpts -> do
       (closeCodebase, theCodebase) <- getCodebaseOrExit mcodepath
       runtime <- RTI.startRuntime
       Server.startServer codebaseServerOpts runtime theCodebase $ \baseUrl -> do
         PT.putPrettyLn $ P.lines
           ["The Unison Codebase UI is running at", P.string $ Server.urlFor Server.UI baseUrl]
         case isHeadless of
             Headless -> do
                 PT.putPrettyLn $ P.lines
                    ["I've started the codebase API server at" , P.string $ Server.urlFor Server.Api baseUrl]
                 PT.putPrettyLn $ P.string "Running the codebase manager headless with "
                     <> P.shown GHC.Conc.numCapabilities
                     <> " "
                     <> plural' GHC.Conc.numCapabilities "cpu" "cpus"
                     <> "."
                 mvar <- newEmptyMVar
                 takeMVar mvar
             WithCLI -> do
                 PT.putPrettyLn $ P.string "Now starting the Unison Codebase Manager..."
                 launch currentDir config runtime theCodebase [] (Just baseUrl)
                 closeCodebase

prepareTranscriptDir :: ShouldForkCodebase -> Maybe FilePath -> IO FilePath
prepareTranscriptDir shouldFork mcodepath = do
  tmp <- Temp.getCanonicalTemporaryDirectory >>= (`Temp.createTempDirectory` "transcript")
  let cbInit = SC.init
  case shouldFork of
    UseFork -> do
      getCodebaseOrExit mcodepath
      path <- Codebase.getCodebaseDir mcodepath
      PT.putPrettyLn $ P.lines [
        P.wrap "Transcript will be run on a copy of the codebase at: ", "",
        P.indentN 2 (P.string path)
        ]
      Path.copyDir (Codebase.codebasePath cbInit path) (Codebase.codebasePath cbInit tmp)
    DontFork -> do
      PT.putPrettyLn . P.wrap $ "Transcript will be run on a new, empty codebase."
      void $ Codebase.openNewUcmCodebaseOrExit cbInit "main.transcript" tmp
  pure tmp

runTranscripts'
  :: Maybe FilePath
  -> FilePath
  -> NonEmpty String
  -> IO Bool
runTranscripts' mcodepath transcriptDir args = do
  currentDir <- getCurrentDirectory
  let (markdownFiles, invalidArgs) = NonEmpty.partition isMarkdown args
  for_ markdownFiles $ \fileName -> do
    parsed <- TR.parseFile fileName
    case parsed of
      Left err ->
        PT.putPrettyLn $ P.callout "❓" (
          P.lines [
            P.indentN 2 "A parsing error occurred while reading a file:", "",
            P.indentN 2 $ P.string err])
      Right stanzas -> do
        configFilePath <- getConfigFilePath mcodepath
        (closeCodebase, theCodebase) <- getCodebaseOrExit $ Just transcriptDir
        mdOut <- TR.run transcriptDir configFilePath stanzas theCodebase
        closeCodebase
        let out = currentDir FP.</>
                   FP.addExtension (FP.dropExtension fileName ++ ".output")
                                   (FP.takeExtension fileName)
        writeUtf8 out mdOut
        putStrLn $ "💾  Wrote " <> out

  when (not . null $ invalidArgs) $ do
    PT.putPrettyLn $ P.callout "❓" (
      P.lines
        [ P.indentN 2 "Transcripts must have an .md or .markdown extension."
        , P.indentN 2 "Skipping the following invalid files:"
        , ""
        , P.bulleted $ fmap (P.bold . P.string . (<> "\n")) invalidArgs
        ])
  pure True

runTranscripts
  :: UsageRenderer
  -> ShouldForkCodebase
  -> ShouldSaveCodebase
  -> Maybe FilePath
  -> NonEmpty String
  -> IO ()
runTranscripts renderUsageInfo shouldFork shouldSaveTempCodebase mcodepath args = do
  progName <- getProgName
  transcriptDir <- prepareTranscriptDir shouldFork mcodepath
  completed <-
    runTranscripts' (Just transcriptDir) transcriptDir args
  case shouldSaveTempCodebase of
    DontSaveCodebase -> removeDirectoryRecursive transcriptDir
    SaveCodebase ->
      if completed
        then
          PT.putPrettyLn $
            P.callout "🌸" (
              P.lines [
                "I've finished running the transcript(s) in this codebase:", "",
                P.indentN 2 (P.string transcriptDir), "",
                P.wrap $ "You can run"
                      <> P.backticked (P.string progName <> " -codebase " <> P.string transcriptDir)
                      <> "to do more work with it."])
        else do
          putStrLn (renderUsageInfo $ Just "transcript")
          Exit.exitWith (Exit.ExitFailure 1)

initialPath :: Path.Absolute
initialPath = Path.absoluteEmpty

launch
  :: FilePath
  -> (Config, IO ())
  -> Rt.Runtime Symbol
  -> Codebase.Codebase IO Symbol Ann
  -> [Either Input.Event Input.Input]
  -> Maybe Server.BaseUrl
  -> IO ()
launch dir config runtime codebase inputs serverBaseUrl =
  CommandLine.main
    dir
    defaultBaseLib
    initialPath
    config
    inputs
    runtime
    codebase
    Version.gitDescribe
    serverBaseUrl

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

defaultBaseLib :: Maybe ReadRemoteNamespace
defaultBaseLib = rightMay $
  runParser VP.defaultBaseLib "version" (Text.pack Version.gitDescribe)

-- | load an existing codebase or exit.
getCodebaseOrExit :: Maybe Codebase.CodebasePath -> IO (IO (), Codebase.Codebase IO Symbol Ann)
getCodebaseOrExit mdir = do
  let cbInit = SC.init
  dir <- Codebase.getCodebaseDir mdir
  Codebase.openCodebase cbInit "main" dir >>= \case
    Left _errRequestedVersion -> do
      let
        sayNoCodebase = noCodebaseMsg <$> prettyExe <*> prettyDir <*> pure (fmap P.string mdir)
        suggestUpgrade = suggestUpgradeMessage <$> prettyExe <*> prettyDir <*> pure (fmap P.string mdir)
        prettyExe = P.text . Text.pack <$> getProgName
        prettyDir = P.string <$> canonicalizePath dir
      PT.putPrettyLn' =<< (FC.codebaseExists dir >>= \case
          False -> sayNoCodebase
          True -> suggestUpgrade)
      Exit.exitFailure
    Right x -> pure x
  where
    noCodebaseMsg :: _
    noCodebaseMsg executable prettyDir mdir =
      let secondLine =
            case mdir of
              Just dir ->
                "Run `" <> executable <> " -codebase " <> dir
                  <> " init` to create one, then try again!"
              Nothing ->
                "Run `" <> executable <> " init` to create one there,"
                  <> " then try again;"
                  <> " or `"
                  <> executable
                  <> " -codebase <dir>` to load a codebase from someplace else!"
       in P.lines
            [ "No codebase exists in " <> prettyDir <> ".",
              secondLine
            ]
    suggestUpgradeMessage exec resolvedDir specifiedDir =
      P.lines
        ( P.wrap
            <$> [ "I looked for a v2 codebase in " <> P.backticked' resolvedDir ","
                    <> "but found only a v1 codebase there.",
                  "",
                  "You can use:"
                ]
        )
        <> P.newline
        <> P.bulleted
          ( P.wrap
              <$> [ P.backticked (P.wrap $ exec <> maybe mempty ("-codebase" <>) specifiedDir <> "init")
                      <> "to create a new v2 codebase alongside it, or",
                    P.backticked (P.wrap $ exec <> "-codebase <dir>")
                      <> "to load a v2 codebase from elsewhere, or",
                    "Use the M2g or M2h release of ucm to upgrade a v1 codebase;"
                      <> "they are available at https://github.com/unisonweb/unison/releases."
                  ]
          )
