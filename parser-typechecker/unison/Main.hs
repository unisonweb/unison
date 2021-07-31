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
import Data.ByteString.Char8 (unpack)
import Data.Configurator.Types (Config)
import qualified Data.Text as Text
import qualified GHC.Conc
import qualified Network.URI.Encode as URI
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
import qualified Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.CommandLine (plural', watchConfig)
import qualified Unison.CommandLine.Main as CommandLine
import Unison.Parser (Ann)
import Unison.Prelude
import qualified Unison.Codebase.Runtime as Rt
import qualified Unison.PrettyTerminal as PT
import qualified Unison.Runtime.Interface as RTI
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import qualified Version
import qualified Unison.Codebase.Conversion.Upgrade12 as Upgrade12
import Compat
import ArgParse

cbInitFor :: CodebaseFormat -> Codebase.Init IO Symbol Ann
cbInitFor = \case V1 -> FC.init; V2 -> SC.init

main :: IO ()
main = do
  progName <- getProgName
  -- hSetBuffering stdout NoBuffering -- cool

  void installSignalHandlers
  (renderUsageInfo, globalOptions, command) <- parseCLIArgs progName Version.gitDescribe
  let GlobalOptions{codebasePath=mcodepath, codebaseFormat=cbFormat} = globalOptions
  let cbInit = cbInitFor cbFormat
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
      (closeCodebase, theCodebase) <- getCodebaseOrExit cbFormat mcodepath
      runtime <- RTI.startRuntime
      execute theCodebase runtime mainName
      closeCodebase
     Run (RunFromFile file mainName)
       | not (isDotU file) -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
       | otherwise -> do
            e <- safeReadUtf8 file
            case e of
              Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
              Right contents -> do
                (closeCodebase, theCodebase) <- getCodebaseOrExit cbFormat mcodepath
                rt <- RTI.startRuntime
                let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
                launch currentDir config rt theCodebase [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
                closeCodebase
     Run (RunFromPipe mainName) -> do
      e <- safeReadUtf8StdIn
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I had trouble reading this input."
        Right contents -> do
          (closeCodebase, theCodebase) <- getCodebaseOrExit cbFormat mcodepath
          rt <- RTI.startRuntime
          let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
          launch
            currentDir config rt theCodebase
            [Left fileEvent, Right $ Input.ExecuteI mainName, Right Input.QuitI]
          closeCodebase
     Transcript shouldFork shouldSaveCodebase transcriptFiles ->
       runTranscripts renderUsageInfo cbFormat shouldFork shouldSaveCodebase mcodepath transcriptFiles
     UpgradeCodebase -> upgradeCodebase mcodepath
     Launch isHeadless -> do
       (closeCodebase, theCodebase) <- getCodebaseOrExit cbFormat mcodepath
       runtime <- RTI.startRuntime
       Server.start runtime theCodebase $ \token port -> do
         let url =
              "http://127.0.0.1:" <> show port <> "/" <> URI.encode (unpack token)
         PT.putPrettyLn $ P.lines
           ["The Unison Codebase UI is running at", P.string $ url <> "/ui"]
         case isHeadless of
             Headless -> do
                 PT.putPrettyLn $ P.lines
                    ["I've started the codebase API server at" , P.string $ url <> "/api"]
                 PT.putPrettyLn $ P.string "Running the codebase manager headless with "
                     <> P.shown GHC.Conc.numCapabilities
                     <> " "
                     <> plural' GHC.Conc.numCapabilities "cpu" "cpus"
                     <> "."
                 mvar <- newEmptyMVar
                 takeMVar mvar
             WithCLI -> do
                 PT.putPrettyLn $ P.string "Now starting the Unison Codebase Manager..."
                 launch currentDir config runtime theCodebase []
                 closeCodebase

upgradeCodebase :: Maybe Codebase.CodebasePath -> IO ()
upgradeCodebase mcodepath =
  Codebase.getCodebaseDir mcodepath >>= \root -> do
    PT.putPrettyLn . P.wrap $
      "I'm upgrading the codebase in " <> P.backticked' (P.string root) "," <> "but it will"
      <> "take a while, and may even run out of memory. If you have"
      <> "trouble, contact us on #alphatesting and we'll try to help."
    Upgrade12.upgradeCodebase root
    PT.putPrettyLn . P.wrap
      $ P.newline
      <> "Try it out and once you're satisfied, you can safely(?) delete the old version from"
      <> P.newline
      <> P.indentN 2 (P.string $ Codebase.codebasePath (FC.init @IO) root)
      <> P.newline
      <> "but there's no rush.  You can access the old codebase again by passing the"
      <> P.backticked "--old-codebase" <> "flag at startup."

prepareTranscriptDir :: CodebaseFormat -> ShouldForkCodebase -> Maybe FilePath -> IO FilePath
prepareTranscriptDir cbFormat shouldFork mcodepath = do
  tmp <- Temp.getCanonicalTemporaryDirectory >>= (`Temp.createTempDirectory` "transcript")
  let cbInit = cbInitFor cbFormat
  case shouldFork of
    UseFork -> do
      getCodebaseOrExit cbFormat mcodepath
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
  :: CodebaseFormat
  -> Maybe FilePath
  -> FilePath
  -> [String]
  -> IO Bool
runTranscripts' codebaseFormat mcodepath transcriptDir args = do
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
              (closeCodebase, theCodebase) <- getCodebaseOrExit codebaseFormat $ Just transcriptDir
              mdOut <- TR.run transcriptDir configFilePath stanzas theCodebase
              closeCodebase
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
  :: UsageRenderer
  -> CodebaseFormat
  -> ShouldForkCodebase
  -> ShouldSaveCodebase
  -> Maybe FilePath
  -> [String]
  -> IO ()
runTranscripts renderUsageInfo cbFormat shouldFork shouldSaveTempCodebase mcodepath args = do
  progName <- getProgName
  transcriptDir <- prepareTranscriptDir cbFormat shouldFork mcodepath
  completed <-
    runTranscripts' cbFormat (Just transcriptDir) transcriptDir args
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
  -> IO ()
launch dir config rt code inputs =
  CommandLine.main dir defaultBaseLib initialPath config inputs rt code Version.gitDescribe

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
getCodebaseOrExit :: CodebaseFormat -> Maybe Codebase.CodebasePath -> IO (IO (), Codebase.Codebase IO Symbol Ann)
getCodebaseOrExit cbFormat mdir = do
  let cbInit = cbInitFor cbFormat
  dir <- Codebase.getCodebaseDir mdir
  Codebase.openCodebase cbInit "main" dir >>= \case
    Left _errRequestedVersion -> do
      let
        sayNoCodebase = noCodebaseMsg <$> prettyExe <*> prettyDir <*> pure (fmap P.string mdir)
        suggestUpgrade = suggestUpgradeMessage <$> prettyExe <*> prettyDir <*> pure (fmap P.string mdir)
        prettyExe = P.text . Text.pack <$> getProgName
        prettyDir = P.string <$> canonicalizePath dir
      PT.putPrettyLn' =<< case cbFormat of
        V1 -> sayNoCodebase
        V2 -> FC.openCodebase dir >>= \case
          Left {} -> sayNoCodebase
          Right {} -> suggestUpgrade
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
            <$> [ "I looked for a" <> prettyFmt V2 <> " codebase in " <> P.backticked' resolvedDir ","
                    <> "but found only a"
                    <> prettyFmt V1
                    <> "codebase there.",
                  "",
                  "You can use:"
                ]
        )
        <> P.newline
        <> P.bulleted
          ( P.wrap
              <$> [ P.backticked (P.wrap $ exec <> maybe mempty ("-codebase" <>) specifiedDir <> "upgrade-codebase")
                      <> "to update it to"
                      <> P.group (prettyFmt V2 <> ","),
                    P.backticked (P.wrap $ exec <> maybe mempty ("-codebase" <>) specifiedDir <> "init")
                      <> "to create a new"
                      <> prettyFmt V2
                      <> "codebase alongside it, or",
                    P.backticked (P.wrap $ exec <> "-codebase <dir>")
                      <> "to load a"
                      <> prettyFmt V2
                      <> "codebase from elsewhere."
                  ]
          )



    prettyFmt :: IsString s => CodebaseFormat -> P.Pretty s
    prettyFmt = \case V1 -> "v1"; V2 -> "v2"
