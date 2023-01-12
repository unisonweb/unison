{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import ArgParse
  ( CodebasePathOption (..),
    Command (Init, Launch, PrintVersion, Run, Transcript),
    GlobalOptions (GlobalOptions, codebasePathOption, exitOption),
    IsHeadless (Headless, WithCLI),
    RunSource (..),
    ShouldDownloadBase (..),
    ShouldExit (DoNotExit, Exit),
    ShouldForkCodebase (..),
    ShouldSaveCodebase (..),
    UsageRenderer,
    parseCLIArgs,
  )
import Compat (defaultInterruptHandler, onWindows, withInterruptHandler)
import Control.Concurrent (newEmptyMVar, runInUnboundThread, takeMVar)
import Control.Concurrent.STM
import Control.Error.Safe (rightMay)
import Control.Exception (evaluate)
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BL
import Data.Configurator.Types (Config)
import Data.Either.Validation (Validation (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import GHC.Conc (setUncaughtExceptionHandler)
import qualified GHC.Conc
import qualified Ki
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Stats (recordRtsStats)
import System.Directory (canonicalizePath, getCurrentDirectory, removeDirectoryRecursive)
import System.Environment (getProgName, lookupEnv, withArgs)
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.IO (stderr)
import System.IO.CodePage (withCP65001)
import System.IO.Error (catchIOError)
import qualified System.IO.Temp as Temp
import qualified System.Path as Path
import Text.Megaparsec (runParser)
import Text.Pretty.Simple (pHPrint)
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Editor.RemoteRepo (ReadShareRemoteNamespace)
import Unison.Codebase.Editor.UriParser (parseReadShareRemoteNamespace)
import qualified Unison.Codebase.Editor.VersionParser as VP
import Unison.Codebase.Execute (execute)
import Unison.Codebase.Init (CodebaseInitOptions (..), InitError (..), InitResult (..), SpecifiedCodebase (..))
import qualified Unison.Codebase.Init as CodebaseInit
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (..))
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Rt
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.CommandLine (plural', watchConfig)
import qualified Unison.CommandLine.Main as CommandLine
import qualified Unison.CommandLine.Types as CommandLine
import Unison.CommandLine.Welcome (CodebaseInitStatus (..))
import qualified Unison.CommandLine.Welcome as Welcome
import qualified Unison.LSP as LSP
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyTerminal as PT
import Unison.Runtime.Exception (RuntimeExn (..))
import qualified Unison.Runtime.Interface as RTI
import qualified Unison.Server.Backend as Backend
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import qualified UnliftIO
import UnliftIO.Directory (getHomeDirectory)
import qualified Version

main :: IO ()
main = withCP65001 . runInUnboundThread . Ki.scoped $ \scope -> do
  -- Replace the default exception handler with one that pretty-prints.
  setUncaughtExceptionHandler (pHPrint stderr)

  interruptHandler <- defaultInterruptHandler
  withInterruptHandler interruptHandler $ do
    void $ Ki.fork scope initHTTPClient
    progName <- getProgName
    -- hSetBuffering stdout NoBuffering -- cool
    (renderUsageInfo, globalOptions, command) <- parseCLIArgs progName (Text.unpack Version.gitDescribeWithDate)
    let GlobalOptions {codebasePathOption = mCodePathOption, exitOption = exitOption} = globalOptions
    withConfig mCodePathOption \config -> do
      currentDir <- getCurrentDirectory
      case command of
        PrintVersion ->
          Text.putStrLn $ Text.pack progName <> " version: " <> Version.gitDescribeWithDate
        Init -> do
          exitError
            ( P.lines
                [ "The Init command has been removed",
                  P.newline,
                  P.wrap "Use --codebase-create to create a codebase at a specified location and open it:",
                  P.indentN 2 (P.hiBlue "$ ucm --codebase-create myNewCodebase"),
                  "Running UCM without the --codebase-create flag: ",
                  P.indentN 2 (P.hiBlue "$ ucm"),
                  P.wrap ("will " <> P.bold "always" <> " create a codebase in your home directory if one does not already exist.")
                ]
            )
        Run (RunFromSymbol mainName) args -> do
          getCodebaseOrExit mCodePathOption SC.MigrateAutomatically \(_, _, theCodebase) -> do
            RTI.withRuntime False RTI.OneOff Version.gitDescribeWithDate \runtime -> do
              withArgs args (execute theCodebase runtime mainName) >>= \case
                Left err -> exitError err
                Right () -> pure ()
        Run (RunFromFile file mainName) args
          | not (isDotU file) -> exitError "Files must have a .u extension."
          | otherwise -> do
              e <- safeReadUtf8 file
              case e of
                Left _ -> exitError "I couldn't find that file or it is for some reason unreadable."
                Right contents -> do
                  getCodebaseOrExit mCodePathOption SC.MigrateAutomatically \(initRes, _, theCodebase) -> do
                    withRuntimes RTI.OneOff \(rt, sbrt) -> do
                      let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
                      let noOpRootNotifier _ = pure ()
                      let noOpPathNotifier _ = pure ()
                      let serverUrl = Nothing
                      let startPath = Nothing
                      launch
                        currentDir
                        config
                        rt
                        sbrt
                        theCodebase
                        [Left fileEvent, Right $ Input.ExecuteI mainName args, Right Input.QuitI]
                        serverUrl
                        startPath
                        ShouldNotDownloadBase
                        initRes
                        noOpRootNotifier
                        noOpPathNotifier
                        CommandLine.ShouldNotWatchFiles
        Run (RunFromPipe mainName) args -> do
          e <- safeReadUtf8StdIn
          case e of
            Left _ -> exitError "I had trouble reading this input."
            Right contents -> do
              getCodebaseOrExit mCodePathOption SC.MigrateAutomatically \(initRes, _, theCodebase) -> do
                withRuntimes RTI.OneOff \(rt, sbrt) -> do
                  let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
                  let noOpRootNotifier _ = pure ()
                  let noOpPathNotifier _ = pure ()
                  let serverUrl = Nothing
                  let startPath = Nothing
                  launch
                    currentDir
                    config
                    rt
                    sbrt
                    theCodebase
                    [Left fileEvent, Right $ Input.ExecuteI mainName args, Right Input.QuitI]
                    serverUrl
                    startPath
                    ShouldNotDownloadBase
                    initRes
                    noOpRootNotifier
                    noOpPathNotifier
                    CommandLine.ShouldNotWatchFiles
        Run (RunCompiled file) args ->
          BL.readFile file >>= \bs ->
            try (evaluate $ RTI.decodeStandalone bs) >>= \case
              Left (PE _cs err) -> do
                exitError . P.lines $
                  [ P.wrap . P.text $
                      "I was unable to parse this file as a compiled\
                      \ program. The parser generated the following error:",
                    "",
                    P.indentN 2 $ err
                  ]
              Right (Left err) ->
                exitError . P.lines $
                  [ P.wrap . P.text $
                      "I was unable to parse this file as a compiled\
                      \ program. The parser generated the following error:",
                    "",
                    P.indentN 2 . P.wrap $ P.string err
                  ]
              Left _ -> do
                exitError . P.wrap . P.text $
                  "I was unable to parse this file as a compiled\
                  \ program. The parser generated an unrecognized error."
              Right (Right (v, rf, w, sto))
                | not vmatch -> mismatchMsg
                | otherwise ->
                    withArgs args (RTI.runStandalone sto w) >>= \case
                      Left err -> exitError err
                      Right () -> pure ()
                where
                  vmatch = v == Version.gitDescribeWithDate
                  ws s = P.wrap (P.text s)
                  ifile
                    | 'c' : 'u' : '.' : rest <- reverse file = reverse rest
                    | otherwise = file
                  mismatchMsg =
                    PT.putPrettyLn . P.lines $
                      [ ws
                          "I can't run this compiled program since \
                          \it works with a different version of Unison \
                          \than the one you're running.",
                        "",
                        "Compiled file version",
                        P.indentN 4 $ P.text v,
                        "",
                        "Your version",
                        P.indentN 4 $ P.text Version.gitDescribeWithDate,
                        "",
                        P.wrap $
                          "The program was compiled from hash "
                            <> (P.text $ "`" <> rf <> "`.")
                            <> "If you have that hash in your codebase,"
                            <> "you can do:",
                        "",
                        P.indentN 4 $
                          ".> compile "
                            <> P.text rf
                            <> " "
                            <> P.string ifile,
                        "",
                        P.wrap
                          "to produce a new compiled program \
                          \that matches your version of Unison."
                      ]
        Transcript shouldFork shouldSaveCodebase mrtsStatsFp transcriptFiles -> do
          let action = runTranscripts renderUsageInfo shouldFork shouldSaveCodebase mCodePathOption transcriptFiles
          case mrtsStatsFp of
            Nothing -> action
            Just fp -> recordRtsStats fp action
        Launch isHeadless codebaseServerOpts downloadBase mayStartingPath shouldWatchFiles -> do
          getCodebaseOrExit mCodePathOption SC.MigrateAfterPrompt \(initRes, _, theCodebase) -> do
            withRuntimes RTI.Persistent \(runtime, sbRuntime) -> do
              rootVar <- newEmptyTMVarIO
              pathVar <- newTVarIO initialPath
              let notifyOnRootChanges :: Branch IO -> STM ()
                  notifyOnRootChanges b = do
                    isEmpty <- isEmptyTMVar rootVar
                    if isEmpty
                      then putTMVar rootVar b
                      else void $ swapTMVar rootVar b
              let notifyOnPathChanges :: Path.Absolute -> STM ()
                  notifyOnPathChanges = writeTVar pathVar
              -- Unfortunately, the windows IO manager on GHC 8.* is prone to just hanging forever
              -- when waiting for input on handles, so if we listen for LSP connections it will
              -- prevent UCM from shutting down properly. Hopefully we can re-enable LSP on
              -- Windows when we move to GHC 9.*
              -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1224
              void . Ki.fork scope $ LSP.spawnLsp theCodebase runtime (readTMVar rootVar) (readTVar pathVar)
              Server.startServer (Backend.BackendEnv {Backend.useNamesIndex = False}) codebaseServerOpts sbRuntime theCodebase $ \baseUrl -> do
                case exitOption of
                  DoNotExit -> do
                    case isHeadless of
                      Headless -> do
                        PT.putPrettyLn $
                          P.lines
                            [ "I've started the Codebase API server at",
                              P.string $ Server.urlFor Server.Api baseUrl,
                              "and the Codebase UI at",
                              P.string $ Server.urlFor Server.UI baseUrl
                            ]
                        PT.putPrettyLn $
                          P.string "Running the codebase manager headless with "
                            <> P.shown GHC.Conc.numCapabilities
                            <> " "
                            <> plural' GHC.Conc.numCapabilities "cpu" "cpus"
                            <> "."
                        mvar <- newEmptyMVar
                        takeMVar mvar
                      WithCLI -> do
                        PT.putPrettyLn $ P.string "Now starting the Unison Codebase Manager (UCM)..."
                        launch
                          currentDir
                          config
                          runtime
                          sbRuntime
                          theCodebase
                          []
                          (Just baseUrl)
                          mayStartingPath
                          downloadBase
                          initRes
                          notifyOnRootChanges
                          notifyOnPathChanges
                          shouldWatchFiles
                  Exit -> do Exit.exitSuccess
  where
    -- (runtime, sandboxed runtime)
    withRuntimes :: RTI.RuntimeHost -> ((RTI.Runtime Symbol, RTI.Runtime Symbol) -> IO a) -> IO a
    withRuntimes mode action =
      RTI.withRuntime False mode Version.gitDescribeWithDate \runtime -> do
        RTI.withRuntime True mode Version.gitDescribeWithDate \sbRuntime -> do
          action (runtime, sbRuntime)
    withConfig :: Maybe CodebasePathOption -> (Config -> IO a) -> IO a
    withConfig mCodePathOption action = do
      UnliftIO.bracket
        ( do
            let mcodepath = fmap codebasePathOptionToPath mCodePathOption
            configFilePath <- getConfigFilePath mcodepath
            catchIOError (watchConfig configFilePath) $ \_ ->
              exitError "Your .unisonConfig could not be loaded. Check that it's correct!"
        )
        (\(_config, cancel) -> cancel)
        (\(config, _cancel) -> action config)

-- | Set user agent and configure TLS on global http client.
-- Note that the authorized http client is distinct from the global http client.
initHTTPClient :: IO ()
initHTTPClient = do
  let (ucmVersion, _date) = Version.gitDescribe
  let userAgent = Text.encodeUtf8 $ "UCM/" <> ucmVersion
  let addUserAgent req = do
        pure $ req {HTTP.requestHeaders = ("User-Agent", userAgent) : HTTP.requestHeaders req}
  let managerSettings = HTTP.tlsManagerSettings {HTTP.managerModifyRequest = addUserAgent}
  manager <- HTTP.newTlsManagerWith managerSettings
  HTTP.setGlobalManager manager

prepareTranscriptDir :: ShouldForkCodebase -> Maybe CodebasePathOption -> IO FilePath
prepareTranscriptDir shouldFork mCodePathOption = do
  tmp <- Temp.getCanonicalTemporaryDirectory >>= (`Temp.createTempDirectory` "transcript")
  let cbInit = SC.init
  case shouldFork of
    UseFork -> do
      -- A forked codebase does not need to Create a codebase, because it already exists
      getCodebaseOrExit mCodePathOption SC.MigrateAutomatically $ const (pure ())
      path <- Codebase.getCodebaseDir (fmap codebasePathOptionToPath mCodePathOption)
      PT.putPrettyLn $
        P.lines
          [ P.wrap "Transcript will be run on a copy of the codebase at: ",
            "",
            P.indentN 2 (P.string path)
          ]
      Path.copyDir (CodebaseInit.codebasePath cbInit path) (CodebaseInit.codebasePath cbInit tmp)
    DontFork -> do
      PT.putPrettyLn . P.wrap $ "Transcript will be run on a new, empty codebase."
      CodebaseInit.withNewUcmCodebaseOrExit cbInit "main.transcript" tmp SC.DoLock (const $ pure ())
  pure tmp

runTranscripts' ::
  String ->
  Maybe FilePath ->
  FilePath ->
  NonEmpty MarkdownFile ->
  IO Bool
runTranscripts' progName mcodepath transcriptDir markdownFiles = do
  currentDir <- getCurrentDirectory
  configFilePath <- getConfigFilePath mcodepath
  -- We don't need to create a codebase through `getCodebaseOrExit` as we've already done so previously.
  and <$> getCodebaseOrExit (Just (DontCreateCodebaseWhenMissing transcriptDir)) SC.MigrateAutomatically \(_, codebasePath, theCodebase) -> do
    TR.withTranscriptRunner Version.gitDescribeWithDate (Just configFilePath) $ \runTranscript -> do
      for markdownFiles $ \(MarkdownFile fileName) -> do
        transcriptSrc <- readUtf8 fileName
        result <- runTranscript fileName transcriptSrc (codebasePath, theCodebase)
        let outputFile = FP.replaceExtension (currentDir FP.</> fileName) ".output.md"
        (output, succeeded) <- case result of
          Left err -> case err of
            TR.TranscriptParseError err -> do
              PT.putPrettyLn $
                P.callout
                  "❓"
                  ( P.lines
                      [ P.indentN 2 "An error occurred while parsing the following file: " <> P.string fileName,
                        "",
                        P.indentN 2 $ P.text err
                      ]
                  )
              pure (err, False)
            TR.TranscriptRunFailure err -> do
              PT.putPrettyLn $
                P.callout
                  "❓"
                  ( P.lines
                      [ P.indentN 2 "An error occurred while running the following file: " <> P.string fileName,
                        "",
                        P.indentN 2 $ P.text err,
                        P.text $
                          "Run `"
                            <> Text.pack progName
                            <> " --codebase "
                            <> Text.pack codebasePath
                            <> "` "
                            <> "to do more work with it."
                      ]
                  )
              pure (err, False)
          Right mdOut -> do
            pure (mdOut, True)
        writeUtf8 outputFile output
        putStrLn $ "💾  Wrote " <> outputFile
        pure succeeded

runTranscripts ::
  UsageRenderer ->
  ShouldForkCodebase ->
  ShouldSaveCodebase ->
  Maybe CodebasePathOption ->
  NonEmpty String ->
  IO ()
runTranscripts renderUsageInfo shouldFork shouldSaveTempCodebase mCodePathOption args = do
  markdownFiles <- case traverse (first (pure @[]) . markdownFile) args of
    Failure invalidArgs -> do
      PT.putPrettyLn $
        P.callout
          "❓"
          ( P.lines
              [ P.indentN 2 "Transcripts must have an .md or .markdown extension.",
                "",
                P.bulleted $ fmap (P.bold . P.string . (<> "\n")) invalidArgs
              ]
          )
      putStrLn (renderUsageInfo $ Just "transcript")
      Exit.exitWith (Exit.ExitFailure 1)
    Success markdownFiles -> pure markdownFiles
  progName <- getProgName
  transcriptDir <- prepareTranscriptDir shouldFork mCodePathOption
  completed <-
    runTranscripts' progName (Just transcriptDir) transcriptDir markdownFiles
  case shouldSaveTempCodebase of
    DontSaveCodebase -> removeDirectoryRecursive transcriptDir
    SaveCodebase ->
      when completed $ do
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
                      <> P.backticked (P.string progName <> " --codebase " <> P.string transcriptDir)
                      <> "to do more work with it."
                ]
            )
  when (not completed) $ Exit.exitWith (Exit.ExitFailure 1)

initialPath :: Path.Absolute
initialPath = Path.absoluteEmpty

launch ::
  FilePath ->
  Config ->
  Rt.Runtime Symbol ->
  Rt.Runtime Symbol ->
  Codebase.Codebase IO Symbol Ann ->
  [Either Input.Event Input.Input] ->
  Maybe Server.BaseUrl ->
  Maybe Path.Absolute ->
  ShouldDownloadBase ->
  InitResult ->
  (Branch IO -> STM ()) ->
  (Path.Absolute -> STM ()) ->
  CommandLine.ShouldWatchFiles ->
  IO ()
launch dir config runtime sbRuntime codebase inputs serverBaseUrl mayStartingPath shouldDownloadBase initResult notifyRootChange notifyPathChange shouldWatchFiles =
  let downloadBase = case defaultBaseLib of
        Just remoteNS | shouldDownloadBase == ShouldDownloadBase -> Welcome.DownloadBase remoteNS
        _ -> Welcome.DontDownloadBase
      isNewCodebase = case initResult of
        CreatedCodebase {} -> NewlyCreatedCodebase
        _ -> PreviouslyCreatedCodebase

      (ucmVersion, _date) = Version.gitDescribe
      welcome = Welcome.welcome isNewCodebase downloadBase dir ucmVersion shouldWatchFiles
   in CommandLine.main
        dir
        welcome
        (fromMaybe initialPath mayStartingPath)
        config
        inputs
        runtime
        sbRuntime
        codebase
        serverBaseUrl
        ucmVersion
        notifyRootChange
        notifyPathChange
        shouldWatchFiles

newtype MarkdownFile = MarkdownFile FilePath

markdownFile :: FilePath -> Validation FilePath MarkdownFile
markdownFile md = case FP.takeExtension md of
  ".md" -> Success $ MarkdownFile md
  ".markdown" -> Success $ MarkdownFile md
  _ -> Failure md

isDotU :: String -> Bool
isDotU file = FP.takeExtension file == ".u"

-- so we can do `ucm --help`, `ucm -help` or `ucm help` (I hate
-- having to remember which one is supported)
isFlag :: String -> String -> Bool
isFlag f arg = arg == f || arg == "-" ++ f || arg == "--" ++ f

getConfigFilePath :: Maybe FilePath -> IO FilePath
getConfigFilePath mcodepath = (FP.</> ".unisonConfig") <$> Codebase.getCodebaseDir mcodepath

defaultBaseLib :: Maybe ReadShareRemoteNamespace
defaultBaseLib =
  let mayBaseSharePath =
        $( do
             mayPath <- TH.runIO (lookupEnv "UNISON_BASE_PATH")
             TH.lift mayPath
         )
   in mayBaseSharePath & \case
        Just s -> eitherToMaybe $ parseReadShareRemoteNamespace "UNISON_BASE_PATH" s
        Nothing -> rightMay $ runParser VP.defaultBaseLib "version" gitRef
  where
    (gitRef, _date) = Version.gitDescribe

getCodebaseOrExit :: Maybe CodebasePathOption -> SC.MigrationStrategy -> ((InitResult, CodebasePath, Codebase IO Symbol Ann) -> IO r) -> IO r
getCodebaseOrExit codebasePathOption migrationStrategy action = do
  initOptions <- argsToCodebaseInitOptions codebasePathOption
  result <- CodebaseInit.withOpenOrCreateCodebase SC.init "main" initOptions SC.DoLock migrationStrategy \case
    cbInit@(CreatedCodebase, dir, _) -> do
      pDir <- prettyDir dir
      PT.putPrettyLn' ""
      PT.putPrettyLn' . P.indentN 2 . P.wrap $ "I created a new codebase for you at" <> P.blue pDir
      action cbInit
    cbInit@(OpenedCodebase, _, _) ->
      action cbInit

  case result of
    Right r -> pure r
    Left (dir, err) ->
      let message = do
            pDir <- prettyDir dir
            executableName <- P.text . Text.pack <$> getProgName

            case err of
              InitErrorOpen err ->
                case err of
                  OpenCodebaseFileLockFailed ->
                    pure
                      ( P.lines
                          [ "Failed to obtain a file lock on the codebase. ",
                            "Perhaps you are running multiple ucm processes against the same codebase."
                          ]
                      )
                  OpenCodebaseDoesntExist ->
                    pure
                      ( P.lines
                          [ "No codebase exists in " <> pDir <> ".",
                            "Run `" <> executableName <> " --codebase-create " <> P.string dir <> " to create one, then try again!"
                          ]
                      )
                  (OpenCodebaseUnknownSchemaVersion _) ->
                    pure
                      ( P.lines
                          [ "I can't read the codebase in " <> pDir <> " because it was constructed using a newer version of unison.",
                            "Please upgrade your version of UCM."
                          ]
                      )
                  (OpenCodebaseRequiresMigration _ _) ->
                    pure
                      ( P.lines
                          [ "The codebase is from an older version of UCM, it needs to be migrated before it can be used.",
                            "You can migrate it by opening it in UCM, e.g. ucm -c mycodebase"
                          ]
                      )
              FoundV1Codebase ->
                pure
                  ( P.lines
                      [ "Found a v1 codebase at " <> pDir <> ".",
                        "v1 codebases are no longer supported in this version of the UCM.",
                        "Please download version M2g of the UCM to upgrade."
                      ]
                  )
              CouldntCreateCodebase errMessage ->
                pure errMessage
       in do
            msg <- message
            PT.putPrettyLn' msg
            Exit.exitFailure
  where
    prettyDir dir = P.string <$> canonicalizePath dir

exitError :: P.Pretty P.ColorText -> IO a
exitError msg = do
  PT.putPrettyLn $ P.callout "⚠️" msg
  Exit.exitFailure

argsToCodebaseInitOptions :: Maybe CodebasePathOption -> IO CodebaseInit.CodebaseInitOptions
argsToCodebaseInitOptions pathOption =
  case pathOption of
    Just (CreateCodebaseWhenMissing path) -> pure $ Specified (CreateWhenMissing path)
    Just (DontCreateCodebaseWhenMissing path) -> pure $ Specified (DontCreateWhenMissing path)
    Nothing -> do Home <$> getHomeDirectory

codebasePathOptionToPath :: CodebasePathOption -> FilePath
codebasePathOptionToPath codebasePathOption =
  case codebasePathOption of
    CreateCodebaseWhenMissing p -> p
    DontCreateCodebaseWhenMissing p -> p
