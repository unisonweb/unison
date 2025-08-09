{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Main
  ( main,
  )
where

import ArgParse
  ( CodebasePathOption (..),
    Command (..),
    GlobalOptions (..),
    IsHeadless (Headless, WithCLI),
    RunSource (..),
    ShouldExit (DoNotExit, Exit),
    ShouldForkCodebase (..),
    ShouldSaveCodebase (..),
    TranscriptCodebaseSetup (..),
    UsageRenderer,
    parseCLIArgs,
  )
import Compat (defaultInterruptHandler, withInterruptHandler)
import Control.Concurrent (newEmptyMVar, runInUnboundThread, takeMVar)
import Control.Exception (displayException, evaluate, fromException)
import Data.Bitraversable (bitraverse)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Either.Validation (Validation (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import GHC.Conc (setUncaughtExceptionHandler)
import GHC.Conc qualified
import Ki qualified
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Stats (recordRtsStats)
import System.Directory
  ( canonicalizePath,
    getCurrentDirectory,
    removeDirectoryRecursive,
  )
import System.Environment (getProgName, withArgs)
import System.Exit (ExitCode (..))
import System.Exit qualified as Exit
import System.Exit qualified as System
import System.FilePath
  ( replaceExtension,
    takeExtension,
    (</>),
  )
import System.IO (stderr)
import System.IO.CodePage (withCP65001)
import System.IO.Temp qualified as Temp
import System.Path qualified as Path
import Text.Megaparsec qualified as MP
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase (Codebase, CodebasePath)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Execute (execute)
import Unison.Codebase.Init (CodebaseInitOptions (..), InitError (..), InitResult (..), SpecifiedCodebase (..))
import Unison.Codebase.Init qualified as CodebaseInit
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime qualified as Rt
import Unison.Codebase.SqliteCodebase qualified as SC
import Unison.Codebase.Transcript.Parser qualified as Transcript
import Unison.Codebase.Transcript.Runner qualified as Transcript
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.CommandLine.Helpers (plural')
import Unison.CommandLine.Main qualified as CommandLine
import Unison.CommandLine.Types qualified as CommandLine
import Unison.CommandLine.Welcome (CodebaseInitStatus (..))
import Unison.CommandLine.Welcome qualified as Welcome
import Unison.Core.Project (ProjectAndBranch (..), ProjectName (..))
import Unison.LSP qualified as LSP
import Unison.LSP.Util.Signal qualified as Signal
import Unison.MCP qualified as MCP
import Unison.MCP.Server qualified as MCP
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyTerminal qualified as PT
import Unison.Project (defaultBranchName)
import Unison.Runtime.Exception (RuntimeExn (..))
import Unison.Runtime.Interface qualified as RTI
import Unison.Server.Backend qualified as Backend
import Unison.Server.CodebaseServer qualified as Server
import Unison.Symbol (Symbol)
import Unison.Util.Pretty qualified as P
import Unison.Version (Version)
import Unison.Version qualified as Version
import UnliftIO qualified as UnliftIO
import UnliftIO.Directory (getHomeDirectory)
import UnliftIO.Directory qualified as Directory

type Runtimes = (RTI.Runtime Symbol, RTI.Runtime Symbol)

main :: Version -> IO ()
main version = do
  -- Replace the default exception handler with one complains loudly, because we shouldn't have any uncaught exceptions.
  -- Sometimes `show` and `displayException` are different strings; in this case, we want to show them both, so this
  -- issue is easier to debug.
  --
  -- We've made one exception for `ExitSuccess`, because we've discovered the `lsp` library unhelpfully throws it from a
  -- background thread as part of the default "exit notification handler", with no way to modify the behavior.
  setUncaughtExceptionHandler \exception -> do
    when (not (isExitSuccess exception)) do
      let shown = tShow exception
      let displayed = Text.pack (displayException exception)
      let indented = Text.unlines . map ("  " <>) . Text.lines

      Text.hPutStrLn stderr . Text.unlines . fold $
        [ [ "Uh oh, an unexpected exception brought the process down! That should never happen. Please file a bug report.",
            "",
            "Here's a stringy rendering of the exception:",
            "",
            indented shown
          ],
          if shown /= displayed
            then
              [ "And here's a different one, in case it's easier to understand:",
                "",
                indented displayed
              ]
            else []
        ]
  -- This makes our error messages more safe w/r to concurrency. Without it sometimes the
  -- error messaging from the UCM server and LSP server (both running in separate threads) get
  -- interleaved.
  -- https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-IO-Handle.html#v:hPutStr
  UnliftIO.hSetBuffering UnliftIO.stderr UnliftIO.LineBuffering

  withCP65001 . runInUnboundThread . Ki.scoped $ \scope -> do
    interruptHandler <- defaultInterruptHandler
    withInterruptHandler interruptHandler $ do
      void $ Ki.fork scope (initHTTPClient version)
      progName <- getProgName
      -- hSetBuffering stdout NoBuffering -- cool
      (renderUsageInfo, globalOptions, command) <- parseCLIArgs progName (Text.unpack (Version.gitDescribeWithDate version))
      let GlobalOptions {codebasePathOption = mCodePathOption, exitOption, lspFormattingConfig} = globalOptions
      currentDir <- getCurrentDirectory
      case command of
        PrintVersion ->
          Text.putStrLn $ Text.pack progName <> " version: " <> Version.gitDescribeWithDate version
        MCPServer -> do
          getCodebaseOrExit mCodePathOption SC.DontLock (SC.MigrateAfterPrompt SC.Backup SC.Vacuum) \(_initRes, _, theCodebase) -> do
            withRuntimes RTI.Persistent \(runtime, sbRuntime) -> do
              MCP.runOnStdIO theCodebase runtime sbRuntime currentDir (Version.gitDescribeWithDate version)
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
          getCodebaseOrExit mCodePathOption SC.DoLock (SC.MigrateAutomatically SC.Backup SC.Vacuum) \(_, _, theCodebase) -> do
            RTI.withRuntime False RTI.OneOff (Version.gitDescribeWithDate version) \runtime -> do
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
                  getCodebaseOrExit mCodePathOption SC.DoLock (SC.MigrateAutomatically SC.Backup SC.Vacuum) \(initRes, _, theCodebase) -> do
                    withRuntimes RTI.OneOff \(rt, sbrt) -> do
                      let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
                      let noOpCheckForChanges _ = pure ()
                      let serverUrl = Nothing
                      startProjectPath <- Codebase.runTransaction theCodebase Codebase.expectCurrentProjectPath
                      launch
                        version
                        currentDir
                        rt
                        sbrt
                        theCodebase
                        [Left fileEvent, Right $ Input.ExecuteI mainName args, Right Input.QuitI]
                        serverUrl
                        (PP.toIds startProjectPath)
                        initRes
                        noOpCheckForChanges
                        CommandLine.ShouldNotWatchFiles
        Run (RunFromPipe mainName) args -> do
          e <- safeReadUtf8StdIn
          case e of
            Left _ -> exitError "I had trouble reading this input."
            Right contents -> do
              getCodebaseOrExit mCodePathOption SC.DoLock (SC.MigrateAutomatically SC.Backup SC.Vacuum) \(initRes, _, theCodebase) -> do
                withRuntimes RTI.OneOff \(rt, sbrt) -> do
                  let fileEvent = Input.UnisonFileChanged (Text.pack "<standard input>") contents
                  let noOpCheckForChanges _ = pure ()
                  let serverUrl = Nothing
                  startProjectPath <- Codebase.runTransaction theCodebase Codebase.expectCurrentProjectPath
                  launch
                    version
                    currentDir
                    rt
                    sbrt
                    theCodebase
                    [Left fileEvent, Right $ Input.ExecuteI mainName args, Right Input.QuitI]
                    serverUrl
                    (PP.toIds startProjectPath)
                    initRes
                    noOpCheckForChanges
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
              Right (Right (v, rf, combIx, sto))
                | not vmatch -> mismatchMsg
                | otherwise ->
                    withArgs args (RTI.runStandalone False sto combIx) >>= \case
                      Left err -> exitError err
                      Right () -> pure ()
                where
                  vmatch = v == Version.gitDescribeWithDate version
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
                        P.indentN 4 $ P.text $ Version.gitDescribeWithDate version,
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
        Transcript codebaseSetup mrtsStatsFp transcriptFiles -> do
          let action = runTranscripts version Verbosity.Verbose renderUsageInfo codebaseSetup mCodePathOption transcriptFiles
          case mrtsStatsFp of
            Nothing -> action
            Just fp -> recordRtsStats fp action
        Launch isHeadless codebaseServerOpts mayStartingProject shouldWatchFiles -> do
          getCodebaseOrExit mCodePathOption SC.DoLock (SC.MigrateAfterPrompt SC.Backup SC.Vacuum) \(initRes, _, theCodebase) -> do
            withRuntimes RTI.Persistent \(runtime, sbRuntime) -> do
              startingProjectPath <- do
                -- If the user didn't provide a starting path on the command line, put them in the most recent
                -- path they cd'd to
                case mayStartingProject of
                  Just startingProject -> do
                    Codebase.runTransaction theCodebase (ProjectUtils.getProjectAndBranchByNames startingProject) >>= \case
                      Nothing -> do
                        PT.putPrettyLn $
                          P.callout
                            "❓"
                            ( P.lines
                                [ P.indentN 2 "I couldn't find the project branch: " <> P.text (into @Text startingProject)
                                ]
                            )
                        System.exitFailure
                      Just pab -> do
                        pure $ PP.fromProjectAndBranch pab Path.Root
                  Nothing -> do
                    Codebase.runTransaction theCodebase Codebase.expectCurrentProjectPath
              currentPP <- Codebase.runTransaction theCodebase do
                PP.toIds <$> Codebase.expectCurrentProjectPath
              changeSignal <- Signal.newSignalIO (Just currentPP)
              let lspCheckForChanges = Signal.writeSignalIO changeSignal
              -- Unfortunately, the windows IO manager on GHC 8.* is prone to just hanging forever
              -- when waiting for input on handles, so if we listen for LSP connections it will
              -- prevent UCM from shutting down properly. Hopefully we can re-enable LSP on
              -- Windows when we move to GHC 9.*
              -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1224
              void . Ki.fork scope $ LSP.spawnLsp lspFormattingConfig theCodebase runtime changeSignal
              let isTest = False
              mcpServerConfig <-
                MCP.initServer theCodebase runtime sbRuntime (pure currentDir) $ Version.gitDescribeWithDate version
              Server.startServer
                isTest
                Backend.BackendEnv {Backend.useNamesIndex = False}
                codebaseServerOpts
                sbRuntime
                theCodebase
                (MCP.mcpServer mcpServerConfig)
                \mayBaseUrl -> case exitOption of
                  DoNotExit -> case isHeadless of
                    Headless -> whenJust mayBaseUrl \baseUrl -> do
                      PT.putPrettyLn $
                        P.lines
                          [ "I've started the Codebase API server at",
                            P.text $ Server.urlFor Server.Api baseUrl,
                            "and the Codebase UI at",
                            P.text $
                              Server.urlFor
                                ( Server.ProjectBranchUI
                                    (ProjectAndBranch (UnsafeProjectName "scratch") defaultBranchName)
                                    Path.Root
                                    Nothing
                                )
                                baseUrl
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
                        version
                        currentDir
                        runtime
                        sbRuntime
                        theCodebase
                        []
                        mayBaseUrl
                        (PP.toIds startingProjectPath)
                        initRes
                        lspCheckForChanges
                        shouldWatchFiles
                  Exit -> Exit.exitSuccess
  where
    -- (runtime, sandboxed runtime)
    withRuntimes :: RTI.RuntimeHost -> (Runtimes -> IO a) -> IO a
    withRuntimes mode action =
      RTI.withRuntime False mode (Version.gitDescribeWithDate version) \runtime -> do
        RTI.withRuntime True mode (Version.gitDescribeWithDate version) \sbRuntime ->
          action (runtime, sbRuntime)

isExitSuccess :: SomeException -> Bool
isExitSuccess =
  (== Just ExitSuccess) . fromException

-- | Set user agent and configure TLS on global http client.
-- Note that the authorized http client is distinct from the global http client.
initHTTPClient :: Version -> IO ()
initHTTPClient version = do
  let (ucmVersion, _date) = Version.gitDescribe version
  let userAgent = Text.encodeUtf8 $ "UCM/" <> ucmVersion
  let addUserAgent req = do
        pure $ req {HTTP.requestHeaders = ("User-Agent", userAgent) : HTTP.requestHeaders req}
  let managerSettings = HTTP.tlsManagerSettings {HTTP.managerModifyRequest = addUserAgent}
  manager <- HTTP.newTlsManagerWith managerSettings
  HTTP.setGlobalManager manager

-- | Prep the codebase for transcripts, then pass the directory to the action.
-- After the action the codebase will be deleted/copied/saved as indicated.
withTranscriptDir :: Verbosity.Verbosity -> String -> TranscriptCodebaseSetup -> Maybe CodebasePathOption -> (FilePath -> IO r) -> IO (Maybe r)
withTranscriptDir verbosity progName codebaseSetup mCodePathOption action = do
  UnliftIO.bracket setup cleanup (\(mayDir, _cleanup) -> for mayDir action)
  where
    setup :: IO (Maybe FilePath, IO ())
    setup = do
      case codebaseSetup of
        InPlace -> do
          -- Create the codebase/migrate it according to codebase path option
          getCodebaseOrExit mCodePathOption SC.DoLock (SC.MigrateAfterPrompt SC.Backup SC.Vacuum) $ const (pure ())
          path <- Codebase.getCodebaseDir (fmap codebasePathOptionToPath mCodePathOption)
          unless (Verbosity.isSilent verbosity) . PT.putPrettyLn $
            P.lines
              [ P.wrap "Transcript will be run in-place on the codebase at: ",
                "",
                P.indentN 2 (P.string path)
              ]
          let after =
                do
                  PT.putPrettyLn
                  $ P.callout
                    "🌸"
                    ( P.lines
                        [ "I've finished running the transcript(s) on the provided codebase:",
                          "",
                          P.indentN 2 (P.string path)
                        ]
                    )
          pure (Just path, after)
        UseTempCodebase shouldFork shouldSaveCodebase -> do
          (tmp, cleanup) <- case shouldSaveCodebase of
            SaveCodebase (Just path) -> do
              let after = do
                    PT.putPrettyLn $
                      P.callout
                        "🌸"
                        ( P.lines
                            [ "I've finished running the transcript(s) in this codebase:",
                              "",
                              P.indentN 2 (P.string path),
                              "",
                              P.wrap $
                                "You can run"
                                  <> P.backticked (P.string progName <> " --codebase " <> P.string path)
                                  <> "to do more work with it."
                            ]
                        )
              pure (path, after)
            _ -> do
              path <- Temp.getCanonicalTemporaryDirectory >>= (`Temp.createTempDirectory` "transcript")
              let cleanup = removeDirectoryRecursive path
              pure (path, cleanup)
          let cbInit = SC.init
          case shouldFork of
            UseFork -> do
              -- A forked codebase does not need to Create a codebase, because it already exists
              getCodebaseOrExit mCodePathOption SC.DoLock (SC.MigrateAutomatically SC.Backup SC.Vacuum) $ const (pure ())
              path <- Codebase.getCodebaseDir (fmap codebasePathOptionToPath mCodePathOption)
              (absPath, absTmp) <- bitraverse Directory.canonicalizePath Directory.canonicalizePath (path, tmp)
              if (absPath == absTmp)
                then do
                  unless (Verbosity.isSilent verbosity) . PT.putPrettyLn $
                    P.hang "⚠️" $
                      P.lines
                        [ "I noticed that you're forking from and saving to the same path at: " <> P.string path,
                          "",
                          "I'll skip running the transcript for now in case this was a mistake.",
                          "If this is what you meant to do, use `transcript.in-place` instead."
                        ]
                  pure (Nothing, pure ())
                else do
                  unless (Verbosity.isSilent verbosity) . PT.putPrettyLn $
                    P.lines
                      [ P.wrap "Transcript will be run on a copy of the codebase at: ",
                        "",
                        P.indentN 2 (P.string path)
                      ]
                  Path.copyDir (CodebaseInit.codebasePath cbInit path) (CodebaseInit.codebasePath cbInit tmp)
                  pure (Just tmp, cleanup)
            DontFork -> do
              PT.putPrettyLn . P.wrap $ "Transcript will be run on a new, empty codebase."
              CodebaseInit.withNewUcmCodebaseOrExit cbInit verbosity "main.transcript" tmp SC.DoLock (const $ pure ())
              pure (Just tmp, cleanup)
    cleanup :: (Maybe FilePath, IO ()) -> IO ()
    cleanup (_transcriptDir, cleanupAction) = do
      cleanupAction

runTranscripts' ::
  Version ->
  String ->
  FilePath ->
  NonEmpty MarkdownFile ->
  IO Bool
runTranscripts' version progName transcriptDir markdownFiles = do
  currentDir <- getCurrentDirectory
  -- We don't need to create a codebase through `getCodebaseOrExit` as we've already done so previously.
  and
    <$> getCodebaseOrExit
      (Just (DontCreateCodebaseWhenMissing transcriptDir))
      SC.DoLock
      (SC.MigrateAutomatically SC.Backup SC.Vacuum)
      \(_, codebasePath, theCodebase) -> do
        let isTest = False
        Transcript.withRunner
          isTest
          Verbosity.Verbose
          (Version.gitDescribeWithDate version)
          \runTranscript -> do
            for markdownFiles $ \(MarkdownFile fileName) -> do
              transcriptSrc <- BS.readFile fileName
              result <- runTranscript fileName transcriptSrc theCodebase
              let outputFile = replaceExtension (currentDir </> fileName) ".output.md"
              output <-
                either
                  ( uncurry ($>) . first (PT.putPrettyLn . P.callout "❓" . P.lines) . \case
                      Transcript.PortBindingFailure ->
                        ( [P.indentN 2 $ "The codebase server failed to start because the chosen port was already in use."],
                          "Port binding failure"
                        )
                      Transcript.ParseError err ->
                        let msg = MP.errorBundlePretty err
                         in ( [ P.indentN 2 $
                                  "An error occurred while parsing the following file: " <> P.string fileName,
                                "",
                                P.indentN 2 $ P.string msg
                              ],
                              Text.pack msg
                            )
                      Transcript.RunFailure msg ->
                        ( [ P.indentN 2 $ "An error occurred while running the following file: " <> P.string fileName,
                            "",
                            P.indentN 2 (P.text $ Transcript.format msg),
                            P.string $
                              "Run `"
                                <> progName
                                <> " --codebase "
                                <> codebasePath
                                <> "` "
                                <> "to do more work with it."
                          ],
                          Transcript.format msg
                        )
                  )
                  (pure . Transcript.format)
                  result
              writeUtf8 outputFile output
              putStrLn $ "💾  Wrote " <> outputFile
              pure $ isRight result

runTranscripts ::
  Version ->
  Verbosity.Verbosity ->
  UsageRenderer ->
  TranscriptCodebaseSetup ->
  Maybe CodebasePathOption ->
  NonEmpty String ->
  IO ()
runTranscripts version verbosity renderUsageInfo codebaseSetup mCodePathOption args = do
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
  completed <-
    fromMaybe False <$> withTranscriptDir verbosity progName codebaseSetup mCodePathOption \transcriptDir -> do
      runTranscripts' version progName transcriptDir markdownFiles
  when (not completed) $ Exit.exitWith (Exit.ExitFailure 1)

launch ::
  Version ->
  FilePath ->
  Rt.Runtime Symbol ->
  Rt.Runtime Symbol ->
  Codebase.Codebase IO Symbol Ann ->
  [Either Input.Event Input.Input] ->
  Maybe Server.BaseUrl ->
  PP.ProjectPathIds ->
  InitResult ->
  (PP.ProjectPathIds -> IO ()) ->
  CommandLine.ShouldWatchFiles ->
  IO ()
launch version dir runtime sbRuntime codebase inputs serverBaseUrl startingPath initResult lspCheckForChanges shouldWatchFiles = do
  showWelcomeHint <- Codebase.runTransaction codebase Queries.doProjectsExist
  let isNewCodebase = case initResult of
        CreatedCodebase -> NewlyCreatedCodebase
        OpenedCodebase -> PreviouslyCreatedCodebase
      (ucmVersion, _date) = Version.gitDescribe version
      welcome = Welcome.welcome isNewCodebase ucmVersion showWelcomeHint
   in CommandLine.main
        dir
        welcome
        startingPath
        inputs
        runtime
        sbRuntime
        codebase
        serverBaseUrl
        ucmVersion
        lspCheckForChanges
        shouldWatchFiles

newtype MarkdownFile = MarkdownFile FilePath

markdownFile :: FilePath -> Validation FilePath MarkdownFile
markdownFile md = case takeExtension md of
  ".md" -> Success $ MarkdownFile md
  ".markdown" -> Success $ MarkdownFile md
  _ -> Failure md

isDotU :: String -> Bool
isDotU file = takeExtension file == ".u"

getCodebaseOrExit :: Maybe CodebasePathOption -> SC.CodebaseLockOption -> SC.MigrationStrategy -> ((InitResult, CodebasePath, Codebase IO Symbol Ann) -> IO r) -> IO r
getCodebaseOrExit codebasePathOption locking migrationStrategy action = do
  initOptions <- argsToCodebaseInitOptions codebasePathOption
  let cbInit = SC.init
  result <- CodebaseInit.withOpenOrCreateCodebase cbInit "main" initOptions locking migrationStrategy \case
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
