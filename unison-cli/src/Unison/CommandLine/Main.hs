module Unison.CommandLine.Main
  ( main,
  )
where

import Compat (withInterruptHandler)
import Control.Exception (displayException, mask)
import Control.Lens ((?~))
import Control.Lens.Lens
import Crypto.Random qualified as Random
import Data.IORef
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.IO qualified as IO
import Ki qualified
import System.Console.Haskeline (Settings (autoAddHistory))
import System.Console.Haskeline qualified as Line
import System.Console.Haskeline.History qualified as Line
import System.FSNotify qualified as FSNotify
import System.IO (hGetEcho, hPutStrLn, hSetEcho, stderr, stdin)
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Auth.CredentialManager qualified as AuthN
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.Pretty qualified as P
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.HandleInput qualified as HandleInput
import Unison.Codebase.Editor.Input (Event (UnisonFileChanged), Input (..))
import Unison.Codebase.Editor.Output (NumberedArgs, Output)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Watch qualified as Watch
import Unison.CommandLine
import Unison.CommandLine.Completion (haskelineTabComplete)
import Unison.CommandLine.InputPattern qualified as IP
import Unison.CommandLine.InputPatterns qualified as IP
import Unison.CommandLine.OutputMessages (fetchIssueFromGitHub, notifyNumbered, notifyUser)
import Unison.CommandLine.Types (ShouldWatchFiles (..))
import Unison.CommandLine.Welcome qualified as Welcome
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyTerminal
import Unison.Project qualified as Project
import Unison.Runtime (Runtime)
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Server.CodebaseServer qualified as Server
import Unison.Share.Codeserver (isCustomCodeserver)
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Util.Pretty qualified as P
import UnliftIO qualified
import UnliftIO.STM

getUserInput ::
  Codebase IO Symbol Ann ->
  AuthenticatedHttpClient ->
  PP.ProjectPath ->
  IO (Branch IO) ->
  NumberedArgs ->
  IO Input
getUserInput codebase authHTTPClient pp currentProjectRoot numberedArgs =
  Line.runInputT
    settings
    (haskelineCtrlCHandling go)
  where
    -- Catch ctrl-c and simply re-render the prompt.
    haskelineCtrlCHandling :: Line.InputT IO b -> Line.InputT IO b
    haskelineCtrlCHandling act = do
      -- We return a Maybe result to ensure we don't nest an action within the masked exception
      -- handler.
      Line.handleInterrupt (pure Nothing) (Line.withInterrupt (Just <$> act)) >>= \case
        Nothing -> haskelineCtrlCHandling act
        Just a -> pure a

    codeserverPrompt :: String
    codeserverPrompt =
      if isCustomCodeserver Codeserver.defaultCodeserver
        then
          "🌐"
            <> Codeserver.codeserverRegName Codeserver.defaultCodeserver
            <> maybe "" (":" <>) (show <$> Codeserver.codeserverPort Codeserver.defaultCodeserver)
            <> "\n"
        else ""

    go :: Line.InputT IO Input
    go = do
      let statusString = if pp.branch.isUpdate || pp.branch.isUpgrade || pp.branch.isMerge then "🧩 " else ""
      let branchString = P.prettyProjectPath pp
      let fullPrompt =
            P.toANSI 80 $
              fold
                [ P.red (P.string codeserverPrompt),
                  statusString,
                  branchString,
                  fromString prompt
                ]
      line <- Line.getInputLine $ Text.unpack fullPrompt
      case line of
        Nothing -> pure QuitI
        Just l -> case fromMaybe [] $ IP.parseArgs l of
          [] -> go
          ws -> do
            liftIO (parseInput codebase pp currentProjectRoot numberedArgs IP.patternMap ws) >>= \case
              Left failure -> do
                -- We still add history that failed to parse so the user can easily reload
                -- the input and fix it.
                Line.modifyHistory $ Line.addHistoryUnlessConsecutiveDupe l
                liftIO . putPrettyLn $ reportParseFailure failure
                go
              Right Nothing -> do
                -- Ctrl-c or some input cancel, re-run the prompt
                go
              Right (Just (expandedArgs, i)) -> do
                let expandedArgs' = IP.unifyArgument <$> expandedArgs
                    expandedArgsStr =
                      expandedArgs'
                        <&> requote
                        & unwords
                when (expandedArgs' /= fmap IP.renderCliArg ws) $ do
                  liftIO . Text.putStrLn $ fullPrompt <> Text.pack expandedArgsStr
                Line.modifyHistory $ Line.addHistoryUnlessConsecutiveDupe expandedArgsStr
                pure i
    requote :: String -> String
    requote s =
      if elem ' ' s
        then "\"" <> s <> "\""
        else s
    settings :: Line.Settings IO
    settings =
      Line.Settings
        { complete = tabComplete,
          historyFile = Just ".unisonHistory",
          autoAddHistory = False
        }
    tabComplete = haskelineTabComplete IP.patternMap codebase authHTTPClient pp

loopStateProjectPath ::
  Codebase IO Symbol Ann ->
  Cli.LoopState ->
  IO PP.ProjectPath
loopStateProjectPath codebase loopState = do
  let ppIds = NEL.head $ Cli.projectPathStack loopState
  ppIds & PP.projectAndBranch_ %%~ \pabIds -> liftIO . Codebase.runTransaction codebase $ ProjectUtils.expectProjectAndBranchByIds pabIds

main ::
  FilePath ->
  Welcome.Welcome ->
  PP.ProjectPathIds ->
  [Either Event Input] ->
  Runtime Symbol ->
  Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Maybe Server.BaseUrl ->
  UCMVersion ->
  AuthN.AuthenticatedHttpClient ->
  AuthN.CredentialManager ->
  (PP.ProjectPathIds -> IO ()) ->
  ShouldWatchFiles ->
  IO ()
main dir welcome ppIds initialInputs runtime sbRuntime codebase serverBaseUrl ucmVersion authHTTPClient credentialManager lspCheckForChanges shouldWatchFiles = do
  -- we don't like FSNotify's debouncing (it seems to drop later events)
  -- so we will be doing our own instead
  let config = FSNotify.defaultConfig
  FSNotify.withManagerConf config \mgr -> do
    Ki.scoped \scope -> do
      -- Pre-load the project root in the background so it'll be ready when a command needs it.
      _ <- Ki.fork scope (Codebase.expectProjectBranchRoot codebase ppIds.project ppIds.branch)
      -- IOSource takes a while to compile, we should start compiling it on startup
      _ <- Ki.fork scope (IO.evaluate IOSource.typecheckedFile)
      -- Fork the file watcher thread, which returns an IO action we can call to get one filesystem event.
      awaitFileEvent <- do
        (fmap . fmap)
          (\(file, contents) -> UnisonFileChanged (Text.pack file) contents)
          ( Watch.watchDirectory
              scope
              mgr
              dir
              -- We could elect to not spawn a file-watching thread at all if --no-file-watch is passed to ucm, but that
              -- is an extremely uncommon option, this isn't super inefficient, and this makes the types simpler.
              case shouldWatchFiles of
                ShouldNotWatchFiles -> const False
                ShouldWatchFiles -> allow
          )

      -- On startup, we tell the user about any existing project names that don't pass the new project name regex,
      -- which isn't enforced yet.

      invalidProjectNamesInputs <- do
        projects <- Codebase.runTransaction codebase Queries.loadAllProjects
        let invalidProjectNames =
              mapMaybe
                ( \project ->
                    if Project.isValidNewProjectName project.name
                      then Nothing
                      else Just project.name
                )
                projects
        pure case invalidProjectNames of
          [] -> []
          _ ->
            let isReservedName (into @Text -> name) = name == "code" || name == "p"
                hasReservedName = isJust (List.find isReservedName invalidProjectNames)
             in [ Right . CreateMessage . P.warnCallout $
                    P.wrap "We're updating UCM's project naming rules, and these names won’t be supported much longer:"
                      <> P.newline
                      <> P.newline
                      <> P.group (P.commas (map P.prettyProjectName invalidProjectNames))
                      <> P.newline
                      <> P.newline
                      <> P.wrap
                        ( "Please"
                            <> IP.makeExample IP.projectRenameInputPattern []
                            <> "them using only ASCII letters, numbers, hyphens, and underscores."
                            <> (if hasReservedName then "(You also can't use the names 'code' or 'p'.)" else mempty)
                        )
                ]

      let initialState = Cli.loopState0 ppIds
      initialInputsRef <- newIORef $ Welcome.run welcome ++ initialInputs ++ invalidProjectNamesInputs
      pageOutput <- newIORef True

      initialEcho <- hGetEcho stdin
      let restoreEcho = (\currentEcho -> when (currentEcho /= initialEcho) $ hSetEcho stdin initialEcho)
      let getInput :: Cli.LoopState -> IO Input
          getInput loopState = do
            currentEcho <- hGetEcho stdin
            liftIO $ restoreEcho currentEcho
            let PP.ProjectAndBranch projId branchId = PP.toProjectAndBranch $ NonEmpty.head loopState.projectPathStack
            let getProjectRoot = liftIO $ Codebase.expectProjectBranchRoot codebase projId branchId
            pp <- loopStateProjectPath codebase loopState
            getUserInput
              codebase
              authHTTPClient
              pp
              getProjectRoot
              (loopState ^. #numberedArgs)
      let notify :: Output -> IO ()
          notify =
            notifyUser (pure dir) fetchIssueFromGitHub
              >=> ( \o ->
                      ifM
                        (readIORef pageOutput)
                        (putPrettyNonempty o)
                        (putPrettyLnUnpaged o)
                  )

      let awaitInput :: Cli.LoopState -> IO (Either Event Input)
          awaitInput loopState = do
            -- use up buffered input before consulting external events
            readIORef initialInputsRef >>= \case
              h : t -> writeIORef initialInputsRef t >> pure h
              [] -> do
                -- Race the user input and file watch.
                action <-
                  Ki.scoped \scope -> do
                    fileEventThread <- Ki.fork scope awaitFileEvent
                    userInputThread <- Ki.fork scope (getInput loopState)
                    (atomically . asum)
                      [ do
                          event <- Ki.await fileEventThread
                          pure do
                            writeIORef pageOutput False
                            pure (Left event),
                        do
                          input <- Ki.await userInputThread
                          pure (pure (Right input))
                      ]
                action

      let env =
            Cli.Env
              { authHTTPClient,
                codebase,
                credentialManager,
                loadSource = defaultLoadSourceFile,
                lspCheckForChanges,
                writeSource = defaultWriteSourceFile,
                generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG,
                notify,
                notifyNumbered = \o ->
                  let (p, args) = notifyNumbered o
                   in putPrettyNonempty p $> args,
                runtime,
                sandboxedRuntime = sbRuntime,
                serverBaseUrl,
                ucmVersion,
                isTranscriptTest = False
              }

      (onInterrupt, waitForInterrupt) <- buildInterruptHandler

      mask \restore -> do
        -- Handle inputs until @HaltRepl@, staying in the loop on Ctrl+C or synchronous exception.
        let loop0 :: Cli.LoopState -> IO ()
            loop0 s0 = do
              let stepInput :: Either Event Input -> IO (Cli.ReturnType (), Cli.LoopState)
                  stepInput input =
                    Cli.runCli env s0 (HandleInput.loop input)

              -- We want to handle file-change events in a way that allow interruption by other file-change events for
              -- the same file. The idea here is that, if we're (say) typechecking a big file any edits made in the
              -- meantime should cause the typecheck to be canceled and started anew.
              --
              -- This does raise the question: what do we do with both of the following, which we could receive while
              -- handling a file-change event?
              --
              --   1. File-change events for a different .u file than the one we're processing.
              --   2. User input (i.e. they're typing stuff into the prompt against the flow of typechecking output).
              --
              -- Our answers:
              --
              --   1. Throw these away.
              --   2. Don't even try to read these, so they'll buffer and be handled later.
              --
              -- This simplifies the implementation and avoids doing weird stuff like handling file-change events that
              -- were made against a arbitrarily different loop state than the one resulting from the handling of the
              -- first file-change event. Users are unlikely to even notice these details, as while one file is
              -- typechecking, they are not likely to be trying to input things into the prompt nor trying to typecheck
              -- a different file.
              let stepEvent :: Event -> IO (Cli.ReturnType (), Cli.LoopState)
                  stepEvent event@(UnisonFileChanged file contents) = do
                    action <-
                      Ki.scoped \scope -> do
                        handleEventThread <- Ki.fork scope (stepInput (Left event))
                        fileEventThread <-
                          Ki.fork scope do
                            let loop =
                                  awaitFileEvent >>= \case
                                    event2@(UnisonFileChanged file2 contents2)
                                      | file2 == file && contents /= contents2 -> pure event2
                                    _ -> loop
                            loop
                        (atomically . asum)
                          [ do
                              result <- Ki.await handleEventThread
                              pure (pure result),
                            do
                              event2 <- Ki.await fileEventThread
                              pure (stepEvent event2)
                          ]
                    action

              let step :: IO (Cli.ReturnType (), Cli.LoopState)
                  step = do
                    input <- awaitInput s0
                    (!result, resultState) <-
                      case input of
                        Left event -> stepEvent event
                        Right _ -> stepInput input
                    let sNext = case input of
                          Left _ -> resultState
                          Right inp -> resultState & #lastInput ?~ inp
                    pure (result, sNext)
              UnliftIO.race waitForInterrupt (UnliftIO.tryAny (restore step)) >>= \case
                -- SIGINT
                Left () -> do
                  hPutStrLn stderr "\nAborted."
                  loop0 s0
                -- Exception during command execution
                Right (Left e) -> do
                  Text.hPutStrLn stderr ("Encountered exception:\n" <> Text.pack (displayException e))
                  loop0 s0
                Right (Right (result, s1)) -> do
                  case result of
                    Cli.Success () -> loop0 s1
                    Cli.Continue -> loop0 s1
                    Cli.HaltRepl -> pure ()

        withInterruptHandler onInterrupt (loop0 initialState)

-- | Installs a posix interrupt handler for catching SIGINT.
-- This replaces GHC's default sigint handler which throws a UserInterrupt async exception
-- and kills the entire process.
--
-- Returns an IO action which blocks until a ctrl-c is detected. It may be used multiple
-- times.
buildInterruptHandler :: IO (IO (), IO ())
buildInterruptHandler = do
  ctrlCMarker <- UnliftIO.newEmptyMVar
  let onInterrupt = void $ UnliftIO.tryPutMVar ctrlCMarker ()
  let waitForInterrupt = UnliftIO.takeMVar ctrlCMarker
  pure $ (onInterrupt, waitForInterrupt)
