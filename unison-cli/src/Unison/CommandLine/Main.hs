module Unison.CommandLine.Main
  ( main,
  )
where

import Compat (withInterruptHandler)
import Control.Concurrent.Async qualified as Async
import Control.Exception (catch, displayException, finally, mask)
import Control.Lens (preview, (?~), (^.))
import Crypto.Random qualified as Random
import Data.Configurator.Types (Config)
import Data.IORef
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Ki qualified
import System.Console.Haskeline (Settings (autoAddHistory))
import System.Console.Haskeline qualified as Line
import System.Console.Haskeline.History qualified as Line
import System.IO (hGetEcho, hPutStrLn, hSetEcho, stderr, stdin)
import System.IO.Error (isDoesNotExistError)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Operations qualified as Ops
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Auth.CredentialManager (newCredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Auth.Tokens qualified as AuthN
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.Pretty (prettyProjectAndBranchName)
import Unison.Cli.ProjectUtils (projectBranchPathPrism)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput qualified as HandleInput
import Unison.Codebase.Editor.Input (Event, Input (..))
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime qualified as Runtime
import Unison.CommandLine
import Unison.CommandLine.Completion (haskelineTabComplete)
import Unison.CommandLine.InputPatterns qualified as IP
import Unison.CommandLine.OutputMessages (notifyNumbered, notifyUser)
import Unison.CommandLine.Types (ShouldWatchFiles (..))
import Unison.CommandLine.Welcome qualified as Welcome
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyTerminal
import Unison.Project (ProjectAndBranch (..))
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Server.CodebaseServer qualified as Server
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Util.Pretty qualified as P
import Unison.Util.TQueue qualified as Q
import UnliftIO qualified
import UnliftIO.Directory qualified as Directory
import UnliftIO.STM

getUserInput ::
  Codebase IO Symbol Ann ->
  AuthenticatedHttpClient ->
  PP.ProjectPath ->
  IO (Branch IO) ->
  [String] ->
  IO Input
getUserInput codebase authHTTPClient ppCtx currentProjectRoot numberedArgs =
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
    go :: Line.InputT IO Input
    go = do
      let (PP.ProjectPath projectName projectBranchName path) = ppCtx ^. PP.ctxAsNames_
      let promptString =
            P.sep
              ":"
              ( catMaybes
                  [ Just (prettyProjectAndBranchName (ProjectAndBranch projectName projectBranchName)),
                    (Just . P.green . P.shown) path
                  ]
              )
      let fullPrompt = P.toANSI 80 (promptString <> fromString prompt)
      line <- Line.getInputLine fullPrompt
      case line of
        Nothing -> pure QuitI
        Just l -> case words l of
          [] -> go
          ws -> do
            liftIO (parseInput codebase ppCtx currentProjectRoot numberedArgs IP.patternMap ws) >>= \case
              Left msg -> do
                -- We still add history that failed to parse so the user can easily reload
                -- the input and fix it.
                Line.modifyHistory $ Line.addHistoryUnlessConsecutiveDupe $ l
                liftIO $ putPrettyLn msg
                go
              Right Nothing -> do
                -- Ctrl-c or some input cancel, re-run the prompt
                go
              Right (Just (expandedArgs, i)) -> do
                let expandedArgsStr = unwords expandedArgs
                when (expandedArgs /= ws) $ do
                  liftIO . putStrLn $ fullPrompt <> expandedArgsStr
                Line.modifyHistory $ Line.addHistoryUnlessConsecutiveDupe $ unwords expandedArgs
                pure i
    settings :: Line.Settings IO
    settings =
      Line.Settings
        { complete = tabComplete,
          historyFile = Just ".unisonHistory",
          autoAddHistory = False
        }
    tabComplete = haskelineTabComplete IP.patternMap codebase authHTTPClient ppCtx

main ::
  FilePath ->
  Welcome.Welcome ->
  PP.ProjectPathIds ->
  Config ->
  [Either Event Input] ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Maybe Server.BaseUrl ->
  UCMVersion ->
  (CausalHash -> STM ()) ->
  (Path.Absolute -> STM ()) ->
  ShouldWatchFiles ->
  IO ()
main dir welcome ppIds config initialInputs runtime sbRuntime nRuntime codebase serverBaseUrl ucmVersion notifyBranchChange notifyPathChange shouldWatchFiles = Ki.scoped \scope -> do
  rootVar <- newEmptyTMVarIO
  _ <- Ki.fork scope do
    root <- Codebase.getRootBranch codebase
    atomically do
      -- Try putting the root, but if someone else as already written over the root, don't
      -- overwrite it.
      void $ tryPutTMVar rootVar root
    -- Start forcing thunks in a background thread.
    -- This might be overly aggressive, maybe we should just evaluate the top level but avoid
    -- recursive "deep*" things.
    UnliftIO.concurrently_
      (UnliftIO.evaluate root)
      (UnliftIO.evaluate IOSource.typecheckedFile) -- IOSource takes a while to compile, we should start compiling it on startup
  let initialState = Cli.loopState0 rootVar ppIds
  Ki.fork_ scope do
    let loop lastRoot = do
          -- This doesn't necessarily notify on _every_ update, but the LSP only needs the
          -- most recent version at any given time, so it's fine to skip some intermediate
          -- versions.
          currentRoot <- atomically do
            currentRoot <- readTMVar rootVar
            guard $ Just currentRoot /= lastRoot
            notifyBranchChange (Branch.headHash currentRoot)
            pure (Just currentRoot)
          loop currentRoot
    loop Nothing
  eventQueue <- Q.newIO
  initialInputsRef <- newIORef $ Welcome.run welcome ++ initialInputs
  pageOutput <- newIORef True
  cancelFileSystemWatch <- case shouldWatchFiles of
    ShouldNotWatchFiles -> pure (pure ())
    ShouldWatchFiles -> watchFileSystem eventQueue dir
  credentialManager <- newCredentialManager
  let tokenProvider = AuthN.newTokenProvider credentialManager
  authHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion
  initialEcho <- hGetEcho stdin
  let restoreEcho = (\currentEcho -> when (currentEcho /= initialEcho) $ hSetEcho stdin initialEcho)
  let getInput :: Cli.LoopState -> IO Input
      getInput loopState = do
        currentEcho <- hGetEcho stdin
        liftIO $ restoreEcho currentEcho
        let getProjectRoot = atomically $ readTMVar rootVar
        Codebase.runTransaction codebase Ops.expectProjectAndBranchNames
        getUserInput
          codebase
          authHTTPClient
          (NEL.head $ Cli.projectPathStack loopState)
          getProjectRoot
          (loopState ^. #numberedArgs)
  let loadSourceFile :: Text -> IO Cli.LoadSourceResult
      loadSourceFile fname =
        if allow $ Text.unpack fname
          then
            let handle :: IOException -> IO Cli.LoadSourceResult
                handle e =
                  case e of
                    _ | isDoesNotExistError e -> return Cli.InvalidSourceNameError
                    _ -> return Cli.LoadError
                go = do
                  contents <- readUtf8 $ Text.unpack fname
                  return $ Cli.LoadSuccess contents
             in catch go handle
          else return Cli.InvalidSourceNameError
  let notify :: Output -> IO ()
      notify =
        notifyUser dir
          >=> ( \o ->
                  ifM
                    (readIORef pageOutput)
                    (putPrettyNonempty o)
                    (putPrettyLnUnpaged o)
              )

  let cleanup :: IO ()
      cleanup = cancelFileSystemWatch
      awaitInput :: Cli.LoopState -> IO (Either Event Input)
      awaitInput loopState = do
        -- use up buffered input before consulting external events
        readIORef initialInputsRef >>= \case
          h : t -> writeIORef initialInputsRef t >> pure h
          [] ->
            -- Race the user input and file watch.
            Async.race (atomically $ Q.peek eventQueue) (getInput loopState) >>= \case
              Left _ -> do
                let e = Left <$> atomically (Q.dequeue eventQueue)
                writeIORef pageOutput False
                e
              x -> do
                writeIORef pageOutput True
                pure x

  let foldLine :: Text
      foldLine = "\n\n---- Anything below this line is ignored by Unison.\n\n"
  let writeSourceFile :: Text -> Text -> IO ()
      writeSourceFile fp contents = do
        path <- Directory.canonicalizePath (Text.unpack fp)
        prependUtf8 path (contents <> foldLine)

  let env =
        Cli.Env
          { authHTTPClient,
            codebase,
            config,
            credentialManager,
            loadSource = loadSourceFile,
            writeSource = writeSourceFile,
            generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG,
            notify,
            notifyNumbered = \o ->
              let (p, args) = notifyNumbered o
               in putPrettyNonempty p $> args,
            runtime,
            sandboxedRuntime = sbRuntime,
            nativeRuntime = nRuntime,
            serverBaseUrl,
            ucmVersion
          }

  (onInterrupt, waitForInterrupt) <- buildInterruptHandler

  mask \restore -> do
    -- Handle inputs until @HaltRepl@, staying in the loop on Ctrl+C or synchronous exception.
    let loop0 :: Cli.LoopState -> IO ()
        loop0 s0 = do
          let step = do
                input <- awaitInput s0
                (!result, resultState) <- Cli.runCli env s0 (HandleInput.loop input)
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
              when ((s0 ^. #currentPath) /= (s1 ^. #currentPath :: Path.Absolute)) (atomically . notifyPathChange $ s1 ^. #currentPath)
              case result of
                Cli.Success () -> loop0 s1
                Cli.Continue -> loop0 s1
                Cli.HaltRepl -> pure ()

    withInterruptHandler onInterrupt (loop0 initialState `finally` cleanup)

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
