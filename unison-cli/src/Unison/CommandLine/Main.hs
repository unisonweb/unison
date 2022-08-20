module Unison.CommandLine.Main
  ( main,
  )
where

import Compat (withInterruptHandler)
import qualified Control.Concurrent.Async as Async
import Control.Exception (catch, finally, mask)
import Control.Lens ((^.))
import Control.Monad.Catch (MonadMask)
import qualified Crypto.Random as Random
import Data.Configurator.Types (Config)
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Text.Lazy
import qualified Ki
import qualified System.Console.Haskeline as Line
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import Text.Pretty.Simple (pShow)
import Unison.Auth.CredentialManager (newCredentialManager)
import qualified Unison.Auth.HTTPClient as AuthN
import qualified Unison.Auth.Tokens as AuthN
import qualified Unison.Cli.Monad as Cli
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.HandleInput as HandleInput
import Unison.Codebase.Editor.Input (Event, Input (..))
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Runtime
import Unison.CommandLine
import Unison.CommandLine.Completion (haskelineTabComplete)
import qualified Unison.CommandLine.InputPatterns as IP
import Unison.CommandLine.OutputMessages (notifyNumbered, notifyUser)
import qualified Unison.CommandLine.Welcome as Welcome
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyTerminal
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Parser as Parser
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.TQueue as Q
import qualified UnliftIO
import UnliftIO.STM

getUserInput ::
  forall m v a.
  (MonadIO m, MonadMask m) =>
  Codebase m v a ->
  IO (Branch m) ->
  Path.Absolute ->
  [String] ->
  m Input
getUserInput codebase getRoot currentPath numberedArgs =
  Line.runInputT
    settings
    (haskelineCtrlCHandling go)
  where
    -- Catch ctrl-c and simply re-render the prompt.
    haskelineCtrlCHandling :: Line.InputT m b -> Line.InputT m b
    haskelineCtrlCHandling act = do
      -- We return a Maybe result to ensure we don't nest an action within the masked exception
      -- handler.
      Line.handleInterrupt (pure Nothing) (Line.withInterrupt (Just <$> act)) >>= \case
        Nothing -> haskelineCtrlCHandling act
        Just a -> pure a
    go :: Line.InputT m Input
    go = do
      line <-
        Line.getInputLine $
          P.toANSI 80 ((P.green . P.shown) currentPath <> fromString prompt)
      case line of
        Nothing -> pure QuitI
        Just l -> case words l of
          [] -> go
          ws -> do
            liftIO (parseInput (Branch.head <$> getRoot) currentPath numberedArgs IP.patternMap ws) >>= \case
              Left msg -> do
                liftIO $ putPrettyLn msg
                go
              Right i -> pure i
    settings :: Line.Settings m
    settings = Line.Settings tabComplete (Just ".unisonHistory") True
    tabComplete = haskelineTabComplete IP.patternMap codebase currentPath

main ::
  FilePath ->
  Welcome.Welcome ->
  Path.Absolute ->
  (Config, IO ()) ->
  [Either Event Input] ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Maybe Server.BaseUrl ->
  UCMVersion ->
  IO ()
main dir welcome initialPath (config, cancelConfig) initialInputs runtime sbRuntime codebase serverBaseUrl ucmVersion = Ki.scoped \scope -> do
  rootVar <- newEmptyTMVarIO
  _ <- Ki.fork scope $ do
    root <- Codebase.getRootBranch codebase
    atomically $ putTMVar rootVar root
    -- Start forcing the thunk in a background thread.
    -- This might be overly aggressive, maybe we should just evaluate the top level but avoid
    -- recursive "deep*" things.
    void $ UnliftIO.evaluate root
    putStrLn "start deep"
    let root0 = Branch.head root
    void $ UnliftIO.evaluate $ Branch._children root0
    void $ UnliftIO.evaluate $ Branch._terms root0
    void $ UnliftIO.evaluate $ Branch._types root0
    void $ UnliftIO.evaluate $ Branch.deepTerms root0
    void $ UnliftIO.evaluate $ Branch.deepTypes root0
    void $ UnliftIO.evaluate $ Branch.deepPaths root0
    putStrLn "done"
  eventQueue <- Q.newIO
  welcomeEvents <- Welcome.run codebase welcome
  initialInputsRef <- newIORef $ welcomeEvents ++ initialInputs
  pageOutput <- newIORef True
  cancelFileSystemWatch <- watchFileSystem eventQueue dir
  let getInput :: Cli.LoopState -> IO Input
      getInput loopState = do
        getUserInput
          codebase
          (atomically . readTMVar $ loopState ^. #root)
          (loopState ^. #currentPath)
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

  let cleanup = do
        Runtime.terminate runtime
        Runtime.terminate sbRuntime
        cancelConfig
        cancelFileSystemWatch
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
  credentialManager <- newCredentialManager
  let tokenProvider = AuthN.newTokenProvider credentialManager
  authHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion

  let env =
        Cli.Env
          { authHTTPClient,
            codebase,
            config,
            credentialManager,
            loadSource = loadSourceFile,
            generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG,
            notify,
            notifyNumbered = \o ->
              let (p, args) = notifyNumbered o
               in putPrettyNonempty p $> args,
            runtime,
            sandboxedRuntime = sbRuntime,
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
                Cli.runCli env s0 (HandleInput.loop input)
          UnliftIO.race waitForInterrupt (UnliftIO.tryAny (restore step)) >>= \case
            -- SIGINT
            Left () -> do
              hPutStrLn stderr "\nAborted."
              loop0 s0
            -- Exception during command execution
            Right (Left e) -> do
              Text.Lazy.hPutStrLn stderr ("Encountered exception:\n" <> pShow e)
              loop0 s0
            Right (Right (result, s1)) ->
              case result of
                Cli.Success () -> loop0 s1
                Cli.Continue -> loop0 s1
                Cli.HaltRepl -> pure ()

    withInterruptHandler onInterrupt (loop0 (Cli.loopState0 rootVar initialPath) `finally` cleanup)

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
