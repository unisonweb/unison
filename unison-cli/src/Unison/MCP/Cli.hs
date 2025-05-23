module Unison.MCP.Cli (runCliMCP) where

import Control.Monad.Reader
import Crypto.Random qualified as Random
import Data.IORef
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Unison.Auth.CredentialManager qualified as AuthN
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Auth.Tokens qualified as AuthN
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput qualified as HandleInput
import Unison.Codebase.Editor.Input (Event, Input)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.CommandLine.OutputMessages qualified as Output
import Unison.MCP.Types
import Unison.MCP.Types qualified as MCP
import Unison.Prelude
import Unison.Syntax.Parser qualified as Parser
import Unison.Util.Pretty qualified as Pretty
import Unison.Version qualified as Version
import UnliftIO.STM
import Prelude hiding (readFile, writeFile)

data CliOutput = CliOutput
  { sourceCodeUpdates :: [Text],
    outputMessages :: [Text]
  }
  deriving (Eq, Show)

runCliMCP :: Either Event Input -> MCP CliOutput
runCliMCP input = do
  credMan <- AuthN.newCredentialManager
  let tokenProvider :: AuthN.TokenProvider
      tokenProvider = AuthN.newTokenProvider credMan
  MCP.Env {ucmVersion, codebase, runtime, workDir} <- ask
  let ucmVersionText = (Version.gitDescribeWithDate ucmVersion)
  authenticatedHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersionText

  outputVar <- newTVarIO Seq.empty
  sourceCodeUpdatesVar <- newTVarIO Seq.empty
  let notify output = do
        pretty <- Output.notifyUser workDir output
        atomically $ modifyTVar outputVar (<> Seq.singleton pretty)
  let notifyNumbered output = do
        let (pretty, nargs) = Output.notifyNumbered output
        atomically $ modifyTVar outputVar (<> Seq.singleton pretty)
        pure nargs

  let loadSource = error "TODO implement loadSource"
  let writeSource _sourceName content replace = do
        if replace
          then do
            atomically $ writeTVar sourceCodeUpdatesVar (Seq.singleton content)
          else do
            atomically $ modifyTVar sourceCodeUpdatesVar (<> Seq.singleton content)

  seedRef <- liftIO $ newIORef (0 :: Int)
  let cliEnv =
        Cli.Env
          { authHTTPClient = authenticatedHTTPClient,
            codebase,
            credentialManager = credMan,
            generateUniqueName = do
              i <- atomicModifyIORef' seedRef \i -> let !i' = i + 1 in (i', i)
              pure (Parser.uniqueBase32Namegen (Random.drgNewSeed (Random.seedFromInteger (fromIntegral i)))),
            loadSource,
            lspCheckForChanges = \_ -> pure (),
            writeSource,
            notify,
            notifyNumbered,
            runtime,
            sandboxedRuntime = error "Sandboxed runtime not implemented",
            nativeRuntime = error "Native runtime not implemented",
            serverBaseUrl = Nothing,
            ucmVersion = ucmVersionText,
            isTranscriptTest = False
          }

  (initialPP, _emptyCausalHashId) <-
    liftIO $ Codebase.runTransaction codebase . liftA2 (,) Codebase.expectCurrentProjectPath $ snd <$> Codebase.emptyCausalHash
  let startState = (Cli.loopState0 (PP.toIds initialPP))
  -- The actual output isn't important, all communication comes from notify, notifyNumbered, and writeSource.
  _ <- liftIO (Cli.runCli cliEnv startState (HandleInput.loop input))
  -- flush the output buffer since it should now be filled.
  atomically $ do
    msgs <- readTVar outputVar
    sourceCodeUpdates <- toList <$> readTVar sourceCodeUpdatesVar
    let outputMessages =
          msgs
            & fmap (Text.pack . Pretty.toPlainUnbroken)
            & toList
    pure $
      CliOutput
        { sourceCodeUpdates,
          outputMessages
        }
