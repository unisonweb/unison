module Unison.MCP.Cli
  ( handleInputMCP,
    ppForProjectContext,
    cliToMCP,
  )
where

import Control.Monad.Reader
import Crypto.Random qualified as Random
import Data.Aeson
import Data.IORef
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Auth.CredentialManager qualified as AuthN
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Auth.Tokens qualified as AuthN
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput qualified as HandleInput
import Unison.Codebase.Editor.Input (Event, Input)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.CommandLine.OutputMessages qualified as Output
import Unison.MCP.Types
import Unison.MCP.Types qualified as MCP
import Unison.Prelude
import Unison.Sqlite (Transaction)
import Unison.Syntax.Parser qualified as Parser
import Unison.Util.Pretty qualified as Pretty
import UnliftIO.STM
import Prelude hiding (readFile, writeFile)

data CliOutput = CliOutput
  { sourceCodeUpdates :: [Text],
    outputMessages :: [Text]
  }
  deriving (Eq, Show)

instance Semigroup CliOutput where
  CliOutput src1 out1 <> CliOutput src2 out2 =
    CliOutput (src1 <> src2) (out1 <> out2)

instance Monoid CliOutput where
  mempty = CliOutput [] []

instance ToJSON CliOutput where
  toJSON (CliOutput sourceCodeUpdates outputMessages) =
    object
      [ "sourceCodeUpdates" .= sourceCodeUpdates,
        "outputMessages" .= outputMessages
      ]

ppForProjectContext :: ProjectContext -> Transaction PP.ProjectPath
ppForProjectContext ProjectContext {projectName, branchName} = do
  project <-
    Queries.loadProjectByName projectName & onNothingM do
      error "TODO: handle project not found"
  branch <-
    Queries.loadProjectBranchByName project.projectId branchName >>= \case
      Nothing -> error "TODO: handle branch not found"
      Just projectBranch -> pure projectBranch
  pure $ PP.fromProjectAndBranch (PP.ProjectAndBranch project branch) Path.Root

handleInputMCP :: ProjectContext -> [Either Event Input] -> MCP CliOutput
handleInputMCP projectContext input = do
  case input of
    (inp : rest) -> do
      (_, cliOutput) <- cliToMCP projectContext (HandleInput.loop inp)
      (cliOutput <>) <$> handleInputMCP projectContext rest
    [] -> pure mempty

cliToMCP :: ProjectContext -> Cli.Cli a -> MCP (Maybe a, CliOutput)
cliToMCP projCtx cli = do
  MCP.Env {ucmVersion, codebase, runtime, workDir} <- ask
  initialPP <- liftIO $ Codebase.runTransaction codebase $ do
    ppForProjectContext projCtx
  credMan <- AuthN.newCredentialManager
  let tokenProvider :: AuthN.TokenProvider
      tokenProvider = AuthN.newTokenProvider credMan
  authenticatedHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion
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
            ucmVersion,
            isTranscriptTest = False
          }

  let startState = (Cli.loopState0 (PP.toIds initialPP))
  -- The actual output isn't important, all communication comes from notify, notifyNumbered, and writeSource.
  (cliResult, _loopState) <- liftIO (Cli.runCli cliEnv startState cli)
  -- flush the output buffer since it should now be filled.
  cliOut <- atomically $ do
    msgs <- readTVar outputVar
    sourceCodeUpdates <- toList <$> readTVar sourceCodeUpdatesVar
    let outputMessages =
          msgs
            & fmap (Text.pack . Pretty.toPlainUnbroken)
            & toList
    pure $
      ( CliOutput
          { sourceCodeUpdates,
            outputMessages
          }
      )
  case cliResult of
    Cli.Continue -> pure (Nothing, cliOut)
    Cli.HaltRepl -> pure (Nothing, cliOut)
    Cli.Success a -> pure (Just a, cliOut)
