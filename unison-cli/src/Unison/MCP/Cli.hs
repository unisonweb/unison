module Unison.MCP.Cli
  ( handleInputMCP,
    ppForProjectName,
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
import Unison.Codebase.Editor.HandleInput qualified as HandleInput
import Unison.Codebase.Editor.Input (Event, Input)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.CommandLine.OutputMessages qualified as Output
import Unison.MCP.Types
import Unison.MCP.Types qualified as MCP
import Unison.Prelude
import Unison.Project (ProjectName)
import Unison.Sqlite (Transaction)
import Unison.Syntax.Parser qualified as Parser
import Unison.Util.Pretty qualified as Pretty
import UnliftIO.STM
import Witch (unsafeFrom)
import Prelude hiding (readFile, writeFile)

data CliOutput = CliOutput
  { sourceCodeUpdates :: [Text],
    outputMessages :: [Text]
  }
  deriving (Eq, Show)

instance ToJSON CliOutput where
  toJSON (CliOutput sourceCodeUpdates outputMessages) =
    object
      [ "sourceCodeUpdates" .= sourceCodeUpdates,
        "outputMessages" .= outputMessages
      ]

ppForProjectName :: ProjectName -> Transaction PP.ProjectPath
ppForProjectName projectName = do
  project <-
    Queries.loadProjectByName projectName & onNothingM do
      error "TODO: handle project not found"
  branch <-
    Queries.loadMostRecentBranch (project ^. #projectId) >>= \case
      Nothing -> do
        let branchName = unsafeFrom @Text "main"
        branch <-
          Queries.loadProjectBranchByName project.projectId branchName & onNothingM do
            error "TODO: handle branch not found"
        pure branch
      Just branchId -> Queries.expectProjectBranch project.projectId branchId
  pure $ PP.fromProjectAndBranch (PP.ProjectAndBranch project branch) Path.Root

handleInputMCP :: PP.ProjectPath -> Either Event Input -> MCP CliOutput
handleInputMCP initialPP input = do
  credMan <- AuthN.newCredentialManager
  let tokenProvider :: AuthN.TokenProvider
      tokenProvider = AuthN.newTokenProvider credMan
  MCP.Env {ucmVersion, codebase, runtime, workDir} <- ask
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

-- cliToMCP :: Cli a -> MCP a
-- cliToMCP cli = do
--   credMan <- AuthN.newCredentialManager
--   let tokenProvider :: AuthN.TokenProvider
--       tokenProvider = AuthN.newTokenProvider credMan
--   MCP.Env {ucmVersion, codebase, runtime, workDir} <- ask
--   authenticatedHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion
--   outputVar <- newTVarIO Seq.empty
--   sourceCodeUpdatesVar <- newTVarIO Seq.empty
--   let notify output = do
--         pretty <- Output.notifyUser workDir output
--         atomically $ modifyTVar outputVar (<> Seq.singleton pretty)
--   let notifyNumbered output = do
--         let (pretty, nargs) = Output.notifyNumbered output
--         atomically $ modifyTVar outputVar (<> Seq.singleton pretty)
--         pure nargs

--   let loadSource = error "TODO implement loadSource"
--   let writeSource _sourceName content replace = do
--         if replace
--           then do
--             atomically $ writeTVar sourceCodeUpdatesVar (Seq.singleton content)
--           else do
--             atomically $ modifyTVar sourceCodeUpdatesVar (<> Seq.singleton content)

--   seedRef <- liftIO $ newIORef (0 :: Int)
--   let cliEnv =
--         Cli.Env
--           { authHTTPClient = authenticatedHTTPClient,
--             codebase,
--             credentialManager = credMan,
--             generateUniqueName = do
--               i <- atomicModifyIORef' seedRef \i -> let !i' = i + 1 in (i', i)
--               pure (Parser.uniqueBase32Namegen (Random.drgNewSeed (Random.seedFromInteger (fromIntegral i)))),
--             loadSource,
--             lspCheckForChanges = \_ -> pure (),
--             writeSource,
--             notify,
--             notifyNumbered,
--             runtime,
--             sandboxedRuntime = error "Sandboxed runtime not implemented",
--             nativeRuntime = error "Native runtime not implemented",
--             serverBaseUrl = Nothing,
--             ucmVersion,
--             isTranscriptTest = False
--           }

--   let startState = (Cli.loopState0 (PP.toIds initialPP))
--   -- The actual output isn't important, all communication comes from notify, notifyNumbered, and writeSource.
--   r <- liftIO (Cli.runCli cliEnv startState (HandleInput.loop input))
--   -- flush the output buffer since it should now be filled.
--   atomically $ do
--     msgs <- readTVar outputVar
--     sourceCodeUpdates <- toList <$> readTVar sourceCodeUpdatesVar
--     let outputMessages =
--           msgs
--             & fmap (Text.pack . Pretty.toPlainUnbroken)
--             & toList
--     pure $ (r,
--       CliOutput
--         { sourceCodeUpdates,
--           outputMessages
--         }
--            )
