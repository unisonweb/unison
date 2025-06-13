module Unison.MCP (runOnStdIO) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (Result (..), fromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (Foldable (..))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Network.MCP.Server
import Network.MCP.Server.StdIO
import Network.MCP.Types
import Text.RawString.QQ (r)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Runtime (Runtime)
import Unison.Core.Project (ProjectName (..))
import Unison.MCP.Cli (handleInputMCP, ppForProjectName)
import Unison.MCP.StaticResources (staticResources)
import Unison.MCP.Types
import Unison.Parser.Ann (Ann)
import Unison.Symbol (Symbol)

runOnStdIO :: Codebase IO Symbol Ann -> Runtime Symbol -> Runtime Symbol -> Runtime Symbol -> FilePath -> Text -> IO ()
runOnStdIO codebase runtime sbRuntime nRuntime workDir ucmVersion = do
  let env =
        Env
          { codebase,
            runtime,
            nRuntime,
            sbRuntime,
            ucmVersion,
            workDir
          }
  -- Create server
  let serverInfo = Implementation "unison-mcp" "0.0.1"
      serverCapabilities =
        ServerCapabilities
          { resourcesCapability = Just $ ResourcesCapability True,
            toolsCapability = Just $ ToolsCapability True,
            promptsCapability = Nothing
          }

  server <- createServer serverInfo serverCapabilities ""

  registerResources server (fst <$> toList staticResources)

  -- Register resource read handler
  registerResourceReadHandler server $ \(ReadResourceRequest {resourceReadUri}) -> do
    case Map.lookup resourceReadUri staticResources of
      Just (_, content) ->
        pure . ReadResourceResult $ [content]
      _ -> pure $ ReadResourceResult []

  registerTools server [projectCodeTool, installLibTool]

  -- Register tool call handler
  registerToolCallHandler server $ \(CallToolRequest {callToolName, callToolArguments}) -> do
    case fromToolName callToolName of
      Nothing -> pure $ CallToolResult [] True
      Just LibInstallTool ->
        case fromJSON callToolArguments of
          Success (LibInstallToolArguments {}) ->
            error ""
            -- do
            --   runMCP env do
            --     handleInstallLib (ProjectAndBranch (UnsafeProjectName projectName) (ProjectBranchNameOrLatestRelease'Name . UnsafeProjectBranchName <$> branchName)
            --     pp <- liftIO $ Codebase.runTransaction codebase $ do
            --       ProjectBranchNameKind projectBranchNameKind <-
            --         Codebase.classifyProjectBranchName (UnsafeProjectName projectName) branchName
            --       ppForProjectName $ UnsafeProjectName projectName
            --     output <- handleInputMCP pp (Right $ Input.InstallLibI projectBranchNameKind)
            --     let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
            --     pure $
            --       CallToolResult
            --         { callToolIsError = False,
            --           callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            --         }
          _ -> pure $ CallToolResult [] True
      Just ProjectCodeTool ->
        case fromJSON callToolArguments of
          Success (ProjectCodeToolArguments {projectName}) ->
            do
              runMCP env do
                pp <- liftIO $ Codebase.runTransaction codebase $ do
                  ppForProjectName $ UnsafeProjectName projectName
                output <- handleInputMCP pp (Right $ Input.EditNamespaceI [])
                let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
                pure $
                  CallToolResult
                    { callToolIsError = False,
                      callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
                    }
          Error {} -> pure $ CallToolResult [] True

  -- Start the server with StdIO transport
  runServerWithSTDIO server

projectCodeTool :: Tool
projectCodeTool =
  Tool
    { toolName = toToolName ProjectCodeTool,
      toolDescription = Just "Fetch all of the code within a project",
      -- JSON Schema for the tool input.
      -- Which is just the name of the project.
      toolInputSchema =
        fromMaybe (error "Invalid projectCodeTool schema") $
          Aeson.decode $
            [r|
        {
          "type": "object",
          "properties": {
            "projectName": {
              "type": "string",
              "description": "The name of the project to fetch code from"
            }
          }
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Project Code",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

installLibTool :: Tool
installLibTool =
  Tool
    { toolName = toToolName LibInstallTool,
      toolDescription = Just "Install a library from Unison Share into the current project.",
      toolInputSchema = [r|
        {
          "type": "object",
          "properties": {
            "projectName": {
              "type": "string",
              "description": "The name of the library project to install"
            },
            "branchName": {
              "type": ["string", "null"],
              "description": "The optional branch of the library project to install. If null, the latest release will be used."
            }
          },
          "required": ["projectName"]
        }
        |]
    , toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Install Library",
              readOnlyHint = Just False,
              destructiveHint = Just True,
              idempotentHint = Just False,
              openWorldHint = Just True
            }
    }
