module Unison.MCP (runOnStdIO) where

import Data.Aeson (Result (..), fromJSON)
import Data.Aeson qualified as Aeson
import Data.Foldable (Foldable (..))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Network.MCP.Server
import Network.MCP.Server.StdIO
import Network.MCP.Types
import Text.RawString.QQ (r)
import Unison.MCP.StaticResources (staticResources)
import Unison.MCP.Types

runOnStdIO :: IO ()
runOnStdIO = do
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

  registerTools server [projectCodeTool]

  -- Register tool call handler
  registerToolCallHandler server $ \(CallToolRequest {callToolName, callToolArguments}) -> do
    case (fromToolName callToolName, fromJSON callToolArguments) of
      (Just ProjectCodeTool, Success (ProjectCodeToolArguments {projectName})) ->
        do
          -- Fetch code from the project (dummy implementation)
          let code = "Code from project: " <> projectName
          pure $
            CallToolResult
              { callToolIsError = False,
                callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just code}]
              }
      (Just _, Error {}) -> pure $ CallToolResult [] True
      (Nothing, _) -> pure $ CallToolResult [] True

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

-- installLibTool :: Tool
-- installLibTool =
--   Tool
--     { toolName = "install-library",
--       toolDescription = Just "Install a library from Unison Share",
--       toolInputSchema = "{...}" -- JSON schema
--     }
