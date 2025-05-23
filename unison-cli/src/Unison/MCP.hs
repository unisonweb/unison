module Unison.MCP (runOnStdIO) where

import Data.Bifunctor (Bifunctor (..))
import Data.Text qualified as Text
import Network.MCP.Server
import Network.MCP.Server.StdIO
import Network.MCP.Types

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

  registerResources server [unisonGuideResource]

  -- Register resource read handler
  registerResourceReadHandler server $ \(ReadResourceRequest {resourceReadUri}) -> do
    case second (Text.splitOn "/") (Text.breakOn "://" resourceReadUri) of
      ("file", ["project", projectName, "dump"]) -> do
        let content = "Project dump content from " <> projectName
        return $
          ReadResourceResult $
            [ ResourceContent
                { resourceContentUri = resourceReadUri,
                  resourceContentMimeType = Just "text/plain",
                  resourceContentText = Just content,
                  resourceContentBlob = Nothing
                }
            ]
      _ -> pure $ ReadResourceResult []

  -- registerTools server [tool]

  -- Register tool call handler
  -- registerToolCallHandler server $ \request -> do
  -- _
  -- Implement tool execution logic
  -- ...

  -- Start the server with StdIO transport
  runServerWithSTDIO server

_projectCodeResource :: Resource
_projectCodeResource =
  Resource
    { resourceUri = "",
      resourceName = "Full Unison Project Code",
      resourceDescription = Just "All the code within a given Unison project",
      resourceMimeType = Just "text/plain",
      resourceTemplate = Just "file://project/{projectName}/dump"
    }

unisonGuideResource :: Resource
unisonGuideResource =
  Resource
    { resourceUri = "file://unison-guide",
      resourceName = "Unison Programming Guide",
      resourceDescription = Just "A complete guide on how to program in Unison in Markdown format",
      resourceMimeType = Just "text/markdown",
      resourceTemplate = Nothing
    }

_myTool :: Tool
_myTool =
  Tool
    { toolName = "my-tool",
      toolDescription = Just "My tool",
      toolInputSchema = "{...}" -- JSON schema
    }
