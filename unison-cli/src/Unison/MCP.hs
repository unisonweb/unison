module Unison.MCP (runOnStdIO) where

import Data.Aeson (Result (..), fromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (Foldable (..))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.MCP.Server
import Network.MCP.Server.StdIO
import Network.MCP.Types
import Text.RawString.QQ (r)
import Unison.Auth.CredentialManager qualified as AuthN
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Auth.Tokens qualified as AuthN
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.HandleInput.InstallLib (handleInstallLib)
import Unison.Codebase.Editor.Input (Event (..), Input (..))
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Runtime (Runtime)
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName (..), ProjectName (..))
import Unison.MCP.Cli (cliToMCP, handleInputMCP)
import Unison.MCP.Share.API qualified as Share
import Unison.MCP.StaticResources (staticResources)
import Unison.MCP.Types
import Unison.Parser.Ann (Ann)
import Unison.Project (ProjectBranchNameOrLatestRelease (..))
import Unison.Symbol (Symbol)
import UnliftIO qualified

serverDescription :: Text
serverDescription =
  [r|
        This server provides endpoints for searching code on Unison Share, which is a platform for sharing Unison projects and libraries.

        It also provides some mechanisms for editing and updating local Unison projects.

        Before doing any work in unison please read the file://unison-guide resource for information on how to write unison.
    |]

runOnStdIO :: Codebase IO Symbol Ann -> Runtime Symbol -> Runtime Symbol -> Runtime Symbol -> FilePath -> Text -> IO ()
runOnStdIO codebase runtime sbRuntime nRuntime workDir ucmVersion = do
  credMan <- AuthN.newCredentialManager
  let tokenProvider :: AuthN.TokenProvider
      tokenProvider = AuthN.newTokenProvider credMan
  authenticatedHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion
  let env =
        Env
          { codebase,
            runtime,
            nRuntime,
            sbRuntime,
            ucmVersion,
            workDir,
            authenticatedHTTPClient
          }
  -- Create server
  let serverInfo = Implementation "unison-mcp" "0.0.1"
      serverCapabilities =
        ServerCapabilities
          { resourcesCapability = Just $ ResourcesCapability True,
            toolsCapability = Just $ ToolsCapability True,
            promptsCapability = Nothing
          }

  server <- createServer serverInfo serverCapabilities serverDescription

  registerResources server (fst <$> toList staticResources)

  -- Register resource read handler
  registerResourceReadHandler server $ \(ReadResourceRequest {resourceReadUri}) -> do
    case Map.lookup resourceReadUri staticResources of
      Just (_, content) ->
        pure . ReadResourceResult $ [content]
      _ -> pure $ ReadResourceResult []

  registerTools
    server
    [ {- projectCodeTool ,-}
      -- Removed for now cuz it fills up too much of the context window.
      installLibTool,
      shareProjectSearchTool,
      typecheckCodeTool,
      docsTool,
      projectReadmeTool
    ]

  -- Register tool call handler
  registerToolCallHandler server $ \(CallToolRequest {callToolName, callToolArguments}) -> do
    runMCP env $ case fromToolName callToolName of
      Nothing -> pure $ CallToolResult [] True
      Just LibInstallTool ->
        case fromJSON callToolArguments of
          Success (LibInstallToolArguments {projectContext, libProjectName, libBranchName}) -> do
            (_r, output) <- cliToMCP projectContext $ do
              handleInstallLib False (ProjectAndBranch (UnsafeProjectName libProjectName) (ProjectBranchNameOrLatestRelease'Name . UnsafeProjectBranchName <$> libBranchName))
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
            pure $
              CallToolResult
                { callToolIsError = False,
                  callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
                }
          _ -> pure $ CallToolResult [] True
      Just ProjectCodeTool ->
        case fromJSON callToolArguments of
          Success (ProjectCodeToolArguments {projectContext}) ->
            do
              output <- handleInputMCP projectContext [Right $ Input.EditNamespaceI []]
              let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
              pure $
                CallToolResult
                  { callToolIsError = False,
                    callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
                  }
          Error {} -> pure $ CallToolResult [] True
      Just ShareProjectSearchTool ->
        case fromJSON callToolArguments of
          Success (ShareProjectSearchToolArguments {query}) -> do
            result <- UnliftIO.liftIO $ Share.shareSearch authenticatedHTTPClient query
            case result of
              Right searchResult -> do
                let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode searchResult
                pure $
                  CallToolResult
                    { callToolIsError = False,
                      callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
                    }
              Left err -> do
                let errorMsg = "Error searching Unison Share: " <> Text.pack (show err)
                pure $
                  CallToolResult
                    { callToolIsError = True,
                      callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just errorMsg}]
                    }
          Error {} -> pure $ CallToolResult [] True
      Just ShareProjectReadmeTool ->
        case fromJSON callToolArguments of
          Success (ShareProjectReadmeToolArguments {projectName, projectOwnerHandle}) -> do
            result <- UnliftIO.liftIO $ Share.shareProjectReadme authenticatedHTTPClient projectOwnerHandle projectName
            case result of
              Right searchResult -> do
                let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode searchResult
                pure $
                  CallToolResult
                    { callToolIsError = False,
                      callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
                    }
              Left err -> do
                let errorMsg = "Error getting readme from Unison Share: " <> Text.pack (show err)
                pure $
                  CallToolResult
                    { callToolIsError = True,
                      callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just errorMsg}]
                    }
          Error {} -> pure $ CallToolResult [] True
      Just TypecheckCodeTool ->
        case fromJSON callToolArguments of
          Success (TypecheckCodeToolArguments {code, projectContext}) -> do
            output <- handleInputMCP projectContext [Left $ UnisonFileChanged "scratch.u" code]
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
            pure $
              CallToolResult
                { callToolIsError = False,
                  callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
                }
          Error {} -> pure $ CallToolResult [] True
      Just DocsTool ->
        case fromJSON callToolArguments of
          Success (DocsToolArguments {name, projectContext}) -> do
            output <- handleInputMCP projectContext [Right $ DocToMarkdownI name]
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
            pure $
              CallToolResult
                { callToolIsError = False,
                  callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
                }
          Error {} -> pure $ CallToolResult [] True

  -- Start the server with StdIO transport
  runServerWithSTDIO server

_projectCodeTool :: Tool
_projectCodeTool =
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
            "projectContext": {
              "type": "object",
              "properties": {
                "projectName": {
                  "type": "string",
                  "description": "The name of the project to fetch code for"
                },
                "branchName": {
                  "type": "string",
                  "description": "The branch of the project to fetch code for"
                }
              },
              "required": ["projectName", "branchName"]
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
      toolInputSchema =
        fromMaybe (error "Invalid projectCodeTool schema") $
          Aeson.decode $
            [r|
        {
          "type": "object",
          "properties": {
            "projectContext": {
              "type": "object",
              "properties": {
                "projectName": {
                  "type": "string",
                  "description": "The name of the project to install the library into"
                },
                "branchName": {
                  "type": "string",
                  "description": "The branch of the project to install the library into"
                }
              },
              "required": ["projectName", "branchName"]
            },
            "libProjectName": {
              "type": "string",
              "description": "The name of the library project to install"
            },
            "libBranchName": {
              "type": ["string", "null"],
              "description": "The optional branch of the library project to install. If null, the latest release will be used."
            }
          },
          "required": ["libProjectName"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Install Library",
              readOnlyHint = Just False,
              destructiveHint = Just True,
              idempotentHint = Just False,
              openWorldHint = Just True
            }
    }

shareProjectSearchTool :: Tool
shareProjectSearchTool =
  Tool
    { toolName = toToolName ShareProjectSearchTool,
      toolDescription = Just "Search Unison Share for projects and libraries.",
      toolInputSchema =
        fromMaybe (error "Invalid shareProjectSearchTool schema") $
          Aeson.decode $
            [r|
        {
          "type": "object",
          "properties": {
            "query": {
              "type": "string",
              "description": "The search query to use. Must only be a single word."
            }
          },
          "required": ["query"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Share Project Search",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just True
            }
    }

typecheckCodeTool :: Tool
typecheckCodeTool =
  Tool
    { toolName = toToolName TypecheckCodeTool,
      toolDescription =
        Just
          [r| Typecheck a code snippet within the context of a project. Only definitions which which are part of libraries or which have been previously added or updated will be available to reference within code.

          The result will indicate any errors and suggested fixes, or will indicate that the code typechecks and is ready to add or update.

          If you would like to test the behaviour of any pure functions, you may prefix a code snippet with an angle bracket.

          e.g.
          ```
          > 1 + 2
          ```

          Or
          ```
          > let
              isGreatarThan3 x = x > 3
              isGreatarThan3 4
        |],
      toolInputSchema =
        fromMaybe (error "Invalid typecheckCodeTool schema") $
          Aeson.decode $
            [r|
        {
          "type": "object",
          "properties": {
            "code": {
              "type": "string",
              "description": "The code to typecheck, as a string."
            },
            "projectContext": {
              "type": "object",
              "properties": {
                "projectName": {
                  "type": "string",
                  "description": "The name of the project to typecheck"
                },
                "branchName": {
                  "type": "string",
                  "description": "The branch of the project to typecheck"
                }
              },
              "required": ["projectName", "branchName"]
            }
          },
          "required": ["code", "projectContext"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Typecheck Code",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

docsTool :: Tool
docsTool =
  Tool
    { toolName = toToolName DocsTool,
      toolDescription = Just "Fetch documentation for the given definition.",
      toolInputSchema =
        fromMaybe (error "Invalid docsTool schema") $
          Aeson.decode $
            [r|
        {
          "type": "object",
          "properties": {
            "name": {
              "type": "string",
              "description": "The definition name to fetch documentation for. E.g. `README` or `data.Map.fromList`"
            },
            "projectContext": {
              "type": "object",
              "properties": {
                "projectName": {
                  "type": "string",
                  "description": "The name of the project to fetch documentation from"
                },
                "branchName": {
                  "type": "string",
                  "description": "The branch of the project to fetch documentation from"
                }
              },
              "required": ["projectName", "branchName"]
            }
          },
          "required": ["name", "projectContext"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Documentation",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

projectReadmeTool :: Tool
projectReadmeTool =
  Tool
    { toolName = toToolName ShareProjectReadmeTool,
      toolDescription = Just "Fetch the README for a project from Unison Share.",
      toolInputSchema =
        fromMaybe (error "Invalid projectReadmeTool schema") $
          Aeson.decode $
            [r|
        {
          "type": "object",
          "properties": {
            "projectName": {
              "type": "string",
              "description": "The name of the project to fetch the README for. E.g. in a project reference like `@owner/project-name` this would be `project-name`"
            },
            "projectOwnerHandle": {
              "type": "string",
              "description": "The handle of the project owner, e.g. in a project reference like `@owner/project-name` this would be `owner`"
            }
          },
          "required": ["projectName", "projectOwnerHandle"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Project README",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just True
            }
    }
