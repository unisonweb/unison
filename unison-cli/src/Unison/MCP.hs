module Unison.MCP (runOnStdIO) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (Result (..), fromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.These (These (..))
import Network.MCP.Server
import Network.MCP.Server.StdIO
import Network.MCP.Types
import Text.RawString.QQ (r)
import Unison.Auth.CredentialManager qualified as AuthN
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Auth.Tokens qualified as AuthN
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput.InstallLib (handleInstallLib)
import Unison.Codebase.Editor.Input (Event (..), FindScope (..), Input (..))
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath
import Unison.Codebase.Runtime (Runtime)
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.HashQualified qualified as HQ
import Unison.MCP.Cli (cliToMCP, handleInputMCP)
import Unison.MCP.Share.API (ReadmeResponse (..))
import Unison.MCP.Share.API qualified as Share
import Unison.MCP.StaticResources (staticResources, unisonGuideText)
import Unison.MCP.Types
import Unison.NameSegment qualified as NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Project (ProjectAndBranchNames (ProjectAndBranchNames'Unambiguous), ProjectBranchNameOrLatestRelease (..))
import Unison.Symbol (Symbol)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Relation qualified as R
import UnliftIO qualified

serverDescription :: Text
serverDescription =
  [r|
        This server provides tools for interacting with Unison Code locally, such as typechecking or reading documentation, as well as tools for searching Unison Share, which is a platform for sharing Unison projects and libraries.

        It also provides some mechanisms for editing and updating local Unison projects, such as installing libraries from Unison Share.

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
            promptsCapability = Just $ PromptsCapability True
          }

  server <- createServer serverInfo serverCapabilities serverDescription

  registerResources server (fst <$> toList staticResources)

  -- Register resource read handler
  registerResourceReadHandler server $ \(ReadResourceRequest {resourceReadUri}) -> do
    case Map.lookup resourceReadUri staticResources of
      Just (_, content) ->
        pure . ReadResourceResult $ [content]
      _ -> pure $ ReadResourceResult []

  registerPrompts server [writeUnisonCodePrompt]

  registerPromptHandler server $ \(GetPromptRequest {getPromptName, getPromptArguments}) -> do
    case getPromptName of
      pName
        | pName == promptName writeUnisonCodePrompt -> do
            pure $
              GetPromptResult
                { getPromptDescription = Just "Ask the agent to write some Unison code for you.",
                  getPromptMessages =
                    [ PromptMessage
                        { promptMessageRole = "assistant",
                          promptMessageContent =
                            PromptContent
                              { promptContentType = TextPromptContent,
                                promptContentText =
                                  Text.unlines
                                    [ "Your role is to be a helpful Unison programming assistant. You will be given a description of a program to write in Unison, and you should write the code to implement it.",
                                      case Map.lookup "preferred-libraries" getPromptArguments of
                                        Just preferredLibs -> "You should use the following unison libraries to accomplish the task if they are applicable: " <> preferredLibs <> " if they are not already installed, you may search share for the libraries and then install them."
                                        Nothing -> "",
                                      "After implementing the code, ensure you typecheck it, and add watch expressions to test any pure functions.",
                                      "You can use the tools available to search Unison Share and the local project and its dependencies for definitions and documentation to help you accomplish your task.",
                                      "",
                                      "You should use the following guidelines when writing Unison code:",
                                      "",
                                      unisonGuideText
                                    ]
                              }
                        }
                    ]
                }
        | otherwise -> error $ "Unknown prompt: " <> Text.unpack pName

  registerToolHandlers env server $
    [ mkToolHandler installLibTool \(LibInstallToolArguments {projectContext, libProjectName, libBranchName}) -> do
        (_r, output) <- lift $ cliToMCP projectContext $ do
          handleInstallLib False (ProjectAndBranch (UnsafeProjectName libProjectName) (ProjectBranchNameOrLatestRelease'Name . UnsafeProjectBranchName <$> libBranchName))
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            },
      mkToolHandler shareProjectSearchTool \(ShareProjectSearchToolArguments {query}) -> do
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
                },
      mkToolHandler typecheckCodeTool \(TypecheckCodeToolArguments {code, projectContext}) -> do
        output <- lift $ handleInputMCP projectContext [Left $ UnisonFileChanged "scratch.u" code]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            },
      mkToolHandler docsTool \(DocsToolArguments {name, projectContext}) -> do
        output <- lift $ handleInputMCP projectContext [Right $ DocToMarkdownI name]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            },
      mkToolHandler shareProjectReadmeTool \(ShareProjectReadmeToolArguments {projectName, projectOwnerHandle}) -> do
        result <- UnliftIO.liftIO $ Share.shareProjectReadme authenticatedHTTPClient projectOwnerHandle projectName
        case result of
          Right ReadmeResponse {markdownReadMe} -> do
            pure $
              CallToolResult
                { callToolIsError = False,
                  callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just markdownReadMe}]
                }
          Left err -> do
            let errorMsg = "Error getting readme from Unison Share: " <> Text.pack (show err)
            pure $
              CallToolResult
                { callToolIsError = True,
                  callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just errorMsg}]
                },
      mkToolHandler listProjectDefinitionsTool \(ProjectContextArgument projectContext) -> do
        lift $
          cliToMCP projectContext Cli.getCurrentBranch0 >>= \case
            (Just b, _output) -> do
              let noLibBranch = Branch.deleteLibdeps b
              if (R.null $ Branch.deepTerms noLibBranch) && (R.null $ Branch.deepTypes noLibBranch)
                then
                  pure $
                    CallToolResult
                      { callToolIsError = False,
                        callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just "No definitions found in the project. There may be definitions within the project's installed libraries."}]
                      }
                else do
                  definitions <- handleInputMCP projectContext [Right $ Input.FindI False (FindLocal Path.Root') []]
                  let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
                  pure $
                    CallToolResult
                      { callToolIsError = False,
                        callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
                      }
            _ ->
              pure $
                CallToolResult
                  { callToolIsError = True,
                    callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just "No current branch found"}]
                  },
      mkToolHandler listProjectLibrariesTool \(ProjectContextArgument projectContext) -> do
        let libPath = Path.AbsolutePath' $ Path.Absolute (Path.fromList [NameSegment.libSegment])
        output <- lift $ handleInputMCP projectContext [Right $ Input.FindShallowI libPath]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            },
      mkToolHandler listLibraryDefinitionsTool \(ListLibraryDefinitionsToolArguments {libName, projectContext}) -> do
        let libPath = Path.AbsolutePath' $ Path.Absolute (Path.fromList [NameSegment.libSegment, NameSegment.unsafeParseText libName])
        definitions <- lift $ handleInputMCP projectContext [Right $ Input.FindI False (FindLocal libPath) []]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            },
      mkToolHandler searchDefinitionsTool \(SearchDefinitionsToolArguments {projectContext, query}) -> do
        definitions <- lift $ handleInputMCP projectContext [Right $ Input.FindI False (FindLocal Path.Root') [Text.unpack query]]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            },
      mkToolHandler searchByTypeTool \(SearchByTypeToolArguments {projectContext, query}) -> do
        definitions <- lift $ handleInputMCP projectContext [Right $ Input.FindI False (FindLocal Path.Root') [":", Text.unpack query]]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            },
      mkToolHandler viewDefinitionsTool \(ViewDefinitionsToolArguments {projectContext, names}) -> do
        case NEL.nonEmpty names of
          Nothing ->
            pure $
              CallToolResult
                { callToolIsError = True,
                  callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just "No names provided to view definitions"}]
                }
          Just nonEmptyNames -> do
            let names' = HQ.NameOnly <$> nonEmptyNames
            definitions <- lift $ handleInputMCP projectContext [Right $ Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal names']
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
            pure $
              CallToolResult
                { callToolIsError = False,
                  callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
                },
      mkToolHandler listLocalProjectsTool \(()) -> do
        pc <- lift $ currentProjectContext
        projects <- lift $ handleInputMCP pc [Right Input.ProjectsI]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode projects
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            },
      mkToolHandler listProjectBranchesTool \(ProjectNameArgument {projectName}) -> do
        projectContext <- lift $ currentProjectContext
        branches <- lift $ handleInputMCP projectContext [Right $ Input.BranchesI (Just projectName)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode branches
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            },
      mkToolHandler getCurrentProjectContextTool \(()) -> do
        projectContext <- lift $ currentProjectContext
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode projectContext
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            },
      mkToolHandler setCurrentProjectContextTool \(ProjectContextArgument projectContext) -> do
        -- Set the current project context
        output <- lift $ handleInputMCP projectContext [Right $ Input.ProjectSwitchI (ProjectAndBranchNames'Unambiguous $ These projectContext.projectName projectContext.branchName)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $
          CallToolResult
            { callToolIsError = False,
              callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just outputJSON}]
            }
    ]

  -- Start the server with StdIO transport
  runServerWithSTDIO server

currentProjectContext :: MCP ProjectContext
currentProjectContext = do
  Env {codebase} <- ask
  pp <- liftIO $ Codebase.runTransaction codebase $ Codebase.expectCurrentProjectPath
  pure $
    ProjectContext
      { projectName = pp.project.name,
        branchName = pp.branch.name
      }

data ToolHandler = ToolHandler
  { tool :: Tool,
    handler :: Aeson.Value -> MCP CallToolResult
  }

mkToolHandler :: (Aeson.FromJSON args) => Tool -> (args -> ExceptT Text MCP CallToolResult) -> ToolHandler
mkToolHandler tool handlerFunc =
  ToolHandler
    { tool,
      handler = \v -> do
        runExceptT (decodeHandler handlerFunc v) >>= \case
          Left err -> pure $ CallToolResult {callToolIsError = True, callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just $ err}]}
          Right result -> pure result
    }
  where
    decodeHandler :: (Aeson.FromJSON args, Applicative m) => (args -> m CallToolResult) -> Aeson.Value -> m CallToolResult
    decodeHandler handlerFunc v = do
      case fromJSON v of
        Success args -> handlerFunc args
        Error err -> pure $ CallToolResult {callToolIsError = True, callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just $ "Invalid arguments: " <> Text.pack err}]}

registerToolHandlers :: Env -> Server -> [ToolHandler] -> IO ()
registerToolHandlers env server handlers = do
  registerTools server (tool <$> handlers)
  registerToolCallHandler server $ \(CallToolRequest {callToolName, callToolArguments}) -> do
    case List.find (\(ToolHandler {tool = Tool {toolName}}) -> toolName == callToolName) handlers of
      Nothing ->
        pure $ CallToolResult {callToolIsError = True, callToolContent = [ToolContent {toolContentType = TextualContent, toolContentText = Just $ "Unknown tool: " <> callToolName}]}
      Just ToolHandler {handler} -> do
        runMCP env $ do
          handler callToolArguments

-- _projectCodeTool :: Tool
-- _projectCodeTool =
--   Tool
--     { toolName = toToolName ProjectCodeTool,
--       toolDescription = Just "Fetch all of the code within a project",
--       -- JSON Schema for the tool input.
--       -- Which is just the name of the project.
--       toolInputSchema =
--         fromMaybe (error "Invalid projectCodeTool schema") $
--           Aeson.decode $
--             [r|
--         {
--           "type": "object",
--           "properties": {
--             "projectContext": {
--               "type": "object",
--               "properties": {
--                 "projectName": {
--                   "type": "string",
--                   "description": "The name of the project to fetch code for"
--                 },
--                 "branchName": {
--                   "type": "string",
--                   "description": "The branch of the project to fetch code for"
--                 }
--               },
--               "required": ["projectName", "branchName"]
--             }
--           }
--         }
--         |],
--       toolAnnotations =
--         Just $
--           ToolAnnotations
--             { title = Just "Project Code",
--               readOnlyHint = Just True,
--               destructiveHint = Just False,
--               idempotentHint = Just True,
--               openWorldHint = Just False
--             }
--     }

installLibTool :: Tool
installLibTool =
  Tool
    { toolName = toToolName LibInstallTool,
      toolDescription = Just "Install a library from Unison Share into the specified project.",
      toolInputSchema =
        fromMaybe (error "Invalid Lib Tool schema") $
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
              "description": "The uer-qualified name of the library project to install, e.g. `@unison/base` or `@ceedubs/json`"
            },
            "libBranchName": {
              "type": ["string", "null"],
              "description": "The optional branch of the library project to install, E.g. `main`. If null, the latest release will be used."
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
              "description": "The search query to use. E.g. \"http client\". By default, each search word is ANDed together, but you can use OR to search for multiple terms. E.g. \"http OR client\" will return results that match either term. You can also exclude results using \"-\", e.g. \"-http\" will exclude results that match the term \"http\". Wrap a term in quotes to search for an exact phrase, e.g. \"\"http client\"\" will search for the exact phrase \"http client\"."
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
              isGreaterThan3 x = x > 3
              isGreaterThan3 4
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
              "description": "The code to typecheck, as a string. All the code you've written which is not yet part of the project must be provided at once."
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
      toolDescription = Just "Fetch documentation for a definition in a local project.",
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

shareProjectReadmeTool :: Tool
shareProjectReadmeTool =
  Tool
    { toolName = toToolName ShareProjectReadmeTool,
      toolDescription = Just "Fetch the README for a project from Unison Share. Read the markdownReadMe value in the response.",
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

listProjectDefinitionsTool :: Tool
listProjectDefinitionsTool =
  Tool
    { toolName = toToolName ListProjectDefinitionsTool,
      toolDescription = Just "List all definitions in the provided project.",
      toolInputSchema =
        fromMaybe (error "Invalid listProjectDefinitionsTool schema") $
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
                  "description": "The name of the project to list definitions for"
                },
                "branchName": {
                  "type": "string",
                  "description": "The branch of the project to list definitions for"
                }
              },
              "required": ["projectName", "branchName"]
            }
          },
          "required": ["projectContext"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "List Project Definitions",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

listProjectLibrariesTool :: Tool
listProjectLibrariesTool =
  Tool
    { toolName = toToolName ListProjectLibrariesTool,
      toolDescription = Just "List the all libraries in the provided project's lib namespace.",
      toolInputSchema =
        fromMaybe (error "Invalid listProjectLibrariesTool schema") $
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
                  "description": "The name of the project to list libraries within."
                },
                "branchName": {
                  "type": "string",
                  "description": "The branch of the project to list libraries within."
                }
              },
              "required": ["projectName", "branchName"]
            }
          },
          "required": ["projectContext"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "List Project Libraries",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

listLibraryDefinitionsTool :: Tool
listLibraryDefinitionsTool =
  Tool
    { toolName = toToolName ListLibraryDefinitionsTool,
      toolDescription = Just "List all definitions in the specified library.",
      toolInputSchema =
        fromMaybe (error "Invalid listLibraryDefinitionsTool schema") $
          Aeson.decode $
            [r|
        {
          "type": "object",
          "properties": {
            "libName": {
              "type": "string",
              "description": "The name of the library to list definitions for, e.g. \"unison_base_1_0_0\""
            },
            "projectContext": {
              "type": "object",
              "properties": {
                "projectName": {
                  "type": "string",
                  "description": "The name of the project to list definitions for"
                },
                "branchName": {
                  "type": "string",
                  "description": "The branch of the project to list definitions for"
                }
              },
              "required": ["projectName", "branchName"]
            }
          },
          "required": ["projectContext", "libName"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "List Library Definitions",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

viewDefinitionsTool :: Tool
viewDefinitionsTool =
  Tool
    { toolName = toToolName ViewDefinitionsTool,
      toolDescription = Just "View the source code of the specified definitions. Definitions inside a library must be prefixed by their full library prefix, e.g. `lib.unison_base_1_0_0.data.List`",
      toolInputSchema =
        fromMaybe (error "Invalid viewDefinitionsTool schema") $
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
                  "description": "The name of the project to view definitions for"
                },
                "branchName": {
                  "type": "string",
                  "description": "The branch of the project to view definitions for"
                }
              },
              "required": ["projectName", "branchName"]
            },
            "names": {
              "type": "array",
              "items": {
                "type": "string",
                "description": "The names of the definitions to view, e.g. `mynamespace.foo` or `lib.unison_base_1_0_0.data.List`"
              },
              "description": "The names of the definitions to view"
            }
          },
          "required": ["projectContext", "names"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "View Definitions",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

listLocalProjectsTool :: Tool
listLocalProjectsTool =
  Tool
    { toolName = toToolName ListLocalProjectsTool,
      toolDescription = Just "List all local projects.",
      toolInputSchema =
        fromMaybe (error "Invalid listLocalProjectsTool schema") $
          Aeson.decode $
            [r|
        {
          "type": "object",
          "properties": {},
          "required": []
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "List Local Projects",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

listProjectBranchesTool :: Tool
listProjectBranchesTool =
  Tool
    { toolName = toToolName ListProjectBranchesTool,
      toolDescription = Just "List all branches of a project.",
      toolInputSchema =
        fromMaybe (error "Invalid listProjectBranchesTool schema") $
          Aeson.decode $
            [r|
        {
          "type": "object",
          "properties": {
            "projectName": {
              "type": "string",
              "description": "The name of the project to list branches for"
            }
          },
          "required": ["projectName"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "List Project Branches",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

getCurrentProjectContextTool :: Tool
getCurrentProjectContextTool =
  Tool
    { toolName = toToolName GetCurrentProjectContextTool,
      toolDescription = Just "Get the current project context.",
      toolInputSchema =
        fromMaybe (error "Invalid getCurrentProjectContextTool schema") $
          Aeson.decode $
            [r|
          { "type": "object",
            "properties": {},
            "required": []
          }
            |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Get Current Project Context",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

setCurrentProjectContextTool :: Tool
setCurrentProjectContextTool =
  Tool
    { toolName = toToolName SetCurrentProjectContextTool,
      toolDescription = Just "Set the current project context.",
      toolInputSchema =
        fromMaybe (error "Invalid setCurrentProjectContextTool schema") $
          Aeson.decode $
            [r|
        {
          "type": "object",
          "properties": {
            "projectName": {
              "type": "string",
              "description": "The name of the project to set as the current project"
            },
            "branchName": {
              "type": "string",
              "description": "The branch of the project to set as the current branch"
            }
          },
          "required": ["projectName", "branchName"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Set Current Project Context",
              readOnlyHint = Just False,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

searchDefinitionsTool :: Tool
searchDefinitionsTool =
  Tool
    { toolName = toToolName SearchDefinitionsTool,
      toolDescription = Just "Search for definitions in the current project or its library dependencies by name.",
      toolInputSchema =
        fromMaybe (error "Invalid searchDefinitionsTool schema") $
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
                  "description": "The name of the project to search within"
                },
                "branchName": {
                  "type": "string",
                  "description": "The branch of the project to search within"
                }
              },
              "required": ["projectName", "branchName"]
            },
            "query": {
              "type": "string",
              "description": "A name to search for, e.g. `foldl`."
            }
          },
        "required": ["projectContext", "query"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Search Definitions By Name",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

searchByTypeTool :: Tool
searchByTypeTool =
  Tool
    { toolName = toToolName SearchByTypeTool,
      toolDescription = Just "Search for definitions in the current project or its library dependencies by type.",
      toolInputSchema =
        fromMaybe (error "Invalid searchDefinitionsTool schema") $
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
                  "description": "The name of the project to search within"
                },
                "branchName": {
                  "type": "string",
                  "description": "The branch of the project to search within"
                }
              },
              "required": ["projectName", "branchName"]
            },
            "query": {
              "type": "string",
              "description": "A type to search for, e.g. `[Nat] -> Nat`."
            }
          },
        "required": ["projectContext", "query"]
        }
        |],
      toolAnnotations =
        Just $
          ToolAnnotations
            { title = Just "Search Definitions By Type",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just False
            }
    }

--- PROMPTs

writeUnisonCodePrompt :: Prompt
writeUnisonCodePrompt =
  Prompt
    { promptName = "unison-programming-assistant",
      promptDescription = Just "Unison Programming Assistant",
      promptArguments =
        [ PromptArgument
            { promptArgumentName = "project-and-branch",
              promptArgumentDescription = Just "[Optional] The Unison project and branch this code should be implemented in. E.g. scratch/main",
              promptArgumentRequired = False
            },
          PromptArgument
            { promptArgumentName = "preferred-libraries",
              promptArgumentDescription = Just "[Optional] Specific libraries you'd like to be used. E.g. `@ceedubs/json and @unison/base`",
              promptArgumentRequired = False
            }
        ]
    }
