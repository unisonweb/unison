module Unison.MCP (runOnStdIO) where

import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.These (These (..))
import Network.MCP.Server.StdIO qualified as MCP
import Network.MCP.Types qualified as MCP
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
import Unison.MCP.Wrapper
import Unison.MCP.Wrapper qualified as MCPWrapper
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

  server <- runMCP env $ MCPWrapper.mkServer serverInfo serverDescription staticResources tools prompts

  -- Start the server with StdIO transport
  MCP.runServerWithSTDIO server

tools :: [MCPWrapper.Tool MCP]
tools =
  [ installLibTool,
    shareProjectSearchTool,
    typecheckCodeTool,
    docsTool,
    shareProjectReadmeTool,
    listProjectDefinitionsTool,
    listProjectLibrariesTool,
    listLibraryDefinitionsTool,
    viewDefinitionsTool,
    listLocalProjectsTool,
    listProjectBranchesTool,
    getCurrentProjectContextTool,
    setCurrentProjectContextTool,
    searchDefinitionsTool,
    searchByTypeTool
  ]

prompts :: [MCPWrapper.Prompt MCP]
prompts =
  [ writeUnisonCodePrompt
  ]

currentProjectContext :: MCP ProjectContext
currentProjectContext = do
  Env {codebase} <- ask
  pp <- liftIO $ Codebase.runTransaction codebase $ Codebase.expectCurrentProjectPath
  pure $
    ProjectContext
      { projectName = pp.project.name,
        branchName = pp.branch.name
      }

-- _projectCodeTool :: Tool
-- _projectCodeTool =
--   Tool
--     { toolName = toToolName ProjectCodeTool,
--       toolDescription = Just "Fetch all of the code within a project",
--       -- JSON Schema for the tool input.
--       -- Which is just the name of the project.
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

installLibTool :: Tool MCP
installLibTool =
  Tool
    { toolName = toToolName LibInstallTool,
      toolDescription = "Install a library from Unison Share into the specified project.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Install Library",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just True
          },
      toolArgType = Proxy,
      toolHandler = \(LibInstallToolArguments {projectContext, libProjectName, libBranchName}) -> do
        (_r, output) <- cliToMCP projectContext $ do
          handleInstallLib False (ProjectAndBranch (UnsafeProjectName libProjectName) (ProjectBranchNameOrLatestRelease'Name . UnsafeProjectBranchName <$> libBranchName))
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

shareProjectSearchTool :: Tool MCP
shareProjectSearchTool =
  Tool
    { toolName = toToolName ShareProjectSearchTool,
      toolDescription = "Search Unison Share for projects and libraries.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Share Project Search",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just True
          },
      toolArgType = Proxy,
      toolHandler = \(ShareProjectSearchToolArguments {query}) -> do
        Env {authenticatedHTTPClient} <- ask
        result <- UnliftIO.liftIO $ Share.shareSearch authenticatedHTTPClient query
        case result of
          Right searchResult -> do
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode searchResult
            pure $ textToolResult outputJSON
          Left err -> do
            let errorMsg = "Error searching Unison Share: " <> Text.pack (show err)
            pure $ errorToolResult errorMsg
    }

typecheckCodeTool :: Tool MCP
typecheckCodeTool =
  Tool
    { toolName = toToolName TypecheckCodeTool,
      toolDescription =
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
      toolAnnotations =
        ToolAnnotations
          { title = Just "Typecheck Code",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(TypecheckCodeToolArguments {code, projectContext}) -> do
        output <- handleInputMCP projectContext [Left $ UnisonFileChanged "scratch.u" code]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

docsTool :: Tool MCP
docsTool =
  Tool
    { toolName = toToolName DocsTool,
      toolDescription = "Fetch documentation for a definition in a local project.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Documentation",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(DocsToolArguments {name, projectContext}) -> do
        output <- handleInputMCP projectContext [Right $ DocToMarkdownI name]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

shareProjectReadmeTool :: Tool MCP
shareProjectReadmeTool =
  Tool
    { toolName = toToolName ShareProjectReadmeTool,
      toolDescription = "Fetch the README for a project from Unison Share. Read the markdownReadMe value in the response.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Project README",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just True
          },
      toolArgType = Proxy,
      toolHandler = \(ShareProjectReadmeToolArguments {projectName, projectOwnerHandle}) -> do
        Env {authenticatedHTTPClient} <- ask
        result <- UnliftIO.liftIO $ Share.shareProjectReadme authenticatedHTTPClient projectOwnerHandle projectName
        case result of
          Right ReadmeResponse {markdownReadMe} -> do
            pure $ textToolResult markdownReadMe
          Left err -> do
            let errorMsg = "Error getting readme from Unison Share: " <> Text.pack (show err)
            pure $ errorToolResult errorMsg
    }

listProjectDefinitionsTool :: Tool MCP
listProjectDefinitionsTool =
  Tool
    { toolName = toToolName ListProjectDefinitionsTool,
      toolDescription = "List all definitions in the provided project.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List Project Definitions",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ProjectContextArgument projectContext) -> do
        output <-
          cliToMCP projectContext Cli.getCurrentBranch0 >>= \case
            (Just b, _output) -> do
              let noLibBranch = Branch.deleteLibdeps b
              if (R.null $ Branch.deepTerms noLibBranch) && (R.null $ Branch.deepTypes noLibBranch)
                then pure $ textToolResult "No definitions found in the project. There may be definitions within the project's installed libraries."
                else jsonToolResult <$> handleInputMCP projectContext [Right $ Input.FindI False (FindLocal Path.Root') []]
            _ -> pure . errorToolResult $ "No current branch found"
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

listProjectLibrariesTool :: Tool MCP
listProjectLibrariesTool =
  Tool
    { toolName = toToolName ListProjectLibrariesTool,
      toolDescription = "List the all libraries in the provided project's lib namespace.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List Project Libraries",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ProjectContextArgument projectContext) -> do
        let libPath = Path.AbsolutePath' $ Path.Absolute (Path.fromList [NameSegment.libSegment])
        output <- handleInputMCP projectContext [Right $ Input.FindShallowI libPath]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

listLibraryDefinitionsTool :: Tool MCP
listLibraryDefinitionsTool =
  Tool
    { toolName = toToolName ListLibraryDefinitionsTool,
      toolDescription = "List all definitions in the specified library.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List Library Definitions",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ListLibraryDefinitionsToolArguments {libName, projectContext}) -> do
        let libPath = Path.AbsolutePath' $ Path.Absolute (Path.fromList [NameSegment.libSegment, NameSegment.unsafeParseText libName])
        definitions <- handleInputMCP projectContext [Right $ Input.FindI False (FindLocal libPath) []]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
        pure $ textToolResult outputJSON
    }

viewDefinitionsTool :: Tool MCP
viewDefinitionsTool =
  Tool
    { toolName = toToolName ViewDefinitionsTool,
      toolDescription = "View the source code of the specified definitions. Definitions inside a library must be prefixed by their full library prefix, e.g. `lib.unison_base_1_0_0.data.List`",
      toolAnnotations =
        ToolAnnotations
          { title = Just "View Definitions",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ViewDefinitionsToolArguments {projectContext, names}) -> do
        case NEL.nonEmpty names of
          Nothing ->
            pure $ errorToolResult "No names provided to view definitions"
          Just nonEmptyNames -> do
            let names' = HQ.NameOnly <$> nonEmptyNames
            definitions <- handleInputMCP projectContext [Right $ Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal names']
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
            pure $ textToolResult outputJSON
    }

listLocalProjectsTool :: Tool MCP
listLocalProjectsTool =
  Tool
    { toolName = toToolName ListLocalProjectsTool,
      toolDescription = "List all local projects.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List Local Projects",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(()) -> do
        pc <- currentProjectContext
        projects <- handleInputMCP pc [Right Input.ProjectsI]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode projects
        pure $ textToolResult outputJSON
    }

listProjectBranchesTool :: Tool MCP
listProjectBranchesTool =
  Tool
    { toolName = toToolName ListProjectBranchesTool,
      toolDescription = "List all branches of a project.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List Project Branches",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ProjectNameArgument {projectName}) -> do
        projectContext <- currentProjectContext
        branches <- handleInputMCP projectContext [Right $ Input.BranchesI (Just projectName)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode branches
        pure $ textToolResult outputJSON
    }

getCurrentProjectContextTool :: Tool MCP
getCurrentProjectContextTool =
  Tool
    { toolName = toToolName GetCurrentProjectContextTool,
      toolDescription = "Get the current project context.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Get Current Project Context",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \() -> do
        projectContext <- currentProjectContext
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode projectContext
        pure $ textToolResult outputJSON
    }

setCurrentProjectContextTool :: Tool MCP
setCurrentProjectContextTool =
  Tool
    { toolName = toToolName SetCurrentProjectContextTool,
      toolDescription = "Set the current project context.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Set Current Project Context",
            readOnlyHint = Just False,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ProjectContextArgument projectContext) -> do
        -- Set the current project context
        output <- handleInputMCP projectContext [Right $ Input.ProjectSwitchI (ProjectAndBranchNames'Unambiguous $ These projectContext.projectName projectContext.branchName)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

searchDefinitionsTool :: Tool MCP
searchDefinitionsTool =
  Tool
    { toolName = toToolName SearchDefinitionsTool,
      toolDescription = "Search for definitions in the current project or its library dependencies by name.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Search Definitions By Name",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(SearchDefinitionsToolArguments {projectContext, query}) -> do
        definitions <- handleInputMCP projectContext [Right $ Input.FindI False (FindLocal Path.Root') [Text.unpack query]]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
        pure $ textToolResult outputJSON
    }

searchByTypeTool :: Tool MCP
searchByTypeTool =
  Tool
    { toolName = toToolName SearchByTypeTool,
      toolDescription = "Search for definitions in the current project or its library dependencies by type.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Search Definitions By Type",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(SearchByTypeToolArguments {projectContext, query}) -> do
        definitions <- handleInputMCP projectContext [Right $ Input.FindI False (FindLocal Path.Root') [":", Text.unpack query]]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
        pure $ textToolResult outputJSON
    }

--- PROMPTs

writeUnisonCodePrompt :: Prompt MCP
writeUnisonCodePrompt =
  Prompt
    { promptName = "unison-programming-assistant",
      promptDescription = "Unison Programming Assistant",
      promptArgs =
        Map.fromList
          [ ( "project-and-branch",
              PromptArgument
                { promptArgumentDescription = "[Optional] The Unison project and branch this code should be implemented in. E.g. scratch/main",
                  promptArgumentRequired = False
                }
            ),
            ( "preferred-libraries",
              PromptArgument
                { promptArgumentDescription = "[Optional] Specific libraries you'd like to be used. E.g. `@ceedubs/json and @unison/base`",
                  promptArgumentRequired = False
                }
            )
          ],
      promptHandler = \args -> do
        pure $
          MCP.GetPromptResult
            { getPromptDescription = Just "Ask the agent to write some Unison code for you.",
              getPromptMessages =
                [ MCP.PromptMessage
                    { promptMessageRole = "assistant",
                      promptMessageContent =
                        MCP.PromptContent
                          { promptContentType = TextPromptContent,
                            promptContentText =
                              Text.unlines
                                [ "Your role is to be a helpful Unison programming assistant. You will be given a description of a program to write in Unison, and you should write the code to implement it.",
                                  case Map.lookup "preferred-libraries" args of
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
    }
