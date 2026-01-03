module Unison.MCP.Tools (tools) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy (..))
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Text.RawString.QQ (r)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput.InstallLib (handleInstallLib)
import Unison.Codebase.Editor.Input (Event (..), FindScope (..), Input (..))
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath
import Unison.Codebase.Runtime.Profile (ProfileSpec (..))
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.MCP.Cli (cliToMCP, handleInputMCP, virtualSourceName)
import Unison.MCP.Share.API (ReadmeResponse (..))
import Unison.MCP.Share.API qualified as Share
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.MCP.Wrapper qualified as MCPWrapper
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude (readUtf8)
import Unison.Project (ProjectBranchNameOrLatestRelease (..))
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Relation qualified as R
import UnliftIO qualified

-- MCP errors are just returned to the agent as text.
type MCPError = Text

type EMCP = ExceptT MCPError MCP

tools :: [MCPWrapper.Tool MCP]
tools =
  [ installLibTool,
    shareProjectSearchTool,
    typecheckCodeTool,
    docsTool,
    runTool,
    shareProjectReadmeTool,
    listProjectDefinitionsTool,
    listProjectLibrariesTool,
    listLibraryDefinitionsTool,
    viewDefinitionsTool,
    updateTool,
    listLocalProjectsTool,
    listProjectBranchesTool,
    getCurrentProjectContextTool,
    searchDefinitionsTool,
    searchByTypeTool,
    dependenciesTool,
    dependentsTool,
    runTestsTool,
    deleteDefinitionsTool,
    renameDefinitionTool,
    moveDefinitionTool,
    moveToTool,
    deleteNamespaceTool
  ]

currentProjectContext :: (MonadIO m, MonadReader Env m) => m ProjectContext
currentProjectContext = do
  Env {codebase} <- ask
  pp <- liftIO $ Codebase.runTransaction codebase $ Codebase.expectCurrentProjectPath
  pure $
    ProjectContext
      { projectName = pp.project.name,
        branchName = pp.branch.name
      }

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
      toolHandler = \(LibInstallToolArguments {projectContext, libProjectName, libBranchName}) -> handleToolError $ do
        (_r, output) <- cliToMCP projectContext (const $ pure ()) $ do
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

-- | Load and typecheck the provided code, THEN run the provided inputs within that scratchfile context.
withCode :: Either FilePath Text -> [Input] -> ProjectContext -> EMCP CallToolResult
withCode code inputs projectContext = do
  (filePath, source) <- case code of
    Left filePath -> (Text.pack filePath,) <$> liftIO (readUtf8 filePath)
    Right codeSnippet -> pure (virtualSourceName, codeSnippet)
  output <- handleInputMCP projectContext ([Left $ UnisonFileChanged filePath source] <> (Right <$> inputs))
  let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
  pure $ textToolResult outputJSON

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
          ```

          If you wish to write unit tests, you may do so like this:

          ```
          test> Nat.tests.additionIsCommutative = test.verify do
            Each.repeat 100
            n = Random.natIn 0 1000
            m = Random.natIn 0 1000
            ensureEqual (n + m) (m + n)
          ```

          If you intend to update code, you may call the Update Definitions tool directly instead, it will typecheck and update in one step.
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
      toolHandler = \(TypecheckCodeToolArguments {code, projectContext}) -> handleToolError do
        -- Just load the code, nothing more
        withCode code [] projectContext
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
      toolHandler = \(DocsToolArguments {name, projectContext}) -> handleToolError $ do
        output <- handleInputMCP projectContext [Right $ DocToMarkdownI name]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

runTool :: Tool MCP
runTool =
  Tool
    { toolName = toToolName RunTool,
      toolDescription = "Execute/Run a given definition.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Run",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(RunToolArguments {mainFunctionName, projectContext, args}) -> handleToolError $ do
        let input = ExecuteI NoProf (HQ.NameOnly mainFunctionName) (Text.unpack <$> args)
        output <- handleInputMCP projectContext [Right input]
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
      toolHandler = \(ShareProjectReadmeToolArguments {projectName, projectOwnerHandle}) -> handleToolError $ do
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
      toolHandler = \(ProjectContextArgument projectContext) -> handleToolError $ do
        let noop _ = pure ()
        output <-
          cliToMCP projectContext noop Cli.getCurrentBranch0 >>= \case
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
      toolHandler = \(ProjectContextArgument projectContext) -> handleToolError $ do
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
      toolHandler = \(ListLibraryDefinitionsToolArguments {libName, projectContext}) -> handleToolError $ do
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
      toolHandler = \(ViewDefinitionsToolArguments {projectContext, names}) -> handleToolError $ do
        case NEL.nonEmpty names of
          Nothing ->
            pure $ errorToolResult "No names provided to view definitions"
          Just nonEmptyNames -> do
            let names' = HQ.NameOnly <$> nonEmptyNames
            definitions <- handleInputMCP projectContext [Right $ Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal names']
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
            pure $ textToolResult outputJSON
    }

updateTool :: Tool MCP
updateTool =
  Tool
    { toolName = toToolName UpdateDefinitionsTool,
      toolDescription = "Typecheck, then update definitions in the codebase to the provided code.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Update Definitions",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(UpdateDefinitionsToolArguments {projectContext, code}) -> handleToolError $ do
        withCode code [Input.Update2I] projectContext
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
      toolHandler = \(()) -> handleToolError $ do
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
      toolHandler = \(ProjectNameArgument {projectName}) -> handleToolError $ do
        projectContext <- currentProjectContext
        branches <- handleInputMCP projectContext [Right $ Input.BranchesI (Just projectName)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode branches
        pure $ textToolResult outputJSON
    }

getCurrentProjectContextTool :: Tool MCP
getCurrentProjectContextTool =
  Tool
    { toolName = toToolName GetCurrentProjectContextTool,
      toolDescription = "Get the current project context. This is useful for determining the user's working branch, but all commands take an explicit project context, so it's unnecessary if you already know which context is desired.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Get Current Project Context",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \() -> handleToolError $ do
        projectContext <- currentProjectContext
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode projectContext
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
      toolHandler = \(SearchDefinitionsToolArguments {projectContext, query}) -> handleToolError $ do
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
      toolHandler = \(SearchByTypeToolArguments {projectContext, query}) -> handleToolError $ do
        definitions <- handleInputMCP projectContext [Right $ Input.FindI False (FindLocal Path.Root') [":", Text.unpack query]]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode definitions
        pure $ textToolResult outputJSON
    }

dependenciesTool :: Tool MCP
dependenciesTool =
  Tool
    { toolName = toToolName DependenciesTool,
      toolDescription = "List the dependencies of a definition.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List all definitions a given term or type depends on.",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ProjectDefinitionNameArgument {projectContext, definitionName}) -> handleToolError $ do
        output <- handleInputMCP projectContext [Right $ Input.ListDependenciesI (HQ.NameOnly definitionName)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

dependentsTool :: Tool MCP
dependentsTool =
  Tool
    { toolName = toToolName DependentsTool,
      toolDescription = "List the dependents of a definition.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "List all definitions that depend on a given term or type.",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(ProjectDefinitionNameArgument {projectContext, definitionName}) -> handleToolError $ do
        output <- handleInputMCP projectContext [Right $ Input.ListDependentsI (HQ.NameOnly definitionName)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

runTestsTool :: Tool MCP
runTestsTool =
  Tool
    { toolName = toToolName TestsTool,
      toolDescription = "Run the pure tests within a project.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Run Pure Tests",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(TestToolArguments {projectContext, subnamespace}) -> handleToolError $ do
        let testInput =
              Input.TestInput
                { includeLibNamespace = False,
                  path = case subnamespace of
                    Nothing -> mempty
                    Just ns -> ns,
                  showFailures = True,
                  showSuccesses = False
                }
        output <- handleInputMCP projectContext [Right $ Input.TestI testInput]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

handleToolError :: EMCP CallToolResult -> MCP CallToolResult
handleToolError action = do
  result <- runExceptT action
  case result of
    Left err -> pure $ errorToolResult err
    Right res -> pure res

deleteDefinitionsTool :: Tool MCP
deleteDefinitionsTool =
  Tool
    { toolName = toToolName DeleteDefinitionsTool,
      toolDescription = "Delete one or more definitions (terms or types) from the codebase.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Delete Definitions",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(DeleteDefinitionsToolArguments {projectContext, names, force}) -> handleToolError $ do
        case NEL.nonEmpty names of
          Nothing ->
            pure $ errorToolResult "No names provided to delete"
          Just nonEmptyNames -> do
            let names' = HQ'.NameOnly <$> NEL.toList nonEmptyNames
            output <- handleInputMCP projectContext [Right $ Input.DeleteI force Input.DeleteTarget'TermOrType names']
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
            pure $ textToolResult outputJSON
    }

renameDefinitionTool :: Tool MCP
renameDefinitionTool =
  Tool
    { toolName = toToolName RenameDefinitionTool,
      toolDescription = "Rename a definition (term, type, or namespace) by changing only its final name segment. The parent path is preserved. For example, `rename foo.bar.baz Qux` changes the name `baz` to `Qux`, producing `foo.bar.Qux`. To move a definition to a different namespace, use `move-to` instead.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Rename Definition",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(RenameDefinitionToolArguments {projectContext, oldName, newNameSegment}) -> handleToolError $ do
        let src = Path.fromName' oldName
        output <- handleInputMCP projectContext [Right $ Input.RenameI src newNameSegment]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

moveDefinitionTool :: Tool MCP
moveDefinitionTool =
  Tool
    { toolName = toToolName MoveDefinitionTool,
      toolDescription = "Move a definition (term, type, or namespace) to a completely new path. For example, `move foo.bar baz.qux` renames `foo.bar` to `baz.qux`. This changes the full path, not just the final segment.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Move Definition",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(MoveDefinitionToolArguments {projectContext, oldName, newName}) -> handleToolError $ do
        let src = Path.fromName' oldName
            dest = Path.fromName' newName
        output <- handleInputMCP projectContext [Right $ Input.MoveAllI src dest]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }

moveToTool :: Tool MCP
moveToTool =
  Tool
    { toolName = toToolName MoveToTool,
      toolDescription = "Move one or more definitions or namespaces into a destination namespace. The final segment of each source is preserved. For example, `moveTo foo.bar dest` moves `foo.bar` into namespace `dest`, producing `dest.bar`. Multiple sources can be moved at once: `moveTo foo bar baz dest` moves all three into `dest`.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Move To",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(MoveToToolArguments {projectContext, sources, destination}) -> handleToolError $ do
        case NEL.nonEmpty sources of
          Nothing ->
            pure $ errorToolResult "No sources provided to move"
          Just nonEmptySources -> do
            output <- handleInputMCP projectContext [Right $ Input.MoveToI nonEmptySources destination]
            let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
            pure $ textToolResult outputJSON
    }

deleteNamespaceTool :: Tool MCP
deleteNamespaceTool =
  Tool
    { toolName = toToolName DeleteNamespaceTool,
      toolDescription = "Delete a namespace and all definitions within it from the codebase.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Delete Namespace",
            readOnlyHint = Just False,
            destructiveHint = Just True,
            idempotentHint = Just False,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \(DeleteNamespaceToolArguments {projectContext, namespaceName, force}) -> handleToolError $ do
        let (path, finalSegment) = Path.splitFromName namespaceName
            split = (Path.Relative path, finalSegment)
            insistence = if force then Input.Force else Input.Try
        output <- handleInputMCP projectContext [Right $ Input.DeleteNamespaceI insistence (Just split)]
        let outputJSON = Text.decodeUtf8 . BL.toStrict $ Aeson.encode output
        pure $ textToolResult outputJSON
    }
