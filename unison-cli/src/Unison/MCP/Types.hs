{-# LANGUAGE NoFieldSelectors #-}

module Unison.MCP.Types
  ( MCP (..),
    Env (..),
    runMCP,
    ToolKind (..),
    ProjectCodeToolArguments (..),
    LibInstallToolArguments (..),
    ShareProjectSearchToolArguments (..),
    TypecheckCodeToolArguments (..),
    ShareProjectReadmeToolArguments (..),
    ListLibraryDefinitionsToolArguments (..),
    ViewDefinitionsToolArguments (..),
    UpdateDefinitionsToolArguments (..),
    SearchDefinitionsToolArguments (..),
    SearchByTypeToolArguments (..),
    DocsToolArguments (..),
    RunToolArguments (..),
    ProjectContext (..),
    ProjectContextArgument (..),
    ProjectNameArgument (..),
    ProjectDefinitionNameArgument (..),
    TestToolArguments (..),
    DeleteDefinitionsToolArguments (..),
    RenameDefinitionToolArguments (..),
    MoveDefinitionToolArguments (..),
    MoveToToolArguments (..),
    DeleteNamespaceToolArguments (..),
    toToolName,
    fromToolName,
  )
where

import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Aeson
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.Path qualified as Path
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName), ProjectName (UnsafeProjectName))
import Unison.MCP.Wrapper (HasInputSchema (..))
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Runtime (Runtime)
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment

data Env = Env
  { codebase :: Codebase IO Symbol Ann,
    runtime :: Runtime Symbol,
    sbRuntime :: Runtime Symbol,
    ucmVersion :: UCMVersion,
    workDir :: Maybe FilePath,
    authenticatedHTTPClient :: AuthenticatedHttpClient
  }

newtype MCP a = MCP
  { unMCP :: ReaderT Env IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader Env)

runMCP :: Env -> MCP a -> IO a
runMCP env (MCP m) = do
  runReaderT m env

data ToolKind
  = ProjectCodeTool
  | LibInstallTool
  | ShareProjectSearchTool
  | ShareProjectReadmeTool
  | TypecheckCodeTool
  | DocsTool
  | RunTool
  | ListProjectDefinitionsTool
  | ListProjectLibrariesTool
  | ListLibraryDefinitionsTool
  | ViewDefinitionsTool
  | UpdateDefinitionsTool
  | SearchDefinitionsTool
  | SearchByTypeTool
  | ListLocalProjectsTool
  | ListProjectBranchesTool
  | GetCurrentProjectContextTool
  | DependenciesTool
  | DependentsTool
  | TestsTool
  | DeleteDefinitionsTool
  | RenameDefinitionTool
  | MoveDefinitionTool
  | MoveToTool
  | DeleteNamespaceTool
  deriving (Eq, Ord, Show, Bounded, Enum)

kindNameMapping :: Map ToolKind Text
kindNameMapping =
  Map.fromList
    [ (ProjectCodeTool, "project-code"),
      (LibInstallTool, "lib-install"),
      (ShareProjectSearchTool, "share-project-search"),
      (ShareProjectReadmeTool, "share-project-readme"),
      (TypecheckCodeTool, "typecheck-code"),
      (DocsTool, "docs"),
      (RunTool, "run"),
      (ListProjectDefinitionsTool, "list-project-definitions"),
      (ListProjectLibrariesTool, "list-project-libraries"),
      (ListLibraryDefinitionsTool, "list-library-definitions"),
      (ViewDefinitionsTool, "view-definitions"),
      (UpdateDefinitionsTool, "update-definitions"),
      (SearchDefinitionsTool, "search-definitions-by-name"),
      (SearchByTypeTool, "search-by-type"),
      (ListLocalProjectsTool, "list-local-projects"),
      (ListProjectBranchesTool, "list-project-branches"),
      (GetCurrentProjectContextTool, "get-current-project-context"),
      (DependenciesTool, "list-definition-dependencies"),
      (DependentsTool, "list-definition-dependents"),
      (TestsTool, "run-tests"),
      (DeleteDefinitionsTool, "delete-definitions"),
      (RenameDefinitionTool, "rename-definition"),
      (MoveDefinitionTool, "move-definition"),
      (MoveToTool, "move-to"),
      (DeleteNamespaceTool, "delete-namespace")
    ]

data ProjectDefinitionNameArgument = ProjectDefinitionNameArgument
  { definitionName :: Name,
    projectContext :: ProjectContext
  }
  deriving (Eq, Show)

instance HasInputSchema ProjectDefinitionNameArgument where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "definitionName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the definition to work with, e.g. `mynamespace.foo` or `lib.unison_base_1_0_0.data.List`." :: Text)
                  ],
              "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext)
            ],
        "required" .= ["definitionName", "projectContext" :: Text]
      ]

instance FromJSON ProjectDefinitionNameArgument where
  parseJSON = withObject "ProjectDefinitionNameArgument" $ \o -> do
    definitionNameText <- o .: "definitionName"
    definitionName <- case Name.parseTextEither definitionNameText of
      Left err -> fail $ "Invalid definition name: " ++ show err
      Right definitionName -> pure definitionName
    projectContext <- o .: "projectContext"
    pure $ ProjectDefinitionNameArgument {definitionName, projectContext}

newtype ProjectContextArgument = ProjectContextArgument ProjectContext
  deriving newtype (Eq, Show)

instance HasInputSchema ProjectContextArgument where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext)
            ],
        "required" .= ["projectContext" :: Text]
      ]

instance FromJSON ProjectContextArgument where
  parseJSON = withObject "ProjectContextArgument" $ \o -> do
    projectContext <- o .: "projectContext"
    pure $ ProjectContextArgument projectContext

data ProjectNameArgument = ProjectNameArgument
  { projectName :: ProjectName
  }
  deriving (Eq, Show)

instance HasInputSchema ProjectNameArgument where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of a project to work within, e.g. `@unison/base` or `@ceedubs/json`" :: Text)
                  ]
            ],
        "required" .= ["projectName" :: Text]
      ]

instance FromJSON ProjectNameArgument where
  parseJSON = withObject "ProjectNameArgument" $ \o -> do
    projectName <- UnsafeProjectName <$> o .: "projectName"
    pure $ ProjectNameArgument {projectName}

data SearchByTypeToolArguments = SearchByTypeToolArguments
  { projectContext :: ProjectContext,
    query :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema SearchByTypeToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "query"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("A type to search for, e.g. `[Nat] -> Nat`." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "query" :: Text]
      ]

instance FromJSON SearchByTypeToolArguments where
  parseJSON = withObject "SearchByTypeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    query <- o .: "query"
    pure $ SearchByTypeToolArguments {projectContext, query}

data SearchDefinitionsToolArguments = SearchDefinitionsToolArguments
  { projectContext :: ProjectContext,
    query :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema SearchDefinitionsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "query"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("A name to search for, e.g. `foldl`." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "query" :: Text]
      ]

instance FromJSON SearchDefinitionsToolArguments where
  parseJSON = withObject "SearchDefinitionsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    query <- o .: "query"
    pure $ SearchDefinitionsToolArguments {projectContext, query}

data ViewDefinitionsToolArguments = ViewDefinitionsToolArguments
  { projectContext :: ProjectContext,
    names :: [Name]
  }
  deriving (Eq, Show)

instance HasInputSchema ViewDefinitionsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "names"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("string" :: Text),
                          "description" .= ("The names of the definitions to view, e.g. `mynamespace.foo` or `lib.unison_base_1_0_0.data.List`." :: Text)
                        ],
                    "description" .= ("The names of the definitions to view." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "names" :: Text]
      ]

instance FromJSON ViewDefinitionsToolArguments where
  parseJSON = withObject "ViewDefinitionsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    names <- fmap Name.unsafeParseText <$> o .: "names"
    pure $ ViewDefinitionsToolArguments {projectContext, names}

data UpdateDefinitionsToolArguments = UpdateDefinitionsToolArguments
  { projectContext :: ProjectContext,
    code :: Either FilePath Text
  }
  deriving (Eq, Show)

instance HasInputSchema UpdateDefinitionsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "code"
                .= object
                  [ "description" .= ("The source code to update definitions to. If a string, it is the source code itself. If a file path, it is the path to a file containing the source code." :: Text),
                    "oneOf"
                      .= [ object
                             [ "description" .= ("The file path to the source code." :: Text),
                               "type" .= ("object" :: Text),
                               "properties"
                                 .= object
                                   [ "filePath"
                                       .= object
                                         [ "type" .= ("string" :: Text),
                                           "description" .= ("An absolute file path to the source code." :: Text)
                                         ]
                                   ],
                               "required" .= ["filePath" :: Text],
                               "additionalProperties" .= False
                             ],
                           object
                             [ "description" .= ("The source code to use." :: Text),
                               "type" .= ("object" :: Text),
                               "properties"
                                 .= object
                                   [ "text"
                                       .= object
                                         [ "type" .= ("string" :: Text),
                                           "description" .= ("The source code to use." :: Text)
                                         ]
                                   ],
                               "required" .= ["text" :: Text],
                               "additionalProperties" .= False
                             ]
                         ]
                  ]
            ],
        "required" .= ["projectContext", "code" :: Text]
      ]

instance FromJSON UpdateDefinitionsToolArguments where
  parseJSON = withObject "UpdateDefinitionsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    source <- o .: "code"
    code <-
      source .:? "filePath" >>= \case
        Just filePath -> pure $ Left filePath
        Nothing -> do
          text <- source .: "text"
          pure $ Right text
    pure $ UpdateDefinitionsToolArguments {projectContext, code}

data ListLibraryDefinitionsToolArguments = ListLibraryDefinitionsToolArguments
  { projectContext :: ProjectContext,
    libName :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema ListLibraryDefinitionsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "libName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the library to list definitions for, e.g. `base` or `json`." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "libName" :: Text]
      ]

instance FromJSON ListLibraryDefinitionsToolArguments where
  parseJSON = withObject "ListLibraryDefinitionsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    libName <- o .: "libName"
    pure $ ListLibraryDefinitionsToolArguments {projectContext, libName}

data ShareProjectReadmeToolArguments = ShareProjectReadmeToolArguments
  { projectName :: Text,
    projectOwnerHandle :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema ShareProjectReadmeToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the project to fetch the README for. E.g. in a project reference like `@owner/project-name` this would be `project-name`" :: Text)
                  ],
              "projectOwnerHandle"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The handle of the project owner, e.g. in a project reference like `@owner/project-name` this would be `owner`" :: Text)
                  ]
            ],
        "required" .= ["projectName", "projectOwnerHandle" :: Text]
      ]

instance FromJSON ShareProjectReadmeToolArguments where
  parseJSON = withObject "ShareProjectReadmeToolArguments" $ \o -> do
    projectName <- o .: "projectName"
    projectOwnerHandle <- o .: "projectOwnerHandle"
    pure $ ShareProjectReadmeToolArguments {projectName, projectOwnerHandle}

data TypecheckCodeToolArguments = TypecheckCodeToolArguments
  { projectContext :: ProjectContext,
    code :: Either FilePath Text
  }
  deriving (Eq, Show)

instance HasInputSchema TypecheckCodeToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "code"
                .= object
                  [ "description" .= ("The source code to typecheck. Either the `sourceCode` key or the `filePath`, but not both." :: Text),
                    "type" .= ("object" :: Text),
                    "properties"
                      .= object
                        [ "sourceCode"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("The source code to typecheck." :: Text)
                              ],
                          "filePath"
                            .= object
                              [ "type" .= ("string" :: Text),
                                "description" .= ("The absolute file path to the source code to typecheck." :: Text)
                              ]
                        ],
                    "additionalProperties" .= False,
                    "minProperties" .= (1 :: Int),
                    "maxProperties" .= (1 :: Int)
                  ]
            ],
        "required" .= ["projectContext", "code" :: Text]
      ]

instance FromJSON TypecheckCodeToolArguments where
  parseJSON = withObject "TypecheckCodeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    source <- o .: "code"
    source .:? "filePath" >>= \case
      Just filePath -> pure $ TypecheckCodeToolArguments {projectContext, code = Left filePath}
      Nothing -> do
        text <- source .: "sourceCode"
        pure $ TypecheckCodeToolArguments {projectContext, code = Right text}

data DocsToolArguments = DocsToolArguments
  { projectContext :: ProjectContext,
    name :: Name
  }
  deriving (Eq, Show)

instance HasInputSchema DocsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "name"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The definition name to fetch documentation for. E.g. `README` or `data.Map.fromList`" :: Text)
                  ],
              "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext)
            ],
        "required" .= ["name", "projectContext" :: Text]
      ]

instance FromJSON DocsToolArguments where
  parseJSON = withObject "DocsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    name <- Name.unsafeParseText <$> o .: "name"
    pure $ DocsToolArguments {projectContext, name}

data RunToolArguments = RunToolArguments
  { projectContext :: ProjectContext,
    mainFunctionName :: Name,
    args :: [Text]
  }
  deriving (Eq, Show)

instance HasInputSchema RunToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "mainFunctionName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the main function to run, e.g. `main` or `mynamespace.myprogram`." :: Text)
                  ],
              "args"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("string" :: Text),
                          "description" .= ("An argument to pass to the main function." :: Text)
                        ],
                    "description" .= ("The arguments to pass to the main function." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "mainFunctionName", "args" :: Text]
      ]

instance FromJSON RunToolArguments where
  parseJSON = withObject "RunToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    mainFunctionNameText <- o .: "mainFunctionName"
    mainFunctionName <- case Name.parseTextEither mainFunctionNameText of
      Left err -> fail $ "Invalid main function name: " ++ show err
      Right name -> pure name
    args <- o .: "args"
    pure $ RunToolArguments {projectContext, mainFunctionName, args}

data ProjectCodeToolArguments = ProjectCodeToolArguments
  { projectContext :: ProjectContext
  }

instance FromJSON ProjectCodeToolArguments where
  parseJSON = withObject "ProjectCodeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    pure $ ProjectCodeToolArguments {projectContext}

data ProjectContext = ProjectContext
  { projectName :: ProjectName,
    branchName :: ProjectBranchName
  }
  deriving (Eq, Show)

instance HasInputSchema ProjectContext where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the project to work within, e.g. `@unison/base` or `@ceedubs/json`" :: Text)
                  ],
              "branchName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The branch of the project to work within, e.g. `main` or `develop`" :: Text)
                  ]
            ],
        "required" .= ["projectName", "branchName" :: Text]
      ]

instance FromJSON ProjectContext where
  parseJSON = withObject "ProjectContext" $ \o -> do
    projectName <- UnsafeProjectName <$> o .: "projectName"
    branchName <- UnsafeProjectBranchName <$> o .: "branchName"
    pure $ ProjectContext {projectName, branchName}

instance ToJSON ProjectContext where
  toJSON (ProjectContext (UnsafeProjectName projectName) (UnsafeProjectBranchName branchName)) =
    object
      [ "projectName" .= projectName,
        "branchName" .= branchName
      ]

data LibInstallToolArguments = LibInstallToolArguments
  { projectContext :: ProjectContext,
    libProjectName :: Text,
    libBranchName :: Maybe Text
  }

instance FromJSON LibInstallToolArguments where
  parseJSON = withObject "LibInstallToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    libProjectName <- o .: "libProjectName"
    libBranchName <- o .:? "libBranchName"
    pure $
      LibInstallToolArguments
        { projectContext,
          libProjectName,
          libBranchName
        }

instance HasInputSchema LibInstallToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "libProjectName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "The user-qualified name of the library project to install, e.g. `@unison/base` or `@ceedubs/json`" .= ("The name of the library project to install" :: Text)
                  ],
              "libBranchName"
                .= object
                  [ "type" .= ["string" :: Text, "null"],
                    "description" .= ("The optional branch of the library project to install, E.g. `main`. If null, the latest release will be used." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "libProjectName" :: Text]
      ]

data ShareProjectSearchToolArguments = ShareProjectSearchToolArguments
  { query :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema ShareProjectSearchToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "query"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The search query to use. E.g. \"http client\". By default, each search word is ANDed together, but you can use OR to search for multiple terms. E.g. \"http OR client\" will return results that match either term. You can also exclude results using \"-\", e.g. \"-http\" will exclude results that match the term \"http\". Wrap a term in quotes to search for an exact phrase, e.g. \"\"http client\"\" will search for the exact phrase \"http client\"." :: Text)
                  ]
            ],
        "required" .= ["query" :: Text]
      ]

instance FromJSON ShareProjectSearchToolArguments where
  parseJSON = withObject "ShareProjectSearchToolArguments" $ \o -> do
    query <- o .: "query"
    pure $ ShareProjectSearchToolArguments {query}

data TestToolArguments = TestToolArguments
  { projectContext :: ProjectContext,
    subnamespace :: Maybe Path.Relative
  }
  deriving (Eq, Show)

instance HasInputSchema TestToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "subnamespace"
                .= object
                  [ "type" .= ["string" :: Text, "null"],
                    "description" .= ("An optional subnamespace within the project to run tests in. E.g. `mynamespace.tests`. If null, tests in the entire project will be run." :: Text)
                  ]
            ],
        "required" .= ["projectContext" :: Text]
      ]

instance FromJSON TestToolArguments where
  parseJSON = withObject "TestToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    subnamespace <- fmap (Path.Relative . Path.unsafeParseText) <$> (o .:? "subnamespace")
    pure $ TestToolArguments {projectContext, subnamespace}

data DeleteDefinitionsToolArguments = DeleteDefinitionsToolArguments
  { projectContext :: ProjectContext,
    names :: [Name],
    force :: Bool
  }
  deriving (Eq, Show)

instance HasInputSchema DeleteDefinitionsToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "names"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("string" :: Text),
                          "description" .= ("A definition name to delete, e.g. `mynamespace.foo` or `MyType`." :: Text)
                        ],
                    "description" .= ("The names of the definitions to delete." :: Text)
                  ],
              "force"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("If true, force delete even if the definition has dependents. Default is false." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "names" :: Text]
      ]

instance FromJSON DeleteDefinitionsToolArguments where
  parseJSON = withObject "DeleteDefinitionsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    names <- fmap Name.unsafeParseText <$> o .: "names"
    force <- o .:? "force" .!= False
    pure $ DeleteDefinitionsToolArguments {projectContext, names, force}

data RenameDefinitionToolArguments = RenameDefinitionToolArguments
  { projectContext :: ProjectContext,
    oldName :: Name,
    newNameSegment :: NameSegment
  }
  deriving (Eq, Show)

instance HasInputSchema RenameDefinitionToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "oldName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The current name of the definition to rename, e.g. `mynamespace.foo` or `MyType`." :: Text)
                  ],
              "newNameSegment"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The new name segment (final part only). For example, to rename `foo.bar` to `foo.baz`, provide `baz`. The parent path is preserved." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "oldName", "newNameSegment" :: Text]
      ]

instance FromJSON RenameDefinitionToolArguments where
  parseJSON = withObject "RenameDefinitionToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    oldName <- Name.unsafeParseText <$> o .: "oldName"
    newNameSegment <- NameSegment.unsafeParseText <$> o .: "newNameSegment"
    pure $ RenameDefinitionToolArguments {projectContext, oldName, newNameSegment}

data MoveDefinitionToolArguments = MoveDefinitionToolArguments
  { projectContext :: ProjectContext,
    oldName :: Name,
    newName :: Name
  }
  deriving (Eq, Show)

instance HasInputSchema MoveDefinitionToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "oldName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The current full path of the definition to move, e.g. `mynamespace.foo` or `MyType`." :: Text)
                  ],
              "newName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The new full path for the definition, e.g. `othernamespace.bar` or `NewType`. Can move to a different namespace." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "oldName", "newName" :: Text]
      ]

instance FromJSON MoveDefinitionToolArguments where
  parseJSON = withObject "MoveDefinitionToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    oldName <- Name.unsafeParseText <$> o .: "oldName"
    newName <- Name.unsafeParseText <$> o .: "newName"
    pure $ MoveDefinitionToolArguments {projectContext, oldName, newName}

data MoveToToolArguments = MoveToToolArguments
  { projectContext :: ProjectContext,
    sources :: [Path.Path'],
    destination :: Path.Path'
  }
  deriving (Eq, Show)

instance HasInputSchema MoveToToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "sources"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items"
                      .= object
                        [ "type" .= ("string" :: Text),
                          "description" .= ("A path to move, e.g. `mynamespace.foo` or `MyType`." :: Text)
                        ],
                    "description" .= ("The paths of the definitions or namespaces to move. The final segment of each source is preserved in the destination." :: Text),
                    "minItems" .= (1 :: Int)
                  ],
              "destination"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The destination namespace to move the sources into, e.g. `othernamespace` or `foo.bar`. Each source's final segment is preserved." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "sources", "destination" :: Text]
      ]

instance FromJSON MoveToToolArguments where
  parseJSON = withObject "MoveToToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    sources <- fmap Path.unsafeParseText' <$> o .: "sources"
    destination <- Path.unsafeParseText' <$> o .: "destination"
    pure $ MoveToToolArguments {projectContext, sources, destination}

data DeleteNamespaceToolArguments = DeleteNamespaceToolArguments
  { projectContext :: ProjectContext,
    namespaceName :: Name,
    force :: Bool
  }
  deriving (Eq, Show)

instance HasInputSchema DeleteNamespaceToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext),
              "namespaceName"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The name of the namespace to delete, e.g. `mynamespace` or `foo.bar`. This will delete the namespace and all definitions within it." :: Text)
                  ],
              "force"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("If true, force delete even if definitions in the namespace have dependents. Default is false." :: Text)
                  ]
            ],
        "required" .= ["projectContext", "namespaceName" :: Text]
      ]

instance FromJSON DeleteNamespaceToolArguments where
  parseJSON = withObject "DeleteNamespaceToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    namespaceName <- Name.unsafeParseText <$> o .: "namespaceName"
    force <- o .:? "force" .!= False
    pure $ DeleteNamespaceToolArguments {projectContext, namespaceName, force}

nameKindMapping :: Map Text ToolKind
nameKindMapping =
  (Map.toList kindNameMapping)
    & map (\(k, v) -> (v, k))
    & Map.fromList

toToolName :: ToolKind -> Text.Text
toToolName kind =
  (Map.lookup kind kindNameMapping)
    & fromMaybe (error $ "Unknown tool kind: " ++ show kind)

fromToolName :: Text.Text -> Maybe ToolKind
fromToolName name = Map.lookup name nameKindMapping
