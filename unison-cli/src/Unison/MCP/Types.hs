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
    SearchDefinitionsToolArguments (..),
    SearchByTypeToolArguments (..),
    DocsToolArguments (..),
    ProjectContext (..),
    ProjectContextArgument (..),
    ProjectNameArgument (..),
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
import Unison.Codebase.Runtime (Runtime)
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName), ProjectName (UnsafeProjectName))
import Unison.MCP.Wrapper (HasInputSchema (..))
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name

data Env = Env
  { codebase :: Codebase IO Symbol Ann,
    runtime :: Runtime Symbol,
    nRuntime :: Runtime Symbol,
    sbRuntime :: Runtime Symbol,
    ucmVersion :: UCMVersion,
    workDir :: FilePath,
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
  | ListProjectDefinitionsTool
  | ListProjectLibrariesTool
  | ListLibraryDefinitionsTool
  | ViewDefinitionsTool
  | SearchDefinitionsTool
  | SearchByTypeTool
  | ListLocalProjectsTool
  | ListProjectBranchesTool
  | GetCurrentProjectContextTool
  | SetCurrentProjectContextTool
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
      (ListProjectDefinitionsTool, "list-project-definitions"),
      (ListProjectLibrariesTool, "list-project-libraries"),
      (ListLibraryDefinitionsTool, "list-library-definitions"),
      (ViewDefinitionsTool, "view-definitions"),
      (SearchDefinitionsTool, "search-definitions-by-name"),
      (SearchByTypeTool, "search-by-type"),
      (ListLocalProjectsTool, "list-local-projects"),
      (ListProjectBranchesTool, "list-project-branches"),
      (GetCurrentProjectContextTool, "get-current-project-context"),
      (SetCurrentProjectContextTool, "set-current-project-context")
    ]

newtype ProjectContextArgument = ProjectContextArgument ProjectContext
  deriving newtype (Eq, Show, HasInputSchema)

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
    code :: Text
  }
  deriving (Eq, Show)

instance HasInputSchema TypecheckCodeToolArguments where
  toInputSchema _ =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "code"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The code to typecheck, as a string. All the code you've written which is not yet part of the project must be provided at once." :: Text)
                  ],
              "projectContext" .= toInputSchema (Proxy :: Proxy ProjectContext)
            ],
        "required" .= ["code", "projectContext" :: Text]
      ]

instance FromJSON TypecheckCodeToolArguments where
  parseJSON = withObject "TypecheckCodeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    code <- o .: "code"
    pure $ TypecheckCodeToolArguments {projectContext, code}

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
