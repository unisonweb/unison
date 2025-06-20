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
    DocsToolArguments (..),
    ProjectContext (..),
    ProjectContextArgument (..),
    toToolName,
    fromToolName,
  )
where

import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Aeson
import Data.Map qualified as Map
import Data.Text qualified as Text
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.Runtime (Runtime)
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName), ProjectName (UnsafeProjectName))
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
  | ListLocalProjectsTool
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
      (ListLocalProjectsTool, "list-local-projects")
    ]

data ProjectContextArgument = ProjectContextArgument ProjectContext
  deriving (Eq, Show)

instance FromJSON ProjectContextArgument where
  parseJSON = withObject "ProjectContextArgument" $ \o -> do
    projectContext <- o .: "projectContext"
    pure $ ProjectContextArgument projectContext

data ViewDefinitionsToolArguments = ViewDefinitionsToolArguments
  { projectContext :: ProjectContext,
    names :: [Name]
  }
  deriving (Eq, Show)

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

instance FromJSON ShareProjectReadmeToolArguments where
  parseJSON = withObject "ShareProjectReadmeToolArguments" $ \o -> do
    projectName <- o .: "projectName"
    projectOwnerHandle <- o .: "projectOwnerHandle"
    pure $ ShareProjectReadmeToolArguments {projectName, projectOwnerHandle}

data TypecheckCodeToolArguments
  = TypecheckCodeToolArguments
  { projectContext :: ProjectContext,
    code :: Text
  }
  deriving (Eq, Show)

instance FromJSON TypecheckCodeToolArguments where
  parseJSON = withObject "TypecheckCodeToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    code <- o .: "code"
    pure $ TypecheckCodeToolArguments {projectContext, code}

data DocsToolArguments
  = DocsToolArguments
  { projectContext :: ProjectContext,
    name :: Name
  }
  deriving (Eq, Show)

instance FromJSON DocsToolArguments where
  parseJSON = withObject "DocsToolArguments" $ \o -> do
    projectContext <- o .: "projectContext"
    name <- Name.unsafeParseText <$> o .: "name"
    pure $ DocsToolArguments {projectContext, name}

data ProjectCodeToolArguments
  = ProjectCodeToolArguments
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

instance FromJSON ProjectContext where
  parseJSON = withObject "ProjectContext" $ \o -> do
    projectName <- UnsafeProjectName <$> o .: "projectName"
    branchName <- UnsafeProjectBranchName <$> o .: "branchName"
    pure $ ProjectContext {projectName, branchName}

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

data ShareProjectSearchToolArguments = ShareProjectSearchToolArguments
  { query :: Text
  }
  deriving (Eq, Show)

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
