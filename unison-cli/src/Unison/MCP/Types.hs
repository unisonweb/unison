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
    ProjectContext (..),
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
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)

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
  | TypecheckCodeTool
  deriving (Eq, Ord, Show, Bounded, Enum)

kindNameMapping :: Map ToolKind Text
kindNameMapping =
  Map.fromList
    [ (ProjectCodeTool, "project-code"),
      (LibInstallTool, "lib-install"),
      (ShareProjectSearchTool, "share-project-search"),
      (TypecheckCodeTool, "typecheck-code")
    ]

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
