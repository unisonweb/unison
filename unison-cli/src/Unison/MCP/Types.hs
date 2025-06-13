{-# LANGUAGE NoFieldSelectors #-}

module Unison.MCP.Types
  ( MCP (..),
    Env (..),
    runMCP,
    ToolKind (..),
    ProjectCodeToolArguments (..),
    LibInstallToolArguments (..),
    toToolName,
    fromToolName,
  )
where

import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Aeson
import Data.Text qualified as Text
import Data.Map qualified as Map
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.Runtime (Runtime)
import Unison.Parser.Ann (Ann)
import Unison.Symbol (Symbol)
import Unison.Prelude

data Env = Env
  { codebase :: Codebase IO Symbol Ann,
    runtime :: Runtime Symbol,
    nRuntime :: Runtime Symbol,
    sbRuntime :: Runtime Symbol,
    ucmVersion :: UCMVersion,
    workDir :: FilePath
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
  deriving (Eq, Ord, Show, Bounded, Enum)

data ProjectCodeToolArguments = ProjectCodeToolArguments {projectName :: Text}

instance FromJSON ProjectCodeToolArguments where
  parseJSON = withObject "ProjectCodeToolArguments" $ \o -> do
    projectName <- o .: "projectName"
    pure $ ProjectCodeToolArguments {projectName}


data LibInstallToolArguments = LibInstallToolArguments
  { projectName :: Text,
    branchName :: Maybe Text
  }
instance FromJSON LibInstallToolArguments where
  parseJSON = withObject "LibInstallToolArguments" $ \o -> do
    projectName <- o .: "projectName"
    branchName <- o .:? "branchName"
    pure $ LibInstallToolArguments {projectName, branchName}

kindNameMapping :: Map ToolKind Text
kindNameMapping = Map.fromList
  [ (ProjectCodeTool, "project-code"),
    (LibInstallTool, "lib-install")
  ]

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
