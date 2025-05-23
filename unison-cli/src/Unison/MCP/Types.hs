{-# LANGUAGE NoFieldSelectors #-}

module Unison.MCP.Types
  ( MCP (..),
    Env (..),
    runMCP,
    ToolKind (..),
    ProjectCodeToolArguments (..),
    toToolName,
    fromToolName,
  )
where

import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Ki qualified
import Unison.Codebase (Codebase)
import Unison.Codebase.Runtime (Runtime)
import Unison.Parser.Ann (Ann)
import Unison.Symbol (Symbol)
import Unison.Version (Version)
import UnliftIO (MonadIO, MonadUnliftIO)

data Env = Env
  { codebase :: Codebase IO Symbol Ann,
    runtime :: Runtime Symbol,
    scope :: Ki.Scope,
    ucmVersion :: Version,
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
  deriving (Eq, Show, Bounded, Enum)

data ProjectCodeToolArguments = ProjectCodeToolArguments {projectName :: Text}

instance FromJSON ProjectCodeToolArguments where
  parseJSON = withObject "ProjectCodeToolArguments" $ \o -> do
    projectName <- o .: "projectName"
    pure $ ProjectCodeToolArguments {projectName}

toToolName :: ToolKind -> Text.Text
toToolName ProjectCodeTool = "project-code"

fromToolName :: Text.Text -> Maybe ToolKind
fromToolName = \case
  "project-code" -> Just ProjectCodeTool
  _ -> Nothing
