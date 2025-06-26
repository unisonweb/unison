module Unison.MCP.Server (mcpServer) where

import Network.MCP.Server qualified as MCP
import Network.MCP.Transport.Types qualified as MCP
import UnliftIO (MonadIO (..))

mcpServer :: (MonadIO m) => MCP.Server -> (MCP.Message -> m (Maybe MCP.Message))
mcpServer server msg = do
  liftIO $ MCP.handleMessage server msg
