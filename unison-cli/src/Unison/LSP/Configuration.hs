{-# LANGUAGE DataKinds #-}

module Unison.LSP.Configuration where

import Data.Aeson
import Data.Text qualified as Text
import Language.LSP.Protocol.Message qualified as Msg
import Unison.LSP.Types
import Unison.Prelude

-- | Handle configuration changes.
updateConfig :: (Applicative m) => Config -> m ()
updateConfig _newConfig = pure ()

parseConfig :: Config -> Value -> Either Text Config
parseConfig _oldConfig newConfig = case fromJSON newConfig of
  Error err -> Left $ Text.pack err
  Success a -> Right a

-- | We could use this notification to cancel/update work-in-progress,
-- but we don't actually need to update the config here, that's handled by the lsp library
-- automatically.
workspaceConfigurationChanged :: Msg.TNotificationMessage 'Msg.Method_WorkspaceDidChangeConfiguration -> Lsp ()
workspaceConfigurationChanged _m = do
  pure ()
