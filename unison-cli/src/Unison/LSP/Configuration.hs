{-# LANGUAGE DataKinds #-}

module Unison.LSP.Configuration where

import Data.Aeson
import qualified Data.Text as Text
import Language.LSP.Types
import qualified Unison.Debug as Debug
import Unison.LSP.Types
import Unison.Prelude

-- | Handle configuration changes
updateConfig :: Config -> Value -> Either Text Config
updateConfig _oldConfig newConfig = Debug.debug Debug.LSP "Configuration Change" $ case fromJSON newConfig of
  Error err -> Left $ Text.pack err
  Success a -> Right a

-- | We could use this notification to cancel/update work-in-progress,
-- but we don't actually need to update the config here, that's handled by the lsp library
-- automatically.
workspaceConfigurationChanged :: NotificationMessage 'WorkspaceDidChangeConfiguration -> Lsp ()
workspaceConfigurationChanged _m = do
  pure ()
