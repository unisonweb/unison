{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin) where

import Control.Monad.Reader
import Unison.Auth.OAuth
import Unison.Auth.Types (Host (..))
import Unison.Codebase.Editor.HandleInput.LoopState
import Unison.Codebase.Editor.Output (Output (CredentialFailureMsg))
import Unison.Prelude
import qualified UnliftIO

defaultShareHost :: Host
defaultShareHost = Host "enlil.unison-lang.org"

authLogin :: UnliftIO.MonadUnliftIO m => Maybe Host -> Action m i v ()
authLogin mayHost = do
  let host = fromMaybe defaultShareHost mayHost
  credsMan <- asks credentialManager
  (Action . lift . lift . lift $ authenticateHost credsMan host) >>= \case
    Left err -> respond (CredentialFailureMsg err)
    Right () -> pure ()
