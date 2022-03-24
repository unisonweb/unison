{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin) where

import Control.Monad.Reader
import Unison.Auth.OAuth
import Unison.Auth.Types (Audience (Share, ShareStaging))
import Unison.Codebase.Editor.HandleInput.LoopState
import Unison.Debug

authLogin :: MonadIO m => Action m i v ()
authLogin = do
  let aud =
        if shouldDebug UseStaging
          then ShareStaging
          else Share
  credsMan <- asks credentialManager
  authWithAudience credsMan aud >>= \case
    Left err -> liftIO $ print err
    Right () -> pure ()
