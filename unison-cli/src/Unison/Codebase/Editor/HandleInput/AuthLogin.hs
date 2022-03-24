{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin) where

import Unison.Auth.OAuth
import Unison.Auth.Types (Audience (Share, ShareStaging))
import Unison.Codebase.Editor.HandleInput.LoopState
import Unison.Debug
import Unison.Prelude

authLogin :: MonadIO m => Action m i v ()
authLogin = do
  let aud =
        if shouldDebug UseStaging
          then ShareStaging
          else Share
  liftIO $ obtainAccessToken aud >>= print
