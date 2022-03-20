{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.HandleInput.ShareLogin (shareLogin) where

import Unison.Auth.TransferServer (initiateFlow)
import Unison.Codebase.Editor.HandleInput.LoopState
import Unison.Prelude

shareLogin :: MonadIO m => Action m i v ()
shareLogin = do
  liftIO $ initiateFlow >>= print
