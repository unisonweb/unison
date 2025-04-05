{-# LANGUAGE DataKinds #-}

module Unison.Server.Local.Endpoints.UCM where

import Unison.Codebase (Codebase)
import Unison.Prelude
import Unison.Server.Backend
import Unison.Server.Local.Endpoints.Current (Current, CurrentEndpoint, serveCurrent)

-- API endpoints which have to do with interacting with UCM directly.
type UCMAPI =
  CurrentEndpoint

ucmServer :: (MonadIO m) => Codebase m v a -> Backend m Current
ucmServer codebase = serveCurrent codebase
