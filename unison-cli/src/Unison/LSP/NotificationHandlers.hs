{-# LANGUAGE DataKinds #-}

module Unison.LSP.NotificationHandlers where

import Language.LSP.Types
import Unison.LSP.Types

initializedHandler :: NotificationMessage 'Initialized -> Lsp ()
initializedHandler _ = pure ()
