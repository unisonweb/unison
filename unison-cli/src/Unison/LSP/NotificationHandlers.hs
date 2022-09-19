{-# LANGUAGE DataKinds #-}

module Unison.LSP.NotificationHandlers where

import Language.LSP.Types
import qualified Unison.Debug as Debug
import Unison.LSP.Types

initializedHandler :: NotificationMessage 'Initialized -> Lsp ()
initializedHandler _ = pure ()

withDebugging :: Show m => (m -> Lsp ()) -> (m -> Lsp ())
withDebugging handler message = do
  Debug.debugM Debug.LSP "Notification" message
  handler message
