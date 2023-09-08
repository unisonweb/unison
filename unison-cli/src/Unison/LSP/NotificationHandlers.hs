{-# LANGUAGE DataKinds #-}

module Unison.LSP.NotificationHandlers where

import Language.LSP.Protocol.Message
import Unison.Debug qualified as Debug
import Unison.LSP.Types

initializedHandler :: TNotificationMessage 'Method_Initialized -> Lsp ()
initializedHandler _ = pure ()

withDebugging :: (Show m) => (m -> Lsp ()) -> (m -> Lsp ())
withDebugging handler message = do
  Debug.debugM Debug.LSP "Notification" message
  handler message
