{-# LANGUAGE DataKinds #-}

module Unison.LSP.NotificationHandlers where

import Language.LSP.Protocol.Message qualified as Msg
import Unison.Debug qualified as Debug
import Unison.LSP.Types

initializedHandler :: Msg.TNotificationMessage 'Msg.Method_Initialized -> Lsp ()
initializedHandler _ = pure ()

withDebugging :: (Show m) => (m -> Lsp ()) -> (m -> Lsp ())
withDebugging handler message = do
  Debug.debugM Debug.LSP "Notification" message
  handler message

setTraceHandler :: Msg.TNotificationMessage 'Msg.Method_SetTrace -> Lsp ()
setTraceHandler _ =
  -- We don't handle trace messages yet, but we get warnings if we don't handle them.
  pure ()
