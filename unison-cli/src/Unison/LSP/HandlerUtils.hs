{-# LANGUAGE PolyKinds #-}

module Unison.LSP.HandlerUtils where

import Control.Lens
import Control.Monad.Reader
import Data.Map qualified as Map
import Language.LSP.Protocol.Lens as LSP
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.Debug qualified as Debug
import Unison.LSP.Types
import Unison.Prelude
import UnliftIO (race_)
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Exception (finally)
import UnliftIO.MVar
import UnliftIO.STM
import UnliftIO.Timeout (timeout)

-- | Cancels an in-flight request
cancelRequest :: Msg.SomeLspId -> Lsp ()
cancelRequest lspId = do
  cancelMapVar <- asks cancellationMapVar
  cancel <- atomically $ do
    cancellers <- readTVar cancelMapVar
    let (mayCancel, newMap) = Map.updateLookupWithKey (\_k _io -> Nothing) (lspId) cancellers
    case mayCancel of
      Nothing -> pure (pure ())
      Just cancel -> do
        writeTVar cancelMapVar newMap
        pure cancel
  liftIO cancel

withDebugging ::
  (Show (Msg.TRequestMessage message), Show (Msg.MessageResult message)) =>
  (Msg.TRequestMessage message -> (Either Msg.ResponseError (Msg.MessageResult message) -> Lsp ()) -> Lsp ()) ->
  Msg.TRequestMessage message ->
  (Either Msg.ResponseError (Msg.MessageResult message) -> Lsp ()) ->
  Lsp ()
withDebugging handler message respond = do
  Debug.debugM Debug.LSP "Request" message
  handler message \response -> do
    Debug.debugM Debug.LSP "Response" response
    respond response

-- | Handler middleware to add the ability for the client to cancel long-running in-flight requests.
withCancellation ::
  forall message.
  Maybe Int ->
  (Msg.TRequestMessage message -> (Either Msg.ResponseError (Msg.MessageResult message) -> Lsp ()) -> Lsp ()) ->
  Msg.TRequestMessage message ->
  (Either Msg.ResponseError (Msg.MessageResult message) -> Lsp ()) ->
  Lsp ()
withCancellation mayTimeoutMillis handler message respond = do
  let reqId = Msg.SomeLspId $ message ^. LSP.id
  -- The server itself seems to be single-threaded, so we need to fork in order to be able to
  -- process cancellation requests while still computing some other response
  void . forkIO $ flip finally (removeFromMap reqId) do
    withTimeout $ race_ (waitForCancel reqId) (handler message respond)
  where
    removeFromMap reqId = do
      cancelMapVar <- asks cancellationMapVar
      atomically $ modifyTVar cancelMapVar $ Map.delete reqId
    withTimeout :: Lsp () -> Lsp ()
    withTimeout action =
      case mayTimeoutMillis of
        Nothing -> action
        Just t -> do
          (timeout (t * 1000) action) >>= \case
            Nothing -> respond $ serverCancelErr "Timeout"
            Just () -> pure ()
    clientCancelErr :: Text -> Either Msg.ResponseError b
    clientCancelErr msg = Left $ Msg.ResponseError (InL LSPErrorCodes_RequestCancelled) msg Nothing
    serverCancelErr :: Text -> Either Msg.ResponseError b
    serverCancelErr msg = Left $ Msg.ResponseError (InL LSPErrorCodes_ServerCancelled) msg Nothing
    -- I intentionally defer adding the canceller until after we've started the request,
    -- No matter what it's possible for a message to be cancelled before the
    -- canceller has been added, but this means we're not blocking the request waiting for
    -- contention on the cancellation map on every request.
    -- The the majority of requests should be fast enough to complete "instantly" anyways.
    waitForCancel :: Msg.SomeLspId -> Lsp ()
    waitForCancel reqId = do
      barrier <- newEmptyMVar
      let canceller = void $ tryPutMVar barrier ()
      cancelMapVar <- asks cancellationMapVar
      atomically do
        modifyTVar' cancelMapVar (Map.insert reqId canceller)
      readMVar barrier
      let msg = "Request Cancelled by client"
      Debug.debugLogM Debug.LSP msg
      respond (clientCancelErr "Request cancelled by client")
