{-# LANGUAGE PolyKinds #-}

module Unison.LSP.HandlerUtils where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Map as Map
import Language.LSP.Types
import Language.LSP.Types.Lens as LSP
import qualified Unison.Debug as Debug
import Unison.LSP.Types
import Unison.Prelude
import UnliftIO (race_)
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Exception (finally)
import UnliftIO.MVar
import UnliftIO.STM
import UnliftIO.Timeout (timeout)

-- | Cancels an in-flight request
cancelRequest :: SomeLspId -> Lsp ()
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
  (Show (RequestMessage message), Show (ResponseResult message)) =>
  (RequestMessage message -> (Either ResponseError (ResponseResult message) -> Lsp ()) -> Lsp ()) ->
  RequestMessage message ->
  (Either ResponseError (ResponseResult message) -> Lsp ()) ->
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
  (RequestMessage message -> (Either ResponseError (ResponseResult message) -> Lsp ()) -> Lsp ()) ->
  RequestMessage message ->
  (Either ResponseError (ResponseResult message) -> Lsp ()) ->
  Lsp ()
withCancellation mayTimeoutMillis handler message respond = do
  let reqId = SomeLspId $ message ^. LSP.id
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
            Nothing -> respond $ cancelErr "Timeout"
            Just () -> pure ()
    cancelErr :: Text -> Either ResponseError b
    cancelErr msg = Left $ ResponseError RequestCancelled msg Nothing
    -- I intentionally defer adding the canceller until after we've started the request,
    -- No matter what it's possible for a message to be cancelled before the
    -- canceller has been added, but this means we're not blocking the request waiting for
    -- contention on the cancellation map on every request.
    -- The the majority of requests should be fast enough to complete "instantly" anyways.
    waitForCancel :: SomeLspId -> Lsp ()
    waitForCancel reqId = do
      barrier <- newEmptyMVar
      let canceller = void $ tryPutMVar barrier ()
      cancelMapVar <- asks cancellationMapVar
      atomically do
        modifyTVar' cancelMapVar (Map.insert reqId canceller)
      readMVar barrier
      let msg = "Request Cancelled by client"
      Debug.debugLogM Debug.LSP msg
      respond (cancelErr "Request cancelled by client")
