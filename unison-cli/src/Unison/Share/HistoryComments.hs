module Unison.Share.HistoryComments (uploadHistoryComments) where

import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.Monoid (Any (..))
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as Text
import Data.Void
import Ki qualified
import Servant.API
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Auth.Tokens (newTokenProvider)
import Unison.Cli.Monad
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Debug qualified as Debug
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.HistoryComment qualified as HC
import Unison.KeyThumbprint (KeyThumbprint (KeyThumbprint))
import Unison.Prelude
import Unison.Server.HistoryComments.Types
import Unison.Server.HistoryComments.Types qualified as Share
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Sqlite qualified as Sqlite
import Unison.Sync.Types (RepoInfo)
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Websockets
import UnliftIO.STM

-- type HistoryCommentsAPI = ("ucm" :> "v1" :> "history-comments" :> HistoryCommentsAPI.API)

-- downloadCommentsClientM :: BranchRef -> WS.Connection -> Servant.ClientM ()
-- uploadCommentsClientM :: BranchRef -> WS.Connection -> Servant.ClientM ()
-- HistoryCommentsAPI.Routes
--   { uploadHistoryComments = downloadCommentsClientM,
--     downloadHistoryComments = uploadCommentsClientM
--   } = Servant.client historyCommentsAPI

-- | Number of comment chunks that can be queued up in the websockets buffer.
msgBufferSize :: Int
msgBufferSize = 20

uploadHistoryComments ::
  -- | The local branch causal to upload comments for.
  Hash32 ->
  -- | The Unison Share URL.
  Codeserver.CodeserverURI ->
  -- | The remote branch to upload for.
  RepoInfo ->
  Cli ()
uploadHistoryComments rootCausalHash32 codeserver repoInfo = do
  Cli.Env {codebase, credentialManager} <- ask
  let path = "/ucm/v1/history-comments/upload?branchRef=" <> Text.unpack (toQueryParam repoInfo)
  -- Enable compression
  let tokenProvider = newTokenProvider credentialManager
  result <- liftIO $ withCodeserverWebsocket @IO @(MsgOrError Void HistoryCommentUploaderChunk) @(MsgOrError Void HistoryCommentDownloaderChunk) msgBufferSize codeserver tokenProvider path \Queues {send, receive} -> Ki.scoped \scope -> do
    commentHashesToSendQ <- newTBMQueueIO 100
    commentHashesToUploadQ <- newTBMQueueIO 100
    -- Is filled when the server notifies us it's done requesting comments
    doneRequestingCommentsMVar <- newEmptyTMVarIO
    errMVar <- newEmptyTMVarIO
    _ <- Ki.fork scope (hashNotifyWorker send commentHashesToSendQ)
    uploaderThread <- Ki.fork scope (uploaderWorker codebase send commentHashesToUploadQ)
    _ <- Ki.fork scope (receiverWorker receive commentHashesToUploadQ errMVar doneRequestingCommentsMVar)
    Codebase.runTransaction codebase $ do
      rootCausalHashId <- Q.expectCausalHashIdByCausalHash $ CausalHash $ Hash32.toHash rootCausalHash32
      Q.streamHistoryCommentsForCausal rootCausalHashId \getCommentIds -> do
        let loop = do
              result <- runMaybeT $ do
                (_commentId, commentHash32) <- MaybeT $ getCommentIds
                lift . Sqlite.unsafeIO $ atomically $ writeTBMQueue commentHashesToSendQ commentHash32
              -- Loop till a send fails or we run out of comments
              case result of
                Just () -> loop
                Nothing -> pure ()
        loop
    -- Close the hashes queue to signal we don't have any more, then wait for the notifier to finish
    atomically $ closeTBMQueue commentHashesToSendQ
    -- Once the comment Hash queue is closed, eventually we'll send a DoneSendingHashesChunk message,
    -- the server will respond with a DoneCheckingHashesChunk message after it's made all necessary
    -- requests.
    --
    -- Then we can close the comment upload hash queue to signal we won't get any more upload requests.
    atomically $ readTMVar doneRequestingCommentsMVar >> closeTBMQueue commentHashesToUploadQ
    -- Now we just have to wait for the uploader to finish sending all the comments we have queued up.
    -- Once we've uploaded everything we can safely exit and the connection will be closed.
    atomically $ Ki.await uploaderThread
  case result of
    Left err -> error $ "uploadCommentsClient:" <> show err
    Right ((), _leftovers {- Messages sent by server after we finished. -}) -> pure ()
  where
    -- Read all available values from a TBMQueue, returning them and whether the queue is closed.
    flushTBMQueue :: TBMQueue a -> STM ([a], Bool)
    flushTBMQueue q = do
      optional (readTBMQueue q) >>= \case
        -- No values available
        Nothing -> empty
        Just Nothing -> do
          -- Queue closed
          pure ([], True)
        Just (Just v) -> do
          (vs, closed) <- flushTBMQueue q <|> pure ([], False)
          pure (v : vs, closed)
    uploaderWorker ::
      Codebase.Codebase IO v a ->
      ( MsgOrError err HistoryCommentUploaderChunk ->
        STM Bool
      ) ->
      TBMQueue Hash32 ->
      IO ()
    uploaderWorker codebase send uploadCommentQueue = do
      let loop = do
            commentHash <- MaybeT $ atomically (readTBMQueue uploadCommentQueue)
            Debug.debugM Debug.Temp "Uploading comment" commentHash
            mapMaybeT (Codebase.runTransaction codebase) $ do
              commentId <- lift $ Q.expectHistoryCommentIdByHash32 commentHash
              (comment, revisions) <- lift $ Q.expectHistoryCommentById commentId
              success <- lift $ Sqlite.unsafeIO $ atomically $ send (Msg $ intoChunk (Left comment))
              guard success
              for_ revisions \revision -> do
                success <- lift $ Sqlite.unsafeIO $ atomically $ send (Msg $ intoChunk (Right revision))
                guard success
            loop
      void . runMaybeT $ loop

    receiverWorker :: STM (Maybe (MsgOrError Void HistoryCommentDownloaderChunk)) -> TBMQueue Hash32 -> TMVar Text -> TMVar () -> IO ()
    receiverWorker receive toUploadQ errMVar doneRequestingCommentsMVar = do
      let loop = do
            msgOrError <- atomically receive
            case msgOrError of
              -- Channel closed, shut down
              Nothing -> pure ()
              Just (Msg msg) -> case msg of
                DoneCheckingHashesChunk -> do
                  -- Notify that the server is done requesting comments
                  atomically $ putTMVar doneRequestingCommentsMVar ()
                  loop
                RequestCommentsChunk comments -> do
                  atomically $ for_ comments $ writeTBMQueue toUploadQ
                  loop
              Just (DeserialiseFailure msg) -> do
                atomically $ putTMVar errMVar $ "uploadHistoryComments: deserialisation failure: " <> msg
      loop

    hashNotifyWorker :: (MsgOrError Void HistoryCommentUploaderChunk -> STM Bool) -> TBMQueue Hash32 -> IO ()
    hashNotifyWorker send q = do
      let loop = do
            isClosed <- atomically $ do
              (newHashes, isClosed) <- flushTBMQueue q
              Any serverClosed <-
                (NESet.nonEmptySet $ Set.fromList newHashes) & foldMapM \newHashesSet -> do
                  Debug.debugM Debug.Temp "Notifying server of comment hashes" newHashesSet
                  Any <$> (send $ Msg $ PossiblyNewHashesChunk newHashesSet)
              pure (isClosed || serverClosed)
            if isClosed
              then do
                Debug.debugM Debug.Temp "sending DoneSendingHashesChunk" ()
                -- If the queue is closed, send a DoneCheckingHashesChunk to notify the server we're done.
                void . atomically $ send (Msg DoneSendingHashesChunk)
              else loop
      loop
    intoChunk = \case
      Left
        ( HC.HistoryComment
            { author,
              createdAt,
              authorThumbprint = KeyThumbprint authorThumbprint,
              causal,
              commentId = commentHash
            }
          ) ->
          HistoryCommentChunk
            Share.HistoryComment
              { author,
                createdAt,
                authorThumbprint,
                Share.causalHash = causal,
                commentHash
              }
      Right
        ( HC.HistoryCommentRevision
            { subject,
              content,
              createdAt,
              comment = commentHash,
              isHidden,
              authorSignature,
              revisionId
            }
          ) ->
          HistoryCommentRevisionChunk
            Share.HistoryCommentRevision
              { subject,
                content,
                createdAt,
                isHidden,
                authorSignature,
                revisionHash = revisionId,
                commentHash
              }
