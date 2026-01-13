module Unison.Share.HistoryComments
  ( uploadHistoryComments,
    downloadHistoryComments,
  )
where

import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.List.NonEmpty qualified as NEL
import Data.Monoid (Any (..))
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as Text
import Data.Void
import Ki qualified
import Servant.API
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Auth.Tokens (newTokenProvider)
import Unison.Cli.Monad
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Debug qualified as Debug
import Unison.Hash (Hash)
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.HashTags
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
  result <- liftIO $ withCodeserverWebsocket @IO @(MsgOrError Void HistoryCommentUploaderChunk) @(MsgOrError UploadCommentsResponse HistoryCommentDownloaderChunk) msgBufferSize codeserver tokenProvider path \Queues {send, receive} -> Ki.scoped \scope -> do
    commentHashesToSendQ <- newTBMQueueIO @(HistoryCommentHash32, [HistoryCommentRevisionHash32]) 100
    commentHashesToUploadQ <- newTBMQueueIO @(Either HistoryCommentHash32 HistoryCommentRevisionHash32) 100
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
                (commentId, commentHash32) <- MaybeT $ getCommentIds
                revisionHashes <- lift $ Q.commentRevisionHashes commentId
                Debug.debugM Debug.Temp "Queueing comment for checking" commentHash32
                lift . Sqlite.unsafeIO $ atomically $ writeTBMQueue commentHashesToSendQ (HistoryCommentHash32 commentHash32, HistoryCommentRevisionHash32 <$> revisionHashes)
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
    Debug.debugLogM Debug.Temp "Uploading history comments: waiting for uploader thread to finish"
    atomically $ Ki.await uploaderThread
    Debug.debugLogM Debug.Temp "Done; closing connection"
  case result of
    Left err -> error $ "uploadCommentsClient: " <> show err
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
      TBMQueue (Either HistoryCommentHash32 HistoryCommentRevisionHash32) ->
      IO ()
    uploaderWorker codebase send uploadCommentQueue = do
      let loop = do
            hash <- MaybeT $ atomically (readTBMQueue uploadCommentQueue)
            mapMaybeT (Codebase.runTransaction codebase) $ do
              case hash of
                Left (HistoryCommentHash32 commentHash) -> do
                  Debug.debugM Debug.Temp "Uploading comment for hash" commentHash
                  commentId <- lift $ Q.expectHistoryCommentIdByHash32 commentHash
                  comment <- lift $ Q.expectHistoryCommentById commentId
                  success <- lift $ Sqlite.unsafeIO $ atomically $ send (Msg $ intoChunk (Left comment))
                  guard success
                Right (HistoryCommentRevisionHash32 revisionHash) -> do
                  revisionId <- lift $ Q.expectHistoryCommentRevisionIdByHash32 revisionHash
                  revision <- lift $ Q.expectHistoryCommentRevisionById revisionId
                  success <- lift $ Sqlite.unsafeIO $ atomically $ send (Msg $ intoChunk (Right revision))
                  guard success
            loop
      void . runMaybeT $ loop

    receiverWorker ::
      STM (Maybe (MsgOrError UploadCommentsResponse HistoryCommentDownloaderChunk)) ->
      TBMQueue
        ( Either
            HistoryCommentHash32
            HistoryCommentRevisionHash32
        ) ->
      TMVar Text ->
      TMVar () ->
      IO ()
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
              Just (UserErr err) -> do
                atomically $ putTMVar errMVar $ "uploadHistoryComments: server error: " <> tShow err
      loop

    hashNotifyWorker :: (MsgOrError Void HistoryCommentUploaderChunk -> STM Bool) -> TBMQueue (HistoryCommentHash32, [HistoryCommentRevisionHash32]) -> IO ()
    hashNotifyWorker send q = do
      let loop = do
            isClosed <- atomically $ do
              (hashesToCheck, isClosed) <- flushTBMQueue q
              Any serverClosed <-
                NEL.nonEmpty hashesToCheck & foldMapM \possiblyNewHashes -> do
                  Debug.debugM Debug.Temp "Sending possibly new hashes:" possiblyNewHashes
                  Any <$> (send $ Msg $ PossiblyNewHashesChunk possiblyNewHashes)
              when (isClosed || serverClosed) $
                Debug.debugLogM Debug.Temp "Hash notify worker: queue closed or server closed connection, no longer sending hashes"
              pure (isClosed || serverClosed)
            if isClosed
              then do
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

-- Re-run the given STM action at most n times, collecting the results into a list.
-- If the action returns Nothing, stop and return what has been collected so far, along with a Bool indicating whether the action was exhausted.
fetchChunk :: (Show a) => Int -> STM (Maybe a) -> STM ([a], Bool)
fetchChunk size action = do
  let go 0 = pure ([], False)
      go n = do
        optional action >>= \case
          Nothing -> do
            -- No more values available at the moment
            empty
          Just Nothing -> do
            -- Queue is closed
            pure ([], True)
          Just (Just val) -> do
            Debug.debugM Debug.Temp "Fetched value from queue" val
            (rest, exhausted) <- go (n - 1) <|> pure ([], False)
            pure (val : rest, exhausted)
  go size

downloadHistoryComments ::
  -- | The Unison Share URL.
  Codeserver.CodeserverURI ->
  -- | The remote branch to upload for.
  RepoInfo ->
  Cli ()
downloadHistoryComments codeserver repoInfo = do
  Cli.Env {codebase, credentialManager} <- ask
  let path = "/ucm/v1/history-comments/download?branchRef=" <> Text.unpack (toQueryParam repoInfo)
  -- Enable compression
  let tokenProvider = newTokenProvider credentialManager
  result <- liftIO $ withCodeserverWebsocket @IO @(MsgOrError Void HistoryCommentDownloaderChunk) @(MsgOrError DownloadCommentsResponse HistoryCommentUploaderChunk) msgBufferSize codeserver tokenProvider path \Queues {send, receive} -> Ki.scoped \scope -> do
    hashesToCheckQ <- liftIO $ newTBMQueueIO @(HistoryCommentHash32, [HistoryCommentRevisionHash32]) 100
    commentsQ <- liftIO $ newTBMQueueIO 100
    errMVar <- liftIO newEmptyTMVarIO
    _receiverThread <- liftIO $ Ki.fork scope $ receiverWorker receive errMVar hashesToCheckQ commentsQ
    inserterThread <- liftIO $ Ki.fork scope $ inserterWorker codebase commentsQ
    _hashCheckingThread <- liftIO $ Ki.fork scope $ hashCheckingWorker codebase send hashesToCheckQ
    Debug.debugLogM Debug.Temp "Downloading history comments: waiting for inserter thread to finish"
    -- The inserter thread will finish when the client closes the connection.
    atomically $ Ki.await inserterThread
  case result of
    Left connException -> error $ "downloadHistoryComments: " <> show connException
    Right ((), _leftovers) -> pure ()
  where
    inserterWorker ::
      Codebase.Codebase IO v a ->
      TBMQueue (Either HistoryComment HistoryCommentRevision) ->
      IO ()
    inserterWorker codebase commentsQ = do
      let loop = do
            (chunk, closed) <- atomically (fetchChunk insertCommentBatchSize (readTBMQueue commentsQ))
            when (not (null chunk)) $ do
              Debug.debugM Debug.Temp "Inserting comments chunk of size" (length chunk)
              Codebase.runTransaction codebase $ do
                for_ chunk \case
                  Left
                    HistoryComment
                      { author,
                        createdAt,
                        authorThumbprint,
                        causalHash,
                        commentHash
                      } -> do
                      causalHashId <- Q.expectCausalHashIdForHash32 causalHash
                      Q.insertHistoryComment
                        HC.HistoryComment
                          { author,
                            createdAt,
                            authorThumbprint = KeyThumbprint authorThumbprint,
                            causal = causalHashId,
                            commentId = HistoryCommentHash $ into @Hash commentHash
                          }
                  Right
                    HistoryCommentRevision
                      { subject,
                        content,
                        createdAt,
                        isHidden,
                        authorSignature,
                        revisionHash,
                        commentHash
                      } -> do
                      Q.insertHistoryCommentRevision
                        HC.HistoryCommentRevision
                          { subject,
                            content,
                            createdAt,
                            comment = HistoryCommentHash $ into @Hash commentHash,
                            isHidden = isHidden,
                            authorSignature = authorSignature,
                            revisionId = HistoryCommentRevisionHash $ into @Hash revisionHash
                          }
            when (not closed) loop
      loop
      Debug.debugLogM Debug.Temp "Inserter worker finished"

    hashCheckingWorker ::
      Codebase.Codebase IO v a ->
      (MsgOrError err HistoryCommentDownloaderChunk -> STM Bool) ->
      TBMQueue (HistoryCommentHash32, [HistoryCommentRevisionHash32]) ->
      IO ()
    hashCheckingWorker codebase send hashesToCheckQ = do
      let loop = do
            (hashes, closed) <- atomically (fetchChunk insertCommentBatchSize (readTBMQueue hashesToCheckQ))
            Debug.debugM Debug.Temp "Checking hashes chunk of size" (length hashes)
            when (not (null hashes)) $ do
              unknownHashes <- do
                Codebase.runTransaction codebase $ do
                  hashes & foldMapM \(HistoryCommentHash32 commentHash, revisionHashes) -> do
                    haveComment <- Q.haveHistoryComment commentHash
                    if haveComment
                      then do
                        revisionHashes & wither \(HistoryCommentRevisionHash32 revisionHash) -> do
                          Q.haveHistoryCommentRevision revisionHash
                            <&> \case
                              True -> Nothing
                              False -> Just $ Right $ HistoryCommentRevisionHash32 $ revisionHash
                      else do
                        pure (pure (Left $ HistoryCommentHash32 commentHash) <> (Right <$> revisionHashes))
              case NESet.nonEmptySet (Set.fromList unknownHashes) of
                Nothing -> pure ()
                Just unknownHashesSet -> do
                  void . atomically $ send $ Msg $ RequestCommentsChunk unknownHashesSet
            when (not closed) loop
      loop
      void . atomically $ send $ Msg $ DoneCheckingHashesChunk
      Debug.debugLogM Debug.Temp "Hash checking worker finished"
    receiverWorker ::
      STM (Maybe (MsgOrError DownloadCommentsResponse HistoryCommentUploaderChunk)) ->
      TMVar Text ->
      TBMQueue
        ( HistoryCommentHash32,
          [HistoryCommentRevisionHash32]
        ) ->
      TBMQueue (Either HistoryComment HistoryCommentRevision) ->
      IO ()
    receiverWorker recv errMVar hashesToCheckQ commentsQ = do
      let loop = do
            next <- atomically do
              recv >>= \case
                Nothing -> do
                  closeTBMQueue hashesToCheckQ
                  closeTBMQueue commentsQ
                  pure (pure ())
                Just (DeserialiseFailure err) -> do
                  putTMVar errMVar $ "downloadHistoryComments: deserialisation failure: " <> err
                  pure (pure ())
                Just (UserErr err) -> do
                  putTMVar errMVar $ "downloadHistoryComments: server error: " <> tShow err
                  pure (pure ())
                Just (Msg msg) -> do
                  case msg of
                    PossiblyNewHashesChunk hashesToCheck -> do
                      for_ hashesToCheck $ \h -> writeTBMQueue hashesToCheckQ h
                    DoneSendingHashesChunk -> do
                      closeTBMQueue hashesToCheckQ
                    HistoryCommentChunk comment -> do
                      writeTBMQueue commentsQ (Left comment)
                    HistoryCommentRevisionChunk revision -> do
                      writeTBMQueue commentsQ (Right revision)
                  pure loop
            next
      loop
      Debug.debugLogM Debug.Temp "Receiver worker finished"
    insertCommentBatchSize = 100
