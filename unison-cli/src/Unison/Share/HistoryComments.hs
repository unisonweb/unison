module Unison.Share.HistoryComments (uploadHistoryComments) where

import Control.Monad.Reader
import Data.Text qualified as Text
import Data.Void
import Servant.API
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Auth.Tokens (newTokenProvider)
import Unison.Cli.Monad
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.HistoryComment qualified as HC
import Unison.KeyThumbprint (KeyThumbprint (KeyThumbprint))
import Unison.Prelude
import Unison.Server.HistoryComments.Types
import Unison.Server.HistoryComments.Types qualified as Share
import Unison.Server.Types
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Sqlite qualified as Sqlite
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
  BranchRef ->
  Cli ()
uploadHistoryComments rootCausalHash32 codeserver branchRef = do
  Cli.Env {codebase, credentialManager} <- ask
  let path = "/ucm/v1/history-comments/upload?branchRef=" <> Text.unpack (toQueryParam branchRef)
  -- Enable compression
  let tokenProvider = newTokenProvider credentialManager
  result <- liftIO $ withCodeserverWebsocket @IO @(MsgOrError Void HistoryCommentChunk) @Text msgBufferSize codeserver tokenProvider path \Queues {send} -> do
    Codebase.runTransaction codebase $ do
      rootCausalHashId <- Q.expectCausalHashIdByCausalHash $ CausalHash $ Hash32.toHash rootCausalHash32
      Q.streamHistoryCommentsForCausal rootCausalHashId \getCommentId -> do
        let loop = do
              result <- runMaybeT $ do
                commentId <- MaybeT $ getCommentId
                (comment, revisions) <- lift $ Q.expectHistoryCommentById commentId
                success <- lift $ Sqlite.unsafeIO $ atomically $ send (Msg $ intoChunk (Left comment))
                guard success
                for_ revisions \revision -> do
                  success <- lift $ Sqlite.unsafeIO $ atomically $ send (Msg $ intoChunk (Right revision))
                  guard success
              -- Loop till a send fails or we run out of comments
              case result of
                Just () -> loop
                Nothing -> pure ()
        loop

  case result of
    Left err -> error $ "uploadCommentsClient:" <> show err
    Right ((), _leftovers {- Messages sent by server after we finished. -}) -> pure ()
  where
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
