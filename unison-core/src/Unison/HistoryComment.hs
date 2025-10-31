module Unison.HistoryComment
  ( LatestHistoryComment,
    HistoryComment (..),
    HistoryCommentRevision (..),
  )
where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

type LatestHistoryComment thumbprint causal revisionId commentId =
  HistoryCommentRevision revisionId UTCTime (HistoryComment UTCTime thumbprint causal commentId)

data HistoryComment createdAt thumbprint causal commentId = HistoryComment
  { author :: Text,
    -- The time the comment was created.
    createdAt :: createdAt,
    authorThumbprint :: thumbprint,
    causal :: causal,
    commentId :: commentId
  }
  deriving (Show, Eq, Functor)

data HistoryCommentRevision revisionId createdAt comment = HistoryCommentRevision
  { subject :: Text,
    content :: Text,
    createdAt :: createdAt,
    -- The comment this is a revision for.
    comment :: comment,
    revisionId :: revisionId
  }
  deriving (Show, Eq, Functor)
