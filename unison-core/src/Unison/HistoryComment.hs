module Unison.HistoryComment
  ( LatestHistoryComment,
    HistoryComment (..),
    HistoryCommentRevision (..),
  )
where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

type LatestHistoryComment thumbprint causal commentId =
  HistoryCommentRevision UTCTime (HistoryComment UTCTime thumbprint causal commentId)

data HistoryComment createdAt thumbprint causal commentId = HistoryComment
  { author :: Text,
    -- The time the comment was created.
    createdAt :: createdAt,
    authorThumbprint :: thumbprint,
    causal :: causal,
    commentId :: commentId
  }
  deriving (Show, Eq, Functor)

data HistoryCommentRevision createdAt comment = HistoryCommentRevision
  { subject :: Text,
    content :: Text,
    createdAt :: createdAt,
    -- The comment this is a revision for.
    comment :: comment
  }
  deriving (Show, Eq, Functor)
