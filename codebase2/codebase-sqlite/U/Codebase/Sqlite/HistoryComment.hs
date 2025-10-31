module U.Codebase.Sqlite.HistoryComment (HistoryComment (..)) where

import Data.Text (Text)

data HistoryComment causal id = HistoryComment
  { author :: Text,
    subject :: Text,
    content :: Text,
    causal :: causal,
    commentId :: id
  }
  deriving (Show, Eq, Functor)
