module U.Codebase.Sqlite.HistoryComment (HistoryComment (..)) where

import Data.Text (Text)

data HistoryComment id = HistoryComment
  { author :: Text,
    subject :: Text,
    content :: Text,
    commentId :: id
  }
  deriving (Show, Eq, Functor)
