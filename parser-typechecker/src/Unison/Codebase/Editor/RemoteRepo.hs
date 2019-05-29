module Unison.Codebase.Editor.RemoteRepo where

import Data.Text (Text)

data RemoteRepo
  = Github { username :: Text, repo :: Text, commit :: Text }
  deriving (Eq, Ord, Show)
