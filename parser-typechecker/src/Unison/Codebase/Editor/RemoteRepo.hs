module Unison.Codebase.Editor.RemoteRepo where

import Data.Text (Text)

data RemoteRepo = GitRepo { url :: Text, commit :: Text }
  deriving (Eq, Ord, Show)
