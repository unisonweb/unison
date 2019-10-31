module Unison.Codebase.Editor.RemoteRepo where

import Unison.Prelude

data RemoteRepo = GitRepo { url :: Text, commit :: Maybe Text }
  deriving (Eq, Ord, Show)
