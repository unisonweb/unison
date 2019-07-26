module Unison.Codebase.GitError where

import Data.Text (Text)
import System.IO (FilePath)
import qualified Unison.Names3 as Names

data GitError = NoGit
              | NoRemoteRepoAt Text
              | NoLocalRepoAt FilePath
              | CheckoutFailed Text
              -- Gives Diff of what's new at the remote
              | PushSourceNotBeforeDestination Text Names.Diff
              | SomeOtherError Text
              deriving Show

