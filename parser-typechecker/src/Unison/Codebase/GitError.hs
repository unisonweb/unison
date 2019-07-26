module Unison.Codebase.GitError where

import Data.Text (Text)
import System.IO (FilePath)
import qualified Unison.Names3 as Names

data GitError = NoGit
              | NoRemoteRepoAt Text
              | NoLocalRepoAt FilePath
              | CheckoutFailed Text
              -- url treeish Diff of what would change on merge with remote
              | PushSourceNotBeforeDestination Text Text Names.Diff
              | SomeOtherError Text
              deriving Show

