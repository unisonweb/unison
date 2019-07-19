module Unison.Codebase.GitError where

import Data.Text (Text)
import System.IO (FilePath)

data GitError = NoGit
              | NoRemoteRepoAt Text
              | NoLocalRepoAt FilePath
              | CheckoutFailed Text
              deriving Show

