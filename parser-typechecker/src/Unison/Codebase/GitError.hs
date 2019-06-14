module Unison.Codebase.GitError where

import Data.Text (Text)
import System.IO (FilePath)

data GitError = NoGit
              | NoGithubAt Text
              | NotAGitRepo FilePath
              | CheckoutFailed Text
              deriving Show

