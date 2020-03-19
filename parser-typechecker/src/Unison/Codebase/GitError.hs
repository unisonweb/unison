module Unison.Codebase.GitError where

import Unison.Prelude

import qualified Unison.Names3 as Names
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.Branch as Branch

data GitError = NoGit
              | NoRemoteRepoAt Text
              | NoLocalRepoAt FilePath
              | CheckoutFailed Text
              -- url commit Diff of what would change on merge with remote
              | PushDestinationHasNewStuff Text (Maybe Text) Names.Diff
              | NoRemoteNamespaceWithHash Text (Maybe Text) ShortBranchHash
              | RemoteNamespaceHashAmbiguous Text (Maybe Text) ShortBranchHash (Set Branch.Hash)
              | Couldn'tLoadRootBranch Text (Maybe Text) (Maybe ShortBranchHash) Branch.Hash
              | SomeOtherError Text
              deriving Show

