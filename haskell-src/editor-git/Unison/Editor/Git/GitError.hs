module Unison.Editor.Git.GitError where

import Unison.Prelude

import Unison.Codebase.Data (CodebasePath)
import Unison.Codebase.Data.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.Branch as Branch
import Unison.Editor.Data.RemoteRepo (RemoteRepo)

data GitError = NoGit
              | UnrecognizableCacheDir Text CodebasePath
              | UnrecognizableCheckoutDir Text CodebasePath
              | CloneException RemoteRepo String
              | PushException RemoteRepo String
              | PushNoOp RemoteRepo
              -- url commit Diff of what would change on merge with remote
              | PushDestinationHasNewStuff RemoteRepo
              | NoRemoteNamespaceWithHash RemoteRepo ShortBranchHash
              | RemoteNamespaceHashAmbiguous RemoteRepo ShortBranchHash (Set Branch.Hash)
              | CouldntLoadRootBranch RemoteRepo Branch.Hash
              | CouldntParseRootBranch RemoteRepo String
              | SomeOtherError String
              deriving Show
