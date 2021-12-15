{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Codebase.GitError where

import Unison.Prelude

import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Codebase.Editor.RemoteRepo (ReadRepo, WriteRepo, ReadRemoteNamespace)
import Unison.Codebase.Path

type CodebasePath = FilePath

data GitProtocolError
  = NoGit
  | UnrecognizableCacheDir ReadRepo CodebasePath
  | UnrecognizableCheckoutDir ReadRepo CodebasePath
  | CloneException ReadRepo String
  | PushException WriteRepo String
  | PushNoOp WriteRepo
    -- url commit Diff of what would change on merge with remote
  | PushDestinationHasNewStuff WriteRepo
  | CleanupError SomeException
  deriving Show

data GitCodebaseError h
  = NoRemoteNamespaceWithHash ReadRepo ShortBranchHash
  | RemoteNamespaceHashAmbiguous ReadRepo ShortBranchHash (Set h)
  | CouldntLoadRootBranch ReadRepo h
  | CouldntLoadSyncedBranch ReadRemoteNamespace h
  | CouldntFindRemoteBranch ReadRepo Path
  deriving Show
