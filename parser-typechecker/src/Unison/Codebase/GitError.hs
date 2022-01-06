module Unison.Codebase.GitError where

import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace, ReadRepo, WriteRepo)
import Unison.Codebase.Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Prelude

type CodebasePath = FilePath

data GitProtocolError
  = NoGit
  | UnrecognizableCacheDir ReadRepo CodebasePath
  | UnrecognizableCheckoutDir ReadRepo CodebasePath
  | --            srcPath  destPath error-description
    CopyException FilePath FilePath String
  | CloneException ReadRepo String
  | PushException WriteRepo String
  | PushNoOp WriteRepo
  | -- url commit Diff of what would change on merge with remote
    PushDestinationHasNewStuff WriteRepo
  | CleanupError SomeException
  deriving (Show)

data GitCodebaseError h
  = NoRemoteNamespaceWithHash ReadRepo ShortBranchHash
  | RemoteNamespaceHashAmbiguous ReadRepo ShortBranchHash (Set h)
  | CouldntLoadRootBranch ReadRepo h
  | CouldntLoadSyncedBranch ReadRemoteNamespace h
  | CouldntFindRemoteBranch ReadRepo Path
  deriving (Show)
