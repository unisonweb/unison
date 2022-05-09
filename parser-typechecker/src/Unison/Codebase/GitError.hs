{-# LANGUAGE DeriveAnyClass #-}

module Unison.Codebase.GitError where

import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace, ReadGitRepo, WriteGitRepo)
import Unison.Codebase.Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Prelude

type CodebasePath = FilePath

data GitProtocolError
  = NoGit
  | UnrecognizableCacheDir ReadGitRepo CodebasePath
  | UnrecognizableCheckoutDir ReadGitRepo CodebasePath
  | --            srcPath  destPath error-description
    CopyException FilePath FilePath String
  | CloneException ReadGitRepo String
  | PushException WriteGitRepo String
  | PushNoOp WriteGitRepo
  | -- url commit Diff of what would change on merge with remote
    PushDestinationHasNewStuff WriteGitRepo
  | CleanupError SomeException
  | -- Thrown when a commit, tag, or branch isn't found in a repo.
    --                repo ref
    RemoteRefNotFound Text Text
  deriving stock (Show)
  deriving anyclass (Exception)

data GitCodebaseError h
  = NoRemoteNamespaceWithHash ReadGitRepo ShortBranchHash
  | RemoteNamespaceHashAmbiguous ReadGitRepo ShortBranchHash (Set h)
  | CouldntLoadRootBranch ReadGitRepo h
  | CouldntParseRemoteBranch ReadGitRepo String
  | CouldntLoadSyncedBranch ReadRemoteNamespace h
  | CouldntFindRemoteBranch ReadGitRepo Path
  deriving (Show)
