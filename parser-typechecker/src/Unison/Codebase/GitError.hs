module Unison.Codebase.GitError where

import Unison.Prelude

import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.RemoteRepo (ReadRepo, WriteRepo)
import U.Codebase.Sqlite.DbId (SchemaVersion)

type CodebasePath = FilePath

data GitError = NoGit
              | UnrecognizableCacheDir Text CodebasePath
              | UnrecognizableCheckoutDir Text CodebasePath
              | CloneException ReadRepo String
              | PushException WriteRepo String
              | PushNoOp WriteRepo
              -- url commit Diff of what would change on merge with remote
              | PushDestinationHasNewStuff WriteRepo
              | NoRemoteNamespaceWithHash ReadRepo ShortBranchHash
              | RemoteNamespaceHashAmbiguous ReadRepo ShortBranchHash (Set Branch.Hash)
              | CouldntLoadRootBranch ReadRepo Branch.Hash
              | CouldntParseRootBranch ReadRepo String
              | CouldntOpenCodebase ReadRepo CodebasePath
              | UnrecognizedSchemaVersion ReadRepo CodebasePath SchemaVersion
              | SomeOtherError String
              | CouldntLoadSyncedBranch Branch.Hash
              deriving Show
