module Unison.Codebase.GitError where

import Unison.Prelude

import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.RemoteRepo (RemoteRepo)
import U.Codebase.Sqlite.DbId (SchemaVersion)

type CodebasePath = FilePath

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
              | CouldntOpenCodebase RemoteRepo CodebasePath
              | UnrecognizedSchemaVersion RemoteRepo CodebasePath SchemaVersion
              | SomeOtherError String
              | CouldntLoadSyncedBranch Branch.Hash
              deriving Show
