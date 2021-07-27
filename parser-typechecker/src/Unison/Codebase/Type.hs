{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Type (Codebase (..), CodebasePath, GitError(..), GetRootBranchError (..), SyncToDir) where

import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace, WriteRepo)
import Unison.Codebase.Patch (Patch)
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Codebase.SyncMode (SyncMode)
import Unison.CodebasePath (CodebasePath)
import Unison.DataDeclaration (Decl)
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.WatchKind as WK
import Unison.Codebase.GitError (GitProtocolError, GitCodebaseError)
import Unison.Codebase.FileCodebase.Codebase (GitFileCodebaseError)
import Unison.Codebase.SqliteCodebase.GitError (GitSqliteCodebaseError)

type SyncToDir m =
  CodebasePath -> -- dest codebase
  SyncMode ->
  Branch m -> -- branch to sync to dest codebase
  m ()

-- | Abstract interface to a user's codebase.
--
-- One implementation is 'Unison.Codebase.FileCodebase' which uses the filesystem.
data Codebase m v a = Codebase
  { getTerm :: Reference.Id -> m (Maybe (Term v a)),
    getTypeOfTermImpl :: Reference.Id -> m (Maybe (Type v a)),
    getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a)),
    putTerm :: Reference.Id -> Term v a -> Type v a -> m (),
    putTypeDeclaration :: Reference.Id -> Decl v a -> m (),
    getRootBranch :: m (Either GetRootBranchError (Branch m)),
    putRootBranch :: Branch m -> m (),
    rootBranchUpdates :: m (IO (), IO (Set Branch.Hash)),
    getBranchForHash :: Branch.Hash -> m (Maybe (Branch m)),
    putBranch :: Branch m -> m (),
    branchExists :: Branch.Hash -> m Bool,
    getPatch :: Branch.EditHash -> m (Maybe Patch),
    putPatch :: Branch.EditHash -> Patch -> m (),
    patchExists :: Branch.EditHash -> m Bool,
    dependentsImpl :: Reference -> m (Set Reference.Id),
    -- This copies all the dependencies of `b` from the specified Codebase into this one
    syncFromDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    -- This copies all the dependencies of `b` from this Codebase
    syncToDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    viewRemoteBranch' :: ReadRemoteNamespace -> m (Either GitError (m (), Branch m, CodebasePath)),
    pushGitRootBranch :: Branch m -> WriteRepo -> SyncMode -> m (Either GitError ()),
    -- Watch expressions are part of the codebase, the `Reference.Id` is
    -- the hash of the source of the watch expression, and the `Term v a`
    -- is the evaluated result of the expression, decompiled to a term.
    watches :: WK.WatchKind -> m [Reference.Id],
    getWatch :: WK.WatchKind -> Reference.Id -> m (Maybe (Term v a)),
    putWatch :: WK.WatchKind -> Reference.Id -> Term v a -> m (),
    clearWatches :: m (),
    getReflog :: m [Reflog.Entry Branch.Hash],
    appendReflog :: Text -> Branch m -> Branch m -> m (),
    -- list of terms of the given type
    termsOfTypeImpl :: Reference -> m (Set Referent.Id),
    -- list of terms that mention the given type anywhere in their signature
    termsMentioningTypeImpl :: Reference -> m (Set Referent.Id),
    -- number of base58 characters needed to distinguish any two references in the codebase
    hashLength :: m Int,
    termReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
    typeReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
    termReferentsByPrefix :: ShortHash -> m (Set Referent.Id),
    branchHashLength :: m Int,
    branchHashesByPrefix :: ShortBranchHash -> m (Set Branch.Hash),
    -- returns `Nothing` to not implemented, fallback to in-memory
    --    also `Nothing` if no LCA
    -- The result is undefined if the two hashes are not in the codebase.
    -- Use `Codebase.lca` which wraps this in a nice API.
    lcaImpl :: Maybe (Branch.Hash -> Branch.Hash -> m (Maybe Branch.Hash)),
    -- `beforeImpl` returns `Nothing` if not implemented by the codebase
    -- `beforeImpl b1 b2` is undefined if `b2` not in the codebase
    --
    --  Use `Codebase.before` which wraps this in a nice API.
    beforeImpl :: Maybe (Branch.Hash -> Branch.Hash -> m Bool)
  }

data GetRootBranchError
  = NoRootBranch
  | CouldntParseRootBranch String
  | CouldntLoadRootBranch Branch.Hash
  deriving Show

data GitError
  = GitProtocolError GitProtocolError
  | GitCodebaseError (GitCodebaseError Branch.Hash)
  | GitFileCodebaseError GitFileCodebaseError
  | GitSqliteCodebaseError GitSqliteCodebaseError
  deriving Show