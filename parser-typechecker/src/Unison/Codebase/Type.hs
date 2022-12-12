{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Type
  ( Codebase (..),
    CodebasePath,
    PushGitBranchOpts (..),
    GitPushBehavior (..),
    GitError (..),
    SyncToDir,
    LocalOrRemote (..),
    gitErrorFromOpenCodebaseError,
  )
where

import U.Codebase.HashTags (BranchHash, CausalHash)
import qualified U.Codebase.Reference as V2
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.RemoteRepo (ReadGitRemoteNamespace, ReadGitRepo, WriteGitRepo)
import Unison.Codebase.GitError (GitCodebaseError, GitProtocolError)
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (..))
import Unison.Codebase.Path (Path)
import Unison.Codebase.SqliteCodebase.GitError (GitSqliteCodebaseError (..))
import Unison.Codebase.SyncMode (SyncMode)
import Unison.CodebasePath (CodebasePath)
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (Decl)
import Unison.Hash (Hash)
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.Sqlite as Sqlite
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.WatchKind as WK

type SyncToDir m =
  CodebasePath -> -- dest codebase
  SyncMode ->
  Branch m -> -- branch to sync to dest codebase
  m ()

-- | Abstract interface to a user's codebase.
data Codebase m v a = Codebase
  { -- | Get a user-defined term from the codebase.
    --
    -- Note that it is possible to call 'putTerm', then 'getTerm', and receive @Nothing@, per the semantics of
    -- 'putTerm'.
    getTerm :: Reference.Id -> m (Maybe (Term v a)),
    -- | Get the type of a user-defined term.
    --
    -- Note that it is possible to call 'putTerm', then 'getTypeOfTermImpl', and receive @Nothing@, per the semantics of
    -- 'putTerm'.
    getTypeOfTermImpl :: Reference.Id -> Sqlite.Transaction (Maybe (Type v a)),
    -- | Get a type declaration.
    --
    -- Note that it is possible to call 'putTypeDeclaration', then 'getTypeDeclaration', and receive @Nothing@, per the
    -- semantics of 'putTypeDeclaration'.
    getTypeDeclaration :: Reference.Id -> Sqlite.Transaction (Maybe (Decl v a)),
    -- | Get the type of a given decl.
    getDeclType :: V2.Reference -> m CT.ConstructorType,
    -- | Enqueue the put of a user-defined term (with its type) into the codebase, if it doesn't already exist. The
    -- implementation may choose to delay the put until all of the term's (and its type's) references are stored as
    -- well.
    putTerm :: Reference.Id -> Term v a -> Type v a -> Sqlite.Transaction (),
    putTermComponent :: Hash -> [(Term v a, Type v a)] -> Sqlite.Transaction (),
    -- | Enqueue the put of a type declaration into the codebase, if it doesn't already exist. The implementation may
    -- choose to delay the put until all of the type declaration's references are stored as well.
    putTypeDeclaration :: Reference.Id -> Decl v a -> Sqlite.Transaction (),
    putTypeDeclarationComponent :: Hash -> [Decl v a] -> Sqlite.Transaction (),
    -- getTermComponent :: Hash -> m (Maybe [Term v a]),
    getTermComponentWithTypes :: Hash -> Sqlite.Transaction (Maybe [(Term v a, Type v a)]),
    -- | Get the root branch.
    getRootBranch :: m (Branch m),
    -- | Like 'putBranch', but also adjusts the root branch pointer afterwards.
    putRootBranch ::
      Text -> -- Reason for the change, will be recorded in the reflog
      Branch m ->
      m (),
    getBranchForHashImpl :: CausalHash -> m (Maybe (Branch m)),
    -- | Put a branch into the codebase, which includes its children, its patches, and the branch itself, if they don't
    -- already exist.
    --
    -- The terms and type declarations that a branch references must already exist in the codebase.
    putBranch :: Branch m -> m (),
    -- | Copy a branch and all of its dependencies from the given codebase into this one.
    syncFromDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    -- | Copy a branch and all of its dependencies from this codebase into the given codebase.
    syncToDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    viewRemoteBranch' :: forall r. ReadGitRemoteNamespace -> Git.GitBranchBehavior -> ((Branch m, CodebasePath) -> m r) -> m (Either GitError r),
    -- | Push the given branch to the given repo, and optionally set it as the root branch.
    pushGitBranch :: forall e. WriteGitRepo -> PushGitBranchOpts -> (Branch m -> m (Either e (Branch m))) -> m (Either GitError (Either e (Branch m))),
    -- | @getWatch k r@ returns watch result @t@ that was previously put by @putWatch k r t@.
    getWatch :: WK.WatchKind -> Reference.Id -> Sqlite.Transaction (Maybe (Term v a)),
    -- | Get the set of user-defined terms-or-constructors that have the given type.
    termsOfTypeImpl :: Reference -> Sqlite.Transaction (Set Referent.Id),
    -- | Get the set of user-defined terms-or-constructors mention the given type anywhere in their signature.
    termsMentioningTypeImpl :: Reference -> Sqlite.Transaction (Set Referent.Id),
    -- | Get the set of user-defined terms-or-constructors whose hash matches the given prefix.
    termReferentsByPrefix :: ShortHash -> m (Set Referent.Id),
    -- Updates the root namespace names index from an old BranchHash to a new one.
    -- This isn't run automatically because it can be a bit slow.
    updateNameLookup ::
      -- Path to the root of the _changes_.
      -- E.g. if you know that all the changes occur at "base.List", you can pass "base.List"
      -- here, and pass the old and new branch hashes for the branch as "base.List".
      -- This allows us to avoid searching for changes in areas where it's impossible for it
      -- to have occurred.
      Path ->
      -- The branch hash at 'Path' which the existing index was built from.
      -- Pass 'Nothing' to build the index from scratch (i.e. compute a diff from an empty branch).
      Maybe BranchHash ->
      -- The new branch
      BranchHash ->
      Sqlite.Transaction (),
    -- | Acquire a new connection to the same underlying database file this codebase object connects to.
    withConnection :: forall x. (Sqlite.Connection -> m x) -> m x,
    -- | Acquire a new connection to the same underlying database file this codebase object connects to.
    withConnectionIO :: forall x. (Sqlite.Connection -> IO x) -> IO x
  }

-- | Whether a codebase is local or remote.
data LocalOrRemote
  = Local
  | Remote
  deriving (Show, Eq, Ord)

data PushGitBranchOpts = PushGitBranchOpts
  { behavior :: GitPushBehavior,
    syncMode :: SyncMode
  }

data GitPushBehavior
  = -- | Don't set root, just sync entities.
    GitPushBehaviorGist
  | -- | After syncing entities, do a fast-forward check, then set the root.
    GitPushBehaviorFf
  | -- | After syncing entities, just set the root (force-pushy).
    GitPushBehaviorForce

data GitError
  = GitProtocolError GitProtocolError
  | GitCodebaseError (GitCodebaseError CausalHash)
  | GitSqliteCodebaseError GitSqliteCodebaseError
  deriving (Show)

instance Exception GitError

gitErrorFromOpenCodebaseError :: CodebasePath -> ReadGitRepo -> OpenCodebaseError -> GitSqliteCodebaseError
gitErrorFromOpenCodebaseError path repo = \case
  OpenCodebaseDoesntExist -> NoDatabaseFile repo path
  OpenCodebaseUnknownSchemaVersion v ->
    UnrecognizedSchemaVersion repo path (fromIntegral v)
  OpenCodebaseRequiresMigration fromSv toSv ->
    CodebaseRequiresMigration fromSv toSv
  OpenCodebaseFileLockFailed -> CodebaseFileLockFailed
