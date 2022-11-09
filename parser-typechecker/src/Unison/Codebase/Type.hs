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

import qualified U.Codebase.Branch as V2
import U.Codebase.HashTags (BranchHash)
import qualified U.Codebase.Reference as V2
import qualified U.Codebase.Reflog as Reflog
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.RemoteRepo (ReadGitRemoteNamespace, ReadGitRepo, WriteGitRepo)
import Unison.Codebase.GitError (GitCodebaseError, GitProtocolError)
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (..))
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.SqliteCodebase.GitError (GitSqliteCodebaseError (..))
import Unison.Codebase.SyncMode (SyncMode)
import Unison.CodebasePath (CodebasePath)
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (Decl)
import Unison.Hash (Hash)
import Unison.Names.Scoped (ScopedNames)
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
    putTerm :: Reference.Id -> Term v a -> Type v a -> m (),
    putTermComponent :: Hash -> [(Term v a, Type v a)] -> Sqlite.Transaction (),
    -- | Enqueue the put of a type declaration into the codebase, if it doesn't already exist. The implementation may
    -- choose to delay the put until all of the type declaration's references are stored as well.
    putTypeDeclaration :: Reference.Id -> Decl v a -> m (),
    putTypeDeclarationComponent :: Hash -> [Decl v a] -> Sqlite.Transaction (),
    -- getTermComponent :: Hash -> m (Maybe [Term v a]),
    getTermComponentWithTypes :: Hash -> Sqlite.Transaction (Maybe [(Term v a, Type v a)]),
    getDeclComponent :: Hash -> Sqlite.Transaction (Maybe [Decl v a]),
    getComponentLength :: Hash -> m (Maybe Reference.CycleSize),
    -- | Get the root causal Hash.
    getRootCausalHash :: m V2.CausalHash,
    -- | Get the root branch.
    getRootBranch :: m (Branch m),
    -- | Get whether the root branch exists.
    getRootBranchExists :: m Bool,
    -- | Like 'putBranch', but also adjusts the root branch pointer afterwards.
    putRootBranch ::
      Text -> -- Reason for the change, will be recorded in the reflog
      Branch m ->
      m (),
    getShallowCausalForHash :: V2.CausalHash -> m (V2.CausalBranch m),
    getBranchForHashImpl :: Branch.CausalHash -> m (Maybe (Branch m)),
    -- | Put a branch into the codebase, which includes its children, its patches, and the branch itself, if they don't
    -- already exist.
    --
    -- The terms and type declarations that a branch references must already exist in the codebase.
    putBranch :: Branch m -> m (),
    -- | Check whether the given branch exists in the codebase.
    branchExists :: Branch.CausalHash -> m Bool,
    -- | Get a patch from the codebase.
    getPatch :: Branch.EditHash -> m (Maybe Patch),
    -- | Put a patch into the codebase.
    --
    -- Note that 'putBranch' may also put patches.
    putPatch :: Branch.EditHash -> Patch -> m (),
    -- | Check whether the given patch exists in the codebase.
    patchExists :: Branch.EditHash -> m Bool,
    -- | Get the set of user-defined terms and type declarations that depend on the given term, type declaration, or
    -- builtin type.
    dependentsImpl :: Queries.DependentsSelector -> Reference -> Sqlite.Transaction (Set Reference.Id),
    dependentsOfComponentImpl :: Hash -> m (Set Reference.Id),
    -- | Copy a branch and all of its dependencies from the given codebase into this one.
    syncFromDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    -- | Copy a branch and all of its dependencies from this codebase into the given codebase.
    syncToDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    viewRemoteBranch' :: forall r. ReadGitRemoteNamespace -> Git.GitBranchBehavior -> ((Branch m, CodebasePath) -> m r) -> m (Either GitError r),
    -- | Push the given branch to the given repo, and optionally set it as the root branch.
    pushGitBranch :: forall e. WriteGitRepo -> PushGitBranchOpts -> (Branch m -> m (Either e (Branch m))) -> m (Either GitError (Either e (Branch m))),
    -- | @watches k@ returns all of the references @r@ that were previously put by a @putWatch k r t@. @t@ can be
    -- retrieved by @getWatch k r@.
    watches :: WK.WatchKind -> m [Reference.Id],
    -- | @getWatch k r@ returns watch result @t@ that was previously put by @putWatch k r t@.
    getWatch :: WK.WatchKind -> Reference.Id -> m (Maybe (Term v a)),
    -- | @putWatch k r t@ puts a watch of kind @k@, with hash-of-expression @r@ and decompiled result @t@ into the
    -- codebase.
    --
    -- For example, in the watch expression below, @k@ is 'WK.Regular', @r@ is the hash of @x@, and @t@ is @7@.
    --
    -- @
    -- > x = 3 + 4
    --   ⧩
    --   7
    -- @
    putWatch :: WK.WatchKind -> Reference.Id -> Term v a -> m (),
    -- | Delete all watches that were put by 'putWatch'.
    clearWatches :: m (),
    -- | Gets the specified number of reflog entries in chronological order, most recent first.
    getReflog :: Int -> m [Reflog.Entry V2.CausalHash Text],
    -- | Get the set of user-defined terms-or-constructors that have the given type.
    termsOfTypeImpl :: Reference -> m (Set Referent.Id),
    -- | Get the set of user-defined terms-or-constructors mention the given type anywhere in their signature.
    termsMentioningTypeImpl :: Reference -> m (Set Referent.Id),
    -- | The number of base32 characters needed to distinguish any two references in the codebase.
    hashLength :: m Int,
    -- | Get the set of user-defined terms whose hash matches the given prefix.
    termReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
    -- | Get the set of type declarations whose hash matches the given prefix.
    typeReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
    -- | Get the set of user-defined terms-or-constructors whose hash matches the given prefix.
    termReferentsByPrefix :: ShortHash -> m (Set Referent.Id),
    -- | The number of base32 characters needed to distinguish any two branch in the codebase.
    branchHashLength :: m Int,
    -- | Get the set of branches whose hash matches the given prefix.
    causalHashesByPrefix :: ShortCausalHash -> m (Set Branch.CausalHash),
    -- returns `Nothing` to not implemented, fallback to in-memory
    --    also `Nothing` if no LCA
    -- The result is undefined if the two hashes are not in the codebase.
    -- Use `Codebase.lca` which wraps this in a nice API.
    lcaImpl :: Maybe (Branch.CausalHash -> Branch.CausalHash -> m (Maybe Branch.CausalHash)),
    -- `beforeImpl` returns `Nothing` if not implemented by the codebase
    -- `beforeImpl b1 b2` is undefined if `b2` not in the codebase
    --
    --  Use `Codebase.before` which wraps this in a nice API.
    beforeImpl :: Maybe (Branch.CausalHash -> Branch.CausalHash -> m Bool),
    -- Use the name lookup index to build a 'Names' for all names found within 'Path' of the current root namespace.
    --
    -- NOTE: this method requires an up-to-date name lookup index, which is
    -- currently not kept up-to-date automatically (because it's slow to do so).
    namesAtPath :: Path -> m ScopedNames,
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
      m (),
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
  | GitCodebaseError (GitCodebaseError Branch.CausalHash)
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
