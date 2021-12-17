{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Type
  ( Codebase (..),
    CodebasePath,
    PushGitBranchOpts (..),
    GitError (..),
    SyncToDir,
  )
where

import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace, WriteRepo)
import Unison.Codebase.GitError (GitCodebaseError, GitProtocolError)
import Unison.Codebase.Patch (Patch)
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Codebase.SqliteCodebase.GitError (GitSqliteCodebaseError)
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
    getTypeOfTermImpl :: Reference.Id -> m (Maybe (Type v a)),
    -- | Get a type declaration.
    --
    -- Note that it is possible to call 'putTypeDeclaration', then 'getTypeDeclaration', and receive @Nothing@, per the
    -- semantics of 'putTypeDeclaration'.
    getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a)),
    -- | Enqueue the put of a user-defined term (with its type) into the codebase, if it doesn't already exist. The
    -- implementation may choose to delay the put until all of the term's (and its type's) references are stored as
    -- well.
    putTerm :: Reference.Id -> Term v a -> Type v a -> m (),
    -- | Enqueue the put of a type declaration into the codebase, if it doesn't already exist. The implementation may
    -- choose to delay the put until all of the type declaration's references are stored as well.
    putTypeDeclaration :: Reference.Id -> Decl v a -> m (),
    -- | Get the root branch.
    getRootBranch :: m (Branch m),
    -- | Like 'putBranch', but also adjusts the root branch pointer afterwards.
    putRootBranch :: Branch m -> m (),
    rootBranchUpdates :: m (IO (), IO (Set Branch.Hash)),
    getBranchForHashImpl :: Branch.Hash -> m (Maybe (Branch m)),
    -- | Put a branch into the codebase, which includes its children, its patches, and the branch itself, if they don't
    -- already exist.
    --
    -- The terms and type declarations that a branch references must already exist in the codebase.
    putBranch :: Branch m -> m (),
    -- | Check whether the given branch exists in the codebase.
    branchExists :: Branch.Hash -> m Bool,
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
    dependentsImpl :: Reference -> m (Set Reference.Id),
    -- | Copy a branch and all of its dependencies from the given codebase into this one.
    syncFromDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    -- | Copy a branch and all of its dependencies from this codebase into the given codebase.
    syncToDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    viewRemoteBranch' :: forall r. ReadRemoteNamespace -> ((Branch m, CodebasePath) -> m r) -> m (Either GitError r),
    -- | Push the given branch to the given repo, and optionally set it as the root branch.
    pushGitBranch :: Branch m -> WriteRepo -> PushGitBranchOpts -> m (Either GitError ()),
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
    -- | Get the entire reflog.
    getReflog :: m [Reflog.Entry Branch.Hash],
    -- | @appendReflog reason before after@ appends a reflog entry.
    --
    -- FIXME: this could have type
    --
    -- @
    -- appendReflog :: Reflog.Entry (Branch m) -> m ()
    -- @
    appendReflog :: Text -> Branch m -> Branch m -> m (),
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

data PushGitBranchOpts = PushGitBranchOpts
  { -- | Set the branch as root?
    setRoot :: Bool,
    syncMode :: SyncMode
  }

data GitError
  = GitProtocolError GitProtocolError
  | GitCodebaseError (GitCodebaseError Branch.Hash)
  | GitSqliteCodebaseError GitSqliteCodebaseError
  deriving (Show)

instance Exception GitError
