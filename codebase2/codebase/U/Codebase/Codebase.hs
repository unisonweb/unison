{-# LANGUAGE RankNTypes #-}

module U.Codebase.Codebase where

import Data.Set (Set)
import Data.Text (Text)
import U.Codebase.Branch (Branch, Patch)
import U.Codebase.Causal (Causal)
import U.Codebase.Decl (Decl)
import U.Codebase.HashTags (BranchHash, CausalHash, PatchHash)
import U.Codebase.Reference (Reference)
import qualified U.Codebase.Reference as Reference
import qualified U.Codebase.Referent as Referent
import qualified U.Codebase.Reflog as Reflog
import U.Codebase.ShortHash (ShortBranchHash, ShortHash)
import U.Codebase.Term (Term)
import U.Codebase.Type (TypeT)
import U.Codebase.WatchKind (WatchKind)
import U.Util.Hash (Hash)

newtype CodebasePath = CodebasePath FilePath

data SyncMode = SyncShortCircuit | SyncComplete

data Codebase m v = Codebase
  { getTerm :: Reference.Id -> m (Maybe (Term v)),
    getTypeOfTerm :: Reference.Id -> m (Maybe (TypeT v)),
    getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v)),
    putTerm :: Reference.Id -> Term v -> TypeT v -> m (),
    putTypeDeclaration :: Reference.Id -> Decl v -> m (),
    getPatch :: PatchHash -> m Patch,
    putPatch :: PatchHash -> Patch -> m (),
    getBranch :: BranchHash -> m (Maybe (Branch m)),
    getRootBranch :: m (Either GetRootBranchError (Branch m)),
    putRootBranch :: Branch m -> m (),
    getBranchForCausal :: CausalHash -> m (Maybe (Branch m)),
    -- | Supports syncing from a current or older codebase format
    syncFromDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    -- | Only writes the latest codebase format
    syncToDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    -- | Watch expressions are part of the codebase, the `Reference.Id` is
    --  the hash of the source of the watch expression, and the `Term v a`
    --  is the evaluated result of the expression, decompiled to a term.
    watches :: WatchKind -> m [Reference.Id],
    getWatch :: WatchKind -> Reference.Id -> m (Maybe (Term v)),
    putWatch :: WatchKind -> Reference.Id -> Term v -> m (),
    getReflog :: m [Reflog.Entry],
    appendReflog :: Text -> Branch m -> Branch m -> m (),
    -- | the nicely-named versions will utilize these, and add builtins to the result set
    termsHavingType :: Reference -> m (Set Referent.Id),
    termsMentioningType :: Reference -> m (Set Referent.Id),
    -- | number of base32 characters needed to distinguish any two hashes in the codebase;
    --  we don't have to compute it separately for different namespaces
    hashLength :: m Int,
    termReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
    typeReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
    termReferentsByPrefix :: ShortHash -> m (Set Referent.Id),
    branchHashesByPrefix :: ShortBranchHash -> m (Set BranchHash),
    --
    lca :: (forall he e. [Causal m CausalHash he e] -> m (Maybe BranchHash)),
    dependents :: Reference -> m (Maybe (Set Reference.Id)),
    termDependencies :: Reference.Id -> m (Maybe (Set Reference.Id)),
    declDependencies :: Reference.Id -> m (Maybe (Set Reference.Id)) --,
    -- -- |terms, types, patches, and branches
    -- branchDependencies ::
    --   Branch.Hash -> m (Maybe (CausalHash, BD.Dependencies)),
    -- -- |the "new" terms and types mentioned in a patch
    -- patchDependencies :: EditHash -> m (Set Reference, Set Reference)
  }

data GetRootBranchError
  = NoRootBranch
  | CouldntLoadRootBranch Hash
  deriving (Show)
