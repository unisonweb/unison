module U.Editor.Codebase where

-- data Codebase m v a = Codebase {
--   getTerm            :: Reference.Id -> m (Maybe (Term v a)),
--   getTypeOfTermImpl  :: Reference.Id -> m (Maybe (Type v a)),
--   getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a)),
  
--   putTerm            :: Reference.Id -> Term v a -> Type v a -> m (),
--   putTypeDeclaration :: Reference.Id -> Decl v a -> m (),
  
--   getBranch           :: Branch.Hash -> m (Maybe (Branch m)),
--   getRootBranch      :: m (Either GetRootBranchError (Branch m)),
--   putRootBranch      :: Branch m -> m (),
  
--   rootBranchUpdates  :: m (m (), m (Set Branch.Hash),
--   getBranchForCausal :: Branch.CausalHash -> m (Maybe (Branch m)),
  
--   -- --|Supports syncing from a current or older codebase format
--   syncFromDirectory  :: CodebasePath -> SyncMode -> Branch m -> m (),
--   -- -- |Only writes the latest codebase format
--   syncToDirectory    :: CodebasePath -> SyncMode -> Branch m -> m (),
--   -- -- ^ maybe return type needs to reflect failure if remote codebase has an old version
  
--   -- -- |Watch expressions are part of the codebase, the `Reference.Id` is
--   -- the hash of the source of the watch expression, and the `Term v a`
--   -- is the evaluated result of the expression, decompiled to a term.
--   watches  :: UF.WatchKind -> m [Reference.Id],
--   getWatch :: UF.WatchKind -> Reference.Id -> m (Maybe (Term v a)),
--   putWatch :: UF.WatchKind -> Reference.Id -> Term v a -> m (),
  
--   getReflog    :: m [Reflog.Entry],
--   appendReflog :: Text -> Branch m -> Branch m -> m (),
  
--   -- -- |the nicely-named versions will utilize these, and add builtins to the result set
--   termsHavingType_impl     :: Reference -> m (Set Referent.Id),
--   termsMentioningType_impl :: Reference -> m (Set Referent.Id),
  
--   -- -- |number of base58 characters needed to distinguish any two hashes in the codebase;
--   -- we don't have to compute it separately for different namespaces
--   hashLength             :: m Int,
--   termReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
--   typeReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
--   termReferentsByPrefix  :: ShortHash -> m (Set Referent.Id),
--   branchHashesByPrefix   :: ShortBranchHash -> m (Set Branch.Hash),
  
--   --
--   lca              :: [Causal m Branch.Raw e] -> m (Maybe Branch.Hash),
--   dependentsImpl   :: Reference -> m (Maybe (Set Reference.Id)),
--   termDependencies :: Reference.Id -> m (Maybe (Set Reference.Id)),
--   declDependencies :: Reference.Id -> m (Maybe (Set Reference.Id)),
--   -- -- |terms, types, patches, and branches
--   branchDependencies ::
--     Branch.Hash -> m (Maybe (Branch.CausalHash, BD.Dependencies)),
--   -- -- |the "new" terms and types mentioned in a patch
--   patchDependencies :: EditHash -> m (Set Reference, Set Reference)

-- }