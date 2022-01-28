{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.SqliteCodebase.MigrateSchema12
  ( migrateSchema12,
  )
where

import Control.Lens
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT), ask, mapReaderT)
import Control.Monad.State.Strict
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Writer.CPS (Writer, execWriter, tell)
import Data.Generics.Product
import Data.Generics.Sum (_Ctor)
import Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Tuple.Extra ((***))
import qualified Data.Zip as Zip
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), PatchHash (..))
import qualified U.Codebase.Reference as UReference
import qualified U.Codebase.Referent as UReferent
import qualified U.Codebase.Sqlite.Branch.Full as S
import qualified U.Codebase.Sqlite.Branch.Full as S.Branch.Full
import U.Codebase.Sqlite.Causal (GDbCausal (..))
import qualified U.Codebase.Sqlite.Causal as SC
import U.Codebase.Sqlite.Connection (Connection)
import U.Codebase.Sqlite.DbId
  ( BranchHashId (..),
    BranchObjectId (..),
    CausalHashId (..),
    HashId,
    ObjectId,
    PatchObjectId (..),
    TextId,
  )
import qualified U.Codebase.Sqlite.LocalizeObject as S.LocalizeObject
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Patch.Format as S.Patch.Format
import qualified U.Codebase.Sqlite.Patch.Full as S
import qualified U.Codebase.Sqlite.Patch.TermEdit as TermEdit
import qualified U.Codebase.Sqlite.Patch.TypeEdit as TypeEdit
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sync (Sync (Sync))
import qualified U.Codebase.Sync as Sync
import U.Codebase.WatchKind (WatchKind)
import qualified U.Codebase.WatchKind as WK
import U.Util.Monoid (foldMapM)
import qualified Unison.ABT as ABT
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.Codebase.SqliteCodebase.MigrateSchema12.DbHelpers as Hashing
import Unison.Codebase.Type (Codebase (Codebase))
import qualified Unison.ConstructorReference as ConstructorReference
import qualified Unison.DataDeclaration as DD
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Hash (Hash)
import qualified Unison.Hash as Unison
import qualified Unison.Hashing.V2.Causal as Hashing
import qualified Unison.Hashing.V2.Convert as Convert
import Unison.Pattern (Pattern)
import qualified Unison.Pattern as Pattern
import Unison.Prelude
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Referent' as Referent'
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Util.Set as Set
import Unison.Var (Var)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket_, onException)

-- todo:
--  * write a harness to call & seed algorithm
--    * [x] embed migration in a transaction/savepoint and ensure that we never leave the codebase in a
--            weird state even if we crash.
--    * [x] may involve writing a `Progress`
--    * raw DB things:
--    * [x] write new namespace root after migration.
--    * [x] overwrite object_id column in hash_object table to point at new objects
--    * [x] delete references to old objects in index tables (where else?)
--    * [x] delete old objects
--
--  * refer to github megaticket https://github.com/unisonweb/unison/issues/2471
--    ☢️ [x] incorporate type signature into hash of term <- chris/arya have started ☢️
--          [x] store type annotation in the term
--    * [x] Refactor Causal helper functions to use V2 hashing
--          * [x] I guess move Hashable to V2.Hashing pseudo-package
--          * [x] Delete V1 Hashing to ensure it's unused
--          * [x] Salt V2 hashes with version number
--    * [ ] confirm that pulls are handled ok
--    * [x] Make a backup of the v1 codebase before migrating, in a temp directory.
--          Include a message explaining where we put it.
--    * [ ] Improved error message (don't crash) if loading a codebase newer than your ucm
--    * [x] Update the schema version in the database after migrating so we only migrate
--    once.

verboseOutput :: Bool
verboseOutput =
  isJust (unsafePerformIO (lookupEnv "UNISON_MIGRATION_DEBUG"))
{-# NOINLINE verboseOutput #-}

migrateSchema12 :: forall a m v. (MonadUnliftIO m, Var v) => Connection -> Codebase m v a -> m ()
migrateSchema12 conn codebase = do
  withinSavepoint "MIGRATESCHEMA12" $ do
    liftIO $ putStrLn $ "Starting codebase migration. This may take a while, it's a good time to make some tea ☕️"
    rootCausalHashId <- runDB conn (liftQ Q.loadNamespaceRoot)
    numEntitiesToMigrate <- runDB conn . liftQ $ do
      sum <$> sequenceA [Q.countObjects, Q.countCausals, Q.countWatches]
    v2EmptyBranchObjectId <- saveV2EmptyBranch conn
    watches <-
      foldMapM
        (\watchKind -> map (W watchKind) <$> Codebase.watches codebase (Cv.watchKind2to1 watchKind))
        [WK.RegularWatch, WK.TestWatch]
    migrationState <-
      (Sync.sync @_ @Entity migrationSync (progress numEntitiesToMigrate) (CausalE rootCausalHashId : watches))
        `runReaderT` Env {db = conn, codebase}
        `execStateT` MigrationState Map.empty Map.empty Map.empty Set.empty 0 v2EmptyBranchObjectId
    let (_, newRootCausalHashId) = causalMapping migrationState ^?! ix rootCausalHashId
    liftIO $ putStrLn $ "Updating Namespace Root..."
    runDB conn . liftQ $ Q.setNamespaceRoot newRootCausalHashId
    liftIO $ putStrLn $ "Rewriting old object IDs..."
    ifor_ (objLookup migrationState) \oldObjId (newObjId, _, _, _) -> do
      (runDB conn . liftQ) do
        Q.recordObjectRehash oldObjId newObjId
    liftIO $ putStrLn $ "Garbage collecting orphaned objects..."
    runDB conn (liftQ Q.garbageCollectObjectsWithoutHashes)
    liftIO $ putStrLn $ "Garbage collecting orphaned watches..."
    runDB conn (liftQ Q.garbageCollectWatchesWithoutObjects)
    liftIO $ putStrLn $ "Updating Schema Version..."
    runDB conn . liftQ $ Q.setSchemaVersion 2
  liftIO $ putStrLn $ "Cleaning up..."
  runDB conn (liftQ Q.vacuum)
  where
    withinSavepoint :: (String -> m c -> m c)
    withinSavepoint name act =
      bracket_
        (runDB conn $ Q.savepoint name)
        (runDB conn $ Q.release name)
        (act `onException` runDB conn (Q.rollbackTo name))
    progress :: Int -> Sync.Progress (ReaderT (Env m v a) (StateT MigrationState m)) Entity
    progress numToMigrate =
      let incrementProgress :: ReaderT (Env m v a) (StateT MigrationState m) ()
          incrementProgress = do
            numDone <- field @"numMigrated" <+= 1
            liftIO $ putStr $ "\r 🏗  " <> show numDone <> " / ~" <> show numToMigrate <> " entities migrated. 🚧"
          need :: Entity -> ReaderT (Env m v a) (StateT MigrationState m) ()
          need e = when verboseOutput $ liftIO $ putStrLn $ "Need: " ++ show e
          done :: Entity -> ReaderT (Env m v a) (StateT MigrationState m) ()
          done e = do
            when verboseOutput $ liftIO $ putStrLn $ "Done: " ++ show e
            incrementProgress
          errorHandler :: Entity -> ReaderT (Env m v a) (StateT MigrationState m) ()
          errorHandler e = do
            case e of
              -- We expect non-fatal errors when migrating watches.
              W {} -> pure ()
              e -> liftIO $ putStrLn $ "Error: " ++ show e
            incrementProgress
          allDone :: ReaderT (Env m v a) (StateT MigrationState m) ()
          allDone = liftIO $ putStrLn $ "\nFinished migrating, initiating cleanup."
       in Sync.Progress {need, done, error = errorHandler, allDone}

type Old a = a

type New a = a

type ConstructorName v = v

type DeclName v = v

data MigrationState = MigrationState
  -- Mapping between old cycle-position -> new cycle-position for a given Decl object.
  { referenceMapping :: Map (Old SomeReferenceId) (New SomeReferenceId),
    causalMapping :: Map (Old CausalHashId) (New (CausalHash, CausalHashId)),
    -- We also store the old hash for this object ID since we need a way to
    -- convert Object Reference IDs into Hash Reference IDs so we can use the referenceMapping.
    objLookup :: Map (Old ObjectId) (New ObjectId, New HashId, New Hash, Old Hash),
    -- Remember the hashes of term/decls that we have already migrated to avoid migrating them twice.
    migratedDefnHashes :: Set (Old Hash),
    numMigrated :: Int,
    v2EmptyBranchHashInfo :: (BranchHashId, Hash)
  }
  deriving (Generic)

data Entity
  = TermComponent Unison.Hash
  | DeclComponent Unison.Hash
  | CausalE CausalHashId
  | BranchE ObjectId
  | PatchE ObjectId
  | W WK.WatchKind Reference.Id
  deriving (Eq, Ord, Show)

data Env m v a = Env {db :: Connection, codebase :: Codebase m v a}

migrationSync ::
  (MonadIO m, Var v) =>
  Sync (ReaderT (Env m v a) (StateT MigrationState m)) Entity
migrationSync = Sync \case
  TermComponent hash -> do
    Env {codebase, db} <- ask
    lift (migrateTermComponent db codebase hash)
  DeclComponent hash -> do
    Env {codebase, db} <- ask
    lift (migrateDeclComponent db codebase hash)
  BranchE objectId -> do
    Env {db} <- ask
    lift (migrateBranch db objectId)
  CausalE causalHashId -> do
    Env {db} <- ask
    lift (migrateCausal db causalHashId)
  PatchE objectId -> do
    Env {db} <- ask
    lift (migratePatch db (PatchObjectId objectId))
  W watchKind watchId -> do
    Env {codebase} <- ask
    lift (migrateWatch codebase watchKind watchId)

runDB :: MonadIO m => Connection -> ReaderT Connection (ExceptT Ops.Error (ExceptT Q.Integrity m)) a -> m a
runDB conn = (runExceptT >=> err) . (runExceptT >=> err) . flip runReaderT conn
  where
    err :: forall e x m. (Show e, Applicative m) => (Either e x -> m x)
    err = \case Left err -> error $ show err; Right a -> pure a

liftQ :: Monad m => ReaderT Connection (ExceptT Q.Integrity m) a -> ReaderT Connection (ExceptT Ops.Error (ExceptT Q.Integrity m)) a
liftQ = mapReaderT lift

migrateCausal :: MonadIO m => Connection -> CausalHashId -> StateT MigrationState m (Sync.TrySyncResult Entity)
migrateCausal conn oldCausalHashId = fmap (either id id) . runExceptT $ do
  whenM (Map.member oldCausalHashId <$> use (field @"causalMapping")) (throwE Sync.PreviouslyDone)

  oldBranchHashId <- runDB conn . liftQ $ Q.loadCausalValueHashId oldCausalHashId
  oldCausalParentHashIds <- runDB conn . liftQ $ Q.loadCausalParents oldCausalHashId

  maybeOldBranchObjId <-
    runDB conn . liftQ $
      Q.maybeObjectIdForAnyHashId (unBranchHashId oldBranchHashId)
  migratedObjIds <- gets objLookup
  -- If the branch for this causal hasn't been migrated, migrate it first.
  let unmigratedBranch =
        case maybeOldBranchObjId of
          Just branchObjId | branchObjId `Map.notMember` migratedObjIds -> [BranchE branchObjId]
          _ -> []

  migratedCausals <- gets causalMapping
  let unmigratedParents =
        oldCausalParentHashIds
          & filter (`Map.notMember` migratedCausals)
          & fmap CausalE
  let unmigratedEntities = unmigratedBranch <> unmigratedParents
  when (not . null $ unmigratedParents <> unmigratedBranch) (throwE $ Sync.Missing unmigratedEntities)

  (newBranchHashId, newBranchHash) <- case maybeOldBranchObjId of
    -- Some codebases are corrupted, likely due to interrupted save operations.
    -- It's unfortunate, but rather than fail the whole migration we'll just replace them
    -- with an empty branch.
    Nothing -> use (field @"v2EmptyBranchHashInfo")
    Just branchObjId -> do
      let (_, newBranchHashId, newBranchHash, _) = migratedObjIds ^?! ix branchObjId
      pure (BranchHashId newBranchHashId, newBranchHash)

  let (newParentHashes, newParentHashIds) =
        oldCausalParentHashIds
          & fmap
            (\oldParentHashId -> migratedCausals ^?! ix oldParentHashId)
          & unzip
          & bimap (Set.fromList . map unCausalHash) Set.fromList

  let newCausalHash :: CausalHash
      newCausalHash =
        CausalHash . Cv.hash1to2 $
          Hashing.hashCausal
            ( Hashing.Causal
                { branchHash = newBranchHash,
                  parents = Set.mapMonotonic Cv.hash2to1 newParentHashes
                }
            )
  newCausalHashId <- runDB conn (Q.saveCausalHash newCausalHash)
  let newCausal =
        DbCausal
          { selfHash = newCausalHashId,
            valueHash = newBranchHashId,
            parents = newParentHashIds
          }
  runDB conn do
    Q.saveCausal (SC.selfHash newCausal) (SC.valueHash newCausal)
    Q.saveCausalParents (SC.selfHash newCausal) (Set.toList $ SC.parents newCausal)

  field @"causalMapping" %= Map.insert oldCausalHashId (newCausalHash, newCausalHashId)

  pure Sync.Done

migrateBranch :: MonadIO m => Connection -> ObjectId -> StateT MigrationState m (Sync.TrySyncResult Entity)
migrateBranch conn oldObjectId = fmap (either id id) . runExceptT $ do
  whenM (Map.member oldObjectId <$> use (field @"objLookup")) (throwE Sync.PreviouslyDone)

  oldBranch <- runDB conn (Ops.loadDbBranchByObjectId (BranchObjectId oldObjectId))
  oldHash <- fmap Cv.hash2to1 . runDB conn $ Ops.loadHashByObjectId oldObjectId
  oldBranchWithHashes <- runDB conn (traverseOf S.branchHashes_ (fmap Cv.hash2to1 . Ops.loadHashByObjectId) oldBranch)
  migratedRefs <- gets referenceMapping
  migratedObjects <- gets objLookup
  migratedCausals <- gets causalMapping
  let allMissingTypesAndTerms :: [Entity]
      allMissingTypesAndTerms =
        oldBranchWithHashes
          ^.. branchSomeRefs_
            . uRefIdAsRefId_
            . filtered (`Map.notMember` migratedRefs)
            . to someReferenceIdToEntity

  let allMissingPatches :: [Entity] =
        oldBranch
          ^.. S.patches_
            . to unPatchObjectId
            . filtered (`Map.notMember` migratedObjects)
            . to PatchE

  let allMissingChildBranches :: [Entity] =
        oldBranch
          ^.. S.childrenHashes_
            . _1
            . to unBranchObjectId
            . filtered (`Map.notMember` migratedObjects)
            . to BranchE

  let allMissingChildCausals :: [Entity] =
        oldBranch
          ^.. S.childrenHashes_
            . _2
            . filtered (`Map.notMember` migratedCausals)
            . to CausalE

  -- Identify dependencies and bail out if they aren't all built
  let allMissingReferences :: [Entity]
      allMissingReferences =
        allMissingTypesAndTerms
          ++ allMissingPatches
          ++ allMissingChildBranches
          ++ allMissingChildCausals

  when (not . null $ allMissingReferences) $
    throwE $ Sync.Missing allMissingReferences

  let remapPatchObjectId patchObjId = case Map.lookup (unPatchObjectId patchObjId) migratedObjects of
        Nothing -> error $ "Expected patch: " <> show patchObjId <> " to be migrated"
        Just (newPatchObjId, _, _, _) -> PatchObjectId newPatchObjId
  let remapCausalHashId causalHashId = case Map.lookup causalHashId migratedCausals of
        Nothing -> error $ "Expected causal hash id: " <> show causalHashId <> " to be migrated"
        Just (_, newCausalHashId) -> newCausalHashId
  let remapBranchObjectId objId = case Map.lookup (unBranchObjectId objId) migratedObjects of
        Nothing -> error $ "Expected object: " <> show objId <> " to be migrated"
        Just (newBranchObjId, _, _, _) -> BranchObjectId newBranchObjId

  let newBranch :: S.DbBranch
      newBranch =
        oldBranch
          & branchSomeRefs_ %~ remapObjIdRefs migratedObjects migratedRefs
          & S.patches_ %~ remapPatchObjectId
          & S.childrenHashes_ %~ (remapBranchObjectId *** remapCausalHashId)

  let (localBranchIds, localBranch) = S.LocalizeObject.localizeBranch newBranch
  newHash <- runDB conn (Ops.liftQ (Hashing.dbBranchHash newBranch))
  newHashId <- runDB conn (Ops.liftQ (Q.saveBranchHash (BranchHash (Cv.hash1to2 newHash))))
  newObjectId <- runDB conn (Ops.saveBranchObject newHashId localBranchIds localBranch)
  field @"objLookup" %= Map.insert oldObjectId (unBranchObjectId newObjectId, unBranchHashId newHashId, newHash, oldHash)
  pure Sync.Done

migratePatch ::
  forall m.
  MonadIO m =>
  Connection ->
  Old PatchObjectId ->
  StateT MigrationState m (Sync.TrySyncResult Entity)
migratePatch conn oldObjectId = fmap (either id id) . runExceptT $ do
  whenM (Map.member (unPatchObjectId oldObjectId) <$> use (field @"objLookup")) (throwE Sync.PreviouslyDone)

  oldHash <- fmap Cv.hash2to1 . runDB conn $ Ops.loadHashByObjectId (unPatchObjectId oldObjectId)
  oldPatch <- runDB conn (Ops.loadDbPatchById oldObjectId)
  let hydrateHashes :: forall m. Q.EDB m => HashId -> m Hash
      hydrateHashes hashId = do
        Cv.hash2to1 <$> Q.loadHashHashById hashId
  let hydrateObjectIds :: forall m. Ops.EDB m => ObjectId -> m Hash
      hydrateObjectIds objId = do
        Cv.hash2to1 <$> Ops.loadHashByObjectId objId

  oldPatchWithHashes :: S.Patch' TextId Hash Hash <-
    runDB conn do
      (oldPatch & S.patchH_ %%~ liftQ . hydrateHashes)
        >>= (S.patchO_ %%~ hydrateObjectIds)

  migratedRefs <- gets referenceMapping
  let isUnmigratedRef ref = Map.notMember ref migratedRefs
  -- 2. Determine whether all things the patch refers to are built.
  let unmigratedDependencies :: [Entity]
      unmigratedDependencies =
        oldPatchWithHashes ^.. patchSomeRefsH_ . uRefIdAsRefId_ . filtered isUnmigratedRef . to someReferenceIdToEntity
          <> oldPatchWithHashes ^.. patchSomeRefsO_ . uRefIdAsRefId_ . filtered isUnmigratedRef . to someReferenceIdToEntity
  when (not . null $ unmigratedDependencies) (throwE (Sync.Missing unmigratedDependencies))

  let hashToHashId :: forall m. Q.EDB m => Hash -> m HashId
      hashToHashId h =
        fromMaybe (error $ "expected hashId for hash: " <> show h) <$> (Q.loadHashIdByHash (Cv.hash1to2 h))
  let hashToObjectId :: forall m. Q.EDB m => Hash -> m ObjectId
      hashToObjectId = hashToHashId >=> Q.expectObjectIdForPrimaryHashId

  migratedReferences <- gets referenceMapping
  let remapRef :: SomeReferenceId -> SomeReferenceId
      remapRef ref = Map.findWithDefault ref ref migratedReferences

  let newPatch =
        oldPatchWithHashes
          & patchSomeRefsH_ . uRefIdAsRefId_ %~ remapRef
          & patchSomeRefsO_ . uRefIdAsRefId_ %~ remapRef

  newPatchWithIds :: S.Patch <-
    runDB conn . liftQ $ do
      (newPatch & S.patchH_ %%~ hashToHashId)
        >>= (S.patchO_ %%~ hashToObjectId)

  let (localPatchIds, localPatch) = S.LocalizeObject.localizePatch newPatchWithIds
  newHash <- runDB conn (liftQ (Hashing.dbPatchHash newPatchWithIds))
  newObjectId <- runDB conn (Ops.saveDbPatch (PatchHash (Cv.hash1to2 newHash)) (S.Patch.Format.Full localPatchIds localPatch))
  newHashId <- runDB conn (liftQ (Q.expectHashIdByHash (Cv.hash1to2 newHash)))
  field @"objLookup" %= Map.insert (unPatchObjectId oldObjectId) (unPatchObjectId newObjectId, newHashId, newHash, oldHash)
  pure Sync.Done

-- | PLAN
-- *
-- NOTE: this implementation assumes that watches will be migrated AFTER everything else is finished.
-- This is because it's difficult for us to know otherwise whether a reference refers to something which doesn't exist, or just
-- something that hasn't been migrated yet. If we do it last, we know that missing references are indeed just missing from the codebase.
migrateWatch ::
  forall m v a.
  (MonadIO m, Ord v) =>
  Codebase m v a ->
  WatchKind ->
  Reference.Id ->
  StateT MigrationState m (Sync.TrySyncResult Entity)
migrateWatch Codebase {getWatch, putWatch} watchKind oldWatchId = fmap (either id id) . runExceptT $ do
  let watchKindV1 = Cv.watchKind2to1 watchKind
  watchResultTerm <-
    (lift . lift) (getWatch watchKindV1 oldWatchId) >>= \case
      -- The hash which we're watching doesn't exist in the codebase, throw out this watch.
      Nothing -> throwE Sync.Done
      Just term -> pure term
  migratedReferences <- gets referenceMapping
  newWatchId <- case Map.lookup (TermReference oldWatchId) migratedReferences of
    (Just (TermReference newRef)) -> pure newRef
    _ -> throwE Sync.NonFatalError
  let maybeRemappedTerm :: Maybe (Term.Term v a)
      maybeRemappedTerm =
        watchResultTerm
          & termReferences_ %%~ \someRef -> Map.lookup someRef migratedReferences
  case maybeRemappedTerm of
    -- One or more references in the result didn't exist in our codebase.
    Nothing -> pure Sync.NonFatalError
    Just remappedTerm -> do
      lift . lift $ putWatch watchKindV1 newWatchId remappedTerm
      pure Sync.Done

uRefIdAsRefId_ :: Iso' (SomeReference (UReference.Id' Hash)) SomeReferenceId
uRefIdAsRefId_ = mapping uRefAsRef_

uRefAsRef_ :: Iso' (UReference.Id' Hash) Reference.Id
uRefAsRef_ = iso intoRef intoURef
  where
    intoRef (UReference.Id hash pos) = Reference.Id hash pos
    intoURef (Reference.Id hash pos) = UReference.Id hash pos

-- Project an S.Referent'' into its SomeReferenceObjId's
someReferent_ ::
  forall t h.
  (forall ref. Traversal' ref (SomeReference ref)) ->
  Traversal' (S.Branch.Full.Referent'' t h) (SomeReference (UReference.Id' h))
someReferent_ typeOrTermTraversal_ =
  (UReferent._Ref . someReference_ typeOrTermTraversal_)
    `failing` ( UReferent._Con
                  . asPair_ -- Need to unpack the embedded reference AND remap between mismatched Constructor ID types.
                  . asConstructorReference_
              )
  where
    asPair_ f (UReference.ReferenceDerived id', conId) =
      f (ConstructorReference.ConstructorReference id' (fromIntegral conId))
        <&> \(ConstructorReference.ConstructorReference newId newConId) ->
          (UReference.ReferenceDerived newId, fromIntegral newConId)
    asPair_ _ (UReference.ReferenceBuiltin x, conId) = pure (UReference.ReferenceBuiltin x, conId)

-- asPair_ f (UReference.ReferenceDerived id', conId) =
--   f (id', fromIntegral conId)
--     <&> \(newId, newConId) -> (UReference.ReferenceDerived newId, fromIntegral newConId)
-- asPair_ _ (UReference.ReferenceBuiltin x, conId) = pure (UReference.ReferenceBuiltin x, conId)

someReference_ ::
  (forall ref. Traversal' ref (SomeReference ref)) ->
  Traversal' (UReference.Reference' t h) (SomeReference (UReference.Id' h))
someReference_ typeOrTermTraversal_ = UReference._ReferenceDerived . typeOrTermTraversal_

someMetadataSetFormat_ ::
  (Ord t, Ord h) =>
  (forall ref. Traversal' ref (SomeReference ref)) ->
  Traversal' (S.Branch.Full.MetadataSetFormat' t h) (SomeReference (UReference.Id' h))
someMetadataSetFormat_ typeOrTermTraversal_ =
  S.Branch.Full.metadataSetFormatReferences_ . someReference_ typeOrTermTraversal_

someReferenceMetadata_ ::
  (Ord k, Ord t, Ord h) =>
  Traversal' k (SomeReference (UReference.Id' h)) ->
  Traversal'
    (Map k (S.Branch.Full.MetadataSetFormat' t h))
    (SomeReference (UReference.Id' h))
someReferenceMetadata_ keyTraversal_ f m =
  Map.toList m
    & traversed . beside keyTraversal_ (someMetadataSetFormat_ asTermReference_) %%~ f
    <&> Map.fromList

branchSomeRefs_ :: (Ord t, Ord h) => Traversal' (S.Branch' t h p c) (SomeReference (UReference.Id' h))
branchSomeRefs_ f S.Branch.Full.Branch {children, patches, terms, types} = do
  let newTypesMap = types & traversed . someReferenceMetadata_ (someReference_ asTypeReference_) %%~ f
  let newTermsMap = terms & traversed . someReferenceMetadata_ (someReferent_ asTermReference_) %%~ f
  S.Branch.Full.Branch <$> newTermsMap <*> newTypesMap <*> pure patches <*> pure children

patchSomeRefsH_ :: (Ord t, Ord h) => Traversal (S.Patch' t h o) (S.Patch' t h o) (SomeReference (UReference.Id' h)) (SomeReference (UReference.Id' h))
patchSomeRefsH_ f S.Patch {termEdits, typeEdits} = do
  newTermEdits <- Map.fromList <$> (Map.toList termEdits & traversed . _1 . (someReferent_ asTermReference_) %%~ f)
  newTypeEdits <- Map.fromList <$> (Map.toList typeEdits & traversed . _1 . (someReference_ asTypeReference_) %%~ f)
  pure S.Patch {termEdits = newTermEdits, typeEdits = newTypeEdits}

patchSomeRefsO_ :: (Ord t, Ord h, Ord o) => Traversal' (S.Patch' t h o) (SomeReference (UReference.Id' o))
patchSomeRefsO_ f S.Patch {termEdits, typeEdits} = do
  newTermEdits <- (termEdits & traversed . Set.traverse . termEditRefs_ %%~ f)
  newTypeEdits <- (typeEdits & traversed . Set.traverse . typeEditRefs_ %%~ f)
  pure (S.Patch {termEdits = newTermEdits, typeEdits = newTypeEdits})

termEditRefs_ :: Traversal' (TermEdit.TermEdit' t h) (SomeReference (UReference.Id' h))
termEditRefs_ f (TermEdit.Replace ref typing) =
  TermEdit.Replace <$> (ref & someReferent_ asTermReference_ %%~ f) <*> pure typing
termEditRefs_ _f (TermEdit.Deprecate) = pure TermEdit.Deprecate

typeEditRefs_ :: Traversal' (TypeEdit.TypeEdit' t h) (SomeReference (UReference.Id' h))
typeEditRefs_ f (TypeEdit.Replace ref) =
  TypeEdit.Replace <$> (ref & someReference_ asTypeReference_ %%~ f)
typeEditRefs_ _f (TypeEdit.Deprecate) = pure TypeEdit.Deprecate

migrateTermComponent ::
  forall m v a.
  (Ord v, Var v, Monad m, MonadIO m) =>
  Connection ->
  Codebase m v a ->
  Unison.Hash ->
  StateT MigrationState m (Sync.TrySyncResult Entity)
migrateTermComponent conn Codebase {..} oldHash = fmap (either id id) . runExceptT $ do
  whenM (Set.member oldHash <$> use (field @"migratedDefnHashes")) (throwE Sync.PreviouslyDone)

  oldComponent <-
    (lift . lift $ getTermComponentWithTypes oldHash) >>= \case
      Nothing -> error $ "Hash was missing from codebase: " <> show oldHash
      Just c -> pure c

  let componentIDMap :: Map (Old Reference.Id) (Term.Term v a, Type v a)
      componentIDMap = Map.fromList $ Reference.componentFor oldHash oldComponent
  let unhashed :: Map (Old Reference.Id) (v, Term.Term v a)
      unhashed = Term.unhashComponent (fst <$> componentIDMap)
  let vToOldReferenceMapping :: Map v (Old Reference.Id)
      vToOldReferenceMapping =
        unhashed
          & Map.toList
          & fmap (\(refId, (v, _trm)) -> (v, refId))
          & Map.fromList

  referencesMap <- gets referenceMapping

  let allMissingReferences :: [Old SomeReferenceId]
      allMissingReferences =
        let missingTermRefs =
              unhashed & foldSetter (traversed . _2 . termReferences_)
            missingTypeRefs =
              componentIDMap
                & foldSetter (traversed . _2 . typeReferences_)
         in filter (`Map.notMember` referencesMap) (missingTermRefs <> missingTypeRefs)

  when (not . null $ allMissingReferences) $
    throwE $ Sync.Missing . nubOrd $ (someReferenceIdToEntity <$> allMissingReferences)

  let getMigratedReference :: Old SomeReferenceId -> New SomeReferenceId
      getMigratedReference ref =
        Map.findWithDefault (error $ "unmigrated reference" <> show ref) ref referencesMap

  let remappedReferences :: Map (Old Reference.Id) (v, Term.Term v a, Type v a) =
        Zip.zipWith
          ( \(v, trm) (_, typ) ->
              ( v,
                trm & termReferences_ %~ getMigratedReference,
                typ & typeReferences_ %~ getMigratedReference
              )
          )
          unhashed
          componentIDMap

  let newTermComponents :: Map v (New Reference.Id, Term.Term v a, Type v a)
      newTermComponents =
        remappedReferences
          & Map.elems
          & fmap (\(v, trm, typ) -> (v, (trm, typ)))
          & Map.fromList
          & Convert.hashTermComponents

  ifor newTermComponents $ \v (newReferenceId, trm, typ) -> do
    let oldReferenceId = vToOldReferenceMapping ^?! ix v
    field @"referenceMapping" %= Map.insert (TermReference oldReferenceId) (TermReference newReferenceId)
    lift . lift $ putTerm newReferenceId trm typ

  -- Need to get one of the new references to grab its hash, doesn't matter which one since
  -- all hashes in the component are the same.
  case newTermComponents ^? traversed . _1 . to Reference.idToHash of
    Nothing -> pure ()
    Just newHash -> insertObjectMappingForHash conn oldHash newHash

  field @"migratedDefnHashes" %= Set.insert oldHash
  pure Sync.Done

migrateDeclComponent ::
  forall m v a.
  (Ord v, Var v, Monad m, MonadIO m) =>
  Connection ->
  Codebase m v a ->
  Unison.Hash ->
  StateT MigrationState m (Sync.TrySyncResult Entity)
migrateDeclComponent conn Codebase {..} oldHash = fmap (either id id) . runExceptT $ do
  whenM (Set.member oldHash <$> use (field @"migratedDefnHashes")) (throwE Sync.PreviouslyDone)

  declComponent :: [DD.Decl v a] <-
    (lift . lift $ getDeclComponent oldHash) >>= \case
      Nothing -> error $ "Expected decl component for hash:" <> show oldHash
      Just dc -> pure dc

  let componentIDMap :: Map (Old Reference.Id) (DD.Decl v a)
      componentIDMap = Map.fromList $ Reference.componentFor oldHash declComponent

  let unhashed :: Map (Old Reference.Id) (DeclName v, DD.Decl v a)
      unhashed = DD.unhashComponent componentIDMap

  let allTypes :: [Type v a]
      allTypes =
        unhashed
          ^.. traversed
            . _2
            . beside DD.asDataDecl_ id
            . to DD.constructors'
            . traversed
            . _3

  migratedReferences <- gets referenceMapping
  let unmigratedRefIds :: [SomeReferenceId]
      unmigratedRefIds =
        allTypes
          & foldSetter
            ( traversed -- Every type in the list
                . typeReferences_
                . filtered (`Map.notMember` migratedReferences)
            )

  when (not . null $ unmigratedRefIds) do
    throwE (Sync.Missing (nubOrd . fmap someReferenceIdToEntity $ unmigratedRefIds))

  -- At this point we know we have all the required mappings from old references  to new ones.
  let remapTerm :: Type v a -> Type v a
      remapTerm = typeReferences_ %~ \ref -> Map.findWithDefault (error "unmigrated reference") ref migratedReferences

  let remappedReferences :: Map (Old Reference.Id) (DeclName v, DD.Decl v a)
      remappedReferences =
        unhashed
          & traversed -- Traverse map of reference IDs
            . _2 -- Select the DataDeclaration
            . beside DD.asDataDecl_ id -- Unpack effect decls
            . DD.constructors_ -- Get the data constructors
            . traversed -- traverse the list of them
            . _3 -- Select the Type term.
          %~ remapTerm

  let declNameToOldReference :: Map (DeclName v) (Old Reference.Id)
      declNameToOldReference = Map.fromList . fmap swap . Map.toList . fmap fst $ remappedReferences

  let newComponent :: [(DeclName v, Reference.Id, DD.Decl v a)]
      newComponent =
        remappedReferences
          & Map.elems
          & Map.fromList
          & Convert.hashDecls
          & fromRight (error "unexpected resolution error")

  for_ newComponent $ \(declName, newReferenceId, dd) -> do
    let oldReferenceId = declNameToOldReference ^?! ix declName
    field @"referenceMapping" %= Map.insert (TypeReference oldReferenceId) (TypeReference newReferenceId)

    let oldConstructorIds :: Map (ConstructorName v) (Old ConstructorId)
        oldConstructorIds =
          (componentIDMap ^?! ix oldReferenceId)
            & DD.asDataDecl
            & DD.constructors'
            & imap (\constructorId (_ann, constructorName, _type) -> (constructorName, fromIntegral constructorId))
            & Map.fromList

    ifor_ (DD.constructors' (DD.asDataDecl dd)) \(fromIntegral -> newConstructorId) (_ann, constructorName, _type) -> do
      field @"referenceMapping"
        %= Map.insert
          (ConstructorReference oldReferenceId (oldConstructorIds ^?! ix constructorName))
          (ConstructorReference newReferenceId newConstructorId)

    lift . lift $ putTypeDeclaration newReferenceId dd

  -- Need to get one of the new references to grab its hash, doesn't matter which one since
  -- all hashes in the component are the same.
  case newComponent ^? traversed . _2 . to Reference.idToHash of
    Nothing -> pure ()
    Just newHash -> insertObjectMappingForHash conn oldHash newHash
  field @"migratedDefnHashes" %= Set.insert oldHash

  pure Sync.Done

insertObjectMappingForHash ::
  (MonadIO m, MonadState MigrationState m) =>
  Connection ->
  Old Hash ->
  New Hash ->
  m ()
insertObjectMappingForHash conn oldHash newHash = do
  (oldObjectId, newHashId, newObjectId) <- runDB conn . liftQ $ do
    oldHashId <- Q.expectHashIdByHash . Cv.hash1to2 $ oldHash
    oldObjectId <- Q.expectObjectIdForPrimaryHashId $ oldHashId
    newHashId <- Q.expectHashIdByHash . Cv.hash1to2 $ newHash
    newObjectId <- Q.expectObjectIdForPrimaryHashId $ newHashId
    pure (oldObjectId, newHashId, newObjectId)
  field @"objLookup" %= Map.insert oldObjectId (newObjectId, newHashId, newHash, oldHash)

typeReferences_ :: (Monad m, Ord v) => LensLike' m (Type v a) SomeReferenceId
typeReferences_ =
  ABT.rewriteDown_ -- Focus all terms
    . ABT.baseFunctor_ -- Focus Type.F
    . Type._Ref -- Only the Ref constructor has references
    . Reference._DerivedId
    . asTypeReference_

termReferences_ :: (Monad m, Ord v) => LensLike' m (Term.Term v a) SomeReferenceId
termReferences_ =
  ABT.rewriteDown_ -- Focus all terms
    . ABT.baseFunctor_ -- Focus Term.F
    . termFReferences_

termFReferences_ :: (Ord tv, Monad m) => LensLike' m (Term.F tv ta pa a) SomeReferenceId
termFReferences_ f t =
  (t & Term._Ref . Reference._DerivedId . asTermReference_ %%~ f)
    >>= Term._Constructor . someRefCon_ %%~ f
    >>= Term._Request . someRefCon_ %%~ f
    >>= Term._Ann . _2 . typeReferences_ %%~ f
    >>= Term._Match . _2 . traversed . Term.matchPattern_ . patternReferences_ %%~ f
    >>= Term._TermLink . referentAsSomeTermReference_ %%~ f
    >>= Term._TypeLink . Reference._DerivedId . asTypeReference_ %%~ f

-- | Build a SomeConstructorReference
someRefCon_ :: Traversal' ConstructorReference.ConstructorReference SomeReferenceId
someRefCon_ = refConPair_ . asConstructorReference_
  where
    refConPair_ :: Traversal' ConstructorReference.ConstructorReference ConstructorReference.ConstructorReferenceId
    refConPair_ f s =
      case s of
        ConstructorReference.ConstructorReference (Reference.Builtin _) _ -> pure s
        ConstructorReference.ConstructorReference (Reference.DerivedId n) c ->
          ( \(ConstructorReference.ConstructorReference n' c') ->
              ConstructorReference.ConstructorReference (Reference.DerivedId n') c'
          )
            <$> f (ConstructorReference.ConstructorReference n c)

patternReferences_ :: Traversal' (Pattern loc) SomeReferenceId
patternReferences_ f = \case
  p@(Pattern.Unbound {}) -> pure p
  p@(Pattern.Var {}) -> pure p
  p@(Pattern.Boolean {}) -> pure p
  p@(Pattern.Int {}) -> pure p
  p@(Pattern.Nat {}) -> pure p
  p@(Pattern.Float {}) -> pure p
  p@(Pattern.Text {}) -> pure p
  p@(Pattern.Char {}) -> pure p
  (Pattern.Constructor loc ref patterns) ->
    (\newRef newPatterns -> Pattern.Constructor loc newRef newPatterns)
      <$> (ref & someRefCon_ %%~ f)
      <*> (patterns & traversed . patternReferences_ %%~ f)
  (Pattern.As loc pat) -> Pattern.As loc <$> patternReferences_ f pat
  (Pattern.EffectPure loc pat) -> Pattern.EffectPure loc <$> patternReferences_ f pat
  (Pattern.EffectBind loc ref patterns pat) ->
    do
      (\newRef newPatterns newPat -> Pattern.EffectBind loc newRef newPatterns newPat)
      <$> (ref & someRefCon_ %%~ f)
      <*> (patterns & traversed . patternReferences_ %%~ f)
      <*> (patternReferences_ f pat)
  (Pattern.SequenceLiteral loc patterns) ->
    Pattern.SequenceLiteral loc <$> (patterns & traversed . patternReferences_ %%~ f)
  Pattern.SequenceOp loc pat seqOp pat2 -> do
    Pattern.SequenceOp loc <$> patternReferences_ f pat <*> pure seqOp <*> patternReferences_ f pat2

referentAsSomeTermReference_ :: Traversal' Referent.Referent SomeReferenceId
referentAsSomeTermReference_ f = \case
  (Referent'.Ref' (Reference.DerivedId refId)) -> do
    newRefId <- refId & asTermReference_ %%~ f
    pure (Referent'.Ref' (Reference.DerivedId newRefId))
  (Referent'.Con' (ConstructorReference.ConstructorReference (Reference.DerivedId refId) conId) conType) ->
    (ConstructorReference.ConstructorReference refId conId & asConstructorReference_ %%~ f)
      <&> \(ConstructorReference.ConstructorReference newRefId newConId) ->
        Referent'.Con'
          (ConstructorReference.ConstructorReference (Reference.DerivedId newRefId) newConId)
          conType
  r -> pure r

type SomeReferenceId = SomeReference Reference.Id

type SomeReferenceObjId = SomeReference (UReference.Id' ObjectId)

remapObjIdRefs ::
  (Map (Old ObjectId) (New ObjectId, New HashId, New Hash, Old Hash)) ->
  (Map SomeReferenceId SomeReferenceId) ->
  SomeReferenceObjId ->
  SomeReferenceObjId
remapObjIdRefs objMapping refMapping someObjIdRef = newSomeObjId
  where
    oldObjId :: ObjectId
    oldObjId = someObjIdRef ^. someRef_ . UReference.idH
    (newObjId, _, _, oldHash) =
      case Map.lookup oldObjId objMapping of
        Nothing -> error $ "Expected object mapping for ID: " <> show oldObjId
        Just found -> found
    oldSomeRefId :: SomeReferenceId
    oldSomeRefId = (someObjIdRef & someRef_ . UReference.idH .~ oldHash) ^. uRefIdAsRefId_
    newSomeRefId :: SomeReferenceId
    newSomeRefId = case Map.lookup oldSomeRefId refMapping of
      Nothing -> error $ "Expected reference mapping for ID: " <> show oldSomeRefId
      Just r -> r
    newSomeObjId :: SomeReference (UReference.Id' (New ObjectId))
    newSomeObjId = (newSomeRefId ^. from uRefIdAsRefId_) & someRef_ . UReference.idH .~ newObjId

data SomeReference ref
  = TermReference ref
  | TypeReference ref
  | ConstructorReference ref ConstructorId
  deriving (Eq, Functor, Generic, Ord, Show, Foldable, Traversable)

someRef_ :: Lens (SomeReference ref) (SomeReference ref') ref ref'
someRef_ = lens getter setter
  where
    setter (TermReference _) r = TermReference r
    setter (TypeReference _) r = TypeReference r
    setter (ConstructorReference _ conId) r = (ConstructorReference r conId)
    getter = \case
      TermReference r -> r
      TypeReference r -> r
      ConstructorReference r _ -> r

_TermReference :: Prism' (SomeReference ref) ref
_TermReference = _Ctor @"TermReference"

-- | This is only safe as long as you don't change the constructor of your SomeReference
asTermReference_ :: Traversal' ref (SomeReference ref)
asTermReference_ f ref =
  f (TermReference ref) <&> \case
    TermReference ref' -> ref'
    _ -> error "asTermReference_: SomeReferenceId constructor was changed."

-- | This is only safe as long as you don't change the constructor of your SomeReference
asTypeReference_ :: Traversal' ref (SomeReference ref)
asTypeReference_ f ref =
  f (TypeReference ref) <&> \case
    TypeReference ref' -> ref'
    _ -> error "asTypeReference_: SomeReferenceId constructor was changed."

-- | This is only safe as long as you don't change the constructor of your SomeReference
asConstructorReference_ :: Traversal' (ConstructorReference.GConstructorReference ref) (SomeReference ref)
asConstructorReference_ f (ConstructorReference.ConstructorReference ref cId) =
  f (ConstructorReference ref cId) <&> \case
    ConstructorReference ref' cId -> ConstructorReference.ConstructorReference ref' cId
    _ -> error "asConstructorReference_: SomeReferenceId constructor was changed."

someReferenceIdToEntity :: SomeReferenceId -> Entity
someReferenceIdToEntity = \case
  (TermReference ref) -> TermComponent (Reference.idToHash ref)
  (TypeReference ref) -> DeclComponent (Reference.idToHash ref)
  -- Constructors are migrated by their decl component.
  (ConstructorReference ref _conId) -> DeclComponent (Reference.idToHash ref)

foldSetter :: LensLike (Writer [a]) s t a a -> s -> [a]
foldSetter t s = execWriter (s & t %%~ \a -> tell [a] *> pure a)

saveV2EmptyBranch :: MonadIO m => Connection -> m (BranchHashId, Hash)
saveV2EmptyBranch conn = do
  let branch = S.emptyBranch
  let (localBranchIds, localBranch) = S.LocalizeObject.localizeBranch branch
  newHash <- runDB conn (Ops.liftQ (Hashing.dbBranchHash branch))
  newHashId <- runDB conn (Ops.liftQ (Q.saveBranchHash (BranchHash (Cv.hash1to2 newHash))))
  _ <- runDB conn (Ops.saveBranchObject newHashId localBranchIds localBranch)
  pure (newHashId, newHash)
