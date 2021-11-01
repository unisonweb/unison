{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.SqliteCodebase.MigrateSchema12 where

import Control.Lens
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT), ask, mapReaderT)
import Control.Monad.State.Strict
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Writer.CPS (Writer, execWriter, tell)
import Data.Generics.Product
import Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Tuple (swap)
import qualified Data.Zip as Zip
import qualified U.Codebase.Causal as C
import U.Codebase.HashTags (CausalHash (CausalHash, unCausalHash), BranchHash (BranchHash), CausalHash)
import qualified U.Codebase.Reference as UReference
import qualified U.Codebase.Referent as UReferent
import qualified U.Codebase.Sqlite.Branch.Format as S.Branch
import qualified U.Codebase.Sqlite.Branch.Full as S
import qualified U.Codebase.Sqlite.Branch.Full as S (DbBranch)
import qualified U.Codebase.Sqlite.Branch.Full as S.Branch.Full
import U.Codebase.Sqlite.Connection (Connection)
import qualified U.Codebase.Sqlite.Causal as SC
import U.Codebase.Sqlite.Causal (DbCausal, GDbCausal (..))
import U.Codebase.Sqlite.DbId (BranchHashId (BranchHashId, unBranchHashId), CausalHashId (CausalHashId, unCausalHashId), HashId (HashId), ObjectId)
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as S.Reference
import U.Codebase.Sync (Sync (Sync))
import qualified U.Codebase.Sync as Sync
import qualified U.Codebase.WatchKind as WK
import qualified U.Util.Monoid as Monoid
import qualified Unison.ABT as ABT
import Unison.Codebase (Codebase (Codebase))
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.SqliteCodebase.MigrateSchema12.DbHelpers as Hashing
import qualified Unison.DataDeclaration as DD
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Hash (Hash)
import qualified Unison.Hash as Unison
import qualified Unison.Hashable as Hashable
import qualified Unison.Hashing.V2.Causal as Hashing
import qualified Unison.Hashing.V2.Convert as Convert
import Unison.Pattern (Pattern)
import Unison.Prelude
import Unison.Reference (Pos)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.Var (Var)
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv

-- lookupCtor :: ConstructorMapping -> ObjectId -> Pos -> ConstructorId -> Maybe (Pos, ConstructorId)
-- lookupCtor (ConstructorMapping cm) oid pos cid =
--   Map.lookup oid cm >>= (Vector.!? fromIntegral pos) >>= (Vector.!? cid)

-- lookupTermRef :: TermLookup -> S.Reference -> Maybe S.Reference
-- lookupTermRef _tl (ReferenceBuiltin t) = Just (ReferenceBuiltin t)
-- lookupTermRef tl (ReferenceDerived id) = ReferenceDerived <$> lookupTermRefId tl id

-- lookupTermRefId :: TermLookup -> S.Reference.Id -> Maybe S.Reference.Id
-- lookupTermRefId tl (Id oid pos) = Id oid <$> lookupTermPos tl oid pos

-- lookupTermPos :: TermLookup -> ObjectId -> Pos -> Maybe Pos
-- lookupTermPos (TermLookup tl) oid pos = Map.lookup oid tl >>= (Vector.!? fromIntegral pos)

-- newtype ConstructorMapping = ConstructorMapping (Map ObjectId (Vector (Vector (Pos, ConstructorId))))
-- newtype TermLookup = TermLookup (Map ObjectId (Vector Pos))

type TypeIdentifier = (ObjectId, Pos)

type Old a = a

type New a = a

type ConstructorName v = v

type ComponentName v = v

data MigrationState = MigrationState
  -- Mapping between old cycle-position -> new cycle-position for a given Decl object.
  { -- declLookup :: Map (Old ObjectId) (Map (Old Pos) (New Pos)),
    referenceMapping :: Map (Old SomeReferenceId) (New SomeReferenceId),
    causalMapping :: Map (Old CausalHashId) (New (CausalHash, CausalHashId)),
    -- Mapping between contructor indexes for the type identified by (ObjectId, Pos)
    ctorLookup :: Map (Old TypeIdentifier) (Map (Old ConstructorId) (New ConstructorId)),
    ctorLookup' :: Map (Old Referent.Id) (New Referent.Id),
    -- This provides the info needed for rewriting a term.  You'll access it with a function :: Old
    termLookup :: Map (Old ObjectId) (New ObjectId, Map (Old Pos) (New Pos)),
    objLookup :: Map (Old ObjectId) (New (ObjectId, HashId, Hash)),
    --
    componentPositionMapping :: Map ObjectId (Map (Old Pos) (New Pos)),
    constructorIDMapping :: Map ObjectId (Map (Old ConstructorId) (New ConstructorId)),
    completed :: Set ObjectId
  }
  deriving (Generic)

-- declLookup :: Map ObjectId (Map Pos (Pos, Map ConstructorId ConstructorId)),

{-
* Load entire codebase as a list
* Pick a term from the codebase
* Look up the references inside the term
* If any haven't been processed, add them to the "to process" stack, push the term you were working on back onto that stack
* Rebuild & rehash the term, store that
* For any data constructor terms inside,
  * Store a map from old ConstructorId to new, based on the old and new reference hashes
* After rebuilding a cycle, map old Pos to new
-}

-- Q: can we plan to hold the whole mapping in memory? ✅
-- Q: a) update database in-place? or b) write to separate database and then overwrite? leaning (b).
-- note: we do need to rebuild namespaces, although we don't need to rehash them.

-- cycle position index `Pos`
-- constructor index `ConstructorId`

{-
data Maybe a = (Just Bar | Nothing X)

-- changes due to missing size from ref(Y)
data X = MkX Y

-- know old hash and old cycle positions
data Y = MkY Int
-}

data Entity
  = TComponent Unison.Hash
  | DComponent Unison.Hash
  | C CausalHashId
  | -- haven't proven we need these yet
    B ObjectId
  | Patch ObjectId
  | W WK.WatchKind S.Reference.IdH -- Hash Reference.Id
  deriving (Eq, Ord, Show)

data Env m v a = Env {db :: Connection, codebase :: Codebase m v a}

--  -> m (TrySyncResult h)
migrationSync ::
  (MonadIO m, Var v) =>
  Sync (ReaderT (Env m v a) (StateT MigrationState m)) Entity
migrationSync = Sync \case
  -- To sync an object,
  --   * If we have already synced it, we are done.
  --   * Otherwise, read the object from the database and switch on its object type.
  --   * See next steps below v
  --
  -- To sync a decl component object,
  --   * If we have not already synced all dependencies, push syncing them onto the front of the work queue.
  --   * Otherwise, ???
  --
  -- To sync a term component object,
  --   * If we have not already synced all dependencies, push syncing them onto the front of the work queue.
  --   * Otherwise, ???
  --
  -- To sync a namespace object,
  --   * Deserialize it and compute its dependencies (terms, types, patches, children).
  --   * If we have not already synced all of its dependencies, push syncing them onto the front of the work queue.
  --   * To sync a 'BranchFull',
  --     * We need to make a new 'BranchFull' in memory, then insert it into the database under a new object id.
  --       * Wait, we need to preserve the ordering of the types/terms, either by not changing them (but the orderings of the
  --         reference ids used in keys is definitely not preserved by this migration), or by permuting the local id vectors,
  --         but we may be at a level too low or high for us to care?
  --     * Its 'LocalBranch' must have all references changed in-place per the (old (object id, pos) => new (object id, pos)) mapping.
  --     * The local IDs within the body _likely_ don't need to change. (why likely?)
  --     * Its 'BranchLocalIds' must be translated from the old codebase object IDs to the new object IDs,
  --       we can use our MigrationState to look these up, since they must have already been migrated.
  --   * To sync a 'BranchDiff',
  --     * These don't exist in schema v1; we can error if we encounter one.
  --
  -- To sync a patch object
  --   * Rewrite all old hashes in the patch to the new hashes.
  --
  -- To sync a watch expression
  --   * ???
  --
  -- To sync a Causal
  --- * If we haven't yet synced its parents, push them onto the work queue
  --- * If we haven't yet synced the causal's value (namespace), push it onto the work queue.
  --- * Rehash the Causal's valueHash AND CausalHash, and add the new causal, its hashes, and hash objects to the codebase under a fresh object ID
  TComponent hash -> do
    Env {codebase} <- ask
    lift (migrateTermComponent codebase hash)
  DComponent hash -> do
    Env {codebase} <- ask
    lift (migrateDeclComponent codebase hash)
  B objectId -> do
    Env {db} <- ask
    lift (migrateBranch db objectId)
  C causalHashId -> do
    Env {db} <- ask
    lift (migrateCausal db causalHashId)
  -- To sync a watch result,
  --   1. ???
  --   2. Synced
  W _watchKind _idH -> undefined
  Patch {} -> undefined

migratePatch :: Hash -> ByteString -> m (Sync.TrySyncResult Entity)
migratePatch = error "not implemented"

runDB :: MonadIO m => Connection -> ReaderT Connection (ExceptT Ops.Error (ExceptT Q.Integrity m)) a -> m a
runDB conn = (runExceptT >=> err) . (runExceptT >=> err) . flip runReaderT conn
  where
    err :: forall e x m. (Either e x -> m x)
    err = \case Left err -> error $ show err; Right a -> pure a

liftQ :: ReaderT Connection (ExceptT Q.Integrity m) a -> ReaderT Connection (ExceptT Ops.Error (ExceptT Q.Integrity m)) a
liftQ = mapReaderT lift

-- loadCausalBranchByCausalHash :: EDB m => CausalHash -> m (Maybe (C.Branch.Causal m))
--
-- Causal Plan

-- * Load a DbCausal (how do we do this)

-- => new function Queries.localCausalByCausalHashId, can model after loadCausalByCausalHash or factor out of

-- * Add valueHashId's ObjectId as a dependency if unmigrated

-- * Add parent causal hash ids as dependencies if unmigrated

-- => Queries.loadCausalParents

-- * Map over Branch hash IDs

-- * Inside saveDBCausal (new / factored out of original)

--   * Save as a new self-hash
--    ==> Queries.saveCausal
--   * Map over parent causal hash IDs
--    ==> Queries.saveCausalParents
migrateCausal :: MonadIO m => Connection -> CausalHashId -> StateT MigrationState m (Sync.TrySyncResult Entity)
migrateCausal conn oldCausalHashId = runDB conn . fmap (either id id) . runExceptT $ do
  oldBranchHashId <- lift . liftQ $ Q.loadCausalValueHashId oldCausalHashId
  oldCausalParentHashIds <- lift . liftQ $ Q.loadCausalParents oldCausalHashId

  -- This fails if the object for the branch doesn't exist, CHECK: we currently expect
  -- this to always be true?
  branchObjId <- lift . liftQ $ Q.expectObjectIdForAnyHashId (unBranchHashId oldBranchHashId)
  migratedObjIds <- gets objLookup
  -- If the branch for this causal hasn't been migrated, migrate it first.
  let unmigratedBranch =
        if (branchObjId `Map.notMember` migratedObjIds)
          then [B branchObjId]
          else []

  migratedCausals <- gets causalMapping
  let unmigratedParents = map C . filter (`Map.member` migratedCausals) $ oldCausalParentHashIds
  let unmigratedEntities = unmigratedBranch <> unmigratedParents
  when (not . null $ unmigratedParents <> unmigratedBranch) (throwE $ Sync.Missing unmigratedEntities)

  (_, _, newBranchHash) <- gets (\MigrationState {..} -> objLookup Map.! branchObjId)

  let newParentHashes =
        oldCausalParentHashIds
          & fmap
            ( \oldParentHashId ->
                let (CausalHash h, _) = migratedCausals Map.! oldParentHashId
                 in h
            )
          & Set.fromList

  let newCausalHash :: CausalHash
      newCausalHash =
        CausalHash . Cv.hash1to2 . Hashable.accumulate $
          Hashing.hashCausal
            ( Hashing.Causal
                { branchHash = newBranchHash,
                  parents = Set.map Cv.hash2to1 newParentHashes
                }
            )
  newCausalHashId <- Q.saveCausalHash newCausalHash
  let newCausal =
        DbCausal
          { selfHash = newCausalHashId,
            valueHash = BranchHashId $ view _2 (migratedObjIds Map.! branchObjId),
            parents = Set.fromList . map (snd . (\old -> migratedCausals Map.! old)) $ oldCausalParentHashIds
          }
  Q.saveCausal (SC.selfHash newCausal) (SC.valueHash newCausal)
  Q.saveCausalParents (SC.selfHash newCausal) (Set.toList $ SC.parents newCausal)

  field @"causalMapping" %= Map.insert oldCausalHashId (newCausalHash, newCausalHashId)

  pure Sync.Done
  -- Plan:
  --   * Load the pieces of a Db.Causal ✅
  --   * Ensure its parent causals and branch (value hash) have been migrated ✅
  --   * Rewrite the value-hash and parent causal hashes ✅
  --   * Save the new causal ✅
  --   * Save Causal Hash mapping to skymap ✅

-- data C.Branch m = Branch
-- { terms    :: Map NameSegment (Map Referent (m MdValues)),
--   types    :: Map NameSegment (Map Reference (m MdValues)),
--   patches  :: Map NameSegment (PatchHash, m Patch),
--   children :: Map NameSegment (Causal m)
-- }

-- data Branch' t h p c = Branch
--   { terms :: Map t (Map (Referent'' t h) (MetadataSetFormat' t h)),
--     types :: Map t (Map (Reference' t h) (MetadataSetFormat' t h)),
--     patches :: Map t p,
--     children :: Map t c
--   }

migrateBranch :: MonadIO m => Connection -> ObjectId -> StateT MigrationState m (Sync.TrySyncResult Entity)
migrateBranch conn oldObjectId = fmap (either id id) . runExceptT $ do
  -- note for tomorrow: we want to just load the (Branch m) instead, forget the DbBranch
  -- dbBranch <- Ops.loadDbBranchByObjectId objectId

  let allMissingTypes = undefined
  let allMissingTerms = undefined
  let allMissingPatches = undefined
  let allMissingChildren = undefined
  let allMissingPredecessors = undefined

  -- Identify dependencies and bail out if they aren't all built
  let allMissingReferences :: [Entity]
      allMissingReferences =
        allMissingTypes
          ++ allMissingTerms
          ++ allMissingPatches
          ++ allMissingChildren
          ++ allMissingPredecessors

  when (not . null $ allMissingReferences) $
    throwE $ Sync.Missing allMissingReferences

  -- Read the old branch
  oldBranch <- runDB conn (Ops.loadDbBranchByObjectId (BranchObjectId oldObjectId))
  -- Remap object id references
  -- TODO: remap sub-namespace causal hashes
  newBranch <- oldBranch & dbBranchObjRefs_ %%~ remapObjIdRefs
  let (localBranchIds, localBranch) = S.Branch.dbToLocalBranch newBranch
  hash <- runDB conn (Ops.liftQ (Hashing.dbBranchHash newBranch))
  newHashId <- runDB conn (Ops.liftQ (Q.saveBranchHash (BranchHash (Cv.hash1to2 hash))))
  newObjectId <- runDB conn (Ops.saveBranchObject newHashId localBranchIds localBranch)
  field @"objLookup" %= Map.insert oldObjectId newObjectId

-- Project an S.Referent'' into its SomeReferenceObjId's
someReferent_ :: Traversal' (S.Branch.Full.Referent'' t ObjectId) SomeReferenceObjId
someReferent_ =
  (UReferent._Ref . someReference_)
    `failing` ( UReferent._Con
                  . asPair_ -- Need to unpack the embedded reference AND remap between mismatched Constructor ID types.
                  . unsafeInsidePrism _ConstructorReference
              )
  where
    asPair_ f (UReference.ReferenceDerived id', conId) =
      f (id', fromIntegral conId)
        <&> \(newId, newConId) -> (UReference.ReferenceDerived newId, fromIntegral newConId)
    asPair_ _ (UReference.ReferenceBuiltin x, conId) = pure (UReference.ReferenceBuiltin x, conId)

someReference_ :: Traversal' (UReference.Reference' t ObjectId) SomeReferenceObjId
someReference_ = UReference._ReferenceDerived . unsafeInsidePrism _TermReference

someMetadataSetFormat :: Ord t => Traversal' (S.Branch.Full.MetadataSetFormat' t ObjectId) SomeReferenceObjId
someMetadataSetFormat = S.Branch.Full.metadataSetFormatReferences_ . someReference_

mapReferentMetadata ::
  (Ord k, Ord t) =>
  Traversal' k SomeReferenceObjId ->
  Traversal'
    (Map k (S.Branch.Full.MetadataSetFormat' t ObjectId))
    (SomeReferenceObjId)
mapReferentMetadata keyTraversal f m =
  Map.toList m
    & traversed . beside keyTraversal someMetadataSetFormat %%~ f
    <&> Map.fromList

dbBranchObjRefs_ :: Traversal' S.DbBranch SomeReferenceObjId
dbBranchObjRefs_ f S.Branch.Full.Branch {children, patches, terms, types} = do
  let newTypesMap = types & traversed . mapReferentMetadata someReference_ %%~ f
  let newTermsMap = terms & traversed . mapReferentMetadata someReferent_ %%~ f
  S.Branch.Full.Branch <$> newTermsMap <*> newTypesMap <*> pure patches <*> pure children

-- convertBranch :: (DB m, MonadState MigrationState m) => DbBranch -> m DbBranch
-- convertBranch dbBranch = _

-- DbBranch -- migrate --> DbBranch -- hydrate for the hash --> Hashing.V2.Branch -- put (Hash, toBranchFormat(DbBranch)) --> COOL

-- function that reads a DbBranch out of codebase

-- Traversal' DbBranch SomeReferenceObjectId
-- DB m => LensLike' m SomeReferenceObjectId SomeReferenceId
-- MonadState MigrationState m => SomeReferenceId -> m SomeReferenceId

-- Traversal' DbBranch (BranchId, CausalHashId)
-- MonadState MigrationState m => (BranchId, CausalHashId) -> m (BranchId, CausalHashId)

-- Traversal' DbBranch PatchId
-- MonadState MigrationState m => PatchObjectId -> m PatchObjectId

--   type DbBranch = Branch' TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId)

-- data Branch' t h p c = Branch
--   { terms :: Map t (Map (Referent'' t h) (MetadataSetFormat' t h)),
--     types :: Map t (Map (Reference' t h) (MetadataSetFormat' t h)),
--     patches :: Map t p,
--     children :: Map t c
--   }

migrateTermComponent :: forall m v a. (Ord v, Var v, Monad m) => Codebase m v a -> Unison.Hash -> StateT MigrationState m (Sync.TrySyncResult Entity)
migrateTermComponent Codebase {..} hash = fmap (either id id) . runExceptT $ do
  component <-
    (lift . lift $ getTermComponentWithTypes hash) >>= \case
      Nothing -> error $ "Hash was missing from codebase: " <> show hash
      Just component -> pure component

  let componentIDMap :: Map (Old Reference.Id) (Term.Term v a, Type v a)
      componentIDMap = Map.fromList $ Reference.componentFor hash component
  let unhashed :: Map (Old Reference.Id) (v, Term.Term v a)
      unhashed = Term.unhashComponent (fst <$> componentIDMap)
  let vToOldReferenceMapping :: Map v (Old Reference.Id)
      vToOldReferenceMapping =
        unhashed
          & Map.toList
          & fmap (\(refId, (v, _trm)) -> (v, refId))
          & Map.fromList

  referencesMap <- gets referenceMapping
  let getMigratedReference :: Old SomeReferenceId -> New SomeReferenceId
      getMigratedReference ref =
        Map.findWithDefault (error "unmigrated reference") ref referencesMap

  let allMissingReferences :: [Old SomeReferenceId]
      allMissingReferences =
        unhashed
          & foldSetter
            ( traversed
                . _2
                . termReferences_
                . filtered (\r -> Map.notMember r referencesMap)
            )

  when (not . null $ allMissingReferences) $
    throwE $ Sync.Missing . nubOrd $ (someReferenceIdToEntity <$> allMissingReferences)

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
          & Convert.hashTermComponents'

  ifor newTermComponents $ \v (newReferenceId, trm, typ) -> do
    let oldReferenceId = vToOldReferenceMapping Map.! v
    field @"referenceMapping" %= Map.insert (TermReference oldReferenceId) (TermReference newReferenceId)
    lift . lift $ putTerm newReferenceId trm typ

  pure Sync.Done

migrateDeclComponent ::
  forall m v a.
  (Ord v, Var v, Monad m) =>
  Codebase m v a ->
  Unison.Hash ->
  StateT MigrationState m (Sync.TrySyncResult Entity)
migrateDeclComponent Codebase {..} hash = fmap (either id id) . runExceptT $ do
  declComponent :: [DD.Decl v a] <-
    (lift . lift $ getDeclComponent hash) >>= \case
      Nothing -> error "handle this" -- not non-fatal!
      Just dc -> pure dc

  let componentIDMap :: Map (Old Reference.Id) (DD.Decl v a)
      componentIDMap = Map.fromList $ Reference.componentFor hash declComponent

  let unhashed :: Map (Old Reference.Id) (v, DD.Decl v a)
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
                . filtered (\r -> Map.notMember r migratedReferences)
            )

  when (not . null $ unmigratedRefIds) do
    throwE (Sync.Missing (nubOrd . fmap someReferenceIdToEntity $ unmigratedRefIds))

  -- At this point we know we have all the required mappings from old references  to new ones.
  let remapTerm :: Type v a -> Type v a
      remapTerm = typeReferences_ %~ \ref -> Map.findWithDefault (error "unmigrated reference") ref migratedReferences

  let remappedReferences :: Map (Old Reference.Id) (v, DD.Decl v a)
      remappedReferences =
        unhashed
          & traversed -- Traverse map of reference IDs
            . _2 -- Select the DataDeclaration
            . beside DD.asDataDecl_ id -- Unpack effect decls
            . DD.constructors_ -- Get the data constructors
            . traversed -- traverse the list of them
            . _3 -- Select the Type term.
          %~ remapTerm
  let vToOldReference :: Map v (Old Reference.Id)
      vToOldReference = Map.fromList . fmap swap . Map.toList . fmap fst $ remappedReferences

  let newComponent :: [(v, Reference.Id, DD.Decl v a)]
      newComponent =
        remappedReferences
          & Map.elems
          & Map.fromList
          & Convert.hashDecls'
          & fromRight (error "unexpected resolution error")
  for_ newComponent $ \(v, newReferenceId, dd) -> do
    let oldReferenceId = vToOldReference Map.! v
    field @"referenceMapping" %= Map.insert (TypeReference oldReferenceId) (TypeReference newReferenceId)

    let oldConstructorIds :: Map (ConstructorName v) (Old ConstructorId)
        oldConstructorIds =
          (componentIDMap Map.! oldReferenceId)
            & DD.asDataDecl
            & DD.constructors'
            & imap (\(fromIntegral -> constructorId) (_ann, name, _type) -> (name, constructorId))
            & Map.fromList

    ifor_ (DD.constructors' (DD.asDataDecl dd)) \(fromIntegral -> newConstructorId) (_ann, name, _type) -> do
      field @"referenceMapping"
        %= Map.insert
          (ConstructorReference oldReferenceId (oldConstructorIds Map.! name))
          (ConstructorReference newReferenceId newConstructorId)

    lift . lift $ putTypeDeclaration newReferenceId dd
  pure Sync.Done

typeReferences_ :: (Monad m, Ord v) => LensLike' m (Type v a) SomeReferenceId
typeReferences_ =
  ABT.rewriteDown_ -- Focus all terms
    . ABT.baseFunctor_ -- Focus Type.F
    . Type._Ref -- Only the Ref constructor has references
    . Reference._DerivedId
    . unsafeInsidePrism _TypeReference

-- | This is only lawful so long as your changes to 's' won't cause the prism to fail to match.
unsafeInsidePrism :: Prism' s a -> Lens' a s
unsafeInsidePrism p f a = do
  fromMaybe a . preview p <$> f (review p a)

termReferences_ :: (Monad m, Ord v) => LensLike' m (Term.Term v a) SomeReferenceId
termReferences_ =
  ABT.rewriteDown_ -- Focus all terms
    . ABT.baseFunctor_ -- Focus Term.F
    . termFReferences_

termFReferences_ :: (Ord tv, Monad m) => LensLike' m (Term.F tv ta pa a) SomeReferenceId
termFReferences_ f t =
  (t & Term._Ref . Reference._DerivedId . unsafeInsidePrism _TermReference %%~ f)
    >>= Term._Constructor . thing . unsafeInsidePrism _ConstructorReference %%~ f
    >>= Term._Request . thing . unsafeInsidePrism _ConstructorReference %%~ f
    >>= Term._Ann . _2 . typeReferences_ %%~ f
    >>= Term._Match . _2 . traversed . Term.matchPattern_ . patternReferences_ %%~ f
    >>= Term._TermLink . referentReferences %%~ f
    >>= Term._TypeLink . Reference._DerivedId . unsafeInsidePrism _TypeReference %%~ f

-- fixme rename
thing :: Traversal' (Reference.Reference, ConstructorId) (Reference.Id, ConstructorId)
thing f s =
  case s of
    (Reference.Builtin _, _) -> pure s
    (Reference.DerivedId n, c) -> (\(n', c') -> (Reference.DerivedId n', c')) <$> f (n, c)

patternReferences_ :: Traversal' (Pattern loc) SomeReferenceId
patternReferences_ = undefined -- types @Reference.Id

referentReferences :: Traversal' Referent.Referent SomeReferenceId
referentReferences = undefined

-- structural type Ping x = P1 (Pong x)
--   P1 : forall x. Pong x -> Ping x

-- structural type Pong x = P2 (Ping x) | P3 Nat
--   P2 : forall x. Ping x -> Pong x
--   P3 : forall x. Nat -> Pong x

-- end up with
-- decl Ping (Ref.Id #abc pos=0)
-- decl Pong (Ref.Id #abc pos=1)
-- ctor P1: #abc pos=0 cid=0
-- ctor P2: #abc pos=1 cid=0
-- ctor P3: #abc pos=1 cid=1
--
-- we unhashComponent and get:
-- { X -> structural type X x = AAA (Y x)
-- , Y -> structural type Y x = BBB (X x) | CCC Nat }

remapReferences ::
  Map (Old Reference.Id) (New Reference.Id) ->
  Type.F (Type v a) ->
  Type.F (Type v a)
remapReferences declMap = \case
  (Type.Ref (Reference.DerivedId refId)) ->
    Type.Ref . Reference.DerivedId $
      fromMaybe
        (error $ "Expected reference to exist in decl mapping, but it wasn't found: " <> show refId)
        (Map.lookup refId declMap)
  x -> x

type SomeReferenceId = SomeReference Reference.Id

type SomeReferenceObjId = SomeReference (UReference.Id' ObjectId)

objIdsToHashed :: MonadState MigrationState m => SomeReferenceObjId -> m SomeReferenceId
objIdsToHashed =
  someRef_ %%~ \(UReference.Id objId pos) -> do
    objMapping <- gets objLookup
    case Map.lookup objId objMapping of
      Nothing -> error $ "Expected object mapping for ID: " <> show objId
      Just (_, _, hash) -> pure (Reference.Id hash pos)

remapObjIdRefs :: MonadState MigrationState m => SomeReferenceObjId -> m SomeReferenceObjId
remapObjIdRefs =
  someRef_ %%~ \(UReference.Id objId pos) -> do
    objMapping <- gets objLookup
    case Map.lookup objId objMapping of
      Nothing -> error $ "Expected object mapping for ID: " <> show objId
      Just (newObjId, _, _) -> pure (UReference.Id newObjId pos)

data SomeReference ref
  = TermReference ref
  | TypeReference ref
  | ConstructorReference ref ConstructorId
  deriving (Eq, Functor, Generic, Ord)

someRef_ :: Traversal (SomeReference ref) (SomeReference ref') ref ref'
someRef_ = param @0

_TermReference :: Prism (SomeReference ref) (SomeReference ref') ref ref'
_TermReference = undefined -- _Ctor @"TermReference"

_TypeReference :: Prism (SomeReference ref) (SomeReference ref') ref ref'
_TypeReference = undefined --_Ctor @"TypeReference"

_ConstructorReference :: Prism (SomeReference ref) (SomeReference ref') (ref, ConstructorId) (ref', ConstructorId)
_ConstructorReference = undefined -- _Ctor @"ConstructorReference"

someReferenceIdToEntity :: SomeReferenceId -> Entity
someReferenceIdToEntity = undefined

-- get references:
--
--   references :: Term f v a -> [Reference.Id]
--
-- are all those references keys in our skymap?
--   yes => migrate term
--   no => returh those references (as Entity, though) as more work to do

-- how to turn Reference.Id into Entity?
--   need its ObjectId,

-- Term f v a -> ValidateT (Seq Reference.Id) m (Term f v a)
--
-- recordRefsInType :: MonadState MigrationState m => Type v a -> WriterT [Reference.Id] m (Type v a)
-- recordRefsInType = _

-- findMissingReferencesInTermF ::
--   (Generic typeVar, Generic typeAnn, Generic patternAnn) =>
--   Term.F typeVar typeAnn patternAnn () ->
--   [Reference.Id]
-- findMissingReferencesInTermF t =
--   -- TODO: Test that this descends into Match cases and finds everything it needs to.
--   t ^.. types @Reference.Id

-- compute correspondence between `v`s in `fst <$> named` compared to `fst <$> new_references` to get a Reference.Id -> Reference.Id mapping
-- mitchell tapped out before understanding the following line
-- compute correspondence between constructors names & constructor indices in corresponding decls
-- submit/mappend these two correspondences to sky mapping

-- Swap the Reference positions according to our map of already computed swaps
-- Hydrate into the parser-typechecker version, get the new hash
-- reserialize it into the sqlite format
-- Compare the old and new sqlite versions to add those ConstructorID/Pos mappings to our context.

-- unrelated Q:
--   do we kinda have circular dependency issues here?
--   parser-typechecker depends on codebase2, but we are talking about doing things at the parser-typechecker level in this migration
--   answer: no

-- unhashComponent
-- :: forall v a. Var v => Map Reference.Id (Decl v a) -> Map Reference.Id (v, Decl v a)

-- DD.unhashComponent

-- [OldDecl] ==map==> [NewDecl] ==number==> [(NewDecl, Int)] ==sort==> [(NewDecl, Int)] ==> permutation is map snd of that

-- type List a = Nil | Cons (List a)

-- unique type Thunk = Thunk (Int ->{MakeThunk} Int)
-- ability MakeThunk where go : (Int -> Int) -> Thunk

-- What mitchell thinks unhashComponent is doing:
--
--  Take a recursive type like
--
--     Fix \myself -> Alternatives [Nil, Cons a myself]
--
--  And write it with variables in place of recursive mentions like
--
--     (Var 1, Alternatives [Nil, Cons a (Var 1)]

-- can derive `original` from Hash + [OldDecl]
-- original :: Map Reference.Id (Decl v a)

-- named, rewritten_dependencies :: Map (Reference.Id {old}) (v, Decl v a {old pos in references})
-- named = Decl.unhashComponent original

-- Mapping from the sky: (Reference.Id -> Reference.Id)

-- rewritten_dependencies = replace_dependency_pos's skymap named

-- new_references :: Map v (Reference.Id {new}, DataDeclaration v a)
-- new_references = Unison.Hashing.V2.Convert.hashDecls $ Map.toList $ Foldable.toList rewritten_dependencies

-- let DeclFormat locallyIndexedComponent = case runGetS S.getDeclFormat declFormatBytes of
--   Left err -> error "something went wrong"
--   Right declFormat -> declFormat

-- Operations.hs converts from S level to C level
-- SqliteCodebase.hs converts from C level to

-- | migrate sqlite codebase from version 1 to 2, return False and rollback on failure
migrateSchema12 :: Applicative m => Connection -> m Bool
migrateSchema12 _db = do
  -- todo: drop and recreate corrected type/mentions index schema
  -- do we want to garbage collect at this time? ✅
  -- or just convert everything without going in dependency order? ✅
  error "todo: go through "
  -- todo: double-hash all the types and produce an constructor mapping
  -- object ids will stay the same
  -- todo: rehash all the terms using the new constructor mapping
  -- and adding the type to the term
  -- do we want to diff namespaces at this time? ❌
  -- do we want to look at supporting multiple simultaneous representations of objects at this time?
  pure "todo: migrate12"
  pure True

-- -- remember that the component order might be different
-- rehashDeclComponent :: [Decl v a] -> (Hash, ConstructorMappings)
-- rehashDeclComponent decls = fmap decls <&> \case
--
--     --
--     error "todo: rehashDeclComponent"

-- rewriteDeclComponent :: DeclFormat.LocallyIndexedComponent -> (Hash, DeclFormat.LocallyIndexedComponent, ConstructorMappings)
-- rewriteDeclComponent =
--     --
--     error "todo: rehashDeclComponent"

-- rehashDeclComponent :: [Decl v a] -> (Hash, DeclFormat.LocallyIndexedComponent, ConstructorMappings)

-- rehashTermComponent :: ConstructorMappings -> TermFormat.LocallyIndexedComponent -> (Hash, TermFormat.LocallyIndexedComponent)
-- rehashTermComponent = error "todo: rehashTermComponent"

-- -- getConstructor :: ConstructorMappings -> ObjectId -> Pos -> ConstructorId
-- -- getConstructor cm

foldSetter :: LensLike (Writer [a]) s t a a -> s -> [a]
foldSetter t s = execWriter (s & t %%~ \a -> tell [a] *> pure a)
