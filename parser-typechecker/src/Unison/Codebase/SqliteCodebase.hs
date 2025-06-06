{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.SqliteCodebase
  ( Unison.Codebase.SqliteCodebase.init,
    Unison.Codebase.SqliteCodebase.initWithSetup,
    MigrationStrategy (..),
    BackupStrategy (..),
    VacuumStrategy (..),
    CodebaseLockOption (..),
    copyCodebase,
  )
where

import Data.Either.Extra ()
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import System.FileLock (SharedExclusive (Exclusive), withTryFileLock)
import U.Codebase.HashTags (BranchHash, CausalHash)
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Codebase (Codebase, CodebasePath)
import Unison.Codebase qualified as Codebase1
import Unison.Codebase.Branch (Branch (..), UnconflictedBranchView (..))
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Init (BackupStrategy (..), CodebaseLockOption (..), MigrationStrategy (..), VacuumStrategy (..))
import Unison.Codebase.Init qualified as Codebase
import Unison.Codebase.Init.CreateCodebaseError qualified as Codebase1
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (..))
import Unison.Codebase.Init.OpenCodebaseError qualified as Codebase1
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Migrations qualified as Migrations
import Unison.Codebase.SqliteCodebase.Operations qualified as CodebaseOps
import Unison.Codebase.SqliteCodebase.Paths
import Unison.Codebase.Type (LocalOrRemote (..))
import Unison.Codebase.Type qualified as C
import Unison.DataDeclaration (Decl)
import Unison.DeclCoherencyCheck (IncoherentDeclReason, checkDeclCoherency, lenientCheckDeclCoherency)
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.Hash (Hash)
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.Prelude
import Unison.Reference (Reference, Reference' (..), TermReferenceId, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Cache qualified as Cache
import Unison.Util.Defns (Defns (..))
import Unison.WatchKind qualified as UF
import UnliftIO (finally)
import UnliftIO qualified as UnliftIO
import UnliftIO.Concurrent qualified as UnliftIO
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)
import UnliftIO.STM

debug :: Bool
debug = False

init ::
  (HasCallStack, MonadUnliftIO m) =>
  Codebase.Init m Symbol Ann
init = initWithSetup (pure ())

-- | Like 'init', but allows passing in an action to be perform when a new codebase is created.
initWithSetup ::
  (HasCallStack, MonadUnliftIO m) =>
  -- Action to perform when a new codebase is created.
  -- It's run after the schema is created in the same transaction.
  Sqlite.Transaction () ->
  Codebase.Init m Symbol Ann
initWithSetup onCreate =
  Codebase.Init
    { withOpenCodebase = withCodebaseOrError,
      withCreatedCodebase = createCodebaseOrError onCreate,
      codebasePath = makeCodebaseDirPath
    }

-- | Create a codebase at the given location.
createCodebaseOrError ::
  (MonadUnliftIO m) =>
  Sqlite.Transaction () ->
  Codebase.DebugName ->
  CodebasePath ->
  CodebaseLockOption ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either Codebase1.CreateCodebaseError r)
createCodebaseOrError onCreate debugName path lockOption action = do
  ifM
    (doesFileExist $ makeCodebasePath path)
    (pure $ Left Codebase1.CreateCodebaseAlreadyExists)
    do
      createDirectoryIfMissing True (makeCodebaseDirPath path)
      withConnection (debugName ++ ".createSchema") path \conn -> do
        Sqlite.trySetJournalMode conn Sqlite.JournalMode'WAL
        Sqlite.runTransaction conn do
          CodebaseOps.createSchema
          onCreate

      sqliteCodebase debugName path Local lockOption DontMigrate action >>= \case
        Left schemaVersion -> error ("Failed to open codebase with schema version: " ++ show schemaVersion ++ ", which is unexpected because I just created this codebase.")
        Right result -> pure (Right result)

-- | Use the codebase in the provided path.
-- The codebase is automatically closed when the action completes or throws an exception.
withCodebaseOrError ::
  forall m r.
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  CodebaseLockOption ->
  MigrationStrategy ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either Codebase1.OpenCodebaseError r)
withCodebaseOrError debugName dir lockOption migrationStrategy action = do
  doesFileExist (makeCodebasePath dir) >>= \case
    False -> pure (Left Codebase1.OpenCodebaseDoesntExist)
    True -> sqliteCodebase debugName dir Local lockOption migrationStrategy action

-- 1) buffer up the component
-- 2) in the event that the component is complete, then what?
--  * can write component provided all of its dependency components are complete.
--    if dependency not complete,
--    register yourself to be written when that dependency is complete

-- | Run an action with a connection to the codebase, closing the connection on completion or
-- failure.
withConnection ::
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  (Sqlite.Connection -> m a) ->
  m a
withConnection name root action =
  Sqlite.withConnection name (makeCodebasePath root) action

sqliteCodebase ::
  forall m r.
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  -- | When local, back up the existing codebase before migrating, in case there's a catastrophic bug in the migration.
  LocalOrRemote ->
  CodebaseLockOption ->
  MigrationStrategy ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either Codebase1.OpenCodebaseError r)
sqliteCodebase debugName root localOrRemote lockOption migrationStrategy action = handleLockOption do
  -- The branchLoadCache ephemerally caches branches in memory, but doesn't prevent them from being GC'd.
  -- This is very useful when loading root branches because the cache shouldn't be limited in size.
  -- But this cache will automatically clean itself up and remove entries that are no longer reachable.
  -- If you load another branch, which shares namespaces with another branch that's in memory (and therefor in the cache)
  -- then those shared namespaces will be loaded from the cache and will be shared in memory.
  branchLoadCache <- newBranchCache
  -- The rootBranchCache is a semispace cache which keeps the most recent branch roots (e.g. project roots) alive in memory.
  -- Unlike the branchLoadCache, this cache is bounded in size and will evict older branches when it reaches its limit.
  -- The two work in tandem, so the rootBranchCache keeps relevant branches alive, and the branchLoadCache
  -- stores ALL the subnamespaces of those branches, deduping them when loading from the DB.
  rootBranchCache <- Cache.semispaceCache 10
  rootBranchCacheTx <- Cache.semispaceCache 10
  declTypeCache <- Cache.semispaceCache 2048
  let getDeclType = CodebaseOps.makeCachedTransaction declTypeCache CodebaseOps.getDeclType
  -- The v1 codebase interface has operations to read and write individual definitions
  -- whereas the v2 codebase writes them as complete components.  These two fields buffer
  -- the individual definitions until a complete component has been written.
  termBuffer :: TVar (Map Hash CodebaseOps.TermBufferEntry) <- newTVarIO Map.empty
  declBuffer :: TVar (Map Hash CodebaseOps.DeclBufferEntry) <- newTVarIO Map.empty

  result <- withConn \conn -> do
    Sqlite.runTransaction conn Migrations.checkCodebaseIsUpToDate >>= \case
      Migrations.CodebaseUpToDate -> pure $ Right ()
      Migrations.CodebaseUnknownSchemaVersion sv -> pure $ Left (OpenCodebaseUnknownSchemaVersion sv)
      Migrations.CodebaseRequiresMigration fromSv toSv ->
        case migrationStrategy of
          DontMigrate -> pure $ Left (OpenCodebaseRequiresMigration fromSv toSv)
          MigrateAfterPrompt backupStrategy vacuumStrategy -> do
            let shouldPrompt = True
            Migrations.ensureCodebaseIsUpToDate localOrRemote root getDeclType termBuffer declBuffer shouldPrompt backupStrategy vacuumStrategy conn
          MigrateAutomatically backupStrategy vacuumStrategy -> do
            let shouldPrompt = False
            Migrations.ensureCodebaseIsUpToDate localOrRemote root getDeclType termBuffer declBuffer shouldPrompt backupStrategy vacuumStrategy conn

  case result of
    Left err -> pure $ Left err
    Right () -> do
      let finalizer :: (MonadIO m) => m ()
          finalizer = do
            decls <- readTVarIO declBuffer
            terms <- readTVarIO termBuffer
            let printBuffer header b =
                  liftIO
                    if b /= mempty
                      then putStrLn header >> putStrLn "" >> print b
                      else pure ()
            printBuffer "Decls:" decls
            printBuffer "Terms:" terms

      flip finally finalizer do
        termCache <- Cache.semispaceCache 8192
        let getTerm = CodebaseOps.makeMaybeCachedTransaction termCache (CodebaseOps.getTerm getDeclType)
        typeOfTermCache <- Cache.semispaceCache 8192
        let getTypeOfTermImpl = CodebaseOps.makeMaybeCachedTransaction typeOfTermCache CodebaseOps.getTypeOfTermImpl
        typeDeclarationCache <- Cache.semispaceCache 1024
        let getTypeDeclaration = CodebaseOps.makeMaybeCachedTransaction typeDeclarationCache CodebaseOps.getTypeDeclaration
        declNumConstructorsCache <- Cache.semispaceCache 1024
        let expectDeclNumConstructors = CodebaseOps.makeCachedTransaction declNumConstructorsCache Operations.expectDeclNumConstructors
        let getBranchForHashTx = CodebaseOps.makeMaybeCachedTransaction rootBranchCacheTx (CodebaseOps.getBranchForHash branchLoadCache getDeclType)

        branchDeclNumConstructorsCache <- Cache.semispaceCache 10
        let getBranchDeclNumConstructors :: Keyed BranchHash UnconflictedBranchView -> Sqlite.Transaction (Map TypeReferenceId Int)
            getBranchDeclNumConstructors =
              CodebaseOps.makeCachedTransaction branchDeclNumConstructorsCache \k ->
                k.value.defns.types
                  & BiMultimap.dom
                  & Set.toList
                  & Foldable.foldlM
                    ( \acc -> \case
                        ReferenceBuiltin _ -> pure acc
                        ReferenceDerived ref -> do
                          num <- expectDeclNumConstructors ref
                          pure $! Map.insert ref num acc
                    )
                    Map.empty

        branchPartialDeclNameLookupCache <- Cache.semispaceCache 10
        let getBranchPartialDeclNameLookup ::
              BranchHash ->
              UnconflictedBranchView ->
              Sqlite.Transaction PartialDeclNameLookup
            getBranchPartialDeclNameLookup =
              let get :: Keyed BranchHash UnconflictedBranchView -> Sqlite.Transaction PartialDeclNameLookup
                  get =
                    CodebaseOps.makeCachedTransaction branchPartialDeclNameLookupCache \k -> do
                      numConstructors <- getBranchDeclNumConstructors k
                      pure (lenientCheckDeclCoherency k.value.nametree numConstructors)
               in \namespaceHash unconflictedView -> get (Keyed namespaceHash unconflictedView)

        branchDeclNameLookupCache <- Cache.semispaceCache 10
        let getBranchDeclNameLookup ::
              BranchHash ->
              UnconflictedBranchView ->
              Sqlite.Transaction (Either IncoherentDeclReason DeclNameLookup)
            getBranchDeclNameLookup =
              let get :: Keyed BranchHash UnconflictedBranchView -> Sqlite.Transaction (Either IncoherentDeclReason DeclNameLookup)
                  get =
                    CodebaseOps.makeCachedTransaction branchDeclNameLookupCache \k -> do
                      numConstructors <- getBranchDeclNumConstructors k
                      pure (checkDeclCoherency k.value.nametree numConstructors)
               in \namespaceHash unconflictedView -> get (Keyed namespaceHash unconflictedView)

        let getTermComponentWithTypes :: Hash -> Sqlite.Transaction (Maybe [(Term Symbol Ann, Type Symbol Ann)])
            getTermComponentWithTypes =
              CodebaseOps.getTermComponentWithTypes getDeclType

            -- putTermComponent :: MonadIO m => Hash -> [(Term Symbol Ann, Type Symbol Ann)] -> m ()
            -- putTerms :: MonadIO m => Map Reference.Id (Term Symbol Ann, Type Symbol Ann) -> m () -- dies horribly if missing dependencies?

            -- option 1: tweak putTerm to incrementally notice the cycle length until each component is full
            -- option 2: switch codebase interface from putTerm to putTerms -- buffering can be local to the function
            -- option 3: switch from putTerm to putTermComponent -- needs to buffer dependencies non-locally (or require application to manage + die horribly)

            putTerm :: Reference.Id -> Term Symbol Ann -> Type Symbol Ann -> Sqlite.Transaction ()
            putTerm id tm tp | debug && trace ("SqliteCodebase.putTerm " ++ show id ++ " " ++ show tm ++ " " ++ show tp) False = undefined
            putTerm id tm tp =
              CodebaseOps.putTerm termBuffer declBuffer id tm tp

            putTermComponent :: Hash -> [(Term Symbol Ann, Type Symbol Ann)] -> Sqlite.Transaction ()
            putTermComponent =
              CodebaseOps.putTermComponent termBuffer declBuffer

            putTypeDeclaration :: Reference.Id -> Decl Symbol Ann -> Sqlite.Transaction ()
            putTypeDeclaration =
              CodebaseOps.putTypeDeclaration termBuffer declBuffer

            putTypeDeclarationComponent :: Hash -> [Decl Symbol Ann] -> Sqlite.Transaction ()
            putTypeDeclarationComponent =
              CodebaseOps.putTypeDeclarationComponent termBuffer declBuffer

            -- if this blows up on cromulent hashes, then switch from `hashToHashId`
            -- to one that returns Maybe.
            getBranchForHash :: CausalHash -> m (Maybe (Branch m))
            getBranchForHash hash =
              runTransaction (fmap (Branch.transform runTransaction) <$> (getBranchForHashTx hash))

            putBranch :: Branch m -> m ()
            putBranch branch =
              withRunInIO \runInIO ->
                runInIO do
                  Cache.insert rootBranchCache (Branch.headHash branch) branch
                  runTransaction (CodebaseOps.putBranch (Branch.transform (Sqlite.unsafeIO . runInIO) branch))

            preloadBranch :: CausalHash -> m ()
            preloadBranch h = do
              void . UnliftIO.forkIO $ void $ do
                getBranchForHash h >>= \case
                  Nothing -> pure ()
                  Just b -> do
                    UnliftIO.evaluate b
                    pure ()

            getWatch :: UF.WatchKind -> Reference.Id -> Sqlite.Transaction (Maybe (Term Symbol Ann))
            getWatch =
              CodebaseOps.getWatch getDeclType

            termsOfTypeImpl :: Reference -> Sqlite.Transaction (Set Referent.Id)
            termsOfTypeImpl =
              CodebaseOps.termsOfTypeImpl getDeclType

            filterTermsByReferentIdHavingTypeImpl :: Reference -> Set Referent.Id -> Sqlite.Transaction (Set Referent.Id)
            filterTermsByReferentIdHavingTypeImpl =
              CodebaseOps.filterReferentsHavingTypeImpl getDeclType

            filterTermsByReferenceIdHavingTypeImpl :: Reference -> Set TermReferenceId -> Sqlite.Transaction (Set TermReferenceId)
            filterTermsByReferenceIdHavingTypeImpl =
              CodebaseOps.filterReferencesHavingTypeImpl

            termsMentioningTypeImpl :: Reference -> Sqlite.Transaction (Set Referent.Id)
            termsMentioningTypeImpl =
              CodebaseOps.termsMentioningTypeImpl getDeclType

            referentsByPrefix :: ShortHash -> Sqlite.Transaction (Set Referent.Id)
            referentsByPrefix =
              CodebaseOps.referentsByPrefix getDeclType

        let codebase =
              C.Codebase
                { getTerm,
                  getTypeOfTermImpl,
                  getTypeDeclaration,
                  getDeclType,
                  expectDeclNumConstructors,
                  putTerm,
                  putTermComponent,
                  putTypeDeclaration,
                  putTypeDeclarationComponent,
                  getTermComponentWithTypes,
                  getBranchForHash,
                  getBranchForHashTx,
                  getBranchPartialDeclNameLookup,
                  getBranchDeclNameLookup,
                  putBranch,
                  getWatch,
                  termsOfTypeImpl,
                  termsMentioningTypeImpl,
                  filterTermsByReferenceIdHavingTypeImpl,
                  filterTermsByReferentIdHavingTypeImpl,
                  termReferentsByPrefix = referentsByPrefix,
                  withConnection = withConn,
                  withConnectionIO = withConnection debugName root,
                  preloadBranch
                }
        Right <$> action codebase
  where
    withConn :: (Sqlite.Connection -> m a) -> m a
    withConn =
      withConnection debugName root

    runTransaction :: Sqlite.Transaction a -> m a
    runTransaction action =
      withConn \conn -> Sqlite.runTransaction conn action

    handleLockOption ma = case lockOption of
      DontLock -> ma
      DoLock -> withRunInIO \runInIO ->
        withTryFileLock (lockfilePath root) Exclusive (\_flock -> runInIO ma) <&> \case
          Nothing -> Left OpenCodebaseFileLockFailed
          Just x -> x

data Entity m
  = B CausalHash (m (Branch m))
  | O Hash

instance Show (Entity m) where
  show (B h _) = "B " ++ take 10 (show h)
  show (O h) = "O " ++ take 10 (show h)

-- | Given two codebase roots (e.g. "./mycodebase"), safely copy the codebase
-- at the source to the destination.
-- Note: this does not copy the .unisonConfig file.
copyCodebase :: (MonadIO m) => CodebasePath -> CodebasePath -> m ()
copyCodebase src dest = liftIO $ do
  createDirectoryIfMissing True (makeCodebaseDirPath dest)
  withConnection ("copy-from:" <> src) src $ \srcConn -> do
    Sqlite.vacuumInto srcConn (makeCodebasePath dest)
  -- We need to reset the journal mode because vacuum-into clears it.
  withConnection ("copy-to:" <> dest) dest $ \destConn -> do
    Sqlite.trySetJournalMode destConn Sqlite.JournalMode'WAL

-- A `Keyed k v` is just a pair `(k, v)`, but where `k` implies `v` (i.e. it's a hash of `v` or similar), and so `k`
-- can be used as the key in a map or set without requiring `Eq` or `Ord` on `v`.
--
-- Motivating use case: a cache of `PartialDeclNameLookup`, keyed by namespace hash id.
data Keyed k v = Keyed
  { key :: k,
    value :: v
  }
  deriving stock (Generic)

instance (Eq k) => Eq (Keyed k v) where
  Keyed x _ == Keyed y _ = x == y

instance (Ord k) => Ord (Keyed k v) where
  Keyed x _ <= Keyed y _ = x <= y
