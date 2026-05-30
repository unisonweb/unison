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

import Data.Char qualified as Char
import Data.Either.Extra ()
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import System.FileLock (SharedExclusive (Exclusive), withFileLock, withTryFileLock)
import U.Codebase.HashTags (BranchHash, CausalHash)
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Codebase (Codebase, CodebasePath)
import Unison.Codebase qualified as Codebase1
import Unison.Codebase.Branch (Branch (..))
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
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration (Decl)
import Unison.DeclCoherencyCheck (IncoherentDeclReasons, checkAllDeclCoherency, lenientCheckDeclCoherency)
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.Hash (Hash)
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.Prelude
import Unison.Reference (Reference, Reference' (..), TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnconflictedLocalDefnsView (UnconflictedLocalDefnsView (..))
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Cache qualified as Cache
import Unison.Util.Defns (Defns (..))
import UnliftIO (finally)
import UnliftIO qualified as UnliftIO
import UnliftIO.Concurrent qualified as UnliftIO
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)
import UnliftIO.Environment (lookupEnv)
import UnliftIO.STM

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

  branchDeclNameLookupCache <- Cache.semispaceCache 10
  branchDeclNumConstructorsCache <- Cache.semispaceCache 10
  branchPartialDeclNameLookupCache <- Cache.semispaceCache 10
  declComponentCache <- Cache.semispaceCache 8192
  declNumConstructorsCache <- Cache.semispaceCache 8192
  declTypeCache <- Cache.semispaceCache 2048
  termComponentWithTypesCache <- Cache.semispaceCache 8192

  let getDeclType = CodebaseOps.makeCachedTransaction declTypeCache CodebaseOps.getDeclType

  -- The v1 codebase interface has operations to read and write individual definitions
  -- whereas the v2 codebase writes them as complete components.  These two fields buffer
  -- the individual definitions until a complete component has been written.
  termBuffer :: TVar (Map Hash CodebaseOps.TermBufferEntry) <- newTVarIO Map.empty
  declBuffer :: TVar (Map Hash CodebaseOps.DeclBufferEntry) <- newTVarIO Map.empty

  ensureMigrated debugName root localOrRemote migrationStrategy getDeclType termBuffer declBuffer >>= \case
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
        let expectDeclNumConstructors :: TypeReferenceId -> Sqlite.Transaction Int
            expectDeclNumConstructors =
              CodebaseOps.makeCachedTransaction declNumConstructorsCache Operations.expectDeclNumConstructors

        let getBranchForHashTx :: CausalHash -> Sqlite.Transaction (Maybe (Branch Sqlite.Transaction))
            getBranchForHashTx =
              CodebaseOps.makeMaybeCachedTransaction rootBranchCacheTx (CodebaseOps.getBranchForHash branchLoadCache getDeclType)

        let getBranchDeclNumConstructors0 ::
              Keyed BranchHash (Set TypeReference) ->
              Sqlite.Transaction (Map TypeReferenceId Int)
            getBranchDeclNumConstructors0 =
              CodebaseOps.makeCachedTransaction branchDeclNumConstructorsCache \k ->
                k.value
                  & Set.toList
                  & Foldable.foldlM
                    ( \acc -> \case
                        ReferenceBuiltin _ -> pure acc
                        ReferenceDerived ref -> do
                          num <- expectDeclNumConstructors ref
                          pure $! Map.insert ref num acc
                    )
                    Map.empty

        let getBranchForHash :: CausalHash -> m (Maybe (Branch m))
            getBranchForHash hash =
              runTransaction (fmap (Branch.transform runTransaction) <$> (getBranchForHashTx hash))

            putBranch :: Branch m -> m ()
            putBranch branch =
              withRunInIO \runInIO ->
                runInIO do
                  Cache.insert rootBranchCache (Branch.headHash branch) branch
                  runTransaction (CodebaseOps.putBranch (Branch.transform (Sqlite.unsafeIO . runInIO) branch))

            preloadBranch :: CausalHash -> m ()
            preloadBranch hash = do
              void . UnliftIO.forkIO $ void $ do
                getBranchForHash hash >>= \case
                  Nothing -> pure ()
                  Just b -> do
                    UnliftIO.evaluate b
                    pure ()

        let getTermComponentWithTypes :: Hash -> Sqlite.Transaction (Maybe [(Term Symbol Ann, Type Symbol Ann)])
            getTermComponentWithTypes =
              CodebaseOps.makeMaybeCachedTransaction
                termComponentWithTypesCache
                (CodebaseOps.getTermComponentWithTypes getDeclType)

        let getTypeDeclarationComponent :: Hash -> Sqlite.Transaction (Maybe [Decl Symbol Ann])
            getTypeDeclarationComponent =
              CodebaseOps.makeMaybeCachedTransaction
                declComponentCache
                CodebaseOps.getDeclComponent

        let codebase =
              C.Codebase
                { getTerm =
                    \(Reference.Id hash pos) -> do
                      getTermComponentWithTypes hash <&> \case
                        Just component -> Just (fst (Reference.getComponentElem component pos))
                        Nothing -> Nothing,
                  getTypeOfTermImpl =
                    \(Reference.Id hash pos) -> do
                      getTermComponentWithTypes hash <&> \case
                        Just component -> Just (snd (Reference.getComponentElem component pos))
                        Nothing -> Nothing,
                  getTypeDeclaration =
                    \(Reference.Id hash pos) -> do
                      getTypeDeclarationComponent hash <&> \case
                        Just component -> Just (Reference.getComponentElem component pos)
                        Nothing -> Nothing,
                  getTypeDeclarationComponent,
                  getTypeAlias = CodebaseOps.getTypeAlias,
                  getTypeEntry = CodebaseOps.getTypeEntry,
                  isTypeAlias = CodebaseOps.isTypeAlias,
                  getDeclType,
                  expectDeclNumConstructors,
                  putTerm = CodebaseOps.putTerm termBuffer declBuffer,
                  putTermComponent = CodebaseOps.putTermComponent termBuffer declBuffer,
                  putTypeDeclaration = CodebaseOps.putTypeDeclaration termBuffer declBuffer,
                  putTypeDeclarationComponent = CodebaseOps.putTypeDeclarationComponent termBuffer declBuffer,
                  putTypeAlias = CodebaseOps.putTypeAlias,
                  getTermComponentWithTypes,
                  getBranchForHash,
                  getBranchForHashTx,
                  getBranchDeclNumConstructors =
                    \namespaceHash refs -> getBranchDeclNumConstructors0 (Keyed namespaceHash refs),
                  getBranchPartialDeclNameLookup =
                    let get :: Keyed BranchHash UnconflictedLocalDefnsView -> Sqlite.Transaction PartialDeclNameLookup
                        get =
                          CodebaseOps.makeCachedTransaction branchPartialDeclNameLookupCache \k -> do
                            numConstructors <-
                              getBranchDeclNumConstructors0
                                (Keyed k.key (BiMultimap.dom k.value.defns.types))
                            pure (lenientCheckDeclCoherency k.value.nametree numConstructors)
                     in \namespaceHash unconflictedView -> get (Keyed namespaceHash unconflictedView),
                  getBranchDeclNameLookup =
                    let get ::
                          Keyed BranchHash UnconflictedLocalDefnsView ->
                          Sqlite.Transaction (Either IncoherentDeclReasons DeclNameLookup)
                        get =
                          CodebaseOps.makeCachedTransaction branchDeclNameLookupCache \k -> do
                            numConstructors <-
                              getBranchDeclNumConstructors0
                                (Keyed k.key (BiMultimap.dom k.value.defns.types))
                            pure (checkAllDeclCoherency k.value.nametree numConstructors)
                     in \namespaceHash unconflictedView -> get (Keyed namespaceHash unconflictedView),
                  putBranch,
                  putBranchTx = \branch -> do
                    Sqlite.unsafeIO (Cache.insert rootBranchCacheTx (Branch.headHash branch) branch)
                    CodebaseOps.putBranch branch,
                  getWatch = CodebaseOps.getWatch getDeclType,
                  termsOfTypeImpl = CodebaseOps.termsOfTypeImpl getDeclType,
                  termsMentioningTypeImpl = CodebaseOps.termsMentioningTypeImpl getDeclType,
                  filterTermsByReferenceIdHavingTypeImpl = CodebaseOps.filterReferencesHavingTypeImpl,
                  filterTermsByReferentIdHavingTypeImpl = CodebaseOps.filterReferentsHavingTypeImpl getDeclType,
                  termReferentsByPrefix = CodebaseOps.referentsByPrefix getDeclType,
                  withConnection = withConnection debugName root,
                  withConnectionIO = withConnection debugName root,
                  preloadBranch
                }
        Right <$> action codebase
  where
    runTransaction :: Sqlite.Transaction a -> m a
    runTransaction action =
      withConnection debugName root \conn -> Sqlite.runTransaction conn action

    handleLockOption ma = case lockOption of
      DontLock -> ma
      DoLock -> withRunInIO \runInIO ->
        withTryFileLock (lockfilePath root) Exclusive (\_flock -> runInIO ma) <&> \case
          Nothing -> Left OpenCodebaseFileLockFailed
          Just x -> x
      BlockUntilLock -> withRunInIO \runInIO ->
        withTryFileLock (lockfilePath root) Exclusive (\_flock -> runInIO ma) >>= \case
          Nothing -> do
            liftIO (putStrLn "Waiting for codebase lock...")
            withFileLock (lockfilePath root) Exclusive (\_flock -> runInIO ma)
          Just x -> pure x

ensureMigrated ::
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  LocalOrRemote ->
  MigrationStrategy ->
  (Reference -> Sqlite.Transaction ConstructorType) ->
  TVar (Map Hash CodebaseOps.TermBufferEntry) ->
  TVar (Map Hash CodebaseOps.DeclBufferEntry) ->
  m (Either OpenCodebaseError ())
ensureMigrated debugName root localOrRemote migrationStrategy getDeclType termBuffer declBuffer = do
  withConnection debugName root \conn -> do
    Sqlite.runTransaction conn Migrations.checkCodebaseIsUpToDate >>= \case
      Migrations.CodebaseUpToDate -> pure $ Right ()
      Migrations.CodebaseUnknownSchemaVersion sv -> pure $ Left (OpenCodebaseUnknownSchemaVersion sv)
      Migrations.CodebaseRequiresMigration fromSv toSv ->
        case migrationStrategy of
          DontMigrate -> pure $ Left (OpenCodebaseRequiresMigration fromSv toSv)
          MigrateAfterPrompt backupStrategy vacuumStrategy -> do
            shouldPrompt <-
              lookupEnv "UNISON_MIGRATION" >>= \case
                Just (fmap Char.toLower -> "auto") -> pure False
                _ -> pure True
            doMigrate shouldPrompt backupStrategy vacuumStrategy
          MigrateAutomatically backupStrategy vacuumStrategy -> doMigrate False backupStrategy vacuumStrategy
        where
          doMigrate shouldPrompt backupStrategy vacuumStrategy =
            Migrations.ensureCodebaseIsUpToDate
              localOrRemote
              root
              getDeclType
              termBuffer
              declBuffer
              shouldPrompt
              backupStrategy
              vacuumStrategy
              conn

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
