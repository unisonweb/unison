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
import Data.Map qualified as Map
import Data.Time (getCurrentTime)
import System.FileLock (SharedExclusive (Exclusive), withTryFileLock)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Reflog qualified as Reflog
import U.Codebase.Sqlite.Operations qualified as Ops
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Codebase (Codebase, CodebasePath)
import Unison.Codebase qualified as Codebase1
import Unison.Codebase.Branch (Branch (..))
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Init (BackupStrategy (..), CodebaseLockOption (..), MigrationStrategy (..), VacuumStrategy (..))
import Unison.Codebase.Init qualified as Codebase
import Unison.Codebase.Init.CreateCodebaseError qualified as Codebase1
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (..))
import Unison.Codebase.Init.OpenCodebaseError qualified as Codebase1
import Unison.Codebase.RootBranchCache
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Codebase.SqliteCodebase.Migrations qualified as Migrations
import Unison.Codebase.SqliteCodebase.Operations qualified as CodebaseOps
import Unison.Codebase.SqliteCodebase.Paths
import Unison.Codebase.Type (LocalOrRemote (..))
import Unison.Codebase.Type qualified as C
import Unison.DataDeclaration (Decl)
import Unison.Hash (Hash)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (Reference, TermReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.WatchKind qualified as UF
import UnliftIO (finally)
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
          Q.createSchema
          void . Ops.saveRootBranch v2HashHandle $ Cv.causalbranch1to2 Branch.empty
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
  rootBranchCache <- newEmptyRootBranchCacheIO
  branchCache <- newBranchCache
  getDeclType <- CodebaseOps.makeCachedTransaction 2048 CodebaseOps.getDeclType
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
        getTerm <- CodebaseOps.makeMaybeCachedTransaction 8192 (CodebaseOps.getTerm getDeclType)
        getTypeOfTermImpl <- CodebaseOps.makeMaybeCachedTransaction 8192 (CodebaseOps.getTypeOfTermImpl)
        getTypeDeclaration <- CodebaseOps.makeMaybeCachedTransaction 1024 CodebaseOps.getTypeDeclaration

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

            getRootBranch :: m (Branch m)
            getRootBranch =
              Branch.transform runTransaction
                <$> fetchRootBranch
                  rootBranchCache
                  (runTransaction (CodebaseOps.uncachedLoadRootBranch branchCache getDeclType))

            putRootBranch :: Text -> Branch m -> m ()
            putRootBranch reason branch1 = do
              now <- liftIO getCurrentTime
              withRunInIO \runInIO -> do
                -- this is naughty, the type says Transaction but it
                -- won't run automatically with whatever Transaction
                -- it is composed into unless the enclosing
                -- Transaction is applied to the same db connection.
                let branch1Trans = Branch.transform (Sqlite.unsafeIO . runInIO) branch1
                    putRootBranchTrans :: Sqlite.Transaction () = do
                      let emptyCausalHash = Branch.headHash Branch.empty
                      fromRootCausalHash <- fromMaybe emptyCausalHash <$> Ops.loadRootCausalHash
                      let toRootCausalHash = Branch.headHash branch1
                      CodebaseOps.putRootBranch branch1Trans
                      Ops.appendReflog (Reflog.Entry {time = now, fromRootCausalHash, toRootCausalHash, reason})

                -- We need to update the database and the cached
                -- value. We want to keep these in sync, so we take
                -- the cache lock while updating sqlite.
                withLock
                  rootBranchCache
                  (\restore _ -> restore $ runInIO $ runTransaction putRootBranchTrans)
                  (\_ -> Just branch1Trans)

            -- if this blows up on cromulent hashes, then switch from `hashToHashId`
            -- to one that returns Maybe.
            getBranchForHash :: CausalHash -> m (Maybe (Branch m))
            getBranchForHash h =
              fmap (Branch.transform runTransaction) <$> runTransaction (CodebaseOps.getBranchForHash branchCache getDeclType h)

            putBranch :: Branch m -> m ()
            putBranch branch =
              withRunInIO \runInIO ->
                runInIO (runTransaction (CodebaseOps.putBranch (Branch.transform (Sqlite.unsafeIO . runInIO) branch)))

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
                  putTerm,
                  putTermComponent,
                  putTypeDeclaration,
                  putTypeDeclarationComponent,
                  getTermComponentWithTypes,
                  getRootBranch,
                  putRootBranch,
                  getBranchForHash,
                  putBranch,
                  getWatch,
                  termsOfTypeImpl,
                  termsMentioningTypeImpl,
                  filterTermsByReferenceIdHavingTypeImpl,
                  filterTermsByReferentIdHavingTypeImpl,
                  termReferentsByPrefix = referentsByPrefix,
                  withConnection = withConn,
                  withConnectionIO = withConnection debugName root
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
