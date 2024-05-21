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

import Control.Monad.Except qualified as Except
import Control.Monad.Extra qualified as Monad
import Data.Char qualified as Char
import Data.Either.Extra ()
import Data.IORef
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import System.Console.ANSI qualified as ANSI
import System.FileLock (SharedExclusive (Exclusive), withTryFileLock)
import System.FilePath qualified as FilePath
import System.FilePath.Posix qualified as FilePath.Posix
import U.Codebase.HashTags (CausalHash, PatchHash (..))
import U.Codebase.Reflog qualified as Reflog
import U.Codebase.Sqlite.Operations qualified as Ops
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Sync22 qualified as Sync22
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import U.Codebase.Sync qualified as Sync
import Unison.Codebase (Codebase, CodebasePath)
import Unison.Codebase qualified as Codebase1
import Unison.Codebase.Branch (Branch (..))
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Git (gitIn, gitInCaptured, gitTextIn, withRepo)
import Unison.Codebase.Editor.Git qualified as Git
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadGitRepo,
    WriteGitRepo (..),
    writeToReadGit,
  )
import Unison.Codebase.GitError qualified as GitError
import Unison.Codebase.Init (BackupStrategy (..), CodebaseLockOption (..), MigrationStrategy (..), VacuumStrategy (..))
import Unison.Codebase.Init qualified as Codebase
import Unison.Codebase.Init.CreateCodebaseError qualified as Codebase1
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (..))
import Unison.Codebase.Init.OpenCodebaseError qualified as Codebase1
import Unison.Codebase.RootBranchCache
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Branch.Dependencies qualified as BD
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Codebase.SqliteCodebase.GitError qualified as GitError
import Unison.Codebase.SqliteCodebase.Migrations qualified as Migrations
import Unison.Codebase.SqliteCodebase.Operations qualified as CodebaseOps
import Unison.Codebase.SqliteCodebase.Paths
import Unison.Codebase.SqliteCodebase.SyncEphemeral qualified as SyncEphemeral
import Unison.Codebase.Type (GitPushBehavior, LocalOrRemote (..))
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
import Unison.Util.Timing (time)
import Unison.WatchKind qualified as UF
import UnliftIO (UnliftIO (..), finally, throwIO, try)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import UnliftIO.Exception (catch)
import UnliftIO.STM

debug, debugProcessBranches :: Bool
debug = False
debugProcessBranches = False

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

data CodebaseStatus
  = ExistingCodebase
  | CreatedCodebase
  deriving (Eq)

-- | Open the codebase at the given location, or create it if one doesn't already exist.
withOpenOrCreateCodebase ::
  (MonadUnliftIO m) =>
  Sqlite.Transaction () ->
  Codebase.DebugName ->
  CodebasePath ->
  LocalOrRemote ->
  CodebaseLockOption ->
  MigrationStrategy ->
  ((CodebaseStatus, Codebase m Symbol Ann) -> m r) ->
  m (Either Codebase1.OpenCodebaseError r)
withOpenOrCreateCodebase onCreate debugName codebasePath localOrRemote lockOption migrationStrategy action = do
  createCodebaseOrError onCreate debugName codebasePath lockOption (action' CreatedCodebase) >>= \case
    Left (Codebase1.CreateCodebaseAlreadyExists) -> do
      sqliteCodebase debugName codebasePath localOrRemote lockOption migrationStrategy (action' ExistingCodebase)
    Right r -> pure (Right r)
  where
    action' openOrCreate codebase = action (openOrCreate, codebase)

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

initSchemaIfNotExist :: (MonadIO m) => FilePath -> m ()
initSchemaIfNotExist path = liftIO do
  unlessM (doesDirectoryExist $ makeCodebaseDirPath path) $
    createDirectoryIfMissing True (makeCodebaseDirPath path)
  unlessM (doesFileExist $ makeCodebasePath path) $
    withConnection "initSchemaIfNotExist" path \conn ->
      Sqlite.runTransaction conn Q.createSchema

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

            syncFromDirectory :: Codebase1.CodebasePath -> Branch m -> m ()
            syncFromDirectory srcRoot b =
              withConnection (debugName ++ ".sync.src") srcRoot \srcConn ->
                withConn \destConn -> do
                  progressStateRef <- liftIO (newIORef emptySyncProgressState)
                  Sqlite.runReadOnlyTransaction srcConn \runSrc ->
                    Sqlite.runWriteTransaction destConn \runDest -> do
                      syncInternal (syncProgress progressStateRef) runSrc runDest b

            syncToDirectory :: Codebase1.CodebasePath -> Branch m -> m ()
            syncToDirectory destRoot b =
              withConn \srcConn ->
                withConnection (debugName ++ ".sync.dest") destRoot \destConn -> do
                  progressStateRef <- liftIO (newIORef emptySyncProgressState)
                  initSchemaIfNotExist destRoot
                  Sqlite.runReadOnlyTransaction srcConn \runSrc ->
                    Sqlite.runWriteTransaction destConn \runDest -> do
                      syncInternal (syncProgress progressStateRef) runSrc runDest b

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
                  syncFromDirectory,
                  syncToDirectory,
                  viewRemoteBranch',
                  pushGitBranch = \repo opts action -> withConn \conn -> pushGitBranch conn repo opts action,
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

syncInternal ::
  forall m.
  (MonadUnliftIO m) =>
  Sync.Progress m Sync22.Entity ->
  (forall a. Sqlite.Transaction a -> m a) ->
  (forall a. Sqlite.Transaction a -> m a) ->
  Branch m ->
  m ()
syncInternal progress runSrc runDest b = time "syncInternal" do
  UnliftIO runInIO <- askUnliftIO

  let syncEnv = Sync22.Env runSrc runDest (16 * 1024 * 1024)
  -- we want to use sync22 wherever possible
  -- so for each source branch, we'll check if it exists in the destination codebase
  -- or if it exists in the source codebase, then we can sync22 it
  -- if it doesn't exist in the dest or source branch,
  -- then just use putBranch to the dest
  sync <- liftIO (Sync22.sync22 v2HashHandle (Sync22.hoistEnv lift syncEnv))
  let doSync :: [Sync22.Entity] -> m ()
      doSync =
        throwExceptT
          . Except.withExceptT SyncEphemeral.Sync22Error
          . Sync.sync' sync (Sync.transformProgress lift progress)
  let processBranches :: [Entity m] -> m ()
      processBranches = \case
        [] -> pure ()
        b0@(B h mb) : rest -> do
          when debugProcessBranches do
            traceM $ "processBranches " ++ show b0
            traceM $ " queue: " ++ show rest
          ifM
            (runDest (CodebaseOps.branchExists h))
            do
              when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " already exists in dest db"
              processBranches rest
            do
              when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " doesn't exist in dest db"
              runSrc (Q.loadCausalHashIdByCausalHash h) >>= \case
                Just chId -> do
                  when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " exists in source db, so delegating to direct sync"
                  doSync [Sync22.C chId]
                  processBranches rest
                Nothing ->
                  mb >>= \b -> do
                    when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " doesn't exist in either db, so delegating to Codebase.putBranch"
                    let (branchDeps, BD.to' -> BD.Dependencies' es ts ds) = BD.fromBranch b
                    when debugProcessBranches do
                      traceM $ "  branchDeps: " ++ show (fst <$> branchDeps)
                      traceM $ "  terms: " ++ show ts
                      traceM $ "  decls: " ++ show ds
                      traceM $ "  edits: " ++ show es
                    (cs, es, ts, ds) <- runDest do
                      cs <- filterM (fmap not . CodebaseOps.branchExists . fst) branchDeps
                      es <- filterM (fmap not . CodebaseOps.patchExists) es
                      ts <- filterM (fmap not . CodebaseOps.termExists) ts
                      ds <- filterM (fmap not . CodebaseOps.declExists) ds
                      pure (cs, es, ts, ds)
                    if null cs && null es && null ts && null ds
                      then do
                        runDest (CodebaseOps.putBranch (Branch.transform (Sqlite.unsafeIO . runInIO) b))
                        processBranches rest
                      else do
                        let bs = map (uncurry B) cs
                            os = map O (coerce @[PatchHash] @[Hash] es <> ts <> ds)
                        processBranches (os ++ bs ++ b0 : rest)
        O h : rest -> do
          when debugProcessBranches $ traceM $ "processBranches O " ++ take 10 (show h)
          oId <- runSrc (Q.expectHashIdByHash h >>= Q.expectObjectIdForAnyHashId)
          doSync [Sync22.O oId]
          processBranches rest
  let bHash = Branch.headHash b
  time "SyncInternal.processBranches" $ processBranches [B bHash (pure b)]

data Entity m
  = B CausalHash (m (Branch m))
  | O Hash

instance Show (Entity m) where
  show (B h _) = "B " ++ take 10 (show h)
  show (O h) = "O " ++ take 10 (show h)

data SyncProgressState = SyncProgressState
  { _needEntities :: Maybe (Set Sync22.Entity),
    _doneEntities :: Either Int (Set Sync22.Entity),
    _warnEntities :: Either Int (Set Sync22.Entity)
  }

emptySyncProgressState :: SyncProgressState
emptySyncProgressState = SyncProgressState (Just mempty) (Right mempty) (Right mempty)

syncProgress :: forall m. (MonadIO m) => IORef SyncProgressState -> Sync.Progress m Sync22.Entity
syncProgress progressStateRef = Sync.Progress (liftIO . need) (liftIO . done) (liftIO . warn) (liftIO allDone)
  where
    quiet = False
    maxTrackedHashCount = 1024 * 1024
    size :: SyncProgressState -> Int
    size = \case
      SyncProgressState Nothing (Left i) (Left j) -> i + j
      SyncProgressState (Just need) (Right done) (Right warn) -> Set.size need + Set.size done + Set.size warn
      SyncProgressState _ _ _ -> undefined

    need, done, warn :: Sync22.Entity -> IO ()
    need h = do
      unless quiet $ Monad.whenM (readIORef progressStateRef <&> (== 0) . size) $ putStr "\n"
      readIORef progressStateRef >>= \case
        SyncProgressState Nothing Left {} Left {} -> pure ()
        SyncProgressState (Just need) (Right done) (Right warn) ->
          if Set.size need + Set.size done + Set.size warn > maxTrackedHashCount
            then writeIORef progressStateRef $ SyncProgressState Nothing (Left $ Set.size done) (Left $ Set.size warn)
            else
              if Set.member h done || Set.member h warn
                then pure ()
                else writeIORef progressStateRef $ SyncProgressState (Just $ Set.insert h need) (Right done) (Right warn)
        SyncProgressState _ _ _ -> undefined
      unless quiet printSynced

    done h = do
      unless quiet $ Monad.whenM (readIORef progressStateRef <&> (== 0) . size) $ putStr "\n"
      readIORef progressStateRef >>= \case
        SyncProgressState Nothing (Left done) warn ->
          writeIORef progressStateRef $ SyncProgressState Nothing (Left (done + 1)) warn
        SyncProgressState (Just need) (Right done) warn ->
          writeIORef progressStateRef $ SyncProgressState (Just $ Set.delete h need) (Right $ Set.insert h done) warn
        SyncProgressState _ _ _ -> undefined
      unless quiet printSynced

    warn h = do
      unless quiet $ Monad.whenM (readIORef progressStateRef <&> (== 0) . size) $ putStr "\n"
      readIORef progressStateRef >>= \case
        SyncProgressState Nothing done (Left warn) ->
          writeIORef progressStateRef $ SyncProgressState Nothing done (Left $ warn + 1)
        SyncProgressState (Just need) done (Right warn) ->
          writeIORef progressStateRef $ SyncProgressState (Just $ Set.delete h need) done (Right $ Set.insert h warn)
        SyncProgressState _ _ _ -> undefined
      unless quiet printSynced

    allDone = do
      readIORef progressStateRef >>= putStrLn . renderState ("  " ++ "Done syncing ")

    printSynced :: IO ()
    printSynced =
      readIORef progressStateRef >>= \s ->
        finally
          do ANSI.hideCursor; putStr . renderState ("  " ++ "Synced ") $ s
          ANSI.showCursor

    renderState :: String -> SyncProgressState -> String
    renderState prefix = \case
      SyncProgressState Nothing (Left done) (Left warn) ->
        "\r" ++ prefix ++ show done ++ " entities" ++ if warn > 0 then " with " ++ show warn ++ " warnings." else "."
      SyncProgressState (Just _need) (Right done) (Right warn) ->
        "\r"
          ++ prefix
          ++ show (Set.size done + Set.size warn)
          ++ " entities"
          ++ if Set.size warn > 0
            then " with " ++ show (Set.size warn) ++ " warnings."
            else "."
      SyncProgressState need done warn ->
        "invalid SyncProgressState "
          ++ show (fmap v need, bimap id v done, bimap id v warn)
      where
        v = const ()

-- FIXME(mitchell) seems like this should have "git" in its name
viewRemoteBranch' ::
  forall m r.
  (MonadUnliftIO m) =>
  ReadGitRemoteNamespace ->
  Git.GitBranchBehavior ->
  ((Branch m, CodebasePath) -> m r) ->
  m (Either C.GitError r)
viewRemoteBranch' ReadGitRemoteNamespace {repo, sch, path} gitBranchBehavior action = UnliftIO.try $ do
  -- set up the cache dir
  time "Git fetch" $
    throwEitherMWith C.GitProtocolError . withRepo repo gitBranchBehavior $ \remoteRepo -> do
      let remotePath = Git.gitDirToPath remoteRepo
          -- In modern UCM all new codebases are created in WAL mode, but it's possible old
          -- codebases were pushed to git in DELETE mode, so when pulling remote branches we
          -- ensure we're in WAL mode just to be safe.
          ensureWALMode conn = Sqlite.trySetJournalMode conn Sqlite.JournalMode'WAL
      -- Tickle the database before calling into `sqliteCodebase`; this covers the case that the database file either
      -- doesn't exist at all or isn't a SQLite database file, but does not cover the case that the database file itself
      -- is somehow corrupt, or not even a Unison database.
      --
      -- FIXME it would probably make more sense to define some proper preconditions on `sqliteCodebase`, and perhaps
      -- update its output type, which currently indicates the only way it can fail is with an `UnknownSchemaVersion`
      -- error.
      (withConnection "codebase exists check" remotePath ensureWALMode) `catch` \exception ->
        if Sqlite.isCantOpenException exception
          then throwIO (C.GitSqliteCodebaseError (GitError.NoDatabaseFile repo remotePath))
          else throwIO exception

      result <- sqliteCodebase "viewRemoteBranch.gitCache" remotePath Remote DoLock (MigrateAfterPrompt Codebase.Backup Codebase.Vacuum) \codebase -> do
        -- try to load the requested branch from it
        branch <- time "Git fetch (sch)" $ case sch of
          -- no sub-branch was specified, so use the root.
          Nothing -> time "Get remote root branch" $ Codebase1.getRootBranch codebase
          -- load from a specific `ShortCausalHash`
          Just sch -> do
            branchCompletions <- Codebase1.runTransaction codebase (Codebase1.causalHashesByPrefix sch)
            case toList branchCompletions of
              [] -> throwIO . C.GitCodebaseError $ GitError.NoRemoteNamespaceWithHash repo sch
              [h] ->
                (Codebase1.getBranchForHash codebase h) >>= \case
                  Just b -> pure b
                  Nothing -> throwIO . C.GitCodebaseError $ GitError.NoRemoteNamespaceWithHash repo sch
              _ -> throwIO . C.GitCodebaseError $ GitError.RemoteNamespaceHashAmbiguous repo sch branchCompletions
        case Branch.getAt path branch of
          Just b -> action (b, remotePath)
          Nothing -> throwIO . C.GitCodebaseError $ GitError.CouldntFindRemoteBranch repo path
      case result of
        Left err -> throwIO . C.GitSqliteCodebaseError $ C.gitErrorFromOpenCodebaseError remotePath repo err
        Right inner -> pure inner

-- | Push a branch to a repo. Optionally attempt to set the branch as the new root, which fails if the branch is not after
-- the existing root.
pushGitBranch ::
  forall m e.
  (MonadUnliftIO m) =>
  Sqlite.Connection ->
  WriteGitRepo ->
  GitPushBehavior ->
  -- An action which accepts the current root branch on the remote and computes a new branch.
  (Branch m -> m (Either e (Branch m))) ->
  m (Either C.GitError (Either e (Branch m)))
pushGitBranch srcConn repo behavior action = UnliftIO.try do
  -- Pull the latest remote into our git cache
  -- Use a local git clone to copy this git repo into a temp-dir
  -- Delete the codebase in our temp-dir
  -- Use sqlite's VACUUM INTO command to make a copy of the remote codebase into our temp-dir
  -- Connect to the copied codebase and sync whatever it is we want to push.
  -- sync the branch to the staging codebase using `syncInternal`, which probably needs to be passed in instead of `syncToDirectory`
  -- if setting the remote root,
  --   do a `before` check on the staging codebase
  --   if it passes, proceed (see below)
  --   if it fails, throw an exception (which will rollback) and clean up.
  -- push from the temp-dir to the remote.
  -- Delete the temp-dir.
  --
  -- set up the cache dir
  throwEitherMWith C.GitProtocolError . withRepo readRepo Git.CreateBranchIfMissing $ \pushStaging -> do
    newBranchOrErr <- throwEitherMWith (C.GitSqliteCodebaseError . C.gitErrorFromOpenCodebaseError (Git.gitDirToPath pushStaging) readRepo)
      . withOpenOrCreateCodebase (pure ()) "push.dest" (Git.gitDirToPath pushStaging) Remote DoLock (MigrateAfterPrompt Codebase.Backup Codebase.Vacuum)
      $ \(codebaseStatus, destCodebase) -> do
        currentRootBranch <-
          Codebase1.runTransaction destCodebase CodebaseOps.getRootBranchExists >>= \case
            False -> pure Branch.empty
            True -> C.getRootBranch destCodebase
        action currentRootBranch >>= \case
          Left e -> pure $ Left e
          Right newBranch -> do
            C.withConnection destCodebase \destConn ->
              doSync codebaseStatus destConn newBranch
            pure (Right newBranch)
    for_ newBranchOrErr $ push pushStaging repo
    pure newBranchOrErr
  where
    readRepo :: ReadGitRepo
    readRepo = writeToReadGit repo
    doSync :: CodebaseStatus -> Sqlite.Connection -> Branch m -> m ()
    doSync codebaseStatus destConn newBranch = do
      progressStateRef <- liftIO (newIORef emptySyncProgressState)
      Sqlite.runReadOnlyTransaction srcConn \runSrc -> do
        Sqlite.runWriteTransaction destConn \runDest -> do
          _ <- syncInternal (syncProgress progressStateRef) runSrc runDest newBranch
          let overwriteRoot forcePush = do
                let newBranchHash = Branch.headHash newBranch
                case codebaseStatus of
                  ExistingCodebase -> do
                    when (not forcePush) do
                      -- the call to runDB "handles" the possible DB error by bombing
                      runDest Ops.loadRootCausalHash >>= \case
                        Nothing -> pure ()
                        Just oldRootHash -> do
                          runDest (CodebaseOps.before oldRootHash newBranchHash) >>= \case
                            False -> throwIO . C.GitProtocolError $ GitError.PushDestinationHasNewStuff repo
                            True -> pure ()
                  CreatedCodebase -> pure ()
                runDest (setRepoRoot newBranchHash)
          case behavior of
            C.GitPushBehaviorGist -> pure ()
            C.GitPushBehaviorFf -> overwriteRoot False
            C.GitPushBehaviorForce -> overwriteRoot True
    setRepoRoot :: CausalHash -> Sqlite.Transaction ()
    setRepoRoot h = do
      let err = error $ "Called SqliteCodebase.setNamespaceRoot on unknown causal hash " ++ show h
      chId <- fromMaybe err <$> Q.loadCausalHashIdByCausalHash h
      Q.setNamespaceRoot chId

    -- This function makes sure that the result of git status is valid.
    -- Valid lines are any of:
    --
    --   ?? .unison/v2/unison.sqlite3 (initial commit to an empty repo)
    --   M .unison/v2/unison.sqlite3  (updating an existing repo)
    --   D .unison/v2/unison.sqlite3-wal (cleaning up the WAL from before bugfix)
    --   D .unison/v2/unison.sqlite3-shm (ditto)
    --
    -- Invalid lines are like:
    --
    --   ?? .unison/v2/unison.sqlite3-wal
    --
    -- Which will only happen if the write-ahead log hasn't been
    -- fully folded into the unison.sqlite3 file.
    --
    -- Returns `Just (hasDeleteWal, hasDeleteShm)` on success,
    -- `Nothing` otherwise. hasDeleteWal means there's the line:
    --   D .unison/v2/unison.sqlite3-wal
    -- and hasDeleteShm is `True` if there's the line:
    --   D .unison/v2/unison.sqlite3-shm
    --
    parseStatus :: Text -> Maybe (Bool, Bool)
    parseStatus status =
      if all okLine statusLines
        then Just (hasDeleteWal, hasDeleteShm)
        else Nothing
      where
        -- `git status` always displays paths using posix forward-slashes,
        -- so we have to convert our expected path to test.
        posixCodebasePath =
          FilePath.Posix.joinPath (FilePath.splitDirectories codebasePath)
        posixLockfilePath = FilePath.replaceExtension posixCodebasePath "lockfile"
        statusLines = Text.unpack <$> Text.lines status
        t = dropWhile Char.isSpace
        okLine (t -> '?' : '?' : (t -> p)) | p == posixCodebasePath || p == posixLockfilePath = True
        okLine (t -> 'M' : (t -> p)) | p == posixCodebasePath = True
        okLine line = isWalDelete line || isShmDelete line
        isWalDelete (t -> 'D' : (t -> p)) | p == posixCodebasePath ++ "-wal" = True
        isWalDelete _ = False
        isShmDelete (t -> 'D' : (t -> p)) | p == posixCodebasePath ++ "-wal" = True
        isShmDelete _ = False
        hasDeleteWal = any isWalDelete statusLines
        hasDeleteShm = any isShmDelete statusLines

    -- Commit our changes
    push :: forall n. (MonadIO n) => Git.GitRepo -> WriteGitRepo -> Branch m -> n Bool -- withIOError needs IO
    push remotePath repo@(WriteGitRepo {url, branch = mayGitBranch}) newRootBranch = time "SqliteCodebase.pushGitRootBranch.push" $ do
      -- has anything changed?
      -- note: -uall recursively shows status for all files in untracked directories
      --   we want this so that we see
      --     `??  .unison/v2/unison.sqlite3` and not
      --     `??  .unison/`
      status <- gitTextIn remotePath ["status", "--short", "-uall"]
      if Text.null status
        then pure False
        else case parseStatus status of
          Nothing ->
            error $
              "An error occurred during push.\n"
                <> "I was expecting only to see "
                <> codebasePath
                <> " modified, but saw:\n\n"
                <> Text.unpack status
                <> "\n\n"
                <> "Please visit https://github.com/unisonweb/unison/issues/2063\n"
                <> "and add any more details about how you encountered this!\n"
          Just (hasDeleteWal, hasDeleteShm) -> do
            -- Only stage files we're expecting; don't `git add --all .`
            -- which could accidentally commit some garbage
            gitIn remotePath ["add", Text.pack codebasePath]
            when hasDeleteWal $ gitIn remotePath ["rm", Text.pack $ codebasePath <> "-wal"]
            when hasDeleteShm $ gitIn remotePath ["rm", Text.pack $ codebasePath <> "-shm"]
            gitIn
              remotePath
              ["commit", "-q", "-m", "Sync branch " <> Text.pack (show $ Branch.headHash newRootBranch)]
            -- Push our changes to the repo, silencing all output.
            -- Even with quiet, the remote (Github) can still send output through,
            -- so we capture stdout and stderr.
            (successful, _stdout, stderr) <- gitInCaptured remotePath $ ["push", url] ++ Git.gitVerbosity ++ maybe [] (pure @[]) mayGitBranch
            when (not successful) . throwIO $ GitError.PushException repo (Text.unpack stderr)
            pure True

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
