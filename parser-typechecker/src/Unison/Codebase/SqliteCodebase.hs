{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.SqliteCodebase
  ( Unison.Codebase.SqliteCodebase.init,
    MigrationStrategy (..),
  )
where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Extra as Monad
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Char as Char
import Data.Either.Extra ()
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import qualified System.Console.ANSI as ANSI
import qualified System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import qualified U.Codebase.Branch as V2Branch
import U.Codebase.HashTags (BranchHash, CausalHash (CausalHash))
import qualified U.Codebase.Reflog as Reflog
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Sync22 as Sync22
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import qualified U.Codebase.Sync as Sync
import qualified U.Util.Cache as Cache
import U.Util.Timing (time)
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase1
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal.Type as Causal
import Unison.Codebase.Editor.Git (gitIn, gitInCaptured, gitTextIn, withRepo)
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadGitRepo,
    WriteGitRepo (..),
    printWriteGitRepo,
    writeToReadGit,
  )
import qualified Unison.Codebase.GitError as GitError
import Unison.Codebase.Init (MigrationStrategy (..))
import qualified Unison.Codebase.Init as Codebase
import qualified Unison.Codebase.Init.CreateCodebaseError as Codebase1
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (..))
import qualified Unison.Codebase.Init.OpenCodebaseError as Codebase1
import Unison.Codebase.Path (Path)
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import qualified Unison.Codebase.SqliteCodebase.Branch.Dependencies as BD
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.Codebase.SqliteCodebase.GitError as GitError
import qualified Unison.Codebase.SqliteCodebase.Migrations as Migrations
import qualified Unison.Codebase.SqliteCodebase.Operations as CodebaseOps
import Unison.Codebase.SqliteCodebase.Paths
import qualified Unison.Codebase.SqliteCodebase.SyncEphemeral as SyncEphemeral
import Unison.Codebase.SyncMode (SyncMode)
import Unison.Codebase.Type (LocalOrRemote (..), PushGitBranchOpts (..))
import qualified Unison.Codebase.Type as C
import Unison.DataDeclaration (Decl)
import Unison.Hash (Hash)
import Unison.Names.Scoped (ScopedNames)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sqlite.Transaction as Sqlite.Transaction
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.WatchKind as UF
import UnliftIO (UnliftIO (..), finally, throwIO, try)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import UnliftIO.Exception (catch)
import UnliftIO.STM

debug, debugProcessBranches :: Bool
debug = False
debugProcessBranches = False

init :: HasCallStack => (MonadUnliftIO m) => Codebase.Init m Symbol Ann
init =
  Codebase.Init
    { withOpenCodebase = withCodebaseOrError,
      withCreatedCodebase = createCodebaseOrError,
      codebasePath = makeCodebaseDirPath
    }

data CodebaseStatus
  = ExistingCodebase
  | CreatedCodebase
  deriving (Eq)

-- | Open the codebase at the given location, or create it if one doesn't already exist.
withOpenOrCreateCodebase ::
  MonadUnliftIO m =>
  Codebase.DebugName ->
  CodebasePath ->
  LocalOrRemote ->
  MigrationStrategy ->
  ((CodebaseStatus, Codebase m Symbol Ann) -> m r) ->
  m (Either Codebase1.OpenCodebaseError r)
withOpenOrCreateCodebase debugName codebasePath localOrRemote migrationStrategy action = do
  createCodebaseOrError debugName codebasePath (action' CreatedCodebase) >>= \case
    Left (Codebase1.CreateCodebaseAlreadyExists) -> do
      sqliteCodebase debugName codebasePath localOrRemote migrationStrategy (action' ExistingCodebase)
    Right r -> pure (Right r)
  where
    action' openOrCreate codebase = action (openOrCreate, codebase)

-- | Create a codebase at the given location.
createCodebaseOrError ::
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either Codebase1.CreateCodebaseError r)
createCodebaseOrError debugName path action = do
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

      sqliteCodebase debugName path Local DontMigrate action >>= \case
        Left schemaVersion -> error ("Failed to open codebase with schema version: " ++ show schemaVersion ++ ", which is unexpected because I just created this codebase.")
        Right result -> pure (Right result)

-- | Use the codebase in the provided path.
-- The codebase is automatically closed when the action completes or throws an exception.
withCodebaseOrError ::
  forall m r.
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  MigrationStrategy ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either Codebase1.OpenCodebaseError r)
withCodebaseOrError debugName dir migrationStrategy action = do
  doesFileExist (makeCodebasePath dir) >>= \case
    False -> pure (Left Codebase1.OpenCodebaseDoesntExist)
    True -> sqliteCodebase debugName dir Local migrationStrategy action

initSchemaIfNotExist :: MonadIO m => FilePath -> m ()
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
  MonadUnliftIO m =>
  Codebase.DebugName ->
  CodebasePath ->
  (Sqlite.Connection -> m a) ->
  m a
withConnection name root action =
  Sqlite.withConnection name (makeCodebasePath root) action

sqliteCodebase ::
  forall m r.
  MonadUnliftIO m =>
  Codebase.DebugName ->
  CodebasePath ->
  -- | When local, back up the existing codebase before migrating, in case there's a catastrophic bug in the migration.
  LocalOrRemote ->
  MigrationStrategy ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either Codebase1.OpenCodebaseError r)
sqliteCodebase debugName root localOrRemote migrationStrategy action = do
  termCache <- Cache.semispaceCache 8192 -- pure Cache.nullCache -- to disable
  typeOfTermCache <- Cache.semispaceCache 8192
  declCache <- Cache.semispaceCache 1024
  rootBranchCache <- newTVarIO Nothing
  branchCache <- newBranchCache
  getDeclType <- CodebaseOps.mkGetDeclType
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
          MigrateAfterPrompt -> do
            let shouldPrompt = True
            Migrations.ensureCodebaseIsUpToDate localOrRemote root getDeclType termBuffer declBuffer shouldPrompt conn
          MigrateAutomatically -> do
            let shouldPrompt = False
            Migrations.ensureCodebaseIsUpToDate localOrRemote root getDeclType termBuffer declBuffer shouldPrompt conn

  case result of
    Left err -> pure $ Left err
    Right () -> do
      let finalizer :: MonadIO m => m ()
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
        let getTerm :: Reference.Id -> m (Maybe (Term Symbol Ann))
            getTerm id =
              runTransaction (CodebaseOps.getTerm getDeclType id)

            getTypeOfTermImpl :: Reference.Id -> Sqlite.Transaction (Maybe (Type Symbol Ann))
            getTypeOfTermImpl id | debug && trace ("getTypeOfTermImpl " ++ show id) False = undefined
            getTypeOfTermImpl id =
              CodebaseOps.getTypeOfTermImpl id

            getTermComponentWithTypes :: Hash -> Sqlite.Transaction (Maybe [(Term Symbol Ann, Type Symbol Ann)])
            getTermComponentWithTypes =
              CodebaseOps.getTermComponentWithTypes getDeclType

            getTypeDeclaration :: Reference.Id -> Sqlite.Transaction (Maybe (Decl Symbol Ann))
            getTypeDeclaration =
              CodebaseOps.getTypeDeclaration

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

            getShallowCausalForHash :: MonadIO m => V2Branch.CausalHash -> m (V2Branch.CausalBranch m)
            getShallowCausalForHash bh =
              V2Branch.hoistCausalBranch runTransaction <$> runTransaction (Ops.expectCausalBranchByCausalHash bh)

            getRootBranch :: TVar (Maybe (Sqlite.DataVersion, Branch Sqlite.Transaction)) -> m (Branch m)
            getRootBranch rootBranchCache =
              Branch.transform runTransaction <$> runTransaction (CodebaseOps.getRootBranch branchCache getDeclType rootBranchCache)

            putRootBranch :: TVar (Maybe (Sqlite.DataVersion, Branch Sqlite.Transaction)) -> Text -> Branch m -> m ()
            putRootBranch rootBranchCache reason branch1 = do
              now <- liftIO getCurrentTime
              withRunInIO \runInIO -> do
                runInIO do
                  runTransaction do
                    let emptyCausalHash = Cv.causalHash1to2 $ Branch.headHash Branch.empty
                    fromRootCausalHash <- fromMaybe emptyCausalHash <$> Ops.loadRootCausalHash
                    let toRootCausalHash = Cv.causalHash1to2 $ Branch.headHash branch1
                    CodebaseOps.putRootBranch rootBranchCache (Branch.transform (Sqlite.unsafeIO . runInIO) branch1)
                    Ops.appendReflog (Reflog.Entry {time = now, fromRootCausalHash, toRootCausalHash, reason})

            -- if this blows up on cromulent hashes, then switch from `hashToHashId`
            -- to one that returns Maybe.
            getBranchForHash :: Branch.CausalHash -> m (Maybe (Branch m))
            getBranchForHash h =
              fmap (Branch.transform runTransaction) <$> runTransaction (CodebaseOps.getBranchForHash branchCache getDeclType h)

            putBranch :: Branch m -> m ()
            putBranch branch =
              withRunInIO \runInIO ->
                runInIO (runTransaction (CodebaseOps.putBranch (Branch.transform (Sqlite.unsafeIO . runInIO) branch)))

            patchExists :: Branch.EditHash -> m Bool
            patchExists h =
              runTransaction (CodebaseOps.patchExists h)

            syncFromDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch m -> m ()
            syncFromDirectory srcRoot _syncMode b =
              withConnection (debugName ++ ".sync.src") srcRoot \srcConn ->
                withConn \destConn -> do
                  progressStateRef <- liftIO (newIORef emptySyncProgressState)
                  Sqlite.runReadOnlyTransaction srcConn \runSrc ->
                    Sqlite.runWriteTransaction destConn \runDest -> do
                      syncInternal (syncProgress progressStateRef) runSrc runDest b

            syncToDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch m -> m ()
            syncToDirectory destRoot _syncMode b =
              withConn \srcConn ->
                withConnection (debugName ++ ".sync.dest") destRoot \destConn -> do
                  progressStateRef <- liftIO (newIORef emptySyncProgressState)
                  initSchemaIfNotExist destRoot
                  Sqlite.runReadOnlyTransaction srcConn \runSrc ->
                    Sqlite.runWriteTransaction destConn \runDest -> do
                      syncInternal (syncProgress progressStateRef) runSrc runDest b

            watches :: UF.WatchKind -> m [Reference.Id]
            watches w =
              runTransaction (CodebaseOps.watches w)

            getWatch :: UF.WatchKind -> Reference.Id -> m (Maybe (Term Symbol Ann))
            getWatch k r =
              runTransaction (CodebaseOps.getWatch getDeclType k r)

            putWatch :: UF.WatchKind -> Reference.Id -> Term Symbol Ann -> m ()
            putWatch k r tm =
              runTransaction (CodebaseOps.putWatch k r tm)

            clearWatches :: m ()
            clearWatches =
              runTransaction CodebaseOps.clearWatches

            getReflog :: Int -> m [Reflog.Entry CausalHash Text]
            getReflog numEntries = runTransaction $ Ops.getReflog numEntries

            termsOfTypeImpl :: Reference -> m (Set Referent.Id)
            termsOfTypeImpl r =
              runTransaction (CodebaseOps.termsOfTypeImpl getDeclType r)

            termsMentioningTypeImpl :: Reference -> m (Set Referent.Id)
            termsMentioningTypeImpl r =
              runTransaction (CodebaseOps.termsMentioningTypeImpl getDeclType r)

            hashLength :: m Int
            hashLength =
              runTransaction CodebaseOps.hashLength

            branchHashLength :: m Int
            branchHashLength =
              runTransaction CodebaseOps.branchHashLength

            termReferencesByPrefix :: ShortHash -> m (Set Reference.Id)
            termReferencesByPrefix sh =
              runTransaction (CodebaseOps.termReferencesByPrefix sh)

            declReferencesByPrefix :: ShortHash -> m (Set Reference.Id)
            declReferencesByPrefix sh =
              runTransaction (CodebaseOps.declReferencesByPrefix sh)

            referentsByPrefix :: ShortHash -> m (Set Referent.Id)
            referentsByPrefix sh =
              runTransaction (CodebaseOps.referentsByPrefix getDeclType sh)

            causalHashesByPrefix :: ShortCausalHash -> m (Set Branch.CausalHash)
            causalHashesByPrefix sh =
              runTransaction (CodebaseOps.causalHashesByPrefix sh)

            sqlLca :: Branch.CausalHash -> Branch.CausalHash -> m (Maybe (Branch.CausalHash))
            sqlLca h1 h2 =
              runTransaction (CodebaseOps.sqlLca h1 h2)

            beforeImpl :: Maybe (Branch.CausalHash -> Branch.CausalHash -> m Bool)
            beforeImpl =
              Just \l r ->
                runTransaction $ fromJust <$> CodebaseOps.before l r

            namesAtPath :: Path -> m ScopedNames
            namesAtPath path =
              runTransaction (CodebaseOps.namesAtPath path)

            updateNameLookup :: Path -> Maybe BranchHash -> BranchHash -> m ()
            updateNameLookup pathPrefix fromBH toBH =
              runTransaction (CodebaseOps.updateNameLookupIndex getDeclType pathPrefix fromBH toBH)

        let codebase =
              C.Codebase
                { getTerm = Cache.applyDefined termCache getTerm,
                  getTypeOfTermImpl = applyDefined typeOfTermCache getTypeOfTermImpl,
                  getTypeDeclaration = applyDefined declCache getTypeDeclaration,
                  getDeclType =
                    \r ->
                      withConn \conn ->
                        Sqlite.runReadOnlyTransaction conn \run -> run (getDeclType r),
                  putTerm,
                  putTermComponent,
                  putTypeDeclaration,
                  putTypeDeclarationComponent,
                  getTermComponentWithTypes,
                  getRootBranch = getRootBranch rootBranchCache,
                  putRootBranch = putRootBranch rootBranchCache,
                  getShallowCausalForHash,
                  getBranchForHashImpl = getBranchForHash,
                  putBranch,
                  patchExists,
                  syncFromDirectory,
                  syncToDirectory,
                  viewRemoteBranch',
                  pushGitBranch = \repo opts action -> withConn \conn -> pushGitBranch conn repo opts action,
                  watches,
                  getWatch,
                  putWatch,
                  clearWatches,
                  getReflog,
                  termsOfTypeImpl,
                  termsMentioningTypeImpl,
                  hashLength,
                  termReferencesByPrefix,
                  typeReferencesByPrefix = declReferencesByPrefix,
                  termReferentsByPrefix = referentsByPrefix,
                  branchHashLength,
                  causalHashesByPrefix,
                  lcaImpl = Just sqlLca,
                  beforeImpl,
                  namesAtPath,
                  updateNameLookup,
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

    -- Like Cache.applyDefined, but in Transaction
    applyDefined ::
      (Applicative g, Traversable g) =>
      Cache.Cache k v ->
      (k -> Sqlite.Transaction (g v)) ->
      k ->
      Sqlite.Transaction (g v)
    applyDefined c f k = do
      conn <- Sqlite.Transaction.unsafeGetConnection
      Sqlite.unsafeIO (Cache.applyDefined c (\k1 -> Sqlite.unsafeUnTransaction (f k1) conn) k)

syncInternal ::
  forall m.
  MonadUnliftIO m =>
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
              let h2 = CausalHash $ Causal.unCausalHash h
              runSrc (Q.loadCausalHashIdByCausalHash h2) >>= \case
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
                            os = map O (es <> ts <> ds)
                        processBranches (os ++ bs ++ b0 : rest)
        O h : rest -> do
          when debugProcessBranches $ traceM $ "processBranches O " ++ take 10 (show h)
          oId <- runSrc (Q.expectHashIdByHash h >>= Q.expectObjectIdForAnyHashId)
          doSync [Sync22.O oId]
          processBranches rest
  let bHash = Branch.headHash b
  time "SyncInternal.processBranches" $ processBranches [B bHash (pure b)]

data Entity m
  = B Branch.CausalHash (m (Branch m))
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

syncProgress :: forall m. MonadIO m => IORef SyncProgressState -> Sync.Progress m Sync22.Entity
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
        "\r" ++ prefix ++ show (Set.size done + Set.size warn)
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

      result <- sqliteCodebase "viewRemoteBranch.gitCache" remotePath Remote MigrateAfterPrompt \codebase -> do
        -- try to load the requested branch from it
        branch <- time "Git fetch (sch)" $ case sch of
          -- no sub-branch was specified, so use the root.
          Nothing -> time "Get remote root branch" $ Codebase1.getRootBranch codebase
          -- load from a specific `ShortCausalHash`
          Just sch -> do
            branchCompletions <- Codebase1.causalHashesByPrefix codebase sch
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
  MonadUnliftIO m =>
  Sqlite.Connection ->
  WriteGitRepo ->
  PushGitBranchOpts ->
  -- An action which accepts the current root branch on the remote and computes a new branch.
  (Branch m -> m (Either e (Branch m))) ->
  m (Either C.GitError (Either e (Branch m)))
pushGitBranch srcConn repo (PushGitBranchOpts behavior _syncMode) action = UnliftIO.try do
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
      . withOpenOrCreateCodebase "push.dest" (Git.gitDirToPath pushStaging) Remote MigrateAfterPrompt
      $ \(codebaseStatus, destCodebase) -> do
        currentRootBranch <-
          Codebase1.runTransaction destCodebase CodebaseOps.getRootBranchExists >>= \case
            False -> pure Branch.empty
            True -> C.getRootBranch destCodebase
        action currentRootBranch >>= \case
          Left e -> pure $ Left e
          Right newBranch -> do
            C.withConnection destCodebase \destConn ->
              doSync codebaseStatus (Git.gitDirToPath pushStaging) destConn newBranch
            pure (Right newBranch)
    for newBranchOrErr $ push pushStaging repo
    pure newBranchOrErr
  where
    readRepo :: ReadGitRepo
    readRepo = writeToReadGit repo
    doSync :: CodebaseStatus -> FilePath -> Sqlite.Connection -> Branch m -> m ()
    doSync codebaseStatus remotePath destConn newBranch = do
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
                          runDest (CodebaseOps.before (Cv.causalHash2to1 oldRootHash) newBranchHash) >>= \case
                            Nothing ->
                              error $
                                "I couldn't find the hash " ++ show newBranchHash
                                  ++ " that I just synced to the cached copy of "
                                  ++ repoString
                                  ++ " in "
                                  ++ show remotePath
                                  ++ "."
                            Just False -> throwIO . C.GitProtocolError $ GitError.PushDestinationHasNewStuff repo
                            Just True -> pure ()
                  CreatedCodebase -> pure ()
                runDest (setRepoRoot newBranchHash)
          case behavior of
            C.GitPushBehaviorGist -> pure ()
            C.GitPushBehaviorFf -> overwriteRoot False
            C.GitPushBehaviorForce -> overwriteRoot True
    repoString = Text.unpack $ printWriteGitRepo repo
    setRepoRoot :: Branch.CausalHash -> Sqlite.Transaction ()
    setRepoRoot h = do
      let h2 = Cv.causalHash1to2 h
          err = error $ "Called SqliteCodebase.setNamespaceRoot on unknown causal hash " ++ show h2
      chId <- fromMaybe err <$> Q.loadCausalHashIdByCausalHash h2
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
        statusLines = Text.unpack <$> Text.lines status
        t = dropWhile Char.isSpace
        okLine (t -> '?' : '?' : (t -> p)) | p == posixCodebasePath = True
        okLine (t -> 'M' : (t -> p)) | p == posixCodebasePath = True
        okLine line = isWalDelete line || isShmDelete line
        isWalDelete (t -> 'D' : (t -> p)) | p == posixCodebasePath ++ "-wal" = True
        isWalDelete _ = False
        isShmDelete (t -> 'D' : (t -> p)) | p == posixCodebasePath ++ "-wal" = True
        isShmDelete _ = False
        hasDeleteWal = any isWalDelete statusLines
        hasDeleteShm = any isShmDelete statusLines

    -- Commit our changes
    push :: forall n. MonadIO n => Git.GitRepo -> WriteGitRepo -> Branch m -> n Bool -- withIOError needs IO
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
