{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.SqliteCodebase
  ( Unison.Codebase.SqliteCodebase.init,
  )
where

import qualified Control.Concurrent
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
import qualified Data.Text.IO as TextIO
import qualified System.Console.ANSI as ANSI
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import qualified U.Codebase.Branch as V2Branch
import U.Codebase.HashTags (CausalHash (CausalHash))
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Sync22 as Sync22
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import qualified U.Codebase.Sync as Sync
import qualified U.Util.Cache as Cache
import qualified U.Util.Hash as H2
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
import qualified Unison.Codebase.Init as Codebase
import qualified Unison.Codebase.Init.CreateCodebaseError as Codebase1
import qualified Unison.Codebase.Init.OpenCodebaseError as Codebase1
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Branch.Dependencies as BD
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.Codebase.SqliteCodebase.GitError as GitError
import Unison.Codebase.SqliteCodebase.Migrations (ensureCodebaseIsUpToDate)
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
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.WatchKind as UF
import UnliftIO (UnliftIO (..), catchIO, finally, throwIO, try)
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
      withCreatedCodebase = withCreatedCodebase',
      codebasePath = makeCodebaseDirPath
    }
  where
    withCreatedCodebase' debugName path action =
      createCodebaseOrError debugName path (action . fst)

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
  ((CodebaseStatus, Codebase m Symbol Ann, Sqlite.Connection) -> m r) ->
  m (Either Codebase1.OpenCodebaseError r)
withOpenOrCreateCodebase debugName codebasePath localOrRemote action = do
  createCodebaseOrError debugName codebasePath (action' CreatedCodebase) >>= \case
    Left (Codebase1.CreateCodebaseAlreadyExists) -> do
      sqliteCodebase debugName codebasePath localOrRemote (action' ExistingCodebase)
    Right r -> pure (Right r)
  where
    action' openOrCreate (codebase, conn) = action (openOrCreate, codebase, conn)

-- | Create a codebase at the given location.
createCodebaseOrError ::
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  ((Codebase m Symbol Ann, Sqlite.Connection) -> m r) ->
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

      sqliteCodebase debugName path Local action >>= \case
        Left schemaVersion -> error ("Failed to open codebase with schema version: " ++ show schemaVersion ++ ", which is unexpected because I just created this codebase.")
        Right result -> pure (Right result)

-- | Use the codebase in the provided path.
-- The codebase is automatically closed when the action completes or throws an exception.
withCodebaseOrError ::
  forall m r.
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either Codebase1.OpenCodebaseError r)
withCodebaseOrError debugName dir action = do
  doesFileExist (makeCodebasePath dir) >>= \case
    False -> pure (Left Codebase1.OpenCodebaseDoesntExist)
    True ->
      sqliteCodebase debugName dir Local (action . fst)

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
  ((Codebase m Symbol Ann, Sqlite.Connection) -> m r) ->
  m (Either Codebase1.OpenCodebaseError r)
sqliteCodebase debugName root localOrRemote action = do
  termCache <- Cache.semispaceCache 8192 -- pure Cache.nullCache -- to disable
  typeOfTermCache <- Cache.semispaceCache 8192
  declCache <- Cache.semispaceCache 1024
  rootBranchCache <- newTVarIO Nothing
  getDeclType <- CodebaseOps.mkGetDeclType
  -- The v1 codebase interface has operations to read and write individual definitions
  -- whereas the v2 codebase writes them as complete components.  These two fields buffer
  -- the individual definitions until a complete component has been written.
  termBuffer :: TVar (Map Hash CodebaseOps.TermBufferEntry) <- newTVarIO Map.empty
  declBuffer :: TVar (Map Hash CodebaseOps.DeclBufferEntry) <- newTVarIO Map.empty

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
    -- Migrate if necessary.
    result <-
      withConn \conn ->
        ensureCodebaseIsUpToDate localOrRemote root getDeclType termBuffer declBuffer conn

    case result of
      Left err -> pure $ Left err
      Right () -> do
        let getTerm :: Reference.Id -> m (Maybe (Term Symbol Ann))
            getTerm id =
              withConn \conn -> Sqlite.runTransaction conn (CodebaseOps.getTerm getDeclType id)

            getTypeOfTermImpl :: Reference.Id -> m (Maybe (Type Symbol Ann))
            getTypeOfTermImpl id | debug && trace ("getTypeOfTermImpl " ++ show id) False = undefined
            getTypeOfTermImpl id =
              withConn \conn -> Sqlite.runTransaction conn (CodebaseOps.getTypeOfTermImpl id)

            getTermComponentWithTypes :: Hash -> m (Maybe [(Term Symbol Ann, Type Symbol Ann)])
            getTermComponentWithTypes h =
              withConn \conn -> Sqlite.runTransaction conn (CodebaseOps.getTermComponentWithTypes getDeclType h)

            getTypeDeclaration :: Reference.Id -> m (Maybe (Decl Symbol Ann))
            getTypeDeclaration id =
              withConn \conn -> Sqlite.runTransaction conn (CodebaseOps.getTypeDeclaration id)

            getDeclComponent :: Hash -> m (Maybe [Decl Symbol Ann])
            getDeclComponent h =
              withConn \conn -> Sqlite.runTransaction conn (CodebaseOps.getDeclComponent h)

            getCycleLength :: Hash -> m (Maybe Reference.CycleSize)
            getCycleLength h =
              withConn \conn -> Sqlite.runTransaction conn (CodebaseOps.getCycleLength h)

            -- putTermComponent :: MonadIO m => Hash -> [(Term Symbol Ann, Type Symbol Ann)] -> m ()
            -- putTerms :: MonadIO m => Map Reference.Id (Term Symbol Ann, Type Symbol Ann) -> m () -- dies horribly if missing dependencies?

            -- option 1: tweak putTerm to incrementally notice the cycle length until each component is full
            -- option 2: switch codebase interface from putTerm to putTerms -- buffering can be local to the function
            -- option 3: switch from putTerm to putTermComponent -- needs to buffer dependencies non-locally (or require application to manage + die horribly)

            putTerm :: Reference.Id -> Term Symbol Ann -> Type Symbol Ann -> m ()
            putTerm id tm tp | debug && trace ("SqliteCodebase.putTerm " ++ show id ++ " " ++ show tm ++ " " ++ show tp) False = undefined
            putTerm id tm tp =
              withConn \conn -> Sqlite.runTransaction conn (CodebaseOps.putTerm termBuffer declBuffer id tm tp)

            putTypeDeclaration :: Reference.Id -> Decl Symbol Ann -> m ()
            putTypeDeclaration id decl =
              withConn \conn -> Sqlite.runTransaction conn (CodebaseOps.putTypeDeclaration termBuffer declBuffer id decl)

            getRootBranchHash :: MonadIO m => m V2Branch.CausalHash
            getRootBranchHash = do
              withConn \conn ->
                Sqlite.runReadOnlyTransaction conn \run ->
                  run Ops.expectRootCausalHash

            getShallowBranchForHash :: MonadIO m => V2Branch.CausalHash -> m (V2Branch.CausalBranch m)
            getShallowBranchForHash bh =
              withConn \conn ->
                Sqlite.runReadOnlyTransaction conn \run -> do
                  V2Branch.hoistCausalBranch run <$> run (Ops.expectCausalBranchByCausalHash bh)

            getRootBranch :: TVar (Maybe (Sqlite.DataVersion, Branch Sqlite.Transaction)) -> m (Branch m)
            getRootBranch rootBranchCache =
              withConn \conn ->
                Sqlite.runReadOnlyTransaction conn \run ->
                  Branch.transform run <$> run (CodebaseOps.getRootBranch getDeclType rootBranchCache)

            getRootBranchExists :: m Bool
            getRootBranchExists =
              withConn \conn ->
                Sqlite.runTransaction conn CodebaseOps.getRootBranchExists

            putRootBranch :: TVar (Maybe (Sqlite.DataVersion, Branch Sqlite.Transaction)) -> Branch m -> m ()
            putRootBranch rootBranchCache branch1 = do
              withConn \conn ->
                withRunInIO \runInIO -> do
                  Sqlite.runTransaction conn do
                    CodebaseOps.putRootBranch rootBranchCache (Branch.transform (Sqlite.unsafeIO . runInIO) branch1)

            rootBranchUpdates :: MonadIO m => TVar (Maybe (Sqlite.DataVersion, a)) -> m (IO (), IO (Set Branch.CausalHash))
            rootBranchUpdates _rootBranchCache = do
              -- branchHeadChanges      <- TQueue.newIO
              -- (cancelWatch, watcher) <- Watch.watchDirectory' (v2dir root)
              -- watcher1               <-
              --   liftIO . forkIO
              --   $ forever
              --   $ do
              --       -- void ignores the name and time of the changed file,
              --       -- and assume 'unison.sqlite3' has changed
              --       (filename, time) <- watcher
              --       traceM $ "SqliteCodebase.watcher " ++ show (filename, time)
              --       readTVarIO rootBranchCache >>= \case
              --         Nothing -> pure ()
              --         Just (v, _) -> do
              --           -- this use of `conn` in a separate thread may be problematic.
              --           -- hopefully sqlite will produce an obvious error message if it is.
              --           v' <- runDB conn Ops.dataVersion
              --           if v /= v' then
              --             atomically
              --               . TQueue.enqueue branchHeadChanges =<< runDB conn Ops.loadRootCausalHash
              --           else pure ()

              --       -- case hashFromFilePath filePath of
              --       --   Nothing -> failWith $ CantParseBranchHead filePath
              --       --   Just h ->
              --       --     atomically . TQueue.enqueue branchHeadChanges $ Branch.CausalHash h
              -- -- smooth out intermediate queue
              -- pure
              --   ( cancelWatch >> killThread watcher1
              --   , Set.fromList <$> Watch.collectUntilPause branchHeadChanges 400000
              --   )
              pure (cleanup, liftIO newRootsDiscovered)
              where
                newRootsDiscovered = do
                  Control.Concurrent.threadDelay maxBound -- hold off on returning
                  pure mempty -- returning nothing
                cleanup = pure ()

            -- if this blows up on cromulent hashes, then switch from `hashToHashId`
            -- to one that returns Maybe.
            getBranchForHash :: Branch.CausalHash -> m (Maybe (Branch m))
            getBranchForHash h =
              withConn \conn ->
                Sqlite.runReadOnlyTransaction conn \run ->
                  fmap (Branch.transform run) <$> run (CodebaseOps.getBranchForHash getDeclType h)

            putBranch :: Branch m -> m ()
            putBranch branch =
              withConn \conn ->
                withRunInIO \runInIO ->
                  Sqlite.runTransaction conn (CodebaseOps.putBranch (Branch.transform (Sqlite.unsafeIO . runInIO) branch))

            isCausalHash :: Branch.CausalHash -> m Bool
            isCausalHash h =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.isCausalHash h)

            getPatch :: Branch.EditHash -> m (Maybe Patch)
            getPatch h =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.getPatch h)

            putPatch :: Branch.EditHash -> Patch -> m ()
            putPatch h p =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.putPatch h p)

            patchExists :: Branch.EditHash -> m Bool
            patchExists h =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.patchExists h)

            dependentsImpl :: Reference -> m (Set Reference.Id)
            dependentsImpl r =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.dependentsImpl r)

            dependentsOfComponentImpl :: Hash -> m (Set Reference.Id)
            dependentsOfComponentImpl h =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.dependentsOfComponentImpl h)

            syncFromDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch m -> m ()
            syncFromDirectory srcRoot _syncMode b =
              withConnection (debugName ++ ".sync.src") srcRoot \srcConn ->
                withConn \conn -> do
                  progressStateRef <- liftIO (newIORef emptySyncProgressState)
                  Sqlite.runReadOnlyTransaction srcConn \runSrc ->
                    Sqlite.runWriteTransaction conn \runDest -> do
                      syncInternal (syncProgress progressStateRef) runSrc runDest b

            syncToDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch m -> m ()
            syncToDirectory destRoot _syncMode b =
              withConn \conn ->
                withConnection (debugName ++ ".sync.dest") destRoot \destConn -> do
                  progressStateRef <- liftIO (newIORef emptySyncProgressState)
                  initSchemaIfNotExist destRoot
                  Sqlite.runReadOnlyTransaction conn \runSrc ->
                    Sqlite.runWriteTransaction destConn \runDest -> do
                      syncInternal (syncProgress progressStateRef) runSrc runDest b

            watches :: UF.WatchKind -> m [Reference.Id]
            watches w =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.watches w)

            getWatch :: UF.WatchKind -> Reference.Id -> m (Maybe (Term Symbol Ann))
            getWatch k r =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.getWatch getDeclType k r)

            putWatch :: UF.WatchKind -> Reference.Id -> Term Symbol Ann -> m ()
            putWatch k r tm =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.putWatch k r tm)

            clearWatches :: m ()
            clearWatches =
              withConn \conn ->
                Sqlite.runTransaction conn CodebaseOps.clearWatches

            getReflog :: m [Reflog.Entry Branch.CausalHash]
            getReflog =
              liftIO $
                ( do
                    contents <- TextIO.readFile (reflogPath root)
                    let lines = Text.lines contents
                    let entries = parseEntry <$> lines
                    pure entries
                )
                  `catchIO` const (pure [])
              where
                parseEntry t = fromMaybe (err t) (Reflog.fromText t)
                err t =
                  error $
                    "I couldn't understand this line in " ++ reflogPath root ++ "\n\n"
                      ++ Text.unpack t

            appendReflog :: Text -> Branch m -> Branch m -> m ()
            appendReflog reason old new =
              liftIO $ TextIO.appendFile (reflogPath root) (t <> "\n")
              where
                t = Reflog.toText $ Reflog.Entry (Branch.headHash old) (Branch.headHash new) reason

            reflogPath :: CodebasePath -> FilePath
            reflogPath root = root </> "reflog"

            termsOfTypeImpl :: Reference -> m (Set Referent.Id)
            termsOfTypeImpl r =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.termsOfTypeImpl getDeclType r)

            termsMentioningTypeImpl :: Reference -> m (Set Referent.Id)
            termsMentioningTypeImpl r =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.termsMentioningTypeImpl getDeclType r)

            hashLength :: m Int
            hashLength =
              withConn \conn ->
                Sqlite.runTransaction conn CodebaseOps.hashLength

            branchHashLength :: m Int
            branchHashLength =
              withConn \conn ->
                Sqlite.runTransaction conn CodebaseOps.branchHashLength

            termReferencesByPrefix :: ShortHash -> m (Set Reference.Id)
            termReferencesByPrefix sh =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.termReferencesByPrefix sh)

            declReferencesByPrefix :: ShortHash -> m (Set Reference.Id)
            declReferencesByPrefix sh =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.declReferencesByPrefix sh)

            referentsByPrefix :: ShortHash -> m (Set Referent.Id)
            referentsByPrefix sh =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.referentsByPrefix getDeclType sh)

            branchHashesByPrefix :: ShortBranchHash -> m (Set Branch.CausalHash)
            branchHashesByPrefix sh =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.branchHashesByPrefix sh)

            sqlLca :: Branch.CausalHash -> Branch.CausalHash -> m (Maybe (Branch.CausalHash))
            sqlLca h1 h2 =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.sqlLca h1 h2)

            beforeImpl :: Maybe (Branch.CausalHash -> Branch.CausalHash -> m Bool)
            beforeImpl =
              Just \l r ->
                withConn \conn ->
                  Sqlite.runTransaction conn $ fromJust <$> CodebaseOps.before l r

            namesAtPath :: Path -> m ScopedNames
            namesAtPath path =
              withConn \conn ->
                Sqlite.runReadOnlyTransaction conn \runTx ->
                  runTx (CodebaseOps.namesAtPath path)

            updateNameLookup :: m ()
            updateNameLookup =
              withConn \conn ->
                Sqlite.runTransaction conn (CodebaseOps.updateNameLookupIndexFromV2Root getDeclType)

        let codebase =
              C.Codebase
                { getTerm = Cache.applyDefined termCache getTerm,
                  getTypeOfTermImpl = Cache.applyDefined typeOfTermCache getTypeOfTermImpl,
                  getTypeDeclaration = Cache.applyDefined declCache getTypeDeclaration,
                  getDeclType =
                    \r ->
                      withConn \conn ->
                        Sqlite.runReadOnlyTransaction conn \run -> run (getDeclType r),
                  putTerm,
                  putTypeDeclaration,
                  getTermComponentWithTypes,
                  getDeclComponent,
                  getComponentLength = getCycleLength,
                  getRootBranch = getRootBranch rootBranchCache,
                  getRootBranchHash,
                  getRootBranchExists,
                  putRootBranch = putRootBranch rootBranchCache,
                  rootBranchUpdates = rootBranchUpdates rootBranchCache,
                  getShallowBranchForHash,
                  getBranchForHashImpl = getBranchForHash,
                  putBranch,
                  branchExists = isCausalHash,
                  getPatch,
                  putPatch,
                  patchExists,
                  dependentsImpl,
                  dependentsOfComponentImpl,
                  syncFromDirectory,
                  syncToDirectory,
                  viewRemoteBranch',
                  pushGitBranch = \repo opts action -> withConn \conn -> pushGitBranch conn repo opts action,
                  watches,
                  getWatch,
                  putWatch,
                  clearWatches,
                  getReflog,
                  appendReflog,
                  termsOfTypeImpl,
                  termsMentioningTypeImpl,
                  hashLength,
                  termReferencesByPrefix,
                  typeReferencesByPrefix = declReferencesByPrefix,
                  termReferentsByPrefix = referentsByPrefix,
                  branchHashLength,
                  branchHashesByPrefix,
                  lcaImpl = Just sqlLca,
                  beforeImpl,
                  namesAtPath,
                  updateNameLookup,
                  withConnection = withConn,
                  withConnectionIO = withConnection debugName root
                }
        Right <$> action (codebase, undefined)
  where
    withConn :: (Sqlite.Connection -> m a) -> m a
    withConn =
      withConnection debugName root

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
            (runDest (CodebaseOps.isCausalHash h))
            do
              when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " already exists in dest db"
              processBranches rest
            do
              when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " doesn't exist in dest db"
              let h2 = CausalHash . Cv.hash1to2 $ Causal.unCausalHash h
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
                      cs <- filterM (fmap not . CodebaseOps.isCausalHash . fst) branchDeps
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
          oId <- runSrc (Q.expectHashIdByHash (Cv.hash1to2 h) >>= Q.expectObjectIdForAnyHashId)
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
viewRemoteBranch' ReadGitRemoteNamespace {repo, sbh, path} gitBranchBehavior action = UnliftIO.try $ do
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

      result <- sqliteCodebase "viewRemoteBranch.gitCache" remotePath Remote \(codebase, _conn) -> do
        -- try to load the requested branch from it
        branch <- time "Git fetch (sbh)" $ case sbh of
          -- no sub-branch was specified, so use the root.
          Nothing -> time "Get remote root branch" $ Codebase1.getRootBranch codebase
          -- load from a specific `ShortBranchHash`
          Just sbh -> do
            branchCompletions <- Codebase1.branchHashesByPrefix codebase sbh
            case toList branchCompletions of
              [] -> throwIO . C.GitCodebaseError $ GitError.NoRemoteNamespaceWithHash repo sbh
              [h] ->
                (Codebase1.getBranchForHash codebase h) >>= \case
                  Just b -> pure b
                  Nothing -> throwIO . C.GitCodebaseError $ GitError.NoRemoteNamespaceWithHash repo sbh
              _ -> throwIO . C.GitCodebaseError $ GitError.RemoteNamespaceHashAmbiguous repo sbh branchCompletions
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
pushGitBranch srcConn repo (PushGitBranchOpts setRoot _syncMode) action = UnliftIO.try do
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
      . withOpenOrCreateCodebase "push.dest" (Git.gitDirToPath pushStaging) Remote
      $ \(codebaseStatus, destCodebase, destConn) -> do
        currentRootBranch <-
          C.getRootBranchExists destCodebase >>= \case
            False -> pure Branch.empty
            True -> C.getRootBranch destCodebase
        action currentRootBranch >>= \case
          Left e -> pure $ Left e
          Right newBranch -> do
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
          when setRoot (overwriteRoot runDest codebaseStatus remotePath newBranch)
    overwriteRoot ::
      (forall a. Sqlite.Transaction a -> m a) ->
      CodebaseStatus ->
      FilePath ->
      Branch m ->
      m ()
    overwriteRoot run codebaseStatus remotePath newBranch = do
      let newBranchHash = Branch.headHash newBranch
      case codebaseStatus of
        ExistingCodebase -> do
          -- the call to runDB "handles" the possible DB error by bombing
          maybeOldRootHash <- fmap Cv.causalHash2to1 <$> run Ops.loadRootCausalHash
          case maybeOldRootHash of
            Nothing -> run (setRepoRoot newBranchHash)
            Just oldRootHash -> do
              run (CodebaseOps.before oldRootHash newBranchHash) >>= \case
                Nothing ->
                  error $
                    "I couldn't find the hash " ++ show newBranchHash
                      ++ " that I just synced to the cached copy of "
                      ++ repoString
                      ++ " in "
                      ++ show remotePath
                      ++ "."
                Just False ->
                  throwIO . C.GitProtocolError $ GitError.PushDestinationHasNewStuff repo
                Just True -> pure ()
        CreatedCodebase -> pure ()
      run (setRepoRoot newBranchHash)
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
