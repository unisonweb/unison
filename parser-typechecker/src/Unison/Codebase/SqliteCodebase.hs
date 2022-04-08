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
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Extra as Monad
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import Data.Bifunctor (Bifunctor (bimap), second)
import Data.Bitraversable (bitraverse)
import qualified Data.Char as Char
import Data.Either.Extra ()
import Data.IORef
import qualified Data.List as List
import Data.List.NonEmpty.Extra (NonEmpty ((:|)), maximum1)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Console.ANSI as ANSI
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import U.Codebase.HashTags (CausalHash (CausalHash, unCausalHash))
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C.Referent
import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Sqlite.ObjectType as OT
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Sync22 as Sync22
import qualified U.Codebase.Sync as Sync
import qualified U.Util.Cache as Cache
import qualified U.Util.Hash as H2
import qualified U.Util.Monoid as Monoid
import U.Util.Timing (time)
import qualified Unison.Builtin as Builtins
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase1
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal.Type as Causal
import Unison.Codebase.Editor.Git (gitIn, gitInCaptured, gitTextIn, withRepo)
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace, ReadRepo, WriteRepo (..), printWriteRepo, writeToRead)
import qualified Unison.Codebase.GitError as GitError
import qualified Unison.Codebase.Init as Codebase
import qualified Unison.Codebase.Init.CreateCodebaseError as Codebase1
import qualified Unison.Codebase.Init.OpenCodebaseError as Codebase1
import Unison.Codebase.Patch (Patch)
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Branch.Dependencies as BD
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.Codebase.SqliteCodebase.GitError as GitError
import Unison.Codebase.SqliteCodebase.Migrations (ensureCodebaseIsUpToDate)
import qualified Unison.Codebase.SqliteCodebase.Operations as Ops2
import Unison.Codebase.SqliteCodebase.Paths
import qualified Unison.Codebase.SqliteCodebase.SyncEphemeral as SyncEphemeral
import Unison.Codebase.SyncMode (SyncMode)
import Unison.Codebase.Type (LocalOrRemote (..), PushGitBranchOpts (..))
import qualified Unison.Codebase.Type as C
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as Decl
import Unison.Hash (Hash)
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import qualified Unison.ShortHash as ShortHash
import Unison.Sqlite (Connection)
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sqlite.Connection as Sqlite.Connection
import qualified Unison.Sqlite.Transaction as Sqlite.Transaction
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Util.Set as Set
import qualified Unison.WatchKind as UF
import UnliftIO (UnliftIO (..), catchIO, finally, throwIO, try)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import UnliftIO.Exception (catch)
import UnliftIO.STM

debug, debugProcessBranches, debugCommitFailedTransaction :: Bool
debug = False
debugProcessBranches = False
debugCommitFailedTransaction = False

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
  ((CodebaseStatus, Codebase m Symbol Ann, Connection) -> m r) ->
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
  ((Codebase m Symbol Ann, Connection) -> m r) ->
  m (Either Codebase1.CreateCodebaseError r)
createCodebaseOrError debugName path action = do
  undefined

-- ifM
--   (doesFileExist $ makeCodebasePath path)
--   (pure $ Left Codebase1.CreateCodebaseAlreadyExists)
--   do
--     createDirectoryIfMissing True (makeCodebaseDirPath path)
--     withConnection (debugName ++ ".createSchema") path $
--       runReaderT do
--         Q.createSchema
--         void . Ops.saveRootBranch $ Cv.causalbranch1to2 Branch.empty

--     sqliteCodebase debugName path Local action >>= \case
--       Left schemaVersion -> error ("Failed to open codebase with schema version: " ++ show schemaVersion ++ ", which is unexpected because I just created this codebase.")
--       Right result -> pure (Right result)

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
  undefined

-- unlessM (doesDirectoryExist $ makeCodebaseDirPath path) $
--   createDirectoryIfMissing True (makeCodebaseDirPath path)
-- unlessM (doesFileExist $ makeCodebasePath path) $
--   withConnection "initSchemaIfNotExist" path $ runReaderT Q.createSchema

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
  (Connection -> m a) ->
  m a
withConnection name root action =
  Sqlite.withConnection name (makeCodebasePath root) \conn -> do
    liftIO (Sqlite.trySetJournalMode conn Sqlite.JournalMode'WAL)
    action conn

sqliteCodebase ::
  forall m r.
  MonadUnliftIO m =>
  Codebase.DebugName ->
  CodebasePath ->
  -- | When local, back up the existing codebase before migrating, in case there's a catastrophic bug in the migration.
  LocalOrRemote ->
  ((Codebase m Symbol Ann, Connection) -> m r) ->
  m (Either Codebase1.OpenCodebaseError r)
sqliteCodebase debugName root localOrRemote action = do
  -- Monad.when debug $ traceM $ "sqliteCodebase " ++ debugName ++ " " ++ root
  withConnection debugName root $ \conn -> do
    termCache <- Cache.semispaceCache 8192 -- pure Cache.nullCache -- to disable
    typeOfTermCache <- Cache.semispaceCache 8192
    declCache <- Cache.semispaceCache 1024
    rootBranchCache <- newTVarIO Nothing
    -- The v1 codebase interface has operations to read and write individual definitions
    -- whereas the v2 codebase writes them as complete components.  These two fields buffer
    -- the individual definitions until a complete component has been written.
    termBuffer :: TVar (Map Hash Ops2.TermBufferEntry) <- newTVarIO Map.empty
    declBuffer :: TVar (Map Hash Ops2.DeclBufferEntry) <- newTVarIO Map.empty
    declTypeCache <- Cache.semispaceCache 2048
    let getTerm :: Reference.Id -> m (Maybe (Term Symbol Ann))
        getTerm id =
          Sqlite.runTransaction conn (Ops2.getTerm (Sqlite.idempotentIO . getDeclTypeIO) id)

        getDeclType :: C.Reference.Reference -> m CT.ConstructorType
        getDeclType =
          liftIO . getDeclTypeIO

        getDeclTypeIO :: C.Reference.Reference -> IO CT.ConstructorType
        getDeclTypeIO =
          Cache.apply declTypeCache \ref ->
            Sqlite.runTransaction conn (Ops2.getDeclType ref)

        getTypeOfTermImpl :: Reference.Id -> m (Maybe (Type Symbol Ann))
        getTypeOfTermImpl id | debug && trace ("getTypeOfTermImpl " ++ show id) False = undefined
        getTypeOfTermImpl id =
          Sqlite.runTransaction conn (Ops2.getTypeOfTermImpl id)

        getTermComponentWithTypes :: Hash -> m (Maybe [(Term Symbol Ann, Type Symbol Ann)])
        getTermComponentWithTypes h =
          Sqlite.runTransaction conn (Ops2.getTermComponentWithTypes (Sqlite.idempotentIO . getDeclTypeIO) h)

        getTypeDeclaration :: Reference.Id -> m (Maybe (Decl Symbol Ann))
        getTypeDeclaration id =
          Sqlite.runTransaction conn (Ops2.getTypeDeclaration id)

        getDeclComponent :: Hash -> m (Maybe [Decl Symbol Ann])
        getDeclComponent h =
          Sqlite.runTransaction conn (Ops2.getDeclComponent h)

        getCycleLength :: Hash -> m (Maybe Reference.CycleSize)
        getCycleLength h =
          Sqlite.runTransaction conn (Ops2.getCycleLength h)

        -- putTermComponent :: MonadIO m => Hash -> [(Term Symbol Ann, Type Symbol Ann)] -> m ()
        -- putTerms :: MonadIO m => Map Reference.Id (Term Symbol Ann, Type Symbol Ann) -> m () -- dies horribly if missing dependencies?

        -- option 1: tweak putTerm to incrementally notice the cycle length until each component is full
        -- option 2: switch codebase interface from putTerm to putTerms -- buffering can be local to the function
        -- option 3: switch from putTerm to putTermComponent -- needs to buffer dependencies non-locally (or require application to manage + die horribly)

        putTerm :: Reference.Id -> Term Symbol Ann -> Type Symbol Ann -> m ()
        putTerm id tm tp | debug && trace (show "SqliteCodebase.putTerm " ++ show id ++ " " ++ show tm ++ " " ++ show tp) False = undefined
        putTerm id tm tp =
          Sqlite.runTransaction conn (Ops2.putTerm termBuffer declBuffer id tm tp)

        putTypeDeclaration :: Reference.Id -> Decl Symbol Ann -> m ()
        putTypeDeclaration id decl =
          Sqlite.runTransaction conn (Ops2.putTypeDeclaration termBuffer declBuffer id decl)

        getRootBranch :: TVar (Maybe (Sqlite.DataVersion, Branch Sqlite.Transaction)) -> m (Branch m)
        getRootBranch rootBranchCache =
          Branch.transform (Sqlite.runTransaction conn) <$> Sqlite.runTransaction conn (Ops2.getRootBranch (Sqlite.idempotentIO . getDeclTypeIO) rootBranchCache)

        getRootBranchExists :: m Bool
        getRootBranchExists =
          Sqlite.runTransaction conn Ops2.getRootBranchExists

        putRootBranch :: TVar (Maybe (Sqlite.DataVersion, Branch Sqlite.Transaction)) -> Branch m -> m ()
        putRootBranch rootBranchCache branch1 =
          withRunInIO \runInIO ->
            Sqlite.runTransaction conn do
              Ops2.putRootBranch rootBranchCache (Branch.transform (Sqlite.idempotentIO . runInIO) branch1)

        rootBranchUpdates :: MonadIO m => TVar (Maybe (Sqlite.DataVersion, a)) -> m (IO (), IO (Set Branch.Hash))
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
          --       --     atomically . TQueue.enqueue branchHeadChanges $ Branch.Hash h
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
        getBranchForHash :: Branch.Hash -> m (Maybe (Branch m))
        getBranchForHash h =
          fmap (Branch.transform (Sqlite.runTransaction conn)) <$> Sqlite.runTransaction conn (Ops2.getBranchForHash (Sqlite.idempotentIO . getDeclTypeIO) h)

        putBranch :: Branch m -> m ()
        putBranch branch =
          withRunInIO \runInIO ->
            Sqlite.runTransaction conn (Ops2.putBranch (Branch.transform (Sqlite.idempotentIO . runInIO) branch))

        isCausalHash :: Branch.Hash -> m Bool
        isCausalHash h =
          Sqlite.runTransaction conn (Ops2.isCausalHash h)

        getPatch :: Branch.EditHash -> m (Maybe Patch)
        getPatch h =
          Sqlite.runTransaction conn (Ops2.getPatch h)

        putPatch :: Branch.EditHash -> Patch -> m ()
        putPatch h p =
          Sqlite.runTransaction conn (Ops2.putPatch h p)

        patchExists :: Branch.EditHash -> m Bool
        patchExists h =
          Sqlite.runTransaction conn (Ops2.patchExists h)

        dependentsImpl :: Reference -> m (Set Reference.Id)
        dependentsImpl r =
          Sqlite.runTransaction conn (Ops2.dependentsImpl r)

        dependentsOfComponentImpl :: Hash -> m (Set Reference.Id)
        dependentsOfComponentImpl h =
          Sqlite.runTransaction conn (Ops2.dependentsOfComponentImpl h)

        syncFromDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch m -> m ()
        syncFromDirectory srcRoot _syncMode b = do
          withConnection (debugName ++ ".sync.src") srcRoot $ \srcConn -> do
            progressStateRef <- liftIO (newIORef emptySyncProgressState)
            syncInternal (syncProgress progressStateRef) srcConn conn b

        syncToDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch m -> m ()
        syncToDirectory destRoot _syncMode b =
          withConnection (debugName ++ ".sync.dest") destRoot $ \destConn -> do
            progressStateRef <- liftIO (newIORef emptySyncProgressState)
            initSchemaIfNotExist destRoot
            syncInternal (syncProgress progressStateRef) conn destConn b

        watches :: UF.WatchKind -> m [Reference.Id]
        watches w =
          Sqlite.runTransaction conn (Ops2.watches w)

        getWatch :: UF.WatchKind -> Reference.Id -> m (Maybe (Term Symbol Ann))
        getWatch k r =
          Sqlite.runTransaction conn (Ops2.getWatch (Sqlite.idempotentIO . getDeclTypeIO) k r)

        putWatch :: UF.WatchKind -> Reference.Id -> Term Symbol Ann -> m ()
        putWatch k r tm =
          Sqlite.runTransaction conn (Ops2.putWatch k r tm)

        clearWatches :: m ()
        clearWatches =
          Sqlite.runTransaction conn Ops2.clearWatches

        getReflog :: m [Reflog.Entry Branch.Hash]
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
          Sqlite.runTransaction conn (Ops2.termsOfTypeImpl (Sqlite.idempotentIO . getDeclTypeIO) r)

        termsMentioningTypeImpl :: Reference -> m (Set Referent.Id)
        termsMentioningTypeImpl r =
          Sqlite.runTransaction conn (Ops2.termsMentioningTypeImpl (Sqlite.idempotentIO . getDeclTypeIO) r)

        hashLength :: m Int
        hashLength =
          Sqlite.runTransaction conn Ops2.hashLength

        branchHashLength :: m Int
        branchHashLength =
          Sqlite.runTransaction conn Ops2.branchHashLength

        termReferencesByPrefix :: ShortHash -> m (Set Reference.Id)
        termReferencesByPrefix sh =
          Sqlite.runTransaction conn (Ops2.termReferencesByPrefix sh)

        declReferencesByPrefix :: ShortHash -> m (Set Reference.Id)
        declReferencesByPrefix sh =
          Sqlite.runTransaction conn (Ops2.declReferencesByPrefix sh)

        referentsByPrefix :: ShortHash -> m (Set Referent.Id)
        referentsByPrefix sh =
          Sqlite.runTransaction conn (Ops2.referentsByPrefix (Sqlite.idempotentIO . getDeclTypeIO) sh)

        branchHashesByPrefix :: ShortBranchHash -> m (Set Branch.Hash)
        branchHashesByPrefix sh =
          Sqlite.runTransaction conn (Ops2.branchHashesByPrefix sh)

        sqlLca :: Branch.Hash -> Branch.Hash -> m (Maybe Branch.Hash)
        sqlLca h1 h2 =
          Sqlite.runTransaction conn (Ops2.sqlLca h1 h2)
    let codebase =
          C.Codebase
            (Cache.applyDefined termCache getTerm)
            (Cache.applyDefined typeOfTermCache getTypeOfTermImpl)
            (Cache.applyDefined declCache getTypeDeclaration)
            putTerm
            putTypeDeclaration
            -- _getTermComponent
            getTermComponentWithTypes
            getDeclComponent
            getCycleLength
            (getRootBranch rootBranchCache)
            getRootBranchExists
            (putRootBranch rootBranchCache)
            (rootBranchUpdates rootBranchCache)
            getBranchForHash
            putBranch
            isCausalHash
            getPatch
            putPatch
            patchExists
            dependentsImpl
            dependentsOfComponentImpl
            syncFromDirectory
            syncToDirectory
            viewRemoteBranch'
            (\r opts action -> pushGitBranch conn r opts action)
            watches
            getWatch
            putWatch
            clearWatches
            getReflog
            appendReflog
            termsOfTypeImpl
            termsMentioningTypeImpl
            hashLength
            termReferencesByPrefix
            declReferencesByPrefix
            referentsByPrefix
            branchHashLength
            branchHashesByPrefix
            (Just sqlLca)
            (Just \l r -> Sqlite.runTransaction conn $ fromJust <$> Ops2.before l r)
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

    flip finally finalizer $ do
      -- Migrate if necessary.
      ensureCodebaseIsUpToDate localOrRemote root conn codebase >>= \case
        Left err -> pure $ Left err
        Right () -> Right <$> action (codebase, conn)

syncInternal ::
  forall m.
  MonadUnliftIO m =>
  Sync.Progress m Sync22.Entity ->
  Connection ->
  Connection ->
  Branch m ->
  m ()
syncInternal progress srcConn destConn b = time "syncInternal" do
  UnliftIO runInIO <- askUnliftIO

  Sqlite.runReadOnlyTransactionIO srcConn \runSrc -> do
    Sqlite.runWriteTransactionIO destConn \runDest -> do
      let syncEnv = Sync22.Env runSrc runDest (16 * 1024 * 1024)
      -- we want to use sync22 wherever possible
      -- so for each source branch, we'll check if it exists in the destination codebase
      -- or if it exists in the source codebase, then we can sync22 it
      -- if it doesn't exist in the dest or source branch,
      -- then just use putBranch to the dest
      sync <- liftIO (Sync22.sync22 (Sync22.mapEnv lift syncEnv))
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
                (runDest (Ops2.isCausalHash h))
                do
                  when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " already exists in dest db"
                  processBranches rest
                do
                  when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " doesn't exist in dest db"
                  let h2 = CausalHash . Cv.hash1to2 $ Causal.unRawHash h
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
                          cs <- filterM (fmap not . Ops2.isCausalHash . fst) branchDeps
                          es <- filterM (fmap not . Ops2.patchExists) es
                          ts <- filterM (fmap not . Ops2.termExists) ts
                          ds <- filterM (fmap not . Ops2.declExists) ds
                          pure (cs, es, ts, ds)
                        if null cs && null es && null ts && null ds
                          then do
                            runDest (Ops2.putBranch (Branch.transform (Sqlite.idempotentIO . runInIO) b))
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
  = B Branch.Hash (m (Branch m))
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

viewRemoteBranch' ::
  forall m r.
  (MonadUnliftIO m) =>
  ReadRemoteNamespace ->
  Git.GitBranchBehavior ->
  ((Branch m, CodebasePath) -> m r) ->
  m (Either C.GitError r)
viewRemoteBranch' (repo, sbh, path) gitBranchBehavior action = UnliftIO.try $ do
  -- set up the cache dir
  time "Git fetch" $
    throwEitherMWith C.GitProtocolError . withRepo repo gitBranchBehavior $ \remoteRepo -> do
      let remotePath = Git.gitDirToPath remoteRepo
      -- Tickle the database before calling into `sqliteCodebase`; this covers the case that the database file either
      -- doesn't exist at all or isn't a SQLite database file, but does not cover the case that the database file itself
      -- is somehow corrupt, or not even a Unison database.
      --
      -- FIXME it would probably make more sense to define some proper preconditions on `sqliteCodebase`, and perhaps
      -- update its output type, which currently indicates the only way it can fail is with an `UnknownSchemaVersion`
      -- error.
      (withConnection "codebase exists check" remotePath \_ -> pure ()) `catch` \exception ->
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
  (MonadUnliftIO m) =>
  Connection ->
  WriteRepo ->
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
            Sqlite.Connection.withSavepoint destConn "push" \_rollback ->
              doSync codebaseStatus (Git.gitDirToPath pushStaging) srcConn destConn newBranch
            pure (Right newBranch)
    for newBranchOrErr $ push pushStaging repo
    pure newBranchOrErr
  where
    readRepo :: ReadRepo
    readRepo = writeToRead repo
    doSync :: CodebaseStatus -> FilePath -> Connection -> Connection -> Branch m -> m ()
    doSync codebaseStatus remotePath srcConn destConn newBranch = do
      progressStateRef <- liftIO (newIORef emptySyncProgressState)
      _ <- syncInternal (syncProgress progressStateRef) srcConn destConn newBranch
      when setRoot . liftIO $
        Sqlite.runTransactionWithAbort
          destConn
          (\abort -> overwriteRoot abort codebaseStatus remotePath newBranch)
    overwriteRoot ::
      (forall e x. Exception e => e -> Sqlite.Transaction x) ->
      CodebaseStatus ->
      FilePath ->
      Branch m ->
      Sqlite.Transaction ()
    overwriteRoot abort codebaseStatus remotePath newBranch = do
      let newBranchHash = Branch.headHash newBranch
      case codebaseStatus of
        ExistingCodebase -> do
          -- the call to runDB "handles" the possible DB error by bombing
          maybeOldRootHash <- fmap Cv.branchHash2to1 <$> Ops.loadRootCausalHash
          case maybeOldRootHash of
            Nothing -> setRepoRoot newBranchHash
            Just oldRootHash -> do
              Ops2.before oldRootHash newBranchHash >>= \case
                Nothing ->
                  error $
                    "I couldn't find the hash " ++ show newBranchHash
                      ++ " that I just synced to the cached copy of "
                      ++ repoString
                      ++ " in "
                      ++ show remotePath
                      ++ "."
                Just False ->
                  abort . C.GitProtocolError $ GitError.PushDestinationHasNewStuff repo
                Just True -> pure ()
        CreatedCodebase -> pure ()
      setRepoRoot newBranchHash
    repoString = Text.unpack $ printWriteRepo repo
    setRepoRoot :: Branch.Hash -> Sqlite.Transaction ()
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
    push :: forall n. MonadIO n => Git.GitRepo -> WriteRepo -> Branch m -> n Bool -- withIOError needs IO
    push remotePath repo@(WriteGitRepo {url' = url, branch = mayGitBranch}) newRootBranch = time "SqliteCodebase.pushGitRootBranch.push" $ do
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
            (successful, _stdout, stderr) <- gitInCaptured remotePath $ ["push", "--quiet", url] ++ maybe [] (pure @[]) mayGitBranch
            when (not successful) . throwIO $ GitError.PushException repo (Text.unpack stderr)
            pure True
