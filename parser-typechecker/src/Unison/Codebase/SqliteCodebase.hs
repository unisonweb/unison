{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.SqliteCodebase (Unison.Codebase.SqliteCodebase.init, unsafeGetConnection) where

import qualified Control.Concurrent
import qualified Control.Exception
import Control.Monad (filterM, unless, when, (>=>))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Extra (ifM, unlessM, (||^))
import qualified Control.Monad.Extra as Monad
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Bifunctor (Bifunctor (bimap, first), second)
import qualified Data.Either.Combinators as Either
import Data.Foldable (Foldable (toList), for_, traverse_)
import Data.Functor (void, (<&>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Traversable (for)
import qualified Data.Validation as Validation
import Data.Word (Word64)
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as Sqlite
import GHC.Stack (HasCallStack)
import qualified System.Console.ANSI as ANSI
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import U.Codebase.HashTags (CausalHash (CausalHash, unCausalHash))
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Sqlite.ObjectType as OT
import U.Codebase.Sqlite.Operations (EDB)
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Sync22 as Sync22
import qualified U.Codebase.Sync as Sync
import qualified U.Codebase.WatchKind as WK
import qualified U.Util.Hash as H2
import qualified U.Util.Monoid as Monoid
import qualified U.Util.Set as Set
import qualified Unison.Builtin as Builtins
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase1
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Editor.Git (gitIn, gitTextIn, pullBranch, withIOError, withStatus)
import Unison.Codebase.Editor.RemoteRepo (RemoteNamespace, RemoteRepo (GitRepo), printRepo)
import Unison.Codebase.GitError (GitError)
import qualified Unison.Codebase.GitError as GitError
import qualified Unison.Codebase.Init as Codebase
import qualified Unison.Codebase.Init as Codebase1
import Unison.Codebase.Patch (Patch)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Branch.Dependencies as BD
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.Codebase.SqliteCodebase.SyncEphemeral as SyncEphemeral
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as Decl
import Unison.Hash (Hash)
import Unison.Parser (Ann)
import Unison.Prelude (MaybeT (runMaybeT), fromMaybe, isJust, trace, traceM)
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import qualified Unison.ShortHash as ShortHash
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Pretty as P
import Unison.Util.Timing (time)
import UnliftIO (MonadIO, catchIO, liftIO)
import UnliftIO.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import UnliftIO.STM
import U.Codebase.Sqlite.DbId (SchemaVersion(SchemaVersion))

debug, debugProcessBranches :: Bool
debug = False
debugProcessBranches = False

codebasePath :: FilePath
codebasePath = ".unison" </> "v2" </> "unison.sqlite3"

v2dir :: FilePath -> FilePath
v2dir root = root </> ".unison" </> "v2"

init :: HasCallStack => MonadIO m => Codebase.Init m Symbol Ann
init = Codebase.Init getCodebaseOrError createCodebaseOrError v2dir

createCodebaseOrError ::
  MonadIO m =>
  CodebasePath ->
  m (Either Codebase1.CreateCodebaseError (Codebase m Symbol Ann))
createCodebaseOrError dir = do
  prettyDir <- P.string <$> canonicalizePath dir
  let convertError = \case
        CreateCodebaseAlreadyExists -> Codebase1.CreateCodebaseAlreadyExists
        CreateCodebaseUnknownSchemaVersion v -> Codebase1.CreateCodebaseOther $ prettyError v
      prettyError :: SchemaVersion -> Codebase1.Pretty
      prettyError v = P.wrap $
        "I don't know how to handle " <> P.shown v <> "in" <> P.backticked' prettyDir "."
  Either.mapLeft convertError <$> createCodebaseOrError' dir

data CreateCodebaseError
  = CreateCodebaseAlreadyExists
  | CreateCodebaseUnknownSchemaVersion SchemaVersion
  deriving (Show)

createCodebaseOrError' ::
  MonadIO m =>
  CodebasePath ->
  m (Either CreateCodebaseError (Codebase m Symbol Ann))
createCodebaseOrError' path = do
  ifM
    (doesFileExist $ path </> codebasePath)
    (pure $ Left CreateCodebaseAlreadyExists)
    do
      createDirectoryIfMissing True (path </> FilePath.takeDirectory codebasePath)
      liftIO $
        Control.Exception.bracket
          (unsafeGetConnection path)
          Sqlite.close
          (runReaderT do
            Q.createSchema
            runExceptT (void . Ops.saveRootBranch $ Cv.causalbranch1to2 Branch.empty) >>= \case
              Left e -> error $ show e
              Right () -> pure ()
            )

      fmap (Either.mapLeft CreateCodebaseUnknownSchemaVersion) (sqliteCodebase path)

-- get the codebase in dir
getCodebaseOrError :: forall m. MonadIO m => CodebasePath -> m (Either Codebase1.Pretty (Codebase m Symbol Ann))
getCodebaseOrError dir = do
  prettyDir <- liftIO $ P.string <$> canonicalizePath dir
  let prettyError v = P.wrap $ "I don't know how to handle " <> P.shown v <> "in" <> P.backticked' prettyDir "."
  fmap (Either.mapLeft prettyError) (sqliteCodebase dir)

initSchemaIfNotExist :: MonadIO m => FilePath -> m ()
initSchemaIfNotExist path = liftIO do
  unlessM (doesDirectoryExist $ path </> FilePath.takeDirectory codebasePath) $
    createDirectoryIfMissing True (path </> FilePath.takeDirectory codebasePath)
  unlessM (doesFileExist $ path </> codebasePath) $
    Control.Exception.bracket
      (unsafeGetConnection path)
      Sqlite.close
      (runReaderT Q.createSchema)

-- checks if a db exists at `path` with the minimum schema
codebaseExists :: MonadIO m => CodebasePath -> m Bool
codebaseExists root = liftIO do
  Monad.when debug $ traceM $ "codebaseExists " ++ root
  Control.Exception.catch @Sqlite.SQLError
    ( sqliteCodebase root >>= \case
        Left _ -> pure False
        Right _ -> pure True
    )
    (const $ pure False)

-- 1) buffer up the component
-- 2) in the event that the component is complete, then what?
--  * can write component provided all of its dependency components are complete.
--    if dependency not complete,
--    register yourself to be written when that dependency is complete

-- an entry for a single hash
data BufferEntry a = BufferEntry
  { -- First, you are waiting for the cycle to fill up with all elements
    -- Then, you check: are all dependencies of the cycle in the db?
    --   If yes: write yourself to database and trigger check of dependents
    --   If no: just wait, do nothing
    beComponentTargetSize :: Maybe Word64,
    beComponent :: Map Reference.Pos a,
    beMissingDependencies :: Set Hash,
    beWaitingDependents :: Set Hash
  }
  deriving (Eq, Show)

prettyBufferEntry :: Show a => Hash -> BufferEntry a -> String
prettyBufferEntry (h :: Hash) BufferEntry {..} =
  "BufferEntry " ++ show h ++ "\n"
    ++ "  { beComponentTargetSize = "
    ++ show beComponentTargetSize
    ++ "\n"
    ++ "  , beComponent = "
    ++ if Map.size beComponent < 2
      then show $ Map.toList beComponent
      else
        mkString (Map.toList beComponent) (Just "\n      [ ") "      , " (Just "]\n")
          ++ "  , beMissingDependencies ="
          ++ if Set.size beMissingDependencies < 2
            then show $ Set.toList beMissingDependencies
            else
              mkString (Set.toList beMissingDependencies) (Just "\n      [ ") "      , " (Just "]\n")
                ++ "  , beWaitingDependents ="
                ++ if Set.size beWaitingDependents < 2
                  then show $ Set.toList beWaitingDependents
                  else
                    mkString (Set.toList beWaitingDependents) (Just "\n      [ ") "      , " (Just "]\n")
                      ++ "  }"
  where
    mkString :: (Foldable f, Show a) => f a -> Maybe String -> String -> Maybe String -> String
    mkString as start middle end = fromMaybe "" start ++ List.intercalate middle (show <$> toList as) ++ fromMaybe "" end

type TermBufferEntry = BufferEntry (Term Symbol Ann, Type Symbol Ann)

type DeclBufferEntry = BufferEntry (Decl Symbol Ann)

unsafeGetConnection :: MonadIO m => CodebasePath -> m Sqlite.Connection
unsafeGetConnection root = do
  Monad.when debug $ traceM $ "unsafeGetconnection " ++ root ++ " -> " ++ (root </> codebasePath)
  conn <- liftIO . Sqlite.open $ root </> codebasePath
  runReaderT Q.setFlags conn
  pure conn

sqliteCodebase :: MonadIO m => CodebasePath -> m (Either SchemaVersion (Codebase m Symbol Ann))
sqliteCodebase root = do
  Monad.when debug $ traceM $ "sqliteCodebase " ++ root
  conn <- unsafeGetConnection root
  runReaderT Q.schemaVersion conn >>= \case
    SchemaVersion 1 -> do
      rootBranchCache <- newTVarIO Nothing
      -- The v1 codebase interface has operations to read and write individual definitions
      -- whereas the v2 codebase writes them as complete components.  These two fields buffer
      -- the individual definitions until a complete component has been written.
      termBuffer :: TVar (Map Hash TermBufferEntry) <- newTVarIO Map.empty
      declBuffer :: TVar (Map Hash DeclBufferEntry) <- newTVarIO Map.empty
      let getTerm :: MonadIO m => Reference.Id -> m (Maybe (Term Symbol Ann))
          getTerm (Reference.Id h1@(Cv.hash1to2 -> h2) i _n) =
            runDB' conn do
              term2 <- Ops.loadTermByReference (C.Reference.Id h2 i)
              Cv.term2to1 h1 (getCycleLen "getTerm") getDeclType term2

          getCycleLen :: EDB m => String -> Hash -> m Reference.Size
          getCycleLen source h = do
            (Ops.getCycleLen . Cv.hash1to2) h `Except.catchError` \case
              e@(Ops.DatabaseIntegrityError (Q.NoObjectForPrimaryHashId {})) -> error $ show e ++ " in " ++ source
              e -> Except.throwError e

          getDeclType :: EDB m => C.Reference.Reference -> m CT.ConstructorType
          getDeclType = \case
            C.Reference.ReferenceBuiltin t ->
              let err =
                    error $
                      "I don't know about the builtin type ##"
                        ++ show t
                        ++ ", but I've been asked for it's ConstructorType."
               in pure . fromMaybe err $
                    Map.lookup (Reference.Builtin t) Builtins.builtinConstructorType
            C.Reference.ReferenceDerived i -> getDeclTypeById i

          getDeclTypeById :: EDB m => C.Reference.Id -> m CT.ConstructorType
          getDeclTypeById = fmap Cv.decltype2to1 . Ops.getDeclTypeByReference

          getTypeOfTermImpl :: MonadIO m => Reference.Id -> m (Maybe (Type Symbol Ann))
          getTypeOfTermImpl id | debug && trace ("getTypeOfTermImpl " ++ show id) False = undefined
          getTypeOfTermImpl (Reference.Id (Cv.hash1to2 -> h2) i _n) =
            runDB' conn do
              type2 <- Ops.loadTypeOfTermByTermReference (C.Reference.Id h2 i)
              Cv.ttype2to1 (getCycleLen "getTypeOfTermImpl") type2

          getTypeDeclaration :: MonadIO m => Reference.Id -> m (Maybe (Decl Symbol Ann))
          getTypeDeclaration (Reference.Id h1@(Cv.hash1to2 -> h2) i _n) =
            runDB' conn do
              decl2 <- Ops.loadDeclByReference (C.Reference.Id h2 i)
              Cv.decl2to1 h1 (getCycleLen "getTypeDeclaration") decl2

          putTerm :: MonadIO m => Reference.Id -> Term Symbol Ann -> Type Symbol Ann -> m ()
          putTerm id tm tp | debug && trace (show "SqliteCodebase.putTerm " ++ show id ++ " " ++ show tm ++ " " ++ show tp) False = undefined
          putTerm (Reference.Id h@(Cv.hash1to2 -> h2) i n') tm tp =
            runDB conn $
              unlessM
                (Ops.objectExistsForHash h2 >>= if debug then \b -> do traceM $ "objectExistsForHash " ++ show h2 ++ " = " ++ show b; pure b else pure)
                ( withBuffer termBuffer h \be@(BufferEntry size comp missing waiting) -> do
                    Monad.when debug $ traceM $ "adding to BufferEntry" ++ show be
                    let size' = Just n'
                    -- if size was previously set, it's expected to match size'.
                    case size of
                      Just n
                        | n /= n' ->
                          error $ "targetSize for term " ++ show h ++ " was " ++ show size ++ ", but now " ++ show size'
                      _ -> pure ()
                    let comp' = Map.insert i (tm, tp) comp
                    -- for the component element that's been passed in, add its dependencies to missing'
                    missingTerms' <-
                      filterM
                        (fmap not . Ops.objectExistsForHash . Cv.hash1to2)
                        [h | Reference.Derived h _i _n <- Set.toList $ Term.termDependencies tm]
                    missingTypes' <-
                      filterM (fmap not . Ops.objectExistsForHash . Cv.hash1to2) $
                        [h | Reference.Derived h _i _n <- Set.toList $ Term.typeDependencies tm]
                          ++ [h | Reference.Derived h _i _n <- Set.toList $ Type.dependencies tp]
                    let missing' = missing <> Set.fromList (missingTerms' <> missingTypes')
                    -- notify each of the dependencies that h depends on them.
                    traverse (addBufferDependent h termBuffer) missingTerms'
                    traverse (addBufferDependent h declBuffer) missingTypes'
                    putBuffer termBuffer h (BufferEntry size' comp' missing' waiting)
                    tryFlushTermBuffer h
                )

          putBuffer :: (MonadIO m, Show a) => TVar (Map Hash (BufferEntry a)) -> Hash -> BufferEntry a -> m ()
          putBuffer tv h e = do
            Monad.when debug $ traceM $ "putBuffer " ++ prettyBufferEntry h e
            atomically $ modifyTVar tv (Map.insert h e)

          withBuffer :: (MonadIO m, Show a) => TVar (Map Hash (BufferEntry a)) -> Hash -> (BufferEntry a -> m b) -> m b
          withBuffer tv h f = do
            Monad.when debug $ readTVarIO tv >>= \tv -> traceM $ "tv = " ++ show tv
            Map.lookup h <$> readTVarIO tv >>= \case
              Just e -> do
                Monad.when debug $ traceM $ "SqliteCodebase.withBuffer " ++ prettyBufferEntry h e
                f e
              Nothing -> do
                Monad.when debug $ traceM $ "SqliteCodebase.with(new)Buffer " ++ show h
                f (BufferEntry Nothing Map.empty Set.empty Set.empty)

          removeBuffer :: (MonadIO m, Show a) => TVar (Map Hash (BufferEntry a)) -> Hash -> m ()
          removeBuffer _tv h | debug && trace ("removeBuffer " ++ show h) False = undefined
          removeBuffer tv h = do
            Monad.when debug $ readTVarIO tv >>= \tv -> traceM $ "before delete: " ++ show tv
            atomically $ modifyTVar tv (Map.delete h)
            Monad.when debug $ readTVarIO tv >>= \tv -> traceM $ "after delete: " ++ show tv

          addBufferDependent :: (MonadIO m, Show a) => Hash -> TVar (Map Hash (BufferEntry a)) -> Hash -> m ()
          addBufferDependent dependent tv dependency = withBuffer tv dependency \be -> do
            putBuffer tv dependency be {beWaitingDependents = Set.insert dependent $ beWaitingDependents be}
          tryFlushBuffer ::
            (EDB m, Show a) =>
            TVar (Map Hash (BufferEntry a)) ->
            (H2.Hash -> [a] -> m ()) ->
            (Hash -> m ()) ->
            Hash ->
            m ()
          tryFlushBuffer _ _ _ h | debug && trace ("tryFlushBuffer " ++ show h) False = undefined
          tryFlushBuffer buf saveComponent tryWaiting h@(Cv.hash1to2 -> h2) =
            -- skip if it has already been flushed
            unlessM (Ops.objectExistsForHash h2) $ withBuffer buf h try
            where
              try (BufferEntry size comp (Set.delete h -> missing) waiting) = case size of
                Just size -> do
                  missing' <-
                    filterM
                      (fmap not . Ops.objectExistsForHash . Cv.hash1to2)
                      (toList missing)
                  Monad.when debug do
                    traceM $ "tryFlushBuffer.missing' = " ++ show missing'
                    traceM $ "tryFlushBuffer.size = " ++ show size
                    traceM $ "tryFlushBuffer.length comp = " ++ show (length comp)
                  if null missing' && size == fromIntegral (length comp)
                    then do
                      saveComponent h2 (toList comp)
                      removeBuffer buf h
                      Monad.when debug $ traceM $ "tryFlushBuffer.notify waiting " ++ show waiting
                      traverse_ tryWaiting waiting
                    else -- update

                      putBuffer buf h $
                        BufferEntry (Just size) comp (Set.fromList missing') waiting
                Nothing ->
                  -- it's never even been added, so there's nothing to do.
                  pure ()

          tryFlushTermBuffer :: EDB m => Hash -> m ()
          tryFlushTermBuffer h | debug && trace ("tryFlushTermBuffer " ++ show h) False = undefined
          tryFlushTermBuffer h =
            tryFlushBuffer
              termBuffer
              ( \h2 ->
                  void . Ops.saveTermComponent h2
                    . fmap (first (Cv.term1to2 h) . second Cv.ttype1to2)
              )
              tryFlushTermBuffer
              h

          tryFlushDeclBuffer :: EDB m => Hash -> m ()
          tryFlushDeclBuffer h | debug && trace ("tryFlushDeclBuffer " ++ show h) False = undefined
          tryFlushDeclBuffer h =
            tryFlushBuffer
              declBuffer
              (\h2 -> void . Ops.saveDeclComponent h2 . fmap (Cv.decl1to2 h))
              (\h -> tryFlushTermBuffer h >> tryFlushDeclBuffer h)
              h

          putTypeDeclaration :: MonadIO m => Reference.Id -> Decl Symbol Ann -> m ()
          putTypeDeclaration (Reference.Id h@(Cv.hash1to2 -> h2) i n') decl =
            runDB conn $
              unlessM
                (Ops.objectExistsForHash h2)
                ( withBuffer declBuffer h \(BufferEntry size comp missing waiting) -> do
                    let size' = Just n'
                    case size of
                      Just n
                        | n /= n' ->
                          error $ "targetSize for type " ++ show h ++ " was " ++ show size ++ ", but now " ++ show size'
                      _ -> pure ()
                    let comp' = Map.insert i decl comp
                    moreMissing <-
                      filterM (fmap not . Ops.objectExistsForHash . Cv.hash1to2) $
                        [h | Reference.Derived h _i _n <- Set.toList $ Decl.declDependencies decl]
                    let missing' = missing <> Set.fromList moreMissing
                    traverse (addBufferDependent h declBuffer) moreMissing
                    putBuffer declBuffer h (BufferEntry size' comp' missing' waiting)
                    tryFlushDeclBuffer h
                )

          getRootBranch :: MonadIO m => TVar (Maybe (Q.DataVersion, Branch m)) -> m (Either Codebase1.GetRootBranchError (Branch m))
          getRootBranch rootBranchCache =
            readTVarIO rootBranchCache >>= \case
              Nothing -> forceReload
              Just (v, b) -> do
                -- check to see if root namespace hash has been externally modified
                -- and reload it if necessary
                v' <- runDB conn Ops.dataVersion
                if v == v' then pure (Right b) else do
                  newRootHash <- runDB conn Ops.loadRootCausalHash
                  if Branch.headHash b == Cv.branchHash2to1 newRootHash
                    then pure (Right b)
                    else do
                      traceM $ "database was externally modified (" ++ show v ++ " -> " ++ show v' ++ ")"
                      forceReload
            where
              forceReload = do
                b <- fmap (Either.mapLeft err)
                    . runExceptT
                    . flip runReaderT conn
                    . fmap (Branch.transform (runDB conn))
                    $ Cv.causalbranch2to1 getCycleLen getDeclType =<< Ops.loadRootCausal
                v <- runDB conn Ops.dataVersion
                for_ b (atomically . writeTVar rootBranchCache . Just . (v,))
                pure b
              err :: Ops.Error -> Codebase1.GetRootBranchError
              err = \case
                Ops.DatabaseIntegrityError Q.NoNamespaceRoot ->
                  Codebase1.NoRootBranch
                Ops.DecodeError (Ops.ErrBranch oId) _bytes _msg ->
                  Codebase1.CouldntParseRootBranch $
                    "Couldn't decode " ++ show oId ++ ": " ++ _msg
                Ops.ExpectedBranch ch _bh ->
                  Codebase1.CouldntLoadRootBranch $ Cv.causalHash2to1 ch
                e -> error $ show e

          putRootBranch :: MonadIO m => TVar (Maybe (Q.DataVersion, Branch m)) -> Branch m -> m ()
          putRootBranch rootBranchCache branch1 = do
            -- todo: check to see if root namespace hash has been externally modified
            -- and do something (merge?) it if necessary. But for now, we just overwrite it.
            runDB conn
              . void
              . Ops.saveRootBranch
              . Cv.causalbranch1to2
              $ Branch.transform (lift . lift) branch1
            atomically $ modifyTVar rootBranchCache (fmap . second $ const branch1)

          rootBranchUpdates :: MonadIO m => TVar (Maybe (Q.DataVersion, a)) -> m (IO (), IO (Set Branch.Hash))
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
          getBranchForHash :: MonadIO m => Branch.Hash -> m (Maybe (Branch m))
          getBranchForHash h = runDB conn do
            Ops.loadCausalBranchByCausalHash (Cv.branchHash1to2 h) >>= \case
              Just b ->
                pure . Just . Branch.transform (runDB conn)
                  =<< Cv.causalbranch2to1 getCycleLen getDeclType b
              Nothing -> pure Nothing

          putBranch :: MonadIO m => Branch m -> m ()
          putBranch branch1 =
            runDB conn
              . void
              . Ops.saveBranch
              . Cv.causalbranch1to2
              $ Branch.transform (lift . lift) branch1

          isCausalHash :: MonadIO m => Branch.Hash -> m Bool
          isCausalHash (Causal.RawHash h) =
            runDB conn $
              Q.loadHashIdByHash (Cv.hash1to2 h) >>= \case
                Nothing -> pure False
                Just hId -> Q.isCausalHash hId

          getPatch :: MonadIO m => Branch.EditHash -> m (Maybe Patch)
          getPatch h =
            runDB conn . runMaybeT $
              MaybeT (Ops.primaryHashToMaybePatchObjectId (Cv.patchHash1to2 h))
                >>= Ops.loadPatchById
                >>= Cv.patch2to1 getCycleLen

          putPatch :: MonadIO m => Branch.EditHash -> Patch -> m ()
          putPatch h p =
            runDB conn . void $
              Ops.savePatch (Cv.patchHash1to2 h) (Cv.patch1to2 p)

          patchExists :: MonadIO m => Branch.EditHash -> m Bool
          patchExists h =
            runDB conn . fmap isJust $
              Ops.primaryHashToMaybePatchObjectId (Cv.patchHash1to2 h)

          dependentsImpl :: MonadIO m => Reference -> m (Set Reference.Id)
          dependentsImpl r =
            runDB conn $
              Set.traverse (Cv.referenceid2to1 (getCycleLen "dependentsImpl"))
                =<< Ops.dependents (Cv.reference1to2 r)

          syncFromDirectory :: MonadIO m => Codebase1.CodebasePath -> SyncMode -> Branch m -> m ()
          syncFromDirectory srcRoot syncMode b =
            flip State.evalStateT emptySyncProgressState $
              syncToDirectory' syncProgress srcRoot root syncMode $
                Branch.transform lift b

          syncToDirectory :: MonadIO m => Codebase1.CodebasePath -> SyncMode -> Branch m -> m ()
          syncToDirectory destRoot syncMode b =
            flip State.evalStateT emptySyncProgressState $
              syncToDirectory' syncProgress root destRoot syncMode $
                Branch.transform lift b

          watches :: MonadIO m => UF.WatchKind -> m [Reference.Id]
          watches w =
            runDB conn $
              Ops.listWatches (Cv.watchKind1to2 w)
                >>= traverse (Cv.referenceid2to1 (getCycleLen "watches"))

          getWatch :: MonadIO m => UF.WatchKind -> Reference.Id -> m (Maybe (Term Symbol Ann))
          getWatch k r@(Reference.Id h _i _n)
            | elem k standardWatchKinds =
              runDB' conn $
                Ops.loadWatch (Cv.watchKind1to2 k) (Cv.referenceid1to2 r)
                  >>= Cv.term2to1 h (getCycleLen "getWatch") getDeclType
          getWatch _unknownKind _ = pure Nothing

          standardWatchKinds = [UF.RegularWatch, UF.TestWatch]

          putWatch :: MonadIO m => UF.WatchKind -> Reference.Id -> Term Symbol Ann -> m ()
          putWatch k r@(Reference.Id h _i _n) tm
            | elem k standardWatchKinds =
              runDB conn $
                Ops.saveWatch
                  (Cv.watchKind1to2 k)
                  (Cv.referenceid1to2 r)
                  (Cv.term1to2 h tm)
          putWatch _unknownKind _ _ = pure ()

          getReflog :: MonadIO m => m [Reflog.Entry]
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

          appendReflog :: MonadIO m => Text -> Branch m -> Branch m -> m ()
          appendReflog reason old new =
            liftIO $ TextIO.appendFile (reflogPath root) (t <> "\n")
            where
              t = Reflog.toText $ Reflog.Entry (Branch.headHash old) (Branch.headHash new) reason

          reflogPath :: CodebasePath -> FilePath
          reflogPath root = root </> "reflog"

          termsOfTypeImpl :: MonadIO m => Reference -> m (Set Referent.Id)
          termsOfTypeImpl r =
            runDB conn $
              Ops.termsHavingType (Cv.reference1to2 r)
                >>= Set.traverse (Cv.referentid2to1 (getCycleLen "termsOfTypeImpl") getDeclType)

          termsMentioningTypeImpl :: MonadIO m => Reference -> m (Set Referent.Id)
          termsMentioningTypeImpl r =
            runDB conn $
              Ops.termsMentioningType (Cv.reference1to2 r)
                >>= Set.traverse (Cv.referentid2to1 (getCycleLen "termsMentioningTypeImpl") getDeclType)

          hashLength :: Applicative m => m Int
          hashLength = pure 10

          branchHashLength :: Applicative m => m Int
          branchHashLength = pure 10

          defnReferencesByPrefix :: MonadIO m => OT.ObjectType -> ShortHash -> m (Set Reference.Id)
          defnReferencesByPrefix _ (ShortHash.Builtin _) = pure mempty
          defnReferencesByPrefix ot (ShortHash.ShortHash prefix (fmap Cv.shortHashSuffix1to2 -> cycle) _cid) =
            Monoid.fromMaybe <$> runDB' conn do
              refs <- do
                Ops.componentReferencesByPrefix ot prefix cycle
                  >>= traverse (C.Reference.idH Ops.loadHashByObjectId)
                  >>= pure . Set.fromList

              Set.fromList <$> traverse (Cv.referenceid2to1 (getCycleLen "defnReferencesByPrefix")) (Set.toList refs)

          termReferencesByPrefix :: MonadIO m => ShortHash -> m (Set Reference.Id)
          termReferencesByPrefix = defnReferencesByPrefix OT.TermComponent

          declReferencesByPrefix :: MonadIO m => ShortHash -> m (Set Reference.Id)
          declReferencesByPrefix = defnReferencesByPrefix OT.DeclComponent

          referentsByPrefix :: MonadIO m => ShortHash -> m (Set Referent.Id)
          referentsByPrefix SH.Builtin {} = pure mempty
          referentsByPrefix (SH.ShortHash prefix (fmap Cv.shortHashSuffix1to2 -> cycle) cid) = runDB conn do
            termReferents <-
              Ops.termReferentsByPrefix prefix cycle
                >>= traverse (Cv.referentid2to1 (getCycleLen "referentsByPrefix") getDeclType)
            declReferents' <- Ops.declReferentsByPrefix prefix cycle (read . Text.unpack <$> cid)
            let declReferents =
                  [ Referent.Con' (Reference.Id (Cv.hash2to1 h) pos len) (fromIntegral cid) (Cv.decltype2to1 ct)
                    | (h, pos, len, ct, cids) <- declReferents',
                      cid <- cids
                  ]
            pure . Set.fromList $ termReferents <> declReferents

          branchHashesByPrefix :: MonadIO m => ShortBranchHash -> m (Set Branch.Hash)
          branchHashesByPrefix sh = runDB conn do
            -- given that a Branch is shallow, it's really `CausalHash` that you'd
            -- refer to to specify a full namespace w/ history.
            -- but do we want to be able to refer to a namespace without its history?
            cs <- Ops.causalHashesByPrefix (Cv.sbh1to2 sh)
            pure $ Set.map (Causal.RawHash . Cv.hash2to1 . unCausalHash) cs

          -- does destPath need to be a codebase?
          syncToDirectory' ::
            forall m.
            MonadIO m =>
            Sync.Progress m Sync22.Entity ->
            CodebasePath ->
            CodebasePath ->
            SyncMode ->
            Branch m ->
            m ()
          syncToDirectory' progress srcPath destPath _mode b = do
            result <- runExceptT do
              initSchemaIfNotExist destPath
              syncEnv@(Sync22.Env srcConn _ _) <-
                Sync22.Env
                  <$> unsafeGetConnection srcPath
                  <*> unsafeGetConnection destPath
                  <*> pure (16 * 1024 * 1024)
              src <-
                lift (sqliteCodebase srcPath)
                  >>= Except.liftEither . Either.mapLeft SyncEphemeral.SrcWrongSchema
              dest <-
                lift (sqliteCodebase destPath)
                  >>= Except.liftEither . Either.mapLeft SyncEphemeral.DestWrongSchema
              -- we want to use sync22 wherever possible
              -- so for each branch, we'll check if it exists in the destination branch
              -- or if it exists in the source branch, then we can sync22 it
              -- oh god but we have to figure out the dbid
              -- if it doesn't exist in the dest or source branch,
              -- then just use putBranch to the dest
              let se :: forall m a. Functor m => (ExceptT Sync22.Error m a -> ExceptT SyncEphemeral.Error m a)
                  se = Except.withExceptT SyncEphemeral.Sync22Error
              let r :: forall m a. (ReaderT Sync22.Env m a -> m a)
                  r = flip runReaderT syncEnv
                  processBranches ::
                    forall m v a.
                    MonadIO m =>
                    Sync.Sync (ReaderT Sync22.Env (ExceptT Sync22.Error m)) Sync22.Entity ->
                    Sync.Progress (ReaderT Sync22.Env (ExceptT Sync22.Error m)) Sync22.Entity ->
                    Codebase1.Codebase m v a ->
                    Codebase1.Codebase m v a ->
                    [Entity m] ->
                    ExceptT Sync22.Error m ()
                  processBranches _ _ _ _ [] = pure ()
                  processBranches sync progress src dest (B h mb : rest) = do
                    when debugProcessBranches $ traceM $ "processBranches B " ++ take 10 (show h)
                    ifM @(ExceptT Sync22.Error m)
                      (lift $ Codebase1.branchExists dest h)
                      do
                          when debugProcessBranches $ traceM "  already exists in dest db"
                          processBranches sync progress src dest rest
                      do
                          when debugProcessBranches $ traceM "  doesn't exist in dest db"
                          let h2 = CausalHash . Cv.hash1to2 $ Causal.unRawHash h
                          lift (flip runReaderT srcConn (Q.loadCausalHashIdByCausalHash h2)) >>= \case
                            Just chId -> do
                              when debugProcessBranches $ traceM $ "  exists in source db, so delegating to direct sync"
                              r $ Sync.sync sync progress [Sync22.C chId]
                              processBranches sync progress src dest rest
                            Nothing ->
                              lift mb >>= \b -> do
                                when debugProcessBranches $ traceM $ "  doesn't exist in either db, so delegating to Codebase.putBranch"
                                let (branchDeps, BD.to' -> BD.Dependencies' es ts ds) = BD.fromBranch b
                                if null branchDeps && null es && null ts && null ds
                                  then lift $ Codebase1.putBranch dest b
                                  else
                                    let bs = map (uncurry B) branchDeps
                                        os = map O (es <> ts <> ds)
                                     in processBranches @m sync progress src dest (os ++ bs ++ B h mb : rest)
                  processBranches sync progress src dest (O h : rest) = do
                    when debugProcessBranches $ traceM $ "processBranches O " ++ take 10 (show h)
                    (runExceptT $ flip runReaderT srcConn (Q.expectHashIdByHash (Cv.hash1to2 h) >>= Q.expectObjectIdForAnyHashId)) >>= \case
                      Left e -> error $ show e
                      Right oId -> do
                        r $ Sync.sync sync progress [Sync22.O oId]
                        processBranches sync progress src dest rest
              sync <- se . r $ Sync22.sync22
              let progress' = Sync.transformProgress (lift . lift) progress
                  bHash = Branch.headHash b
              se $ processBranches sync progress' src dest [B bHash (pure b)]
              testWatchRefs <- lift . fmap concat $ for [WK.TestWatch] \wk ->
                fmap (Sync22.W wk) <$> flip runReaderT srcConn (Q.loadWatchesByWatchKind wk)
              se . r $ Sync.sync sync progress' testWatchRefs
            pure $ Validation.valueOr (error . show) result

      -- we don't currently have any good opportunity to call this sanity check;
      -- at ucm shutdown
      let _finalizer :: MonadIO m => m ()
          _finalizer = do
            decls <- readTVarIO declBuffer
            terms <- readTVarIO termBuffer
            let printBuffer header b =
                  liftIO
                    if b /= mempty
                      then putStrLn header >> putStrLn "" >> print b
                      else pure ()
            printBuffer "Decls:" decls
            printBuffer "Terms:" terms

      pure . Right $
        ( Codebase1.Codebase
            getTerm
            getTypeOfTermImpl
            getTypeDeclaration
            putTerm
            putTypeDeclaration
            (getRootBranch rootBranchCache)
            (putRootBranch rootBranchCache)
            (rootBranchUpdates rootBranchCache)
            getBranchForHash
            putBranch
            isCausalHash
            getPatch
            putPatch
            patchExists
            dependentsImpl
            syncFromDirectory
            syncToDirectory
            viewRemoteBranch'
            (pushGitRootBranch syncToDirectory)
            watches
            getWatch
            putWatch
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
        )
    v -> pure . Left $ v

runDB' :: MonadIO m => Connection -> MaybeT (ReaderT Connection (ExceptT Ops.Error m)) a -> m (Maybe a)
runDB' conn = runDB conn . runMaybeT

runDB :: MonadIO m => Connection -> ReaderT Connection (ExceptT Ops.Error m) a -> m a
runDB conn = (runExceptT >=> err) . flip runReaderT conn
  where
    err = \case Left err -> error $ show err; Right a -> pure a

data Entity m
  = B Branch.Hash (m (Branch m))
  | O Hash

data SyncProgressState = SyncProgressState
  { _needEntities :: Maybe (Set Sync22.Entity),
    _doneEntities :: Either Int (Set Sync22.Entity),
    _warnEntities :: Either Int (Set Sync22.Entity)
  }

emptySyncProgressState :: SyncProgressState
emptySyncProgressState = SyncProgressState (Just mempty) (Right mempty) (Right mempty)

syncProgress :: MonadState SyncProgressState m => MonadIO m => Sync.Progress m Sync22.Entity
syncProgress = Sync.Progress need done warn allDone
  where
    quiet = False
    maxTrackedHashCount = 1024 * 1024
    size :: SyncProgressState -> Int
    size = \case
      SyncProgressState Nothing (Left i) (Left j) -> i + j
      SyncProgressState (Just need) (Right done) (Right warn) -> Set.size need + Set.size done + Set.size warn
      SyncProgressState _ _ _ -> undefined

    need, done, warn :: (MonadState SyncProgressState m, MonadIO m) => Sync22.Entity -> m ()
    need h = do
      unless quiet $ Monad.whenM (State.gets size <&> (== 0)) $ liftIO $ putStr "\n"
      State.get >>= \case
        SyncProgressState Nothing Left {} Left {} -> pure ()
        SyncProgressState (Just need) (Right done) (Right warn) ->
          if Set.size need + Set.size done + Set.size warn > maxTrackedHashCount
            then State.put $ SyncProgressState Nothing (Left $ Set.size done) (Left $ Set.size warn)
            else
              if Set.member h done || Set.member h warn
                then pure ()
                else State.put $ SyncProgressState (Just $ Set.insert h need) (Right done) (Right warn)
        SyncProgressState _ _ _ -> undefined
      unless quiet printSynced

    done h = do
      unless quiet $ Monad.whenM (State.gets size <&> (== 0)) $ liftIO $ putStr "\n"
      State.get >>= \case
        SyncProgressState Nothing (Left done) warn ->
          State.put $ SyncProgressState Nothing (Left (done + 1)) warn
        SyncProgressState (Just need) (Right done) warn ->
          State.put $ SyncProgressState (Just $ Set.delete h need) (Right $ Set.insert h done) warn
        SyncProgressState _ _ _ -> undefined
      unless quiet printSynced

    warn h = do
      unless quiet $ Monad.whenM (State.gets size <&> (== 0)) $ liftIO $ putStr "\n"
      State.get >>= \case
        SyncProgressState Nothing done (Left warn) ->
          State.put $ SyncProgressState Nothing done (Left $ warn + 1)
        SyncProgressState (Just need) done (Right warn) ->
          State.put $ SyncProgressState (Just $ Set.delete h need) done (Right $ Set.insert h warn)
        SyncProgressState _ _ _ -> undefined
      unless quiet printSynced

    allDone = do
      State.get >>= liftIO . putStr . renderState ("Done syncing ")
      liftIO ANSI.showCursor

    printSynced :: (MonadState SyncProgressState m, MonadIO m) => m ()
    printSynced = liftIO ANSI.hideCursor >> State.get >>= liftIO . putStr . (\s -> renderState "Synced " s)

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
  forall m.
  MonadIO m =>
  RemoteNamespace ->
  m (Either GitError (Branch m, CodebasePath))
viewRemoteBranch' (repo, sbh, path) = runExceptT do
  -- set up the cache dir
  remotePath <- time "Git fetch" $ pullBranch repo
  ifM
    (codebaseExists remotePath)
    do
        codebase <-
          lift (sqliteCodebase remotePath)
            >>= Validation.valueOr (\_missingSchema -> throwError $ GitError.CouldntOpenCodebase repo remotePath) . fmap pure
        -- try to load the requested branch from it
        branch <- time "Git fetch (sbh)" $ case sbh of
          -- load the root branch
          Nothing ->
            lift (Codebase1.getRootBranch codebase) >>= \case
              Left Codebase1.NoRootBranch -> pure Branch.empty
              Left (Codebase1.CouldntLoadRootBranch h) ->
                throwError $ GitError.CouldntLoadRootBranch repo h
              Left (Codebase1.CouldntParseRootBranch s) ->
                throwError $ GitError.CouldntParseRootBranch repo s
              Right b -> pure b
          -- load from a specific `ShortBranchHash`
          Just sbh -> do
            branchCompletions <- lift $ Codebase1.branchHashesByPrefix codebase sbh
            case toList branchCompletions of
              [] -> throwError $ GitError.NoRemoteNamespaceWithHash repo sbh
              [h] ->
                (lift $ Codebase1.getBranchForHash codebase h) >>= \case
                  Just b -> pure b
                  Nothing -> throwError $ GitError.NoRemoteNamespaceWithHash repo sbh
              _ -> throwError $ GitError.RemoteNamespaceHashAmbiguous repo sbh branchCompletions
        pure (Branch.getAt' path branch, remotePath)
    -- else there's no initialized codebase at this repo; we pretend there's an empty one.
    (pure (Branch.empty, remotePath))

-- Given a branch that is "after" the existing root of a given git repo,
-- stage and push the branch (as the new root) + dependencies to the repo.
pushGitRootBranch ::
  MonadIO m =>
  Codebase1.SyncToDir m ->
  Branch m ->
  RemoteRepo ->
  SyncMode ->
  m (Either GitError ())
pushGitRootBranch syncToDirectory branch repo syncMode = runExceptT do
  -- Pull the remote repo into a staging directory
  (remoteRoot, remotePath) <- Except.ExceptT $ viewRemoteBranch' (repo, Nothing, Path.empty)
  ifM
    (pure (remoteRoot == Branch.empty) ||^ lift (remoteRoot `Branch.before` branch))
    -- ours is newer 👍, meaning this is a fast-forward push,
    -- so sync branch to staging area
    (stageAndPush remotePath)
    (throwError $ GitError.PushDestinationHasNewStuff repo)
  where
    -- | this will bomb if `h` is not a causal in the codebase
    setRepoRoot :: MonadIO m => CodebasePath -> Branch.Hash -> m ()
    setRepoRoot root h = do
      conn <- unsafeGetConnection root
      let h2 = Cv.causalHash1to2 h
          err = error $ "Called SqliteCodebase.setNamespaceRoot on unknown causal hash " ++ show h2
      flip runReaderT conn $ do
        chId <- fromMaybe err <$> Q.loadCausalHashIdByCausalHash h2
        Q.setNamespaceRoot chId

    stageAndPush remotePath = do
      let repoString = Text.unpack $ printRepo repo
      withStatus ("Staging files for upload to " ++ repoString ++ " ...") do
        lift (syncToDirectory remotePath syncMode branch)
        setRepoRoot remotePath (Branch.headHash branch)
      -- push staging area to remote
      withStatus ("Uploading to " ++ repoString ++ " ...") $
        unlessM
          ( push remotePath repo
              `withIOError` (throwError . GitError.PushException repo . show)
          )
          (throwError $ GitError.PushNoOp repo)
    -- Commit our changes
    push :: CodebasePath -> RemoteRepo -> IO Bool -- withIOError needs IO
    push remotePath (GitRepo url gitbranch) = do
      -- has anything changed?
      status <- gitTextIn remotePath ["status", "--short"]
      if Text.null status
        then pure False
        else do
          gitIn remotePath ["add", "--all", "."]
          gitIn
            remotePath
            ["commit", "-q", "-m", "Sync branch " <> Text.pack (show $ Branch.headHash branch)]
          -- Push our changes to the repo
          case gitbranch of
            Nothing -> gitIn remotePath ["push", "--quiet", url]
            Just gitbranch ->
              error $
                "Pushing to a specific branch isn't fully implemented or tested yet.\n"
                  ++ "InputPatterns.parseUri was expected to have prevented you "
                  ++ "from supplying the git treeish `"
                  ++ Text.unpack gitbranch
                  ++ "`!"
              -- gitIn remotePath ["push", "--quiet", url, gitbranch]
          pure True
