{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
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
import Control.Monad (filterM, unless, when, (>=>))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Extra (ifM, unlessM)
import qualified Control.Monad.Extra as Monad
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Bifunctor (Bifunctor (bimap), second)
import qualified Data.Char as Char
import qualified Data.Either.Combinators as Either
import Data.Foldable (Foldable (toList), for_, traverse_)
import Data.Functor (void, (<&>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Traversable (for)
import Data.Word (Word64)
import qualified Database.SQLite.Simple as Sqlite
import GHC.Stack (HasCallStack)
import qualified System.Console.ANSI as ANSI
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import U.Codebase.HashTags (CausalHash (CausalHash, unCausalHash))
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C.Referent
import U.Codebase.Sqlite.Connection (Connection (Connection))
import qualified U.Codebase.Sqlite.Connection as Connection
import U.Codebase.Sqlite.DbId (SchemaVersion (SchemaVersion), ObjectId)
import qualified U.Codebase.Sqlite.JournalMode as JournalMode
import qualified U.Codebase.Sqlite.ObjectType as OT
import U.Codebase.Sqlite.Operations (EDB)
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Sync22 as Sync22
import qualified U.Codebase.Sync as Sync
import qualified U.Codebase.WatchKind as WK
import qualified U.Util.Cache as Cache
import qualified U.Util.Hash as H2
import qualified Unison.Hashing.V2.Convert as Hashing
import qualified U.Util.Monoid as Monoid
import qualified Unison.Util.Set as Set
import U.Util.Timing (time)
import qualified Unison.Builtin as Builtins
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase1
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Editor.Git (gitIn, gitTextIn, pullBranch)
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace, WriteRepo (WriteGitRepo), printWriteRepo, writeToRead)
import qualified Unison.Codebase.GitError as GitError
import qualified Unison.Codebase.Init as Codebase
import qualified Unison.Codebase.Init.CreateCodebaseError as Codebase1
import Unison.Codebase.Patch (Patch)
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Branch.Dependencies as BD
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.Codebase.SqliteCodebase.GitError as GitError
import qualified Unison.Codebase.SqliteCodebase.SyncEphemeral as SyncEphemeral
import Unison.Codebase.SyncMode (SyncMode)
import Unison.Codebase.Type (PushGitBranchOpts(..))
import qualified Unison.Codebase.Type as C
import Unison.ConstructorReference (GConstructorReference(..))
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as Decl
import Unison.Hash (Hash)
import Unison.Parser.Ann (Ann)
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
import qualified Unison.Util.Pretty as P
import qualified Unison.WatchKind as UF
import UnliftIO (MonadIO, catchIO, finally, liftIO, MonadUnliftIO, throwIO)
import qualified UnliftIO
import UnliftIO.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import UnliftIO.STM
import UnliftIO.Exception (catch, bracket)

debug, debugProcessBranches, debugCommitFailedTransaction :: Bool
debug = False
debugProcessBranches = False
debugCommitFailedTransaction = False

codebasePath :: FilePath
codebasePath = ".unison" </> "v2" </> "unison.sqlite3"

v2dir :: FilePath -> FilePath
v2dir root = root </> ".unison" </> "v2"

init :: HasCallStack => (MonadUnliftIO m) => Codebase.Init m Symbol Ann
init = Codebase.Init
  { withOpenCodebase=getCodebaseOrError
  , withCreatedCodebase=createCodebaseOrError
  , codebasePath=v2dir
  }

createCodebaseOrError ::
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either Codebase1.CreateCodebaseError r)
createCodebaseOrError debugName dir action = do
  prettyDir <- P.string <$> canonicalizePath dir
  let convertError = \case
        CreateCodebaseAlreadyExists -> Codebase1.CreateCodebaseAlreadyExists
        CreateCodebaseUnknownSchemaVersion v -> Codebase1.CreateCodebaseOther $ prettyError v
      prettyError :: SchemaVersion -> Codebase1.Pretty
      prettyError v = P.wrap $
        "I don't know how to handle " <> P.shown v <> "in" <> P.backticked' prettyDir "."
  Either.mapLeft convertError <$> createCodebaseOrError' debugName dir action

data CreateCodebaseError
  = CreateCodebaseAlreadyExists
  | CreateCodebaseUnknownSchemaVersion SchemaVersion
  deriving (Show)

createCodebaseOrError' ::
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either CreateCodebaseError r)
createCodebaseOrError' debugName path action = do
  ifM
    (doesFileExist $ path </> codebasePath)
    (pure $ Left CreateCodebaseAlreadyExists)
    do
      createDirectoryIfMissing True (path </> FilePath.takeDirectory codebasePath)
      liftIO $
        withConnection (debugName ++ ".createSchema") path $
          ( runReaderT do
              Q.createSchema
              runExceptT (void . Ops.saveRootBranch $ Cv.causalbranch1to2 Branch.empty) >>= \case
                Left e -> error $ show e
                Right () -> pure ()
          )

      fmap (Either.mapLeft CreateCodebaseUnknownSchemaVersion) (sqliteCodebase debugName path action)

withOpenOrCreateCodebaseConnection ::
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  FilePath ->
  (Connection -> m r) ->
  m r
withOpenOrCreateCodebaseConnection debugName path action = do
  unlessM
    (doesFileExist $ path </> codebasePath)
    (initSchemaIfNotExist path)
  withConnection debugName path action

-- get the codebase in dir
getCodebaseOrError ::
  forall m r.
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either Codebase1.Pretty r)
getCodebaseOrError debugName dir action = do
  prettyDir <- liftIO $ P.string <$> canonicalizePath dir
  let prettyError v = P.wrap $ "I don't know how to handle " <> P.shown v <> "in" <> P.backticked' prettyDir "."
  doesFileExist (dir </> codebasePath) >>= \case
    -- If the codebase file doesn't exist, just return any string. The string is currently ignored (see
    -- Unison.Codebase.Init.getCodebaseOrExit).
    False -> pure (Left "codebase doesn't exist")
    True -> fmap (Either.mapLeft prettyError) (sqliteCodebase debugName dir action)

initSchemaIfNotExist :: MonadIO m => FilePath -> m ()
initSchemaIfNotExist path = liftIO do
  unlessM (doesDirectoryExist $ path </> FilePath.takeDirectory codebasePath) $
    createDirectoryIfMissing True (path </> FilePath.takeDirectory codebasePath)
  unlessM (doesFileExist $ path </> codebasePath) $
    withConnection "initSchemaIfNotExist" path $ runReaderT Q.createSchema

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

-- | Create a new sqlite connection to the database at the given path.
--   the caller is responsible for calling the returned cleanup method once finished with the
--   connection.
--   The connection may not be used after it has been cleaned up.
--   Prefer using 'withConnection' if you can, as it guarantees the connection will be properly
--   closed for you.
unsafeGetConnection ::
  MonadIO m =>
  Codebase.DebugName ->
  CodebasePath ->
  m (IO (), Connection)
unsafeGetConnection name root = do
  let path = root </> codebasePath
  Monad.when debug $ traceM $ "unsafeGetconnection " ++ name ++ " " ++ root ++ " -> " ++ path
  (Connection name path -> conn) <- liftIO $ Sqlite.open path
  runReaderT Q.setFlags conn
  pure (shutdownConnection conn, conn)
  where
    shutdownConnection :: MonadIO m => Connection -> m ()
    shutdownConnection conn = do
      Monad.when debug $ traceM $ "shutdown connection " ++ show conn
      liftIO $ Sqlite.close (Connection.underlying conn)

-- | Run an action with a connection to the codebase, closing the connection on completion or
-- failure.
withConnection ::
  MonadUnliftIO m =>
  Codebase.DebugName ->
  CodebasePath ->
  (Connection -> m a) ->
  m a
withConnection name root act = do
  bracket
    (unsafeGetConnection name root)
    (\(closeConn, _) -> liftIO closeConn)
    (\(_, conn) -> act conn)

sqliteCodebase ::
  (MonadUnliftIO m) =>
  Codebase.DebugName ->
  CodebasePath ->
  (Codebase m Symbol Ann -> m r) ->
  m (Either SchemaVersion r)
sqliteCodebase debugName root action = do
 Monad.when debug $ traceM $ "sqliteCodebase " ++ debugName ++ " " ++ root
 withConnection debugName root $ \conn -> do
  termCache <- Cache.semispaceCache 8192 -- pure Cache.nullCache -- to disable
  typeOfTermCache <- Cache.semispaceCache 8192
  declCache <- Cache.semispaceCache 1024
  runReaderT Q.schemaVersion conn >>= \case
    SchemaVersion 1 -> do
      rootBranchCache <- newTVarIO Nothing
      -- The v1 codebase interface has operations to read and write individual definitions
      -- whereas the v2 codebase writes them as complete components.  These two fields buffer
      -- the individual definitions until a complete component has been written.
      termBuffer :: TVar (Map Hash TermBufferEntry) <- newTVarIO Map.empty
      declBuffer :: TVar (Map Hash DeclBufferEntry) <- newTVarIO Map.empty
      cycleLengthCache <- Cache.semispaceCache 8192
      declTypeCache <- Cache.semispaceCache 2048
      let getTerm :: MonadIO m => Reference.Id -> m (Maybe (Term Symbol Ann))
          getTerm (Reference.Id h1@(Cv.hash1to2 -> h2) i _n) =
            runDB' conn do
              term2 <- Ops.loadTermByReference (C.Reference.Id h2 i)
              lift $ Cv.term2to1 h1 (getCycleLen "getTerm") getDeclType term2

          getCycleLen :: EDB m => String -> Hash -> m Reference.Size
          getCycleLen source = Cache.apply cycleLengthCache \h ->
            (Ops.getCycleLen . Cv.hash1to2) h `Except.catchError` \case
              e@(Ops.DatabaseIntegrityError (Q.NoObjectForPrimaryHashId {})) -> pure . error $ show e ++ " in " ++ source
              e -> Except.throwError e

          getDeclType :: EDB m => C.Reference.Reference -> m CT.ConstructorType
          getDeclType = Cache.apply declTypeCache \case
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

          addTermComponentTypeIndex :: EDB m => ObjectId -> [Type Symbol Ann] -> m ()
          addTermComponentTypeIndex oId types = for_ (types `zip` [0..]) \(tp, i) -> do
            let self = C.Referent.RefId (C.Reference.Id oId i)
                typeForIndexing = Hashing.typeToReference tp
                typeMentionsForIndexing = Hashing.typeToReferenceMentions tp
            Ops.addTypeToIndexForTerm self (Cv.reference1to2 typeForIndexing)
            Ops.addTypeMentionsToIndexForTerm self (Set.map Cv.reference1to2 typeMentionsForIndexing)

          addDeclComponentTypeIndex :: EDB m => ObjectId -> [[Type Symbol Ann]] -> m ()
          addDeclComponentTypeIndex oId ctorss =
            for_ (ctorss `zip` [0..]) \(ctors, i) ->
              for_ (ctors `zip` [0..]) \(tp, j) -> do
                let self = C.Referent.ConId (C.Reference.Id oId i) j
                    typeForIndexing = Hashing.typeToReference tp
                    typeMentionsForIndexing = Hashing.typeToReferenceMentions tp
                Ops.addTypeToIndexForTerm self (Cv.reference1to2 typeForIndexing)
                Ops.addTypeMentionsToIndexForTerm self (Set.map Cv.reference1to2 typeMentionsForIndexing)

          tryFlushTermBuffer :: EDB m => Hash -> m ()
          tryFlushTermBuffer h | debug && trace ("tryFlushTermBuffer " ++ show h) False = undefined
          tryFlushTermBuffer h =
            tryFlushBuffer
              termBuffer
              ( \h2 component -> do
                  oId <- Ops.saveTermComponent h2
                    $ fmap (bimap (Cv.term1to2 h) Cv.ttype1to2) component
                  addTermComponentTypeIndex oId (fmap snd component)
              )
              tryFlushTermBuffer
              h

          tryFlushDeclBuffer :: EDB m => Hash -> m ()
          tryFlushDeclBuffer h | debug && trace ("tryFlushDeclBuffer " ++ show h) False = undefined
          tryFlushDeclBuffer h =
            tryFlushBuffer
              declBuffer
              (\h2 component -> do
                oId <- Ops.saveDeclComponent h2 $ fmap (Cv.decl1to2 h) component
                addDeclComponentTypeIndex oId $
                  fmap (map snd . Decl.constructors . Decl.asDataDecl) component
              )
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
              forceReload = time "Get root branch" do
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
          putBranch = runDB conn . putBranch'

          isCausalHash :: MonadIO m => Branch.Hash -> m Bool
          isCausalHash = runDB conn . isCausalHash'

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
          patchExists = runDB conn . patchExists'

          dependentsImpl :: MonadIO m => Reference -> m (Set Reference.Id)
          dependentsImpl r =
            runDB conn $
              Set.traverse (Cv.referenceid2to1 (getCycleLen "dependentsImpl"))
                =<< Ops.dependents (Cv.reference1to2 r)

          syncFromDirectory :: MonadUnliftIO m => Codebase1.CodebasePath -> SyncMode -> Branch m -> m ()
          syncFromDirectory srcRoot _syncMode b = do
            withConnection (debugName ++ ".sync.src") srcRoot $ \srcConn -> do
              flip State.evalStateT emptySyncProgressState $ do
                syncInternal syncProgress srcConn conn $ Branch.transform lift b

          syncToDirectory :: MonadUnliftIO m => Codebase1.CodebasePath -> SyncMode -> Branch m -> m ()
          syncToDirectory destRoot _syncMode b =
            withConnection (debugName ++ ".sync.dest") destRoot $ \destConn ->
              flip State.evalStateT emptySyncProgressState $ do
                initSchemaIfNotExist destRoot
                syncInternal syncProgress conn destConn $ Branch.transform lift b

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

          clearWatches :: MonadIO m => m ()
          clearWatches = runDB conn Ops.clearWatches

          getReflog :: MonadIO m => m [Reflog.Entry Branch.Hash]
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
                  [ Referent.ConId (ConstructorReference (Reference.Id (Cv.hash2to1 h) pos len) (fromIntegral cid)) (Cv.decltype2to1 ct)
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

          sqlLca :: MonadIO m => Branch.Hash -> Branch.Hash -> m (Maybe Branch.Hash)
          sqlLca h1 h2 =
            liftIO $ withConnection  (debugName ++ ".lca.left") root $ \c1 -> do
                     withConnection  (debugName ++ ".lca.right") root $ \c2 -> do
                       runDB conn
                         . (fmap . fmap) Cv.causalHash2to1
                         $ Ops.lca (Cv.causalHash1to2 h1) (Cv.causalHash1to2 h2) c1 c2
      let
       codebase = C.Codebase
        (Cache.applyDefined termCache getTerm)
        (Cache.applyDefined typeOfTermCache getTypeOfTermImpl)
        (Cache.applyDefined declCache getTypeDeclaration)
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
        (\b r opts -> pushGitBranch conn b r opts)
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
        (Just \l r -> runDB conn $ fromJust <$> before l r)

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
      (Right <$> action codebase) `finally` finalizer
    v -> pure $ Left v

-- well one or the other. :zany_face: the thinking being that they wouldn't hash-collide
termExists', declExists' :: MonadIO m => Hash -> ReaderT Connection (ExceptT Ops.Error m) Bool
termExists' = fmap isJust . Ops.primaryHashToMaybeObjectId . Cv.hash1to2
declExists' = termExists'

patchExists' :: MonadIO m => Branch.EditHash -> ReaderT Connection (ExceptT Ops.Error m) Bool
patchExists' h = fmap isJust $ Ops.primaryHashToMaybePatchObjectId (Cv.patchHash1to2 h)

putBranch' :: MonadIO m => Branch m -> ReaderT Connection (ExceptT Ops.Error m) ()
putBranch' branch1 =
  void . Ops.saveBranch . Cv.causalbranch1to2 $
    Branch.transform (lift . lift) branch1

isCausalHash' :: MonadIO m => Branch.Hash -> ReaderT Connection (ExceptT Ops.Error m) Bool
isCausalHash' (Causal.RawHash h) =
  Q.loadHashIdByHash (Cv.hash1to2 h) >>= \case
    Nothing -> pure False
    Just hId -> Q.isCausalHash hId

before :: MonadIO m => Branch.Hash -> Branch.Hash -> ReaderT Connection m (Maybe Bool)
before h1 h2 =
  Ops.before (Cv.causalHash1to2 h1) (Cv.causalHash1to2 h2)

syncInternal ::
  forall m.
  MonadIO m =>
  Sync.Progress m Sync22.Entity ->
  Connection ->
  Connection ->
  Branch m ->
  m ()
syncInternal progress srcConn destConn b = time "syncInternal" do
  runDB srcConn $ Q.savepoint "sync"
  runDB destConn $ Q.savepoint "sync"
  result <- runExceptT do
    let syncEnv = Sync22.Env srcConn destConn (16 * 1024 * 1024)
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
          forall m.
          MonadIO m =>
          Sync.Sync (ReaderT Sync22.Env (ExceptT Sync22.Error m)) Sync22.Entity ->
          Sync.Progress (ReaderT Sync22.Env (ExceptT Sync22.Error m)) Sync22.Entity ->
          [Entity m] ->
          ExceptT Sync22.Error m ()
        processBranches _ _ [] = pure ()
        processBranches sync progress (b0@(B h mb) : rest) = do
          when debugProcessBranches do
            traceM $ "processBranches " ++ show b0
            traceM $ " queue: " ++ show rest
          ifM @(ExceptT Sync22.Error m)
            (lift . runDB destConn $ isCausalHash' h)
            do
              when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " already exists in dest db"
              processBranches sync progress rest
            do
              when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " doesn't exist in dest db"
              let h2 = CausalHash . Cv.hash1to2 $ Causal.unRawHash h
              lift (flip runReaderT srcConn (Q.loadCausalHashIdByCausalHash h2)) >>= \case
                Just chId -> do
                  when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " exists in source db, so delegating to direct sync"
                  r $ Sync.sync' sync progress [Sync22.C chId]
                  processBranches sync progress rest
                Nothing ->
                  lift mb >>= \b -> do
                    when debugProcessBranches $ traceM $ "  " ++ show b0 ++ " doesn't exist in either db, so delegating to Codebase.putBranch"
                    let (branchDeps, BD.to' -> BD.Dependencies' es ts ds) = BD.fromBranch b
                    when debugProcessBranches do
                      traceM $ "  branchDeps: " ++ show (fst <$> branchDeps)
                      traceM $ "  terms: " ++ show ts
                      traceM $ "  decls: " ++ show ds
                      traceM $ "  edits: " ++ show es
                    (cs, es, ts, ds) <- lift $ runDB destConn do
                      cs <- filterM (fmap not . isCausalHash' . fst) branchDeps
                      es <- filterM (fmap not . patchExists') es
                      ts <- filterM (fmap not . termExists') ts
                      ds <- filterM (fmap not . declExists') ds
                      pure (cs, es, ts, ds)
                    if null cs && null es && null ts && null ds
                      then do
                        lift . runDB destConn $ putBranch' b
                        processBranches @m sync progress rest
                      else do
                        let bs = map (uncurry B) cs
                            os = map O (es <> ts <> ds)
                        processBranches @m sync progress (os ++ bs ++ b0 : rest)
        processBranches sync progress (O h : rest) = do
          when debugProcessBranches $ traceM $ "processBranches O " ++ take 10 (show h)
          (runExceptT $ flip runReaderT srcConn (Q.expectHashIdByHash (Cv.hash1to2 h) >>= Q.expectObjectIdForAnyHashId)) >>= \case
            Left e -> error $ show e
            Right oId -> do
              r $ Sync.sync' sync progress [Sync22.O oId]
              processBranches sync progress rest
    sync <- se . r $ Sync22.sync22
    let progress' = Sync.transformProgress (lift . lift) progress
        bHash = Branch.headHash b
    se $ time "SyncInternal.processBranches" $ processBranches sync progress' [B bHash (pure b)]
    testWatchRefs <- time "SyncInternal enumerate testWatches" $
      lift . fmap concat $ for [WK.TestWatch] \wk ->
        fmap (Sync22.W wk) <$> flip runReaderT srcConn (Q.loadWatchesByWatchKind wk)
    se . r $ Sync.sync sync progress' testWatchRefs
  let onSuccess a = runDB destConn (Q.release "sync") *> pure a
      onFailure e = do
        if debugCommitFailedTransaction
          then runDB destConn (Q.release "sync")
          else runDB destConn (Q.rollbackRelease "sync")
        error (show e)
  runDB srcConn $ Q.rollbackRelease "sync" -- (we don't write to the src anyway)
  either onFailure onSuccess result

runDB' :: MonadIO m => Connection -> MaybeT (ReaderT Connection (ExceptT Ops.Error m)) a -> m (Maybe a)
runDB' conn = runDB conn . runMaybeT

runDB :: MonadIO m => Connection -> ReaderT Connection (ExceptT Ops.Error m) a -> m a
runDB conn = (runExceptT >=> err) . flip runReaderT conn
  where
    err = \case Left err -> error $ show err; Right a -> pure a

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
      State.get >>= liftIO . putStrLn . renderState ("  " ++ "Done syncing ")

    printSynced :: (MonadState SyncProgressState m, MonadIO m) => m ()
    printSynced = State.get >>= \s -> liftIO $
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
  ((Branch m, CodebasePath) -> m r) ->
  m (Either C.GitError r)
viewRemoteBranch' (repo, sbh, path) action = UnliftIO.try do
  -- set up the cache dir
  remotePath <- UnliftIO.fromEitherM . runExceptT . withExceptT C.GitProtocolError . time "Git fetch" $ pullBranch repo

  -- Tickle the database before calling into `sqliteCodebase`; this covers the case that the database file either
  -- doesn't exist at all or isn't a SQLite database file, but does not cover the case that the database file itself is
  -- somehow corrupt, or not even a Unison database.
  --
  -- FIXME it would probably make more sense to define some proper preconditions on `sqliteCodebase`, and perhaps update
  -- its output type, which currently indicates the only way it can fail is with an `UnknownSchemaVersion` error.
  (withConnection "codebase exists check" remotePath \_ -> pure ()) `catch` \sqlError ->
    case Sqlite.sqlError sqlError of
      Sqlite.ErrorCan'tOpen -> throwIO (C.GitSqliteCodebaseError (GitError.NoDatabaseFile repo remotePath))
      -- Unexpected error from sqlite
      _ -> throwIO sqlError

  result <- sqliteCodebase "viewRemoteBranch.gitCache" remotePath \codebase -> do
    -- try to load the requested branch from it
    branch <- time "Git fetch (sbh)" $ case sbh of
      -- no sub-branch was specified, so use the root.
      Nothing ->
        (time "Get remote root branch" $ Codebase1.getRootBranch codebase) >>= \case
          -- this NoRootBranch case should probably be an error too.
          Left Codebase1.NoRootBranch -> pure Branch.empty
          Left (Codebase1.CouldntLoadRootBranch h) ->
            throwIO . C.GitCodebaseError $ GitError.CouldntLoadRootBranch repo h
          Left (Codebase1.CouldntParseRootBranch s) ->
            throwIO . C.GitSqliteCodebaseError $ GitError.GitCouldntParseRootBranchHash repo s
          Right b -> pure b
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
    Left schemaVersion -> throwIO . C.GitSqliteCodebaseError $ GitError.UnrecognizedSchemaVersion repo remotePath schemaVersion
    Right inner -> pure inner

-- Push a branch to a repo. Optionally attempt to set the branch as the new root, which fails if the branch is not after
-- the existing root.
pushGitBranch ::
  (MonadUnliftIO m) =>
  Connection ->
  Branch m ->
  WriteRepo ->
  PushGitBranchOpts ->
  m (Either C.GitError ())
pushGitBranch srcConn branch repo (PushGitBranchOpts setRoot _syncMode) = runExceptT @C.GitError do
  -- pull the remote repo to the staging directory
  -- open a connection to the staging codebase
  -- create a savepoint on the staging codebase
  -- sync the branch to the staging codebase using `syncInternal`, which probably needs to be passed in instead of `syncToDirectory`
  -- if setting the remote root,
  --   do a `before` check on the staging codebase
  --   if it passes, proceed (see below)
  --   if it fails, rollback to the savepoint and clean up.
  -- otherwise, proceed: release the savepoint (commit it), clean up, and git-push the result

  -- set up the cache dir
  remotePath <- time "Git fetch" $ withExceptT C.GitProtocolError $ pullBranch (writeToRead repo)
  ExceptT . withOpenOrCreateCodebaseConnection "push.dest" remotePath $ \destConn -> do
    flip runReaderT destConn $ Q.savepoint "push"
    flip State.execStateT emptySyncProgressState $
      syncInternal syncProgress srcConn destConn (Branch.transform lift branch)
    flip runReaderT destConn do
      result <- if setRoot
        then do
          let newRootHash = Branch.headHash branch
          -- the call to runDB "handles" the possible DB error by bombing
          (fmap . fmap) Cv.branchHash2to1 (runDB destConn Ops.loadMaybeRootCausalHash) >>= \case
            Nothing -> do
              setRepoRoot newRootHash
              Q.release "push"
              pure $ Right ()
            Just oldRootHash -> do
              before oldRootHash newRootHash >>= \case
                Nothing ->
                  error $
                    "I couldn't find the hash " ++ show newRootHash
                      ++ " that I just synced to the cached copy of "
                      ++ repoString
                      ++ " in "
                      ++ show remotePath
                      ++ "."
                Just False -> do
                  Q.rollbackRelease "push"
                  pure . Left . C.GitProtocolError $ GitError.PushDestinationHasNewStuff repo
                Just True -> do
                  setRepoRoot newRootHash
                  Q.release "push"
                  pure $ Right ()
        else do
          Q.release "push"
          pure $ Right ()

      Q.setJournalMode JournalMode.DELETE
      pure result
  liftIO do
    void $ push remotePath repo
  where
    repoString = Text.unpack $ printWriteRepo repo
    setRepoRoot :: Q.DB m => Branch.Hash -> m ()
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
        statusLines = Text.unpack <$> Text.lines status
        t = dropWhile Char.isSpace
        okLine (t -> '?' : '?' : (t -> p)) | p == codebasePath = True
        okLine (t -> 'M' : (t -> p)) | p == codebasePath = True
        okLine line = isWalDelete line || isShmDelete line
        isWalDelete (t -> 'D' : (t -> p)) | p == codebasePath ++ "-wal" = True
        isWalDelete _ = False
        isShmDelete (t -> 'D' : (t -> p)) | p == codebasePath ++ "-wal" = True
        isShmDelete _ = False
        hasDeleteWal = any isWalDelete statusLines
        hasDeleteShm = any isShmDelete statusLines

    -- Commit our changes
    push :: CodebasePath -> WriteRepo -> IO Bool -- withIOError needs IO
    push remotePath (WriteGitRepo url) = time "SqliteCodebase.pushGitRootBranch.push" $ do
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
                <> "I was expecting only to see .unison/v2/unison.sqlite3 modified, but saw:\n\n"
                <> Text.unpack status
                <> "\n\n"
                <> "Please visit https://github.com/unisonweb/unison/issues/2063\n"
                <> "and add any more details about how you encountered this!\n"
          Just (hasDeleteWal, hasDeleteShm) -> do
            -- Only stage files we're expecting; don't `git add --all .`
            -- which could accidentally commit some garbage
            gitIn remotePath ["add", ".unison/v2/unison.sqlite3"]
            when hasDeleteWal $ gitIn remotePath ["rm", ".unison/v2/unison.sqlite3-wal"]
            when hasDeleteShm $ gitIn remotePath ["rm", ".unison/v2/unison.sqlite3-shm"]
            gitIn
              remotePath
              ["commit", "-q", "-m", "Sync branch " <> Text.pack (show $ Branch.headHash branch)]
            -- Push our changes to the repo
            gitIn remotePath ["push", "--quiet", url]
            pure True
