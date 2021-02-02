{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Codebase.SqliteCodebase where

-- initCodebase :: Branch.Cache IO -> FilePath -> IO (Codebase IO Symbol Ann)

-- import qualified U.Codebase.Sqlite.Operations' as Ops

import qualified Control.Exception
import Control.Monad (filterM, (>=>))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Extra (unlessM)
import qualified Control.Monad.Extra as Monad
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Bifunctor (Bifunctor (first), second)
import qualified Data.Either.Combinators as Either
import Data.Foldable (Foldable (toList), traverse_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Word (Word64)
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as Sqlite
import System.Directory (canonicalizePath)
import qualified System.Exit as SysExit
import System.FilePath ((</>))
import U.Codebase.HashTags (CausalHash (unCausalHash))
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Sqlite.ObjectType as OT
import U.Codebase.Sqlite.Operations (EDB)
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Util.Hash as H2
import qualified U.Util.Monoid as Monoid
import qualified U.Util.Set as Set
import qualified Unison.Builtin as Builtins
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase as Codebase1
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as Decl
import Unison.Hash (Hash)
import Unison.Parser (Ann)
import Unison.Prelude (MaybeT (runMaybeT), fromMaybe, trace, traceM)
import qualified Unison.PrettyTerminal as PT
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
import UnliftIO (MonadIO, catchIO)
import UnliftIO.Directory (createDirectoryIfMissing, getHomeDirectory)
import qualified UnliftIO.Environment as SysEnv
import UnliftIO.STM
import qualified System.FilePath as FilePath
import qualified Control.Concurrent

codebasePath :: FilePath
codebasePath = ".unison" </> "v2" </> "unison.sqlite3"

-- get the codebase in dir, or in the home directory if not provided.
getCodebaseOrExit :: Maybe FilePath -> IO (IO (), Codebase1.Codebase IO Symbol Ann)
getCodebaseOrExit mdir = do
  dir <- getCodebaseDir mdir
  progName <- SysEnv.getProgName
  prettyDir <- P.string <$> canonicalizePath dir
  let errMsg = getNoCodebaseErrorMsg ((P.text . Text.pack) progName) prettyDir mdir
  sqliteCodebase dir >>= \case
    Left _missingSchema -> do
      PT.putPrettyLn' errMsg
      SysExit.exitFailure
    Right c -> pure c

getNoCodebaseErrorMsg :: IsString s => P.Pretty s -> P.Pretty s -> Maybe FilePath -> P.Pretty s
getNoCodebaseErrorMsg executable prettyDir mdir =
  let secondLine =
        case mdir of
          Just dir ->
            "Run `" <> executable <> " -codebase " <> fromString dir
              <> " init` to create one, then try again!"
          Nothing ->
            "Run `" <> executable <> " init` to create one there,"
              <> " then try again;"
              <> " or `"
              <> executable
              <> " -codebase <dir>` to load a codebase from someplace else!"
   in P.lines
        [ "No codebase exists in " <> prettyDir <> ".",
          secondLine
        ]

initCodebaseAndExit :: Maybe FilePath -> IO ()
initCodebaseAndExit mdir = do
  dir <- getCodebaseDir mdir
  (closeCodebase, _codebase) <- initCodebase dir
  closeCodebase
  SysExit.exitSuccess

-- initializes a new codebase here (i.e. `ucm -codebase dir init`)
initCodebase :: FilePath -> IO (IO (), Codebase1.Codebase IO Symbol Ann)
initCodebase path = do
  traceM $ "initCodebase " ++ path
  prettyDir <- P.string <$> canonicalizePath path

  Monad.whenM (codebaseExists path) do
    PT.putPrettyLn'
      . P.wrap
      $ "It looks like " <> prettyDir <> " already exists."
    SysExit.exitFailure

  PT.putPrettyLn'
    . P.wrap
    $ "Initializing a new codebase in: "
      <> prettyDir

  -- run sql create scripts
  createDirectoryIfMissing True (path </> FilePath.takeDirectory codebasePath)
  Control.Exception.bracket
    (unsafeGetConnection path)
    Sqlite.close
    (runReaderT Q.createSchema)

  (closeCodebase, theCodebase) <- sqliteCodebase path >>= \case
    Right x -> pure x
    Left x -> error $ show x ++ " :) "
  Codebase1.initializeCodebase theCodebase
  pure (closeCodebase, theCodebase)

getCodebaseDir :: Maybe FilePath -> IO FilePath
getCodebaseDir = maybe getHomeDirectory pure

-- checks if a db exists at `path` with the minimum schema
codebaseExists :: CodebasePath -> IO Bool
codebaseExists root = do
  traceM $ "codebaseExists " ++ root
  Control.Exception.catch @Sqlite.SQLError
    (sqliteCodebase root >>= \case
      Left _ -> pure False
      Right (close, _codebase) -> close >> pure True)
    (const $ pure False)

-- and <$> traverse doesDirectoryExist (minimalCodebaseStructure root)

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

type TermBufferEntry = BufferEntry (Term Symbol Ann, Type Symbol Ann)

type DeclBufferEntry = BufferEntry (Decl Symbol Ann)

unsafeGetConnection :: CodebasePath -> IO Sqlite.Connection
unsafeGetConnection root = do
  traceM $ "unsafeGetconnection " ++ root ++ " -> " ++ (root </> codebasePath)
  conn <- Sqlite.open $ root </> codebasePath
  runReaderT Q.setFlags conn
  pure conn

sqliteCodebase :: CodebasePath -> IO (Either [(Q.SchemaType, Q.SchemaName)] (IO (), Codebase1.Codebase IO Symbol Ann))
sqliteCodebase root = do
  traceM $ "sqliteCodebase " ++ root
  conn <- unsafeGetConnection root
  runReaderT Q.checkForMissingSchema conn >>= \case
    [] -> do
      termBuffer :: TVar (Map Hash TermBufferEntry) <- newTVarIO Map.empty
      declBuffer :: TVar (Map Hash DeclBufferEntry) <- newTVarIO Map.empty
      let getTerm :: Reference.Id -> IO (Maybe (Term Symbol Ann))
          getTerm (Reference.Id h1@(Cv.hash1to2 -> h2) i _n) =
            runDB' conn do
              term2 <- Ops.loadTermByReference (C.Reference.Id h2 i)
              Cv.term2to1 h1 getCycleLen getDeclType term2

          getCycleLen :: EDB m => Hash -> m Reference.Size
          getCycleLen = Ops.getCycleLen . Cv.hash1to2

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

          getTypeOfTermImpl :: Reference.Id -> IO (Maybe (Type Symbol Ann))
          getTypeOfTermImpl id | trace ("getTypeOfTermImpl " ++ show id) False = undefined
          getTypeOfTermImpl (Reference.Id (Cv.hash1to2 -> h2) i _n) =
            runDB' conn do
              type2 <- Ops.loadTypeOfTermByTermReference (C.Reference.Id h2 i)
              Cv.ttype2to1 getCycleLen type2

          getTypeDeclaration :: Reference.Id -> IO (Maybe (Decl Symbol Ann))
          getTypeDeclaration (Reference.Id h1@(Cv.hash1to2 -> h2) i _n) =
            runDB' conn do
              decl2 <- Ops.loadDeclByReference (C.Reference.Id h2 i)
              Cv.decl2to1 h1 getCycleLen decl2

          putTerm :: Reference.Id -> Term Symbol Ann -> Type Symbol Ann -> IO ()
          putTerm (Reference.Id h@(Cv.hash1to2 -> h2) i n') tm tp =
            runDB conn $
              unlessM
                (Ops.objectExistsForHash h2 >>= \b -> do traceM $ "objectExistsForHash " ++ show h2 ++ " = " ++ show b; pure b)
                ( withBuffer termBuffer h \(BufferEntry size comp missing waiting) -> do
                    let size' = Just n'
                    -- if size was previously set, it's expected to match size'.
                    case size of
                      Just n | n /= n' ->
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
            traceM $ "putBuffer " ++ show h ++ " " ++ show e
            atomically $ modifyTVar tv (Map.insert h e)

          withBuffer :: MonadIO m => TVar (Map Hash (BufferEntry a)) -> Hash -> (BufferEntry a -> m b) -> m b
          withBuffer tv h f =
            Map.lookup h <$> readTVarIO tv >>= \case
              Just e -> f e
              Nothing -> f (BufferEntry Nothing Map.empty Set.empty Set.empty)

          removeBuffer :: MonadIO m => TVar (Map Hash (BufferEntry a)) -> Hash -> m ()
          removeBuffer tv h = atomically $ modifyTVar tv (Map.delete h)

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
                  if null missing' && size == fromIntegral (length comp)
                    then do
                      saveComponent h2 (toList comp)
                      removeBuffer buf h
                      traverse_ tryWaiting waiting
                    else -- update
                      putBuffer buf h $
                        BufferEntry (Just size) comp (Set.fromList missing') waiting
                Nothing -> -- it's never even been added, so there's nothing to do.
                  pure ()

          tryFlushTermBuffer :: EDB m => Hash -> m ()
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
          tryFlushDeclBuffer h =
            tryFlushBuffer
              declBuffer
              (\h2 -> void . Ops.saveDeclComponent h2 . fmap (Cv.decl1to2 h))
              (\h -> tryFlushTermBuffer h >> tryFlushDeclBuffer h)
              h

          putTypeDeclaration :: Reference.Id -> Decl Symbol Ann -> IO ()
          putTypeDeclaration (Reference.Id h@(Cv.hash1to2 -> h2) i n') decl =
            runDB conn $
              unlessM
                (Ops.objectExistsForHash h2)
                ( withBuffer declBuffer h \(BufferEntry size comp missing waiting) -> do
                    let size' = Just n'
                    case size of
                      Just n | n /= n' ->
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

          getRootBranch :: IO (Either Codebase1.GetRootBranchError (Branch IO))
          getRootBranch =
            fmap (Either.mapLeft err)
              . runExceptT
              . flip runReaderT conn
              . fmap (Branch.transform (runDB conn))
              $ Cv.causalbranch2to1 getCycleLen getDeclType =<< Ops.loadRootCausal
            where
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

          putRootBranch :: Branch IO -> IO ()
          putRootBranch branch1 =
            runDB conn
              . void
              . Ops.saveRootBranch
              . Cv.causalbranch1to2
              $ Branch.transform (lift . lift) branch1

          rootBranchUpdates :: IO (IO (), IO (Set Branch.Hash))
          rootBranchUpdates = pure (cleanup, newRootsDiscovered) 
            where
              newRootsDiscovered = do
                Control.Concurrent.threadDelay maxBound -- hold off on returning
                pure mempty -- returning nothing
              cleanup = pure ()

          -- if this blows up on cromulent hashes, then switch from `hashToHashId`
          -- to one that returns Maybe.
          getBranchForHash :: Branch.Hash -> IO (Maybe (Branch IO))
          getBranchForHash h = runDB conn do
            Ops.loadCausalBranchByCausalHash (Cv.branchHash1to2 h) >>= \case
              Just b ->
                pure . Just . Branch.transform (runDB conn)
                  =<< Cv.causalbranch2to1 getCycleLen getDeclType b
              Nothing -> pure Nothing

          dependentsImpl :: Reference -> IO (Set Reference.Id)
          dependentsImpl r =
            runDB conn $
              Set.traverse (Cv.referenceid2to1 getCycleLen)
                =<< Ops.dependents (Cv.reference1to2 r)

          syncFromDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch IO -> IO ()
          syncFromDirectory = error "todo"

          syncToDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch IO -> IO ()
          syncToDirectory = error "todo"

          watches :: UF.WatchKind -> IO [Reference.Id]
          watches w =
            runDB conn $
              Ops.listWatches (Cv.watchKind1to2 w)
                >>= traverse (Cv.referenceid2to1 getCycleLen)

          -- getWatch :: UF.WatchKind -> Reference.Id -> IO (Maybe (Term Symbol Ann))
          getWatch k r@(Reference.Id h _i _n) =
            runDB' conn $
              Ops.loadWatch (Cv.watchKind1to2 k) (Cv.referenceid1to2 r)
                >>= Cv.term2to1 h getCycleLen getDeclType

          putWatch :: UF.WatchKind -> Reference.Id -> Term Symbol Ann -> IO ()
          putWatch k r@(Reference.Id h _i _n) tm =
            runDB conn $
              Ops.saveWatch
                (Cv.watchKind1to2 k)
                (Cv.referenceid1to2 r)
                (Cv.term1to2 h tm)

          getReflog :: IO [Reflog.Entry]
          getReflog =
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

          appendReflog :: Text -> Branch IO -> Branch IO -> IO ()
          appendReflog reason old new =
            let t =
                  Reflog.toText $
                    Reflog.Entry (Branch.headHash old) (Branch.headHash new) reason
             in TextIO.appendFile (reflogPath root) (t <> "\n")

          reflogPath :: CodebasePath -> FilePath
          reflogPath root = root </> "reflog"

          termsOfTypeImpl :: Reference -> IO (Set Referent.Id)
          termsOfTypeImpl r =
            runDB conn $
              Ops.termsHavingType (Cv.reference1to2 r)
                >>= Set.traverse (Cv.referentid2to1 getCycleLen getDeclType)

          termsMentioningTypeImpl :: Reference -> IO (Set Referent.Id)
          termsMentioningTypeImpl r =
            runDB conn $
              Ops.termsMentioningType (Cv.reference1to2 r)
                >>= Set.traverse (Cv.referentid2to1 getCycleLen getDeclType)

          hashLength :: IO Int
          hashLength = pure 10

          branchHashLength :: IO Int
          branchHashLength = pure 10

          defnReferencesByPrefix :: OT.ObjectType -> ShortHash -> IO (Set Reference.Id)
          defnReferencesByPrefix _ (ShortHash.Builtin _) = pure mempty
          defnReferencesByPrefix ot (ShortHash.ShortHash prefix (fmap Cv.shortHashSuffix1to2 -> cycle) _cid) =
            Monoid.fromMaybe <$> runDB' conn do
              refs <- do
                Ops.componentReferencesByPrefix ot prefix cycle
                  >>= traverse (C.Reference.idH Ops.loadHashByObjectId)
                  >>= pure . Set.fromList

              Set.fromList <$> traverse (Cv.referenceid2to1 getCycleLen) (Set.toList refs)

          termReferencesByPrefix :: ShortHash -> IO (Set Reference.Id)
          termReferencesByPrefix = defnReferencesByPrefix OT.TermComponent

          declReferencesByPrefix :: ShortHash -> IO (Set Reference.Id)
          declReferencesByPrefix = defnReferencesByPrefix OT.DeclComponent

          referentsByPrefix :: ShortHash -> IO (Set Referent.Id)
          referentsByPrefix SH.Builtin {} = pure mempty
          referentsByPrefix (SH.ShortHash prefix (fmap Cv.shortHashSuffix1to2 -> cycle) cid) = runDB conn do
            termReferents <-
              Ops.termReferentsByPrefix prefix cycle
                >>= traverse (Cv.referentid2to1 getCycleLen getDeclType)
            declReferents' <- Ops.declReferentsByPrefix prefix cycle (read . Text.unpack <$> cid)
            let declReferents =
                  [ Referent.Con' (Reference.Id (Cv.hash2to1 h) pos len) (fromIntegral cid) (Cv.decltype2to1 ct)
                    | (h, pos, len, ct, cids) <- declReferents',
                      cid <- cids
                  ]
            pure . Set.fromList $ termReferents <> declReferents

          branchHashesByPrefix :: ShortBranchHash -> IO (Set Branch.Hash)
          branchHashesByPrefix sh = runDB conn do
            -- given that a Branch is shallow, it's really `CausalHash` that you'd
            -- refer to to specify a full namespace w/ history.
            -- but do we want to be able to refer to a namespace without its history?
            cs <- Ops.causalHashesByPrefix (Cv.sbh1to2 sh)
            pure $ Set.map (Causal.RawHash . Cv.hash2to1 . unCausalHash) cs

      -- Do we want to include causal hashes here or just namespace hashes?
      -- Could we expose just one or the other of them to the user?
      -- Git uses commit hashes and tree hashes (analogous to causal hashes
      -- and namespace hashes, respectively), but the user is presented
      -- primarily with commit hashes.
      -- Arya leaning towards doing the same for Unison.

      let finalizer = do
            Sqlite.close conn
            decls <- readTVarIO declBuffer
            terms <- readTVarIO termBuffer
            let printBuffer header b =
                  if b /= mempty
                    then putStrLn header >> putStrLn "" >> print b
                    else pure ()
            printBuffer "Decls:" decls
            printBuffer "Terms:" terms

      pure . Right $
        ( finalizer,
          Codebase1.Codebase
            getTerm
            getTypeOfTermImpl
            getTypeDeclaration
            putTerm
            putTypeDeclaration
            getRootBranch
            putRootBranch
            rootBranchUpdates
            getBranchForHash
            dependentsImpl
            syncFromDirectory
            syncToDirectory
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
    missingSchema -> pure . Left $ missingSchema

runDB' :: Connection -> MaybeT (ReaderT Connection (ExceptT Ops.Error IO)) a -> IO (Maybe a)
runDB' conn = runDB conn . runMaybeT

runDB :: Connection -> ReaderT Connection (ExceptT Ops.Error IO) a -> IO a
runDB conn = (runExceptT >=> err) . flip runReaderT conn
  where
    err = \case Left err -> error $ show err; Right a -> pure a
