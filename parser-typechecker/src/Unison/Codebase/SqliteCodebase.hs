{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.SqliteCodebase where

-- initCodebase :: Branch.Cache IO -> FilePath -> IO (Codebase IO Symbol Ann)

import Control.Concurrent.STM
import Control.Monad (join)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Data.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Word (Word64)
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as Sqlite
import System.FilePath ((</>))
import qualified U.Codebase.Decl as V2.Decl
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Sqlite.ObjectType as OT
import qualified U.Codebase.Sqlite.Operations as Ops
import U.Codebase.Sqlite.Queries (DB)
import qualified U.Util.Monoid as Monoid
import qualified Unison.Builtin as Builtins
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase as Codebase1
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (Decl)
import Unison.Hash (Hash)
import Unison.Parser (Ann)
import Unison.Prelude (MaybeT (runMaybeT), fromMaybe)
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as ShortHash
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import UnliftIO (catchIO)

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

type TermBufferEntry = BufferEntry (Term Symbol Ann, Type Symbol Ann)

type DeclBufferEntry = BufferEntry (Decl Symbol Ann)

sqliteCodebase :: CodebasePath -> IO (IO (), Codebase1.Codebase IO Symbol Ann)
sqliteCodebase root = do
  conn :: Sqlite.Connection <- Sqlite.open $ root </> "v2" </> "unison.sqlite3"
  termBuffer :: TVar (Map Hash TermBufferEntry) <- newTVarIO Map.empty
  declBuffer :: TVar (Map Hash DeclBufferEntry) <- newTVarIO Map.empty
  let getRootBranch :: IO (Either Codebase1.GetRootBranchError (Branch IO))
      putRootBranch :: Branch IO -> IO ()
      rootBranchUpdates :: IO (IO (), IO (Set Branch.Hash))
      getBranchForHash :: Branch.Hash -> IO (Maybe (Branch IO))
      dependentsImpl :: Reference -> IO (Set Reference.Id)
      syncFromDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch IO -> IO ()
      syncToDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch IO -> IO ()
      watches :: UF.WatchKind -> IO [Reference.Id]
      getWatch :: UF.WatchKind -> Reference.Id -> IO (Maybe (Term Symbol Ann))
      putWatch :: UF.WatchKind -> Reference.Id -> Term Symbol Ann -> IO ()
      termsOfTypeImpl :: Reference -> IO (Set Referent.Id)
      termsMentioningTypeImpl :: Reference -> IO (Set Referent.Id)
      branchHashesByPrefix :: ShortBranchHash -> IO (Set Branch.Hash)

      getTerm :: Reference.Id -> IO (Maybe (Term Symbol Ann))
      getTerm (Reference.Id h1@(Cv.hash1to2 -> h2) i _n) =
        runDB conn do
          term2 <- Ops.loadTermByReference (C.Reference.Id h2 i)
          Cv.term2to1 h1 getCycleLen getDeclType term2

      getCycleLen :: DB m => Hash -> MaybeT m Reference.Size
      getCycleLen = Ops.getCycleLen . Cv.hash1to2

      getDeclType :: DB m => C.Reference.Reference -> MaybeT m CT.ConstructorType
      getDeclType = \case
        C.Reference.ReferenceBuiltin t ->
          MaybeT (pure $ Map.lookup (Reference.Builtin t) Builtins.builtinConstructorType)
        C.Reference.ReferenceDerived i -> getDeclTypeById i

      getDeclTypeById :: DB m => C.Reference.Id -> MaybeT m CT.ConstructorType
      getDeclTypeById = fmap Cv.decltype2to1 . Ops.getDeclTypeByReference

      getTypeOfTermImpl :: Reference.Id -> IO (Maybe (Type Symbol Ann))
      getTypeOfTermImpl (Reference.Id (Cv.hash1to2 -> h2) i _n) =
        runDB conn do
          type2 <- Ops.loadTypeOfTermByTermReference (C.Reference.Id h2 i)
          Cv.ttype2to1 getCycleLen type2

      getTypeDeclaration :: Reference.Id -> IO (Maybe (Decl Symbol Ann))
      getTypeDeclaration (Reference.Id h1@(Cv.hash1to2 -> h2) i _n) =
        runDB conn do
          decl2 <- Ops.loadDeclByReference (C.Reference.Id h2 i)
          Cv.decl2to1 h1 getCycleLen decl2

      putTerm :: Reference.Id -> Term Symbol Ann -> Type Symbol Ann -> IO ()
      putTerm r@(Reference.Id h i n) = error "todo"
        updateBufferEntry termBuffer h $ \be -> error "todo"

-- data BufferEntry a = BufferEntry
--   { -- First, you are waiting for the cycle to fill up with all elements
--     -- Then, you check: are all dependencies of the cycle in the db?
--     --   If yes: write yourself to database and trigger check of dependents
--     --   If no: just wait, do nothing
--     beComponentTargetSize :: Maybe Word64,
--     beComponent :: Map Reference.Pos a,
--     beMissingDependencies :: Set Hash,
--     beWaitingDependents :: Set Hash
--   }

      updateBufferEntry ::
        TVar (Map Hash (BufferEntry a)) ->
        Hash ->
        -- this signature may need to change
        (BufferEntry a -> (BufferEntry a, b)) ->
        IO [b]
      updateBufferEntry = error "todo"

      tryWriteBuffer :: Hash -> TVar (Map Hash (BufferEntry a)) -> IO ()
      tryWriteBuffer = error "todo"

      putTypeDeclaration :: Reference.Id -> Decl Symbol Ann -> IO ()
      putTypeDeclaration = error "todo"

      getRootBranch = error "todo"
      putRootBranch = error "todo"
      rootBranchUpdates = error "todo"
      getBranchForHash = error "todo"
      dependentsImpl = error "todo"
      syncFromDirectory = error "todo"
      syncToDirectory = error "todo"
      watches = error "todo"
      getWatch = error "todo"
      putWatch = error "todo"

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

      termsOfTypeImpl = error "todo"
      termsMentioningTypeImpl = error "todo"

      hashLength :: IO Int
      hashLength = pure 10

      branchHashLength :: IO Int
      branchHashLength = pure 10

      defnReferencesByPrefix :: OT.ObjectType -> ShortHash -> IO (Set Reference.Id)
      defnReferencesByPrefix _ (ShortHash.Builtin _) = pure mempty
      defnReferencesByPrefix ot (ShortHash.ShortHash prefix cycle _cid) =
        Monoid.fromMaybe <$> runDB conn do
          refs <- lift $ Ops.componentReferencesByPrefix ot prefix (Cv.shortHashSuffix1to2 <$> cycle)
          Set.fromList <$> traverse (Cv.referenceid2to1 getCycleLen) (Set.toList refs)

      termReferencesByPrefix :: ShortHash -> IO (Set Reference.Id)
      termReferencesByPrefix = defnReferencesByPrefix OT.TermComponent

      declReferencesByPrefix :: ShortHash -> IO (Set Reference.Id)
      declReferencesByPrefix = defnReferencesByPrefix OT.DeclComponent

      -- this implementation is wrong; it should filter by ctor id if provided
      termReferentsByPrefix :: ShortHash -> IO (Set Referent.Id)
      termReferentsByPrefix sh = do
        terms <- termReferencesByPrefix sh
        let termReferents = Set.map Referent.Ref' terms
        decls <- declReferencesByPrefix sh
        declReferents <- Set.fromList . join <$> traverse go (Foldable.toList decls)
        pure (termReferents <> declReferents)
        where
          getDeclCtorCount :: DB m => Reference.Id -> m (CT.ConstructorType, Word64)
          getDeclCtorCount (Reference.Id (Cv.hash1to2 -> h2) i _n) = do
            -- this is a database integrity error if the decl doesn't exist in the database
            decl20 <- runMaybeT $ Ops.loadDeclByReference (C.Reference.Id h2 i)
            let decl2 = fromMaybe (error "database integrity error") decl20
            pure
              ( Cv.decltype2to1 $ V2.Decl.declType decl2,
                fromIntegral . length $ V2.Decl.constructorTypes decl2
              )
          go rid = runDB' conn do
            (ct, ctorCount) <- getDeclCtorCount rid
            pure [Referent.Con' rid (fromIntegral cid) ct | cid <- [0 .. ctorCount - 1]]

      branchHashesByPrefix = error "todo"
  let finalizer = Sqlite.close conn
  pure $
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
        termReferentsByPrefix
        branchHashLength
        branchHashesByPrefix
    )

-- x :: DB m => MaybeT m (Term Symbol) -> MaybeT m (Term Symbol Ann)
-- x = error "not implemented"

runDB :: Connection -> MaybeT (ReaderT Connection IO) a -> IO (Maybe a)
runDB conn action = flip runReaderT conn $ runMaybeT action

runDB' :: Connection -> MaybeT (ReaderT Connection IO) a -> IO a
runDB' conn action = flip runReaderT conn $ fmap err $ runMaybeT action
  where
    err = fromMaybe (error "database consistency error")
