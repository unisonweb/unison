{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Conversion.Sync12 where

import Control.Lens
import Control.Monad.Except (MonadError, runExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Validate (MonadValidate, ValidateT, runValidateT)
import qualified Control.Monad.Validate as Validate
import Control.Monad.Writer
import Data.Bifoldable (bitraverse_)
import Data.Foldable (traverse_)
import qualified Data.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Traversable (for)
import Database.SQLite.Simple (Connection)
import U.Codebase.Sqlite.DbId (Generation)
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sync (Sync (Sync), TrySyncResult)
import qualified U.Codebase.Sync as Sync
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Conversion.Sync12BranchDependencies as BD
import Unison.Codebase.Patch (Patch (..))
import Unison.DataDeclaration (DataDeclaration, Decl)
import qualified Unison.DataDeclaration as DD
import Unison.Hash (Hash)
import qualified Unison.Hashable as H
import qualified Unison.LabeledDependency as LD
import Unison.Prelude (Set, Word64, ifM, (<&>))
import qualified Unison.Reference as Reference
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Util.Relation as Relation
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TypeEdit as TypeEdit

data Env m = Env
  { srcCodebase :: Codebase m Symbol (),
    destCodebase :: Codebase m Symbol (),
    destConnection :: Connection
  }

data Entity m
  = C Branch.Hash (m (Branch m))
  | T Hash Reference.Size
  | D Hash Reference.Size
  | P Branch.EditHash

data Entity'
  = C' Branch.Hash
  | T' Hash
  | D' Hash
  | P' Branch.EditHash
  deriving (Eq, Ord, Show)

toEntity' :: Entity m -> Entity'
toEntity' = \case
  C h _ -> C' h
  T h _ -> T' h
  D h _ -> D' h
  P h -> P' h

instance Eq (Entity m) where x == y = toEntity' x == toEntity' y

instance Ord (Entity m) where compare x y = compare (toEntity' x) (toEntity' y)

data BranchStatus
  = BranchOk
  | BranchMissing
  | BranchReplaced Branch.Hash

data TermStatus
  = TermOk
  | TermMissing
  | TermMissingType
  | TermMissingDependencies

data DeclStatus
  = DeclOk
  | DeclMissing
  | DeclMissingDependencies

data PatchStatus
  = PatchOk
  | PatchMissing
  | PatchReplaced Branch.EditHash

data Status = Status
  { _branchStatus :: Map Branch.Hash BranchStatus,
    _termStatus :: Map Hash TermStatus,
    _declStatus :: Map Hash DeclStatus,
    _patchStatus :: Map Branch.EditHash PatchStatus
  }

makeLenses ''Status

instance Show (Entity m) where
  show = \case
    C h _ -> "C " ++ show h
    T h len -> "T " ++ show h ++ " " ++ show len
    D h len -> "D " ++ show h ++ " " ++ show len
    P h -> "P " ++ show h

sync12 ::
  (MonadIO m, MonadRWS (Env m) () Status m) =>
  m (Sync m (Entity m))
sync12 = do
  gc <- runDest' $ Q.getNurseryGeneration
  pure $ Sync (trySync (succ gc))

trySync ::
  forall m.
  MonadRWS (Env m) () Status m =>
  Generation ->
  Entity m ->
  m (TrySyncResult (Entity m))
trySync _gc e = do
  Env src dest _ <- Reader.ask
  case e of
    C h mb ->
      isSyncedCausal h >>= \case
        True -> pure Sync.PreviouslyDone
        False -> do
          result <- runValidateT @(Set (Entity m)) @m @() do
            b <- lift mb
            (h', b') <- repairBranch b
            setBranchStatus h h'
            lift $ Codebase.putBranch dest b'
          case result of
            Left deps -> pure . Sync.Missing $ Foldable.toList deps
            Right () -> pure Sync.Done
    T h n ->
      getTermStatus h >>= \case
        Just {} -> pure Sync.PreviouslyDone
        Nothing -> do
          result <- runValidateT do
            runExceptT (checkTermComponent h n) >>= \case
              Left status -> setTermStatus h status
              Right component -> do
                Foldable.for_ (zip component [0 ..]) \((term, typ), i) ->
                  lift $ Codebase.putTerm dest (Reference.Id h i n) term typ
                setTermStatus h TermOk
          case result of
            Left deps -> pure . Sync.Missing $ Foldable.toList deps
            Right () -> pure Sync.Done
    D h n ->
      getDeclStatus h >>= \case
        Just {} -> pure Sync.PreviouslyDone
        Nothing -> do
          result <- runValidateT do
            runExceptT (checkDeclComponent h n) >>= \case
              Left status -> setDeclStatus h status
              Right component -> do
                Foldable.for_ (zip component [0 ..]) \(decl, i) ->
                  lift $ Codebase.putTypeDeclaration dest (Reference.Id h i n) decl
                setDeclStatus h DeclOk
          case result of
            Left deps -> pure . Sync.Missing $ Foldable.toList deps
            Right () -> pure Sync.Done
    P h ->
      getPatchStatus h >>= \case
        Just {} -> pure Sync.PreviouslyDone
        Nothing -> do
          result <- runValidateT do
            runExceptT (checkPatch h) >>= \case
              Left status -> setPatchStatus h status
              Right (h', patch) -> do
                lift $ Codebase.putPatch dest h patch
                setPatchStatus h PatchOk
          case result of
            Left deps -> pure . Sync.Missing $ Foldable.toList deps
            Right () -> pure Sync.Done
  where
    isSyncedCausal :: Branch.Hash -> m Bool
    isSyncedCausal = undefined
    getTermStatus h = use (termStatus . at h)
    getDeclStatus h = use (declStatus . at h)
    getPatchStatus h = use (patchStatus . at h)
    setTermStatus h s = termStatus . at h .= Just s
    setDeclStatus h s = declStatus . at h .= Just s
    setPatchStatus h s = patchStatus . at h .= Just s
    setBranchStatus :: forall m. MonadState Status m => Branch.Hash -> Branch.Hash -> m ()
    setBranchStatus h h' =
      if h == h'
        then branchStatus . at h .= Just BranchOk
        else branchStatus . at h .= Just (BranchReplaced h')

    checkTermComponent ::
      forall m.
      (MonadState Status m, MonadReader (Env m) m) =>
      Hash ->
      Reference.Size ->
      EVT TermStatus m [(Term Symbol (), Type Symbol ())]
    checkTermComponent h n = do
      Env src _ _ <- Reader.ask
      for [Reference.Id h i n | i <- [0 .. n -1]] \r -> do
        term <- lift . lift $ Codebase.getTerm src r
        typ <- lift . lift $ Codebase.getTypeOfTermImpl src r
        case (term, typ) of
          (Nothing, _) -> Except.throwError TermMissing
          (_, Nothing) -> Except.throwError TermMissingType
          (Just term, Just typ) -> do
            let termDeps = Term.labeledDependencies term
                typeDeps = Type.dependencies typ
            let checkDecl = \case
                  Reference.Builtin {} -> pure ()
                  Reference.DerivedId (Reference.Id h' _ n') ->
                    getDeclStatus h' >>= \case
                      Just DeclOk -> pure ()
                      Just _ -> Except.throwError TermMissingDependencies
                      Nothing -> Validate.dispute . Set.singleton $ D h' n'
                checkTerm = \case
                  Reference.Builtin {} -> pure ()
                  Reference.DerivedId (Reference.Id h' _ n') ->
                    getTermStatus h' >>= \case
                      Just TermOk -> pure ()
                      Just _ -> Except.throwError TermMissingDependencies
                      Nothing -> Validate.dispute . Set.singleton $ T h' n'
            traverse_ (bitraverse_ checkDecl checkTerm . LD.toReference) termDeps
            traverse_ checkDecl typeDeps
            pure (term, typ)

    checkDeclComponent ::
      forall m.
      (MonadState Status m, MonadReader (Env m) m) =>
      Hash ->
      Reference.Size ->
      EVT DeclStatus m [Decl Symbol ()]
    checkDeclComponent h n = do
      Env src _ _ <- Reader.ask
      for [Reference.Id h i n | i <- [0 .. n -1]] \r -> do
        decl <- lift . lift $ Codebase.getTypeDeclaration src r
        case decl of
          Nothing -> Except.throwError DeclMissing
          Just decl -> do
            let deps = DD.declDependencies decl
                checkDecl = \case
                  Reference.Builtin {} -> pure ()
                  Reference.DerivedId (Reference.Id h' _ n') ->
                    getDeclStatus h' >>= \case
                      Just DeclOk -> pure ()
                      Just _ -> Except.throwError DeclMissingDependencies
                      Nothing -> Validate.dispute . Set.singleton $ D h' n'
            traverse_ checkDecl deps
            pure decl

    checkPatch ::
      forall m.
      (MonadState Status m, MonadReader (Env m) m) =>
      Branch.EditHash ->
      EVT PatchStatus m (Branch.EditHash, Patch)
    checkPatch h = do
      Env src _ _ <- Reader.ask
      (lift . lift $ Codebase.getPatch src h) >>= \case
        Nothing -> Except.throwError PatchMissing
        Just patch -> do
          (h', patch) <- lift $ repairPatch patch
          if h == h'
            then setPatchStatus h PatchOk
            else setPatchStatus h (PatchReplaced h')
          pure (h', patch)


    repairBranch :: Branch m -> ValidateT (Set (Entity m)) m (Branch.Hash, Branch m)
    repairBranch = error "not implemented"

    repairPatch ::forall m. MonadState Status m =>
      Patch -> ValidateT (Set (Entity m)) m (Branch.EditHash, Patch)
    repairPatch (Patch termEdits typeEdits) = do
      termEdits' <- Relation.filterM (uncurry filterTermEdit) termEdits
      typeEdits' <- Relation.filterM (uncurry filterTypeEdit) typeEdits
      let patch = Patch termEdits' typeEdits'
      pure (H.accumulate' patch, patch)
      where
        filterTermEdit _ = \case
          TermEdit.Deprecate -> pure True
          TermEdit.Replace (Reference.Builtin _) _ -> pure True
          TermEdit.Replace (Reference.DerivedId (Reference.Id h _ n)) _ ->
            getTermStatus h >>= \case
              Nothing -> Validate.refute (Set.singleton $ T h n)
              Just TermOk -> pure True
              Just _ -> pure False
        filterTypeEdit _ = \case
          TypeEdit.Deprecate -> pure True
          TypeEdit.Replace (Reference.Builtin _) -> pure True
          TypeEdit.Replace (Reference.DerivedId (Reference.Id h _ n)) ->
            getDeclStatus h >>= \case
              Nothing -> Validate.refute (Set.singleton $ D h n)
              Just DeclOk -> pure True
              Just _ -> pure False

type EVT e m = ExceptT e (ValidateT (Set (Entity m)) m)

runSrc, runDest :: MonadReader (Env m) m => (Codebase m Symbol () -> a) -> m a
runSrc = (Reader.reader srcCodebase <&>)
runDest = (Reader.reader destCodebase <&>)

runDest' ::
  (MonadReader (Env m) m) =>
  ReaderT Connection m a ->
  m a
runDest' ma = Reader.reader destConnection >>= flip runDB ma

runDB :: Connection -> ReaderT Connection m a -> m a
runDB conn action = Reader.runReaderT action conn
