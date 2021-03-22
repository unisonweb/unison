{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Conversion.Sync12 where

import Control.Lens
import Control.Monad.Except (MonadError, runExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Reader
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState)
import Control.Monad.Validate (MonadValidate, runValidateT)
import qualified Control.Monad.Validate as Validate
import Control.Natural (type (~>))
import Data.Bifoldable (bitraverse_)
import Data.Foldable (traverse_)
import qualified Data.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Traversable (for)
import Database.SQLite.Simple (Connection)
import U.Codebase.Sqlite.DbId (Generation)
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sync (Sync (Sync), TrySyncResult)
import qualified U.Codebase.Sync as Sync
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (UnwrappedBranch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Patch (Patch (..))
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as DD
import Unison.Hash (Hash)
import qualified Unison.Hashable as H
import qualified Unison.LabeledDependency as LD
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.Prelude (Set)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as Relation
import Unison.Util.Star3 (Star3 (Star3))

data Env m = Env
  { srcCodebase :: Codebase m Symbol (),
    destCodebase :: Codebase m Symbol (),
    destConnection :: Connection
  }

data Entity m
  = C Branch.Hash (m (UnwrappedBranch m))
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

instance Eq (Entity m) where
  x == y = toEntity' x == toEntity' y

instance Ord (Entity m) where
  x `compare` y = toEntity' x `compare` toEntity' y

data BranchStatus m
  = BranchOk
  | BranchReplaced Branch.Hash (UnwrappedBranch m)

data BranchStatus'
  = BranchOk'
  | BranchReplaced' Branch.Hash
  deriving (Eq, Ord)

toBranchStatus' :: BranchStatus m -> BranchStatus'
toBranchStatus' = \case
  BranchOk -> BranchOk'
  BranchReplaced h _ -> BranchReplaced' h

instance Eq (BranchStatus m) where
  x == y = toBranchStatus' x == toBranchStatus' y

instance Ord (BranchStatus m) where
  x `compare` y = toBranchStatus' x `compare` toBranchStatus' y

type V m n = MonadValidate (Set (Entity m)) n

type E e n = MonadError e n

type S m n = MonadState (Status m) n

type R m n = MonadReader (Env m) n

type RS m n = (R m n, S m n)

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

data Status m = Status
  { _branchStatus :: Map Branch.Hash (BranchStatus m),
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
  forall m n.
  (MonadIO n, RS m n, Applicative m) =>
  (m ~> n) ->
  n (Sync n (Entity m))
sync12 t = do
  gc <- runDest' $ Q.getNurseryGeneration
  pure $ Sync (trySync t (succ gc))

trySync ::
  forall m n.
  (R m n, S m n, Applicative m) =>
  (m ~> n) ->
  Generation ->
  Entity m ->
  n (TrySyncResult (Entity m))
trySync t _gc e = do
  Env src dest _ <- Reader.ask
  case e of
    C h mc ->
      isSyncedCausal h >>= \case
        True -> pure Sync.PreviouslyDone
        False -> do
          c <- t mc
          runValidateT @_ @n (repairBranch c) >>= \case
            Left deps -> pure . Sync.Missing $ Foldable.toList deps
            Right c' -> do
              let h' = Causal.currentHash c'
              if h == h'
                then setBranchStatus @m @n h BranchOk
                else setBranchStatus h (BranchReplaced h' c')
              t $ Codebase.putBranch dest (Branch.Branch c')
              pure Sync.Done
    T h n ->
      getTermStatus h >>= \case
        Just {} -> pure Sync.PreviouslyDone
        Nothing -> do
          runExceptT (runValidateT (checkTermComponent (lift . lift . t) h n)) >>= \case
            Left status -> do
              setTermStatus h status
              pure Sync.NonFatalError
            Right (Left deps) ->
              pure . Sync.Missing $ Foldable.toList deps
            Right (Right component) -> do
              Foldable.for_ (zip component [0 ..]) \((term, typ), i) ->
                t $ Codebase.putTerm dest (Reference.Id h i n) term typ
              setTermStatus h TermOk
              pure Sync.Done
    D h n ->
      getDeclStatus h >>= \case
        Just {} -> pure Sync.PreviouslyDone
        Nothing ->
          runExceptT (runValidateT (checkDeclComponent (lift . lift . t) h n)) >>= \case
            Left status -> do
              setDeclStatus h status
              pure Sync.NonFatalError
            Right (Left deps) ->
              pure . Sync.Missing $ Foldable.toList deps
            Right (Right component) -> do
              Foldable.for_ (zip component [0 ..]) \(decl, i) ->
                t $ Codebase.putTypeDeclaration dest (Reference.Id h i n) decl
              setDeclStatus h DeclOk
              pure Sync.Done
    P h ->
      getPatchStatus h >>= \case
        Just {} -> pure Sync.PreviouslyDone
        Nothing ->
          runExceptT (runValidateT (checkPatch (lift . lift . t) h)) >>= \case
            Left status -> setPatchStatus h status >> pure Sync.NonFatalError
            Right (Left deps) -> pure . Sync.Missing $ Foldable.toList deps
            Right (Right (h', patch')) -> do
              t $ Codebase.putPatch dest h' patch'
              setPatchStatus h PatchOk
              pure Sync.Done

isSyncedCausal :: forall n. Branch.Hash -> n Bool
isSyncedCausal = undefined

getBranchStatus :: S m n => Branch.Hash -> n (Maybe (BranchStatus m))
getBranchStatus h = use (branchStatus . at h)

getTermStatus :: S m n => Hash -> n (Maybe TermStatus)
getTermStatus h = use (termStatus . at h)

getDeclStatus :: S m n => Hash -> n (Maybe DeclStatus)
getDeclStatus h = use (declStatus . at h)

getPatchStatus :: S m n => Hash -> n (Maybe PatchStatus)
getPatchStatus h = use (patchStatus . at h)

setTermStatus :: S m n => Hash -> TermStatus -> n ()
setTermStatus h s = termStatus . at h .= Just s

setDeclStatus :: S m n => Hash -> DeclStatus -> n ()
setDeclStatus h s = declStatus . at h .= Just s

setPatchStatus :: S m n => Hash -> PatchStatus -> n ()
setPatchStatus h s = patchStatus . at h .= Just s

setBranchStatus :: forall m n. S m n => Branch.Hash -> BranchStatus m -> n ()
setBranchStatus h s = branchStatus . at h .= Just s

checkTermComponent ::
  forall m n.
  (RS m n, V m n, E TermStatus n) =>
  (m ~> n) ->
  Hash ->
  Reference.Size ->
  n [(Term Symbol (), Type Symbol ())]
checkTermComponent t h n = do
  Env src _ _ <- Reader.ask
  for [Reference.Id h i n | i <- [0 .. n -1]] \r -> do
    term <- t $ Codebase.getTerm src r
    typ <- t $ Codebase.getTypeOfTermImpl src r
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
  forall m n.
  (RS m n, E DeclStatus n, V m n) =>
  (m ~> n) ->
  Hash ->
  Reference.Size ->
  n [Decl Symbol ()]
checkDeclComponent t h n = do
  Env src _ _ <- Reader.ask
  for [Reference.Id h i n | i <- [0 .. n -1]] \r -> do
    decl <- t $ Codebase.getTypeDeclaration src r
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
  forall m n.
  (RS m n, E PatchStatus n, V m n) =>
  (m ~> n) ->
  Branch.EditHash ->
  n (Branch.EditHash, Patch)
checkPatch t h = do
  Env src _ _ <- Reader.ask
  t (Codebase.getPatch src h) >>= \case
    Nothing -> Except.throwError PatchMissing
    Just patch -> do
      (h', patch) <- repairPatch patch
      if h == h'
        then setPatchStatus h PatchOk
        else setPatchStatus h (PatchReplaced h')
      pure (h', patch)

repairBranch ::
  forall m n.
  (S m n, V m n, Applicative m) =>
  UnwrappedBranch m ->
  n (UnwrappedBranch m)
repairBranch = \case
  Causal.One _h e -> do
    e' <- repairBranch0 e
    pure $ Causal.one e'
  Causal.Cons _h e (ht, mt) -> do
    getBranchStatus ht >>= \case
      Nothing -> Validate.refute . Set.singleton $ C ht mt
      Just tailStatus -> do
        e' <- repairBranch0 e
        pure case tailStatus of
          BranchOk -> Causal.cons' e' ht mt
          BranchReplaced _ht' t' -> Causal.consDistinct e' t'
  Causal.Merge _h e (Map.toList -> tails) -> do
    tails' <-
      Map.fromList <$> for tails \(ht, mt) ->
        getBranchStatus ht >>= \case
          Nothing -> Validate.refute . Set.singleton $ C ht mt
          Just tailStatus ->
            pure case tailStatus of
              BranchOk -> (ht, mt)
              BranchReplaced ht' t' -> (ht', pure t')
    e' <- repairBranch0 e
    let h' = Causal.RawHash $ Causal.hash (e', Map.keys tails')
    pure $ Causal.Merge h' e' tails'

repairBranch0 ::
  forall m n.
  (S m n, V m n, Applicative m) =>
  Branch.Branch0 m ->
  n (Branch.Branch0 m)
repairBranch0 b = do
  terms' <- filterBranchTermStar (view Branch.terms b)
  types' <- filterBranchTypeStar (view Branch.types b)
  children' <- filterBranchChildren (view Branch.children b)
  edits' <- filterBranchEdits (view Branch.edits b)
  pure @n $ Branch.branch0 terms' types' children' edits'

repairPatch ::
  forall m n.
  (MonadState (Status m) n, MonadValidate (Set (Entity m)) n) =>
  Patch ->
  n (Branch.EditHash, Patch)
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
          Nothing -> Validate.refute . Set.singleton $ T h n
          Just TermOk -> pure True
          Just _ -> pure False
    filterTypeEdit _ = \case
      TypeEdit.Deprecate -> pure True
      TypeEdit.Replace (Reference.Builtin _) -> pure True
      TypeEdit.Replace (Reference.DerivedId (Reference.Id h _ n)) ->
        getDeclStatus h >>= \case
          Nothing -> Validate.refute . Set.singleton $ D h n
          Just DeclOk -> pure True
          Just _ -> pure False

filterBranchTermStar :: (S m n, V m n) => Metadata.Star Referent NameSegment -> n (Metadata.Star Referent NameSegment)
filterBranchTermStar (Star3 _refs names _mdType mdTypeValues) = do
  names' <- filterTermNames names
  mdTypeValues' <- filterMetadata mdTypeValues
  let refs' = Relation.dom names'
  let mdType' = error "Can I get away with not populating the mdType column?"
  pure $ Star3 refs' names' mdType' mdTypeValues'

filterBranchTypeStar :: (S m n, V m n) => Metadata.Star Reference.Reference NameSegment -> n (Metadata.Star Reference.Reference NameSegment)
filterBranchTypeStar (Star3 _refs names _mdType mdTypeValues) = do
  names' <- filterTypeNames names
  let refs' = Relation.dom names'
  mdTypeValues' <- filterMetadata mdTypeValues
  let mdType' = error "Can I get away with not populating the mdType column?"
  pure $ Star3 refs' names mdType' mdTypeValues'

filterMetadata :: Relation r (Metadata.Type, Metadata.Value) -> n (Relation r (Metadata.Type, Metadata.Value))
filterMetadata = error "not implemented"

filterTermNames :: (S m n, V m n) => Relation Referent NameSegment -> n (Relation Referent NameSegment)
filterTermNames = Relation.filterDomM validateTermReferent

validateTermReferent :: (S m n, V m n) => Referent -> n Bool
validateTermReferent = \case
  Referent.Ref r -> validateTermReference r
  Referent.Con r _ _ -> validateTypeReference r

validateTermReference :: (S m n, V m n) => Reference.Reference -> n Bool
validateTermReference = \case
  Reference.Builtin {} -> pure True
  Reference.DerivedId (Reference.Id h _i n) ->
    getTermStatus h >>= \case
      Nothing -> Validate.refute . Set.singleton $ T h n
      Just TermOk -> pure True
      Just _ -> pure False

validateTypeReference :: (S m n, V m n) => Reference.Reference -> n Bool
validateTypeReference = \case
  Reference.Builtin t -> pure True
  Reference.DerivedId (Reference.Id h _i n) ->
    getTermStatus h >>= \case
      Nothing -> Validate.refute . Set.singleton $ T h n
      Just TermOk -> pure True
      Just _ -> pure False

filterTypeNames :: (S m n, V m n) => Relation Reference.Reference NameSegment -> n (Relation Reference.Reference NameSegment)
filterTypeNames = Relation.filterDomM validateTypeReference

filterBranchChildren :: (S m n, V m n, Applicative m) => Map NameSegment (Branch.Branch m) -> n (Map NameSegment (Branch.Branch m))
filterBranchChildren = fmap Map.fromList . traverse go . Map.toList
  where
    go orig@(ns, Branch.Branch c) =
      getBranchStatus (Causal.currentHash c) >>= \case
        Nothing -> Validate.refute . Set.singleton $ C (Causal.currentHash c) (pure c)
        Just BranchOk -> pure orig
        Just (BranchReplaced _h c) -> pure (ns, Branch.Branch c)

-- | if a dependency is missing, then remove the entry
filterBranchEdits :: (S m n, V m n) => Map NameSegment (Branch.EditHash, m Patch) -> n (Map NameSegment (Branch.EditHash, m Patch))
filterBranchEdits = fmap (Map.fromList . catMaybes) . traverse go . Map.toList
  where
    go (ns, (h, _)) =
      getPatchStatus h >>= \case
        Nothing -> Validate.refute . Set.singleton $ P h
        Just PatchOk -> rebuild h
        Just PatchMissing -> pure Nothing
        Just (PatchReplaced h) -> rebuild h
      where
        rebuild h = pure $ Just (ns, (h, err h))
        err h = error $ "expected to short-circuit already-synced patch " ++ show h

runSrc, runDest :: R m n => (Codebase m Symbol () -> a) -> n a
runSrc = (Reader.reader srcCodebase <&>)
runDest = (Reader.reader destCodebase <&>)

runDest' :: R m n => ReaderT Connection n a -> n a
runDest' ma = Reader.reader destConnection >>= flip runDB ma

runDB :: Connection -> ReaderT Connection m a -> m a
runDB conn action = Reader.runReaderT action conn

-- each entity has to check to see
-- a) if it exists (if not, mark as missing in Status)
-- b) if any of its dependencies have not yet been synced
-- (if so, note as validation)
-- c) if any of its dependencies are missing from the source codebase
-- (if so, then filter them if possible, otherwise give this entity an error Status)