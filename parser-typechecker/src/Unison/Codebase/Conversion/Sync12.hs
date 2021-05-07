{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Conversion.Sync12 where

import Control.Lens
import qualified Control.Lens as Lens
import Control.Monad.Except (MonadError, runExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Extra ((&&^))
import Control.Monad.Reader
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import Control.Monad.Validate (MonadValidate, runValidateT)
import qualified Control.Monad.Validate as Validate
import Control.Natural (type (~>))
import Data.Bifoldable (bitraverse_)
import Data.Foldable (traverse_)
import qualified Data.Foldable as Foldable
import Data.Functor (($>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Traversable (for)
import Database.SQLite.Simple (Connection)
import Debug.Trace (traceM)
import System.IO (stdout)
import System.IO.Extra (hFlush)
import U.Codebase.Sync (Sync (Sync), TrySyncResult)
import qualified U.Codebase.Sync as Sync
import qualified U.Util.Monoid as Monoid
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
import Unison.NameSegment (NameSegment)
import Unison.Prelude (Set)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.UnisonFile (WatchKind)
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as Relation
import Unison.Util.Star3 (Star3 (Star3))

debug :: Bool
debug = False

data Env m a = Env
  { srcCodebase :: Codebase m Symbol a,
    destCodebase :: Codebase m Symbol a,
    destConnection :: Connection
  }

data Entity m
  = C Branch.Hash (m (UnwrappedBranch m))
  | T Hash Reference.Size
  | W WatchKind Reference.Id
  | D Hash Reference.Size
  | P Branch.EditHash

type V m n = MonadValidate (Set (Entity m)) n

type E e n = MonadError e n

type S m n = MonadState (Status m) n

type R m n a = MonadReader (Env m a) n

type RS m n a = (R m n a, S m n)

data BranchStatus m
  = BranchOk
  | BranchReplaced Branch.Hash (UnwrappedBranch m)

data TermStatus
  = TermOk
  | TermMissing
  | TermMissingType
  | TermMissingDependencies
  deriving (Show)

data WatchStatus
  = WatchOk
  | WatchNotCached
  | WatchMissingDependencies
  deriving (Show)

data DeclStatus
  = DeclOk
  | DeclMissing
  | DeclMissingDependencies
  deriving (Show)

data PatchStatus
  = PatchOk
  | PatchMissing
  | PatchReplaced Branch.EditHash
  deriving (Show)

data Status m = Status
  { _branchStatus :: Map Branch.Hash (BranchStatus m),
    _termStatus :: Map Hash TermStatus,
    _declStatus :: Map Hash DeclStatus,
    _patchStatus :: Map Branch.EditHash PatchStatus,
    _watchStatus :: Map (WatchKind, Reference.Id) WatchStatus
  }

emptyStatus :: Status m
emptyStatus = Status mempty mempty mempty mempty mempty

makeLenses ''Status

sync12 ::
  (MonadIO f, MonadReader (Env p x) f, RS m n a, Applicative m) =>
  (m ~> n) ->
  f (Sync n (Entity m))
sync12 t = pure $ Sync (trySync t)

-- For each entity, we have to check to see
-- a) if it exists (if not, mark as missing in Status)
-- b) if any of its dependencies have not yet been synced
--    (if so, note as validation)
-- c) if any of its dependencies are missing from the source codebase
--     (if so, then filter them if possible, otherwise give this entity an
--      error Status)

trySync ::
  forall m n a.
  (R m n a, S m n, Applicative m) =>
  (m ~> n) ->
  Entity m ->
  n (TrySyncResult (Entity m))
trySync t e = do
  Env _ dest _ <- Reader.ask
  case e of
    C h mc -> do
      getBranchStatus h >>= \case
        Just {} -> pure Sync.PreviouslyDone
        Nothing -> t (Codebase.branchExists dest h) >>= \case
          True -> setBranchStatus h BranchOk $> Sync.PreviouslyDone
          False -> do
            c <- t mc
            runValidateT (repairBranch c) >>= \case
              Left deps -> pure . Sync.Missing $ Foldable.toList deps
              Right c' -> do
                let h' = Causal.currentHash c'
                t $ Codebase.putBranch dest (Branch.Branch c')
                if h == h'
                  then do
                    setBranchStatus h BranchOk
                    pure Sync.Done
                  else do
                    setBranchStatus h (BranchReplaced h' c')
                    pure Sync.NonFatalError
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
    W k r ->
      getWatchStatus k r >>= \case
        Just {} -> pure Sync.PreviouslyDone
        Nothing -> do
          runExceptT (runValidateT (checkWatchComponent (lift . lift . t) k r)) >>= \case
            Left status -> do
              setWatchStatus k r status
              pure Sync.NonFatalError
            Right (Left deps) ->
              pure . Sync.Missing $ Foldable.toList deps
            Right (Right watchResult) -> do
              t $ Codebase.putWatch dest k r watchResult
              setWatchStatus k r WatchOk
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
              setPatchStatus h if h == h' then PatchOk else PatchReplaced h'
              pure Sync.Done

getBranchStatus :: S m n => Branch.Hash -> n (Maybe (BranchStatus m))
getBranchStatus h = use (branchStatus . at h)

getTermStatus :: S m n => Hash -> n (Maybe TermStatus)
getTermStatus h = use (termStatus . at h)

getDeclStatus :: S m n => Hash -> n (Maybe DeclStatus)
getDeclStatus h = use (declStatus . at h)

getPatchStatus :: S m n => Hash -> n (Maybe PatchStatus)
getPatchStatus h = use (patchStatus . at h)

getWatchStatus :: S m n => WatchKind -> Reference.Id -> n (Maybe WatchStatus)
getWatchStatus w r = use (watchStatus . at (w, r))

setTermStatus :: S m n => Hash -> TermStatus -> n ()
setTermStatus h s = do
  when debug (traceM $ "setTermStatus " ++ take 10 (show h) ++ " " ++ show s)
  termStatus . at h .= Just s

setDeclStatus :: S m n => Hash -> DeclStatus -> n ()
setDeclStatus h s = do
  when debug (traceM $ "setDeclStatus " ++ take 10 (show h) ++ " " ++ show s)
  declStatus . at h .= Just s

setPatchStatus :: S m n => Hash -> PatchStatus -> n ()
setPatchStatus h s = do
  when debug (traceM $ "setPatchStatus " ++ take 10 (show h) ++ " " ++ show s)
  patchStatus . at h .= Just s

setBranchStatus :: S m n => Branch.Hash -> BranchStatus m -> n ()
setBranchStatus h s = do
  when debug (traceM $ "setBranchStatus " ++ take 10 (show h) ++ " " ++ show s)
  branchStatus . at h .= Just s

setWatchStatus :: S m n => WatchKind -> Reference.Id -> WatchStatus -> n ()
setWatchStatus k r@(Reference.Id h i _) s = do
  when debug (traceM $ "setWatchStatus " ++ show k ++ " " ++ take 10 (show h) ++ " " ++ show i)
  watchStatus . at (k, r) .= Just s

checkTermComponent ::
  forall m n a.
  (RS m n a, V m n, E TermStatus n) =>
  (m ~> n) ->
  Hash ->
  Reference.Size ->
  n [(Term Symbol a, Type Symbol a)]
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
              Reference.Builtin {} ->
                pure ()
              Reference.DerivedId (Reference.Id h' _ _)
                | h == h' ->
                  pure () -- ignore self-references
              Reference.DerivedId (Reference.Id h' _ n') ->
                getTermStatus h' >>= \case
                  Just TermOk -> pure ()
                  Just _ -> Except.throwError TermMissingDependencies
                  Nothing -> Validate.dispute . Set.singleton $ T h' n'
        traverse_ (bitraverse_ checkDecl checkTerm . LD.toReference) termDeps
        traverse_ checkDecl typeDeps
        pure (term, typ)

checkWatchComponent ::
  forall m n a.
  (RS m n a, V m n, E WatchStatus n) =>
  (m ~> n) ->
  WatchKind ->
  Reference.Id ->
  n (Term Symbol a)
checkWatchComponent t k r@(Reference.Id h _ _) = do
  Env src _ _ <- Reader.ask
  (t $ Codebase.getWatch src k r) >>= \case
    Nothing -> Except.throwError WatchNotCached
    Just watchResult -> do
      let deps = Term.labeledDependencies watchResult
      let checkDecl = \case
            Reference.Builtin {} -> pure ()
            Reference.DerivedId (Reference.Id h' _ n') ->
              getDeclStatus h' >>= \case
                Just DeclOk -> pure ()
                Just _ -> Except.throwError WatchMissingDependencies
                Nothing -> Validate.dispute . Set.singleton $ D h' n'
          checkTerm = \case
            Reference.Builtin {} ->
              pure ()
            Reference.DerivedId (Reference.Id h' _ _)
              | h == h' ->
                pure () -- ignore self-references
            Reference.DerivedId (Reference.Id h' _ n') ->
              getTermStatus h' >>= \case
                Just TermOk -> pure ()
                Just _ -> Except.throwError WatchMissingDependencies
                Nothing -> Validate.dispute . Set.singleton $ T h' n'
      traverse_ (bitraverse_ checkDecl checkTerm . LD.toReference) deps
      pure watchResult

checkDeclComponent ::
  forall m n a.
  (RS m n a, E DeclStatus n, V m n) =>
  (m ~> n) ->
  Hash ->
  Reference.Size ->
  n [Decl Symbol a]
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
              Reference.DerivedId (Reference.Id h' _ _) | h == h' -> pure ()
              Reference.DerivedId (Reference.Id h' _ n') ->
                getDeclStatus h' >>= \case
                  Just DeclOk -> pure ()
                  Just _ -> Except.throwError DeclMissingDependencies
                  Nothing -> Validate.dispute . Set.singleton $ D h' n'
        traverse_ checkDecl deps
        pure decl

checkPatch ::
  forall m n a.
  (RS m n a, E PatchStatus n, V m n) =>
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
    -- filtering `old` is part of a workaround for ucm currently
    -- requiring the actual component in order to construct a
    -- reference to it.  See Sync22.syncPatchLocalIds
    helpTermEdit = \case
      Reference.Builtin _ -> pure True
      Reference.DerivedId (Reference.Id h _ n) ->
        getTermStatus h >>= \case
          Nothing -> Validate.refute . Set.singleton $ T h n
          Just TermOk -> pure True
          Just _ -> pure False
    helpTypeEdit = \case
      Reference.Builtin _ -> pure True
      Reference.DerivedId (Reference.Id h _ n) ->
        getDeclStatus h >>= \case
          Nothing -> Validate.refute . Set.singleton $ D h n
          Just DeclOk -> pure True
          Just _ -> pure False
    filterTermEdit old new = do
      oldOk <- helpTermEdit old
      newOk <- case new of
        TermEdit.Deprecate -> pure True
        TermEdit.Replace new _typing -> helpTermEdit new
      pure $ oldOk && newOk
    filterTypeEdit old new = do
      oldOk <- helpTypeEdit old
      newOk <- case new of
        TypeEdit.Deprecate -> pure True
        TypeEdit.Replace new -> helpTypeEdit new
      pure $ oldOk && newOk

filterBranchTermStar :: (S m n, V m n) => Metadata.Star Referent NameSegment -> n (Metadata.Star Referent NameSegment)
filterBranchTermStar (Star3 _refs names _mdType md) = do
  names' <- filterTermNames names
  let refs' = Relation.dom names'
  mdTypeValues' <- filterMetadata $ Relation.restrictDom refs' md
  let mdType' = Relation.mapRan fst mdTypeValues'
  pure $ Star3 refs' names' mdType' mdTypeValues'

filterBranchTypeStar :: (S m n, V m n) => Metadata.Star Reference.Reference NameSegment -> n (Metadata.Star Reference.Reference NameSegment)
filterBranchTypeStar (Star3 _refs names _mdType md) = do
  names' <- filterTypeNames names
  let refs' = Relation.dom names'
  mdTypeValues' <- filterMetadata $ Relation.restrictDom refs' md
  let mdType' = Relation.mapRan fst mdTypeValues'
  pure $ Star3 refs' names mdType' mdTypeValues'

filterMetadata :: (S m n, V m n, Ord r) => Relation r (Metadata.Type, Metadata.Value) -> n (Relation r (Metadata.Type, Metadata.Value))
filterMetadata = Relation.filterRanM \(t, v) ->
  validateTypeReference t &&^ validateTermReference v

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
  Reference.Builtin {} -> pure True
  Reference.DerivedId (Reference.Id h _i n) ->
    getDeclStatus h >>= \case
      Nothing -> Validate.refute . Set.singleton $ D h n
      Just DeclOk -> pure True
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

runSrc, runDest :: R m n a => (Codebase m Symbol a -> x) -> n x
runSrc = (Reader.reader srcCodebase <&>)
runDest = (Reader.reader destCodebase <&>)

runDest' :: R m n x => ReaderT Connection n a -> n a
runDest' ma = Reader.reader destConnection >>= flip runDB ma

runDB :: Connection -> ReaderT Connection m a -> m a
runDB conn action = Reader.runReaderT action conn

data DoneCount = DoneCount
  { _doneBranches :: Int,
    _doneTerms :: Int,
    _doneDecls :: Int,
    _donePatches :: Int,
    _doneWatches :: Int
  }

data ErrorCount = ErrorCount
  { _errorBranches :: Int,
    _errorTerms :: Int,
    _errorDecls :: Int,
    _errorPatches :: Int,
    _errorWatches :: Int
  }

emptyDoneCount :: DoneCount
emptyDoneCount = DoneCount 0 0 0 0 0

emptyErrorCount :: ErrorCount
emptyErrorCount = ErrorCount 0 0 0 0 0

makeLenses ''DoneCount
makeLenses ''ErrorCount

type ProgressState m = (DoneCount, ErrorCount, Status m)

simpleProgress :: MonadState (ProgressState m) n => MonadIO n => Sync.Progress n (Entity m)
simpleProgress = Sync.Progress need done error allDone
  where
    newlines = False
    logEntities = False
    -- ignore need
    need e =
      when logEntities $ liftIO $ putStrLn $ "need " ++ show e

    done e = do
      when logEntities $ liftIO $ putStrLn $ "done " ++ show e
      case e of
        C {} -> _1 . doneBranches += 1
        T {} -> _1 . doneTerms += 1
        D {} -> _1 . doneDecls += 1
        P {} -> _1 . donePatches += 1
        W {} -> _1 . doneWatches += 1
      printProgress

    error e = do
      when logEntities $ liftIO $ putStrLn $ "error " ++ show e
      case e of
        C {} -> _2 . errorBranches += 1
        T {} -> _2 . errorTerms += 1
        D {} -> _2 . errorDecls += 1
        P {} -> _2 . errorPatches += 1
        W {} -> _2 . errorWatches += 1
      printProgress

    allDone :: MonadState (DoneCount, ErrorCount, Status m) n => MonadIO n => n ()
    allDone = do
      Status branches terms decls patches watches <- Lens.use Lens._3
      liftIO $ putStrLn "Finished."
      Foldable.for_ (Map.toList decls) \(h, s) -> case s of
        DeclOk -> pure ()
        DeclMissing -> liftIO . putStrLn $ "I couldn't find the decl " ++ show h ++ ", so I filtered it out of the sync."
        DeclMissingDependencies -> liftIO . putStrLn $ "One or more dependencies of decl " ++ show h ++ " were missing, so I filtered it out of the sync."
      Foldable.for_ (Map.toList terms) \(h, s) -> case s of
        TermOk -> pure ()
        TermMissing -> liftIO . putStrLn $ "I couldn't find the  term " ++ show h ++ "so I filtered it out of the sync."
        TermMissingType -> liftIO . putStrLn $ "The type of term " ++ show h ++ " was missing, so I filtered it out of the sync."
        TermMissingDependencies -> liftIO . putStrLn $ "One or more dependencies of term " ++ show h ++ " were missing, so I filtered it out of the sync."
      Foldable.for_ (Map.toList watches) \((k, r), s) -> case s of
        WatchOk -> pure ()
        WatchNotCached -> pure ()
        WatchMissingDependencies -> liftIO . putStrLn $ "One or more dependencies of watch expression " ++ show (k, r) ++ " were missing, so I skipped it."
      Foldable.for_ (Map.toList patches) \(h, s) -> case s of
        PatchOk -> pure ()
        PatchMissing -> liftIO . putStrLn $ "I couldn't find the patch " ++ show h ++ ", so I filtered it out of the sync."
        PatchReplaced h' -> liftIO . putStrLn $ "I replaced the patch " ++ show h ++ " with the filtered version " ++ show h' ++ "."
      Foldable.for_ (Map.toList branches) \(h, s) -> case s of
        BranchOk -> pure ()
        BranchReplaced h' _ -> liftIO . putStrLn $ "I replaced the branch " ++ show h ++ " with the filtered version " ++ show h' ++ "."

    printProgress :: MonadState (ProgressState m) n => MonadIO n => n ()
    printProgress = do
      (DoneCount b t d p w, ErrorCount b' t' d' p' w', _) <- State.get
      let ways :: [Maybe String] =
            [ Monoid.whenM (b > 0 || b' > 0) (Just $ show (b + b') ++ " branches" ++ Monoid.whenM (b' > 0) (" (" ++ show b' ++ " repaired)")),
              Monoid.whenM (t > 0 || t' > 0) (Just $ show t ++ " terms" ++ Monoid.whenM (t' > 0) (" (" ++ show t' ++ " errors)")),
              Monoid.whenM (w > 0 || w' > 0) (Just $ show w ++ " test results" ++ Monoid.whenM (w' > 0) (" (" ++ show w' ++ " errors)")),
              Monoid.whenM (d > 0 || d' > 0) (Just $ show d ++ " types" ++ Monoid.whenM (d' > 0) (" (" ++ show d' ++ " errors)")),
              Monoid.whenM (p > 0 || p' > 0) (Just $ show p ++ " patches" ++ Monoid.whenM (p' > 0) (" (" ++ show p' ++ " errors)"))
            ]
      liftIO do
        putStr $ "\rSynced " ++ List.intercalate ", " (catMaybes ways) ++ Monoid.whenM newlines "\n"
        hFlush stdout

instance Show (Entity m) where
  show = show . toEntity'

data Entity'
  = C' Branch.Hash
  | T' Hash
  | D' Hash
  | P' Branch.EditHash
  | W' WatchKind Reference.Id
  deriving (Eq, Ord, Show)

toEntity' :: Entity m -> Entity'
toEntity' = \case
  C h _ -> C' h
  T h _ -> T' h
  D h _ -> D' h
  P h -> P' h
  W k r -> W' k r

instance Eq (Entity m) where
  x == y = toEntity' x == toEntity' y

instance Ord (Entity m) where
  x `compare` y = toEntity' x `compare` toEntity' y

data BranchStatus'
  = BranchOk'
  | BranchReplaced' Branch.Hash
  deriving (Eq, Ord, Show)

toBranchStatus' :: BranchStatus m -> BranchStatus'
toBranchStatus' = \case
  BranchOk -> BranchOk'
  BranchReplaced h _ -> BranchReplaced' h

instance Eq (BranchStatus m) where
  x == y = toBranchStatus' x == toBranchStatus' y

instance Ord (BranchStatus m) where
  x `compare` y = toBranchStatus' x `compare` toBranchStatus' y

instance Show (BranchStatus m) where
  show = show . toBranchStatus'
