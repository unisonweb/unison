{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Branch
  ( -- * Branch types
    Branch (..),
    Branch0 (..),
    MergeMode (..),
    Raw (..),
    Star,
    Hash,
    EditHash,
    pattern Hash,

    -- * Branch construction
    empty,
    empty0,
    branch0,
    one,
    toCausalRaw,
    transform,

    -- * Branch history

    -- ** History queries
    isEmpty,
    isEmpty0,
    isOne,
    head,
    headHash,
    before,
    findHistoricalHQs,
    findHistoricalRefs,
    findHistoricalRefs',
    namesDiff,

    -- ** History updates
    step,
    stepEverywhere,
    uncons,
    merge,
    merge',

    -- * Branch children

    -- ** Children lenses
    children,

    -- ** Children queries
    toList0,
    getAt,
    getAt',
    getAt0,

    -- ** Children updates
    setChildBranch,
    stepManyAt,
    stepManyAt0,
    stepManyAtM,
    modifyAtM,

    -- * Branch terms/types

    -- ** Term/type lenses
    terms,
    types,

    -- ** Term/type queries
    deepReferents,
    deepTypeReferences,
    toNames0,

    -- ** Term/type updates
    addTermName,
    addTypeName,
    deleteTermName,
    deleteTypeName,

    -- * Branch patches

    -- ** Patch queries
    deepEdits',
    getPatch,
    getMaybePatch,

    -- ** Patch updates
    replacePatch,
    deletePatch,
    modifyPatches,

    -- * Branch serialization
    cachedRead,
    boundedCache,
    Cache,
    sync,

    -- * Unused
    childrenR,
    debugPaths,
    editedPatchRemoved,
    editsR,
    findHistoricalSHs,
    fork,
    lca,
    move,
    numHashChars,
    printDebugPaths,
    removedPatchEdited,
    stepAt,
    stepAtM,
    termsR,
    typesR,
  )
where

import Control.Lens hiding (children, cons, transform, uncons)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Data.Bifunctor (second)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Set as Set
import Unison.Codebase.Causal
  ( Causal,
    pattern RawCons,
    pattern RawMerge,
    pattern RawOne,
  )
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Patch (Patch)
import qualified Unison.Codebase.Patch as Patch
import Unison.Codebase.Path (Path (..))
import qualified Unison.Codebase.Path as Path
import qualified Unison.Hash as Hash
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import Unison.Hashable (Hashable)
import qualified Unison.Hashable as H
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name (..))
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment)
import qualified Unison.NameSegment as NameSegment
import Unison.Names2 (Names' (Names), Names0)
import qualified Unison.Names2 as Names
import qualified Unison.Names3 as Names
import Unison.Prelude hiding (empty)
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import qualified Unison.Util.Cache as Cache
import qualified Unison.Util.List as List
import Unison.Util.Map (unionWithM)
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation4 as R4
import qualified Unison.Util.Star3 as Star3
import Prelude hiding (head, read, subtract)

newtype Branch m = Branch {_history :: Causal m Raw (Branch0 m)}
  deriving (Eq, Ord)

type Hash = Causal.RawHash Raw

type EditHash = Hash.Hash

-- Star3 r n Metadata.Type (Metadata.Type, Metadata.Value)
type Star r n = Metadata.Star r n

data Branch0 m = Branch0
  { _terms :: Star Referent NameSegment,
    _types :: Star Reference NameSegment,
    _children :: Map NameSegment (Branch m),
    _edits :: Map NameSegment (EditHash, m Patch),
    -- names and metadata for this branch and its children
    -- (ref, (name, value)) iff ref has metadata `value` at name `name`
    deepTerms :: Relation Referent Name,
    deepTypes :: Relation Reference Name,
    deepTermMetadata :: Metadata.R4 Referent Name,
    deepTypeMetadata :: Metadata.R4 Reference Name,
    deepPaths :: Set Path,
    deepEdits :: Map Name EditHash
  }

-- Represents a shallow diff of a Branch0.
-- Each of these `Star`s contain metadata as well, so an entry in
-- `added` or `removed` could be an update to the metadata.
data BranchDiff = BranchDiff
  { addedTerms :: Star Referent NameSegment,
    removedTerms :: Star Referent NameSegment,
    addedTypes :: Star Reference NameSegment,
    removedTypes :: Star Reference NameSegment,
    changedPatches :: Map NameSegment Patch.PatchDiff
  }
  deriving (Eq, Ord, Show)

instance Semigroup BranchDiff where
  left <> right =
    BranchDiff
      { addedTerms = addedTerms left <> addedTerms right,
        removedTerms = removedTerms left <> removedTerms right,
        addedTypes = addedTypes left <> addedTypes right,
        removedTypes = removedTypes left <> removedTypes right,
        changedPatches =
          Map.unionWith (<>) (changedPatches left) (changedPatches right)
      }

instance Monoid BranchDiff where
  mappend = (<>)
  mempty = BranchDiff mempty mempty mempty mempty mempty

-- The raw Branch
data Raw = Raw
  { _termsR :: Star Referent NameSegment,
    _typesR :: Star Reference NameSegment,
    _childrenR :: Map NameSegment Hash,
    _editsR :: Map NameSegment EditHash
  }

makeLenses ''Branch
makeLensesFor [("_edits", "edits")] ''Branch0
makeLenses ''Raw

toNames0 :: Branch0 m -> Names0
toNames0 b =
  Names
    (R.swap . deepTerms $ b)
    (R.swap . deepTypes $ b)

-- This stops searching for a given ShortHash once it encounters
-- any term or type in any Branch0 that satisfies that ShortHash.
findHistoricalSHs ::
  Monad m => Set ShortHash -> Branch m -> m (Set ShortHash, Names0)
findHistoricalSHs =
  findInHistory
    (\sh r _n -> sh `SH.isPrefixOf` Referent.toShortHash r)
    (\sh r _n -> sh `SH.isPrefixOf` Reference.toShortHash r)

-- This stops searching for a given HashQualified once it encounters
-- any term or type in any Branch0 that satisfies that HashQualified.
findHistoricalHQs ::
  Monad m =>
  Set HashQualified ->
  Branch m ->
  m (Set HashQualified, Names0)
findHistoricalHQs =
  findInHistory
    (\hq r n -> HQ.matchesNamedReferent n r hq)
    (\hq r n -> HQ.matchesNamedReference n r hq)

findHistoricalRefs ::
  Monad m =>
  Set LabeledDependency ->
  Branch m ->
  m (Set LabeledDependency, Names0)
findHistoricalRefs =
  findInHistory
    (\query r _n -> LD.fold (const False) (== r) query)
    (\query r _n -> LD.fold (== r) (const False) query)

findHistoricalRefs' ::
  Monad m =>
  Set Reference ->
  Branch m ->
  m (Set Reference, Names0)
findHistoricalRefs' =
  findInHistory
    (\queryRef r _n -> r == Referent.Ref queryRef)
    (\queryRef r _n -> r == queryRef)

findInHistory ::
  forall m q.
  (Monad m, Ord q) =>
  (q -> Referent -> Name -> Bool) ->
  (q -> Reference -> Name -> Bool) ->
  Set q ->
  Branch m ->
  m (Set q, Names0)
findInHistory termMatches typeMatches queries b =
  (Causal.foldHistoryUntil f (queries, mempty) . _history) b <&> \case
    -- could do something more sophisticated here later to report that some SH
    -- couldn't be found anywhere in the history.  but for now, I assume that
    -- the normal thing will happen when it doesn't show up in the namespace.
    Causal.Satisfied (_, names) -> (mempty, names)
    Causal.Unsatisfied (missing, names) -> (missing, names)
  where
    -- in order to not favor terms over types, we iterate through the ShortHashes,
    -- for each `remainingQueries`, if we find a matching Referent or Reference,
    -- we remove `q` from the accumulated `remainingQueries`, and add the Ref* to
    -- the accumulated `names0`.
    f acc@(remainingQueries, _) b0 = (acc', null remainingQueries')
      where
        acc'@(remainingQueries', _) = foldl' findQ acc remainingQueries
        findQ :: (Set q, Names0) -> q -> (Set q, Names0)
        findQ acc sh =
          foldl'
            (doType sh)
            ( foldl'
                (doTerm sh)
                acc
                (R.toList $ deepTerms b0)
            )
            (R.toList $ deepTypes b0)
        doTerm q acc@(remainingSHs, names0) (r, n) =
          if termMatches q r n
            then (Set.delete q remainingSHs, Names.addTerm n r names0)
            else acc
        doType q acc@(remainingSHs, names0) (r, n) =
          if typeMatches q r n
            then (Set.delete q remainingSHs, Names.addType n r names0)
            else acc

deepReferents :: Branch0 m -> Set Referent
deepReferents = R.dom . deepTerms

deepTypeReferences :: Branch0 m -> Set Reference
deepTypeReferences = R.dom . deepTypes

terms :: Lens' (Branch0 m) (Star Referent NameSegment)
terms = lens _terms (\Branch0 {..} x -> branch0 x _types _children _edits)

types :: Lens' (Branch0 m) (Star Reference NameSegment)
types = lens _types (\Branch0 {..} x -> branch0 _terms x _children _edits)

children :: Lens' (Branch0 m) (Map NameSegment (Branch m))
children = lens _children (\Branch0 {..} x -> branch0 _terms _types x _edits)

-- creates a Branch0 from the primary fields and derives the others.
branch0 ::
  Metadata.Star Referent NameSegment ->
  Metadata.Star Reference NameSegment ->
  Map NameSegment (Branch m) ->
  Map NameSegment (EditHash, m Patch) ->
  Branch0 m
branch0 terms types children edits =
  Branch0
    terms
    types
    children
    edits
    deepTerms'
    deepTypes'
    deepTermMetadata'
    deepTypeMetadata'
    deepPaths'
    deepEdits'
  where
    nameSegToName = Name.unsafeFromText . NameSegment.toText
    deepTerms' =
      (R.mapRan nameSegToName . Star3.d1) terms
        <> foldMap go (Map.toList children)
      where
        go (nameSegToName -> n, b) =
          R.mapRan (Name.joinDot n) (deepTerms $ head b) -- could use mapKeysMonotonic
    deepTypes' =
      (R.mapRan nameSegToName . Star3.d1) types
        <> foldMap go (Map.toList children)
      where
        go (nameSegToName -> n, b) =
          R.mapRan (Name.joinDot n) (deepTypes $ head b) -- could use mapKeysMonotonic
    deepTermMetadata' =
      R4.mapD2 nameSegToName (Metadata.starToR4 terms)
        <> foldMap go (Map.toList children)
      where
        go (nameSegToName -> n, b) =
          R4.mapD2 (Name.joinDot n) (deepTermMetadata $ head b)
    deepTypeMetadata' =
      R4.mapD2 nameSegToName (Metadata.starToR4 types)
        <> foldMap go (Map.toList children)
      where
        go (nameSegToName -> n, b) =
          R4.mapD2 (Name.joinDot n) (deepTypeMetadata $ head b)
    deepPaths' =
      Set.map Path.singleton (Map.keysSet children)
        <> foldMap go (Map.toList children)
      where
        go (nameSeg, b) = Set.map (Path.cons nameSeg) (deepPaths $ head b)
    deepEdits' =
      Map.mapKeys nameSegToName (Map.map fst edits)
        <> foldMap go (Map.toList children)
      where
        go (nameSeg, b) =
          Map.mapKeys (nameSegToName nameSeg `Name.joinDot`) . deepEdits $ head b

head :: Branch m -> Branch0 m
head (Branch c) = Causal.head c

headHash :: Branch m -> Hash
headHash (Branch c) = Causal.currentHash c

deepEdits' :: Branch0 m -> Map Name (EditHash, m Patch)
deepEdits' b = go id b
  where
    -- can change this to an actual prefix once Name is a [NameSegment]
    go :: (Name -> Name) -> Branch0 m -> Map Name (EditHash, m Patch)
    go addPrefix Branch0 {..} =
      Map.mapKeysMonotonic (addPrefix . Name.fromSegment) _edits
        <> foldMap f (Map.toList _children)
      where
        f :: (NameSegment, Branch m) -> Map Name (EditHash, m Patch)
        f (c, b) = go (addPrefix . Name.joinDot (Name.fromSegment c)) (head b)

data MergeMode = RegularMerge | SquashMerge deriving (Eq, Ord, Show)

merge :: forall m. Monad m => Branch m -> Branch m -> m (Branch m)
merge = merge' RegularMerge

-- Discards the history of a Branch0's children, recursively
discardHistory0 :: Applicative m => Branch0 m -> Branch0 m
discardHistory0 = over children (fmap tweak)
  where
    tweak b = cons (discardHistory0 (head b)) empty

merge' :: forall m. Monad m => MergeMode -> Branch m -> Branch m -> m (Branch m)
merge' _ b1 b2 | isEmpty b1 = pure b2
merge' mode b1 b2 | isEmpty b2 = case mode of
  RegularMerge -> pure b1
  SquashMerge -> pure $ cons (discardHistory0 (head b1)) b2
merge' mode (Branch x) (Branch y) =
  Branch <$> case mode of
    RegularMerge -> Causal.threeWayMerge combine x y
    SquashMerge -> Causal.squashMerge combine x y
  where
    combine :: Maybe (Branch0 m) -> Branch0 m -> Branch0 m -> m (Branch0 m)
    combine Nothing l r = merge0 mode l r
    combine (Just ca) l r = do
      dl <- diff0 ca l
      dr <- diff0 ca r
      head0 <- apply ca (dl <> dr)
      children <-
        Map.mergeA
          (Map.traverseMaybeMissing $ combineMissing ca)
          (Map.traverseMaybeMissing $ combineMissing ca)
          (Map.zipWithAMatched $ const (merge' mode))
          (_children l)
          (_children r)
      pure $ branch0 (_terms head0) (_types head0) children (_edits head0)

    combineMissing ca k cur =
      case Map.lookup k (_children ca) of
        Nothing -> pure $ Just cur
        Just old -> do
          nw <- merge' mode (cons empty0 old) cur
          if isEmpty0 $ head nw
            then pure Nothing
            else pure $ Just nw

    apply :: Branch0 m -> BranchDiff -> m (Branch0 m)
    apply b0 BranchDiff {..} = do
      patches <-
        sequenceA $
          Map.differenceWith patchMerge (pure @m <$> _edits b0) changedPatches
      let newPatches = makePatch <$> Map.difference changedPatches (_edits b0)
          makePatch Patch.PatchDiff {..} =
            let p = Patch.Patch _addedTermEdits _addedTypeEdits
             in (H.accumulate' p, pure p)
      pure $
        branch0
          (Star3.difference (_terms b0) removedTerms <> addedTerms)
          (Star3.difference (_types b0) removedTypes <> addedTypes)
          (_children b0)
          (patches <> newPatches)
    patchMerge mhp Patch.PatchDiff {..} = Just $ do
      (_, mp) <- mhp
      p <- mp
      let np =
            Patch.Patch
              { _termEdits =
                  R.difference (Patch._termEdits p) _removedTermEdits
                    <> _addedTermEdits,
                _typeEdits =
                  R.difference (Patch._typeEdits p) _removedTypeEdits
                    <> _addedTypeEdits
              }
      pure (H.accumulate' np, pure np)

-- `before b1 b2` is true if `b2` incorporates all of `b1`
before :: Monad m => Branch m -> Branch m -> m Bool
before (Branch x) (Branch y) = Causal.before x y

merge0 :: forall m. Monad m => MergeMode -> Branch0 m -> Branch0 m -> m (Branch0 m)
merge0 mode b1 b2 = do
  c3 <- unionWithM (merge' mode) (_children b1) (_children b2)
  e3 <- unionWithM g (_edits b1) (_edits b2)
  pure $
    branch0
      (_terms b1 <> _terms b2)
      (_types b1 <> _types b2)
      c3
      e3
  where
    g :: (EditHash, m Patch) -> (EditHash, m Patch) -> m (EditHash, m Patch)
    g (h1, m1) (h2, _) | h1 == h2 = pure (h1, m1)
    g (_, m1) (_, m2) = do
      e1 <- m1
      e2 <- m2
      let e3 = e1 <> e2
      pure (H.accumulate' e3, pure e3)

pattern Hash h = Causal.RawHash h

toList0 :: Branch0 m -> [(Path, Branch0 m)]
toList0 = go Path.empty
  where
    go p b =
      (p, b) :
      ( Map.toList (_children b)
          >>= ( \(seg, cb) ->
                  go (Path.snoc p seg) (head cb)
              )
      )

printDebugPaths :: Branch m -> String
printDebugPaths = unlines . map show . Set.toList . debugPaths

debugPaths :: Branch m -> Set (Path, Hash)
debugPaths = go Path.empty
  where
    go p b =
      Set.insert (p, headHash b) . Set.unions $
        [go (Path.snoc p seg) b | (seg, b) <- Map.toList $ _children (head b)]

data Target = TargetType | TargetTerm | TargetBranch
  deriving (Eq, Ord, Show)

instance Eq (Branch0 m) where
  a == b =
    view terms a == view terms b
      && view types a == view types b
      && view children a == view children b
      && (fmap fst . view edits) a == (fmap fst . view edits) b

data ForkFailure = SrcNotFound | DestExists

-- consider delegating to Names.numHashChars when ready to implement?
-- are those enough?
-- could move this to a read-only field in Branch0
-- could move a Names0 to a read-only field in Branch0 until it gets too big
numHashChars :: Branch m -> Int
numHashChars _b = 3

-- This type is a little ugly, so we wrap it up with a nice type alias for
-- use outside this module.
type Cache m = Cache.Cache m (Causal.RawHash Raw) (Causal m Raw (Branch0 m))

boundedCache :: MonadIO m => Word -> m (Cache m)
boundedCache = Cache.semispaceCache

-- Can use `Cache.nullCache` to disable caching if needed
cachedRead ::
  forall m.
  Monad m =>
  Cache m ->
  Causal.Deserialize m Raw Raw ->
  (EditHash -> m Patch) ->
  Hash ->
  m (Branch m)
cachedRead cache deserializeRaw deserializeEdits h =
  Branch <$> Causal.cachedRead cache d h
  where
    fromRaw :: Raw -> m (Branch0 m)
    fromRaw Raw {..} = do
      children <- traverse go _childrenR
      edits <- for _editsR $ \hash -> (hash,) . pure <$> deserializeEdits hash
      pure $ branch0 _termsR _typesR children edits
    go = cachedRead cache deserializeRaw deserializeEdits
    d :: Causal.Deserialize m Raw (Branch0 m)
    d h =
      deserializeRaw h >>= \case
        RawOne raw -> RawOne <$> fromRaw raw
        RawCons raw h -> flip RawCons h <$> fromRaw raw
        RawMerge raw hs -> flip RawMerge hs <$> fromRaw raw

sync ::
  Monad m =>
  (Hash -> m Bool) ->
  Causal.Serialize m Raw Raw ->
  (EditHash -> m Patch -> m ()) ->
  Branch m ->
  m ()
sync exists serializeRaw serializeEdits b = do
  _written <- State.execStateT (sync' exists serializeRaw serializeEdits b) mempty
  -- traceM $ "Branch.sync wrote " <> show (Set.size written) <> " namespace files."
  pure ()

-- serialize a `Branch m` indexed by the hash of its corresponding Raw
sync' ::
  forall m.
  Monad m =>
  (Hash -> m Bool) ->
  Causal.Serialize m Raw Raw ->
  (EditHash -> m Patch -> m ()) ->
  Branch m ->
  StateT (Set Hash) m ()
sync' exists serializeRaw serializeEdits b =
  Causal.sync
    exists
    serialize0
    (view history b)
  where
    serialize0 :: Causal.Serialize (StateT (Set Hash) m) Raw (Branch0 m)
    serialize0 h b0 = case b0 of
      RawOne b0 -> do
        writeB0 b0
        lift $ serializeRaw h $ RawOne (toRaw b0)
      RawCons b0 ht -> do
        writeB0 b0
        lift $ serializeRaw h $ RawCons (toRaw b0) ht
      RawMerge b0 hs -> do
        writeB0 b0
        lift $ serializeRaw h $ RawMerge (toRaw b0) hs
      where
        writeB0 :: Branch0 m -> StateT (Set Hash) m ()
        writeB0 b0 = do
          for_ (view children b0) $ \c -> do
            queued <- State.get
            when (Set.notMember (headHash c) queued) $
              sync' exists serializeRaw serializeEdits c
          for_ (view edits b0) (lift . uncurry serializeEdits)

-- this has to serialize the branch0 and its descendants in the tree,
-- and then serialize the rest of the history of the branch as well

toRaw :: Branch0 m -> Raw
toRaw Branch0 {..} =
  Raw _terms _types (headHash <$> _children) (fst <$> _edits)

toCausalRaw :: Branch m -> Causal.Raw Raw Raw
toCausalRaw = \case
  Branch (Causal.One _h e) -> RawOne (toRaw e)
  Branch (Causal.Cons _h e (ht, _m)) -> RawCons (toRaw e) ht
  Branch (Causal.Merge _h e tls) -> RawMerge (toRaw e) (Map.keysSet tls)

-- copy a path to another path
fork ::
  Applicative m =>
  Path ->
  Path ->
  Branch m ->
  Either ForkFailure (Branch m)
fork src dest root = case getAt src root of
  Nothing -> Left SrcNotFound
  Just src' -> case setIfNotExists dest src' root of
    Nothing -> Left DestExists
    Just root' -> Right root'

-- Move the node at src to dest.
-- It's okay if `dest` is inside `src`, just create empty levels.
-- Try not to `step` more than once at each node.
move ::
  Applicative m =>
  Path ->
  Path ->
  Branch m ->
  Either ForkFailure (Branch m)
move src dest root = case getAt src root of
  Nothing -> Left SrcNotFound
  Just src' ->
    -- make sure dest doesn't already exist
    case getAt dest root of
      Just _destExists -> Left DestExists
      Nothing ->
        -- find and update common ancestor of `src` and `dest`:
        Right $ modifyAt ancestor go root
        where
          (ancestor, relSrc, relDest) = Path.relativeToAncestor src dest
          go = deleteAt relSrc . setAt relDest src'

setIfNotExists ::
  Applicative m => Path -> Branch m -> Branch m -> Maybe (Branch m)
setIfNotExists dest b root = case getAt dest root of
  Just _destExists -> Nothing
  Nothing -> Just $ setAt dest b root

setAt :: Applicative m => Path -> Branch m -> Branch m -> Branch m
setAt path b = modifyAt path (const b)

deleteAt :: Applicative m => Path -> Branch m -> Branch m
deleteAt path = setAt path empty

-- returns `Nothing` if no Branch at `path` or if Branch is empty at `path`
getAt ::
  Path ->
  Branch m ->
  Maybe (Branch m)
getAt path root = case Path.uncons path of
  Nothing -> if isEmpty root then Nothing else Just root
  Just (seg, path) -> case Map.lookup seg (_children $ head root) of
    Just b -> getAt path b
    Nothing -> Nothing

getAt' :: Path -> Branch m -> Branch m
getAt' p b = fromMaybe empty $ getAt p b

getAt0 :: Path -> Branch0 m -> Branch0 m
getAt0 p b = case Path.uncons p of
  Nothing -> b
  Just (seg, path) -> case Map.lookup seg (_children b) of
    Just c -> getAt0 path (head c)
    Nothing -> empty0

empty :: Branch m
empty = Branch $ Causal.one empty0

one :: Branch0 m -> Branch m
one = Branch . Causal.one

empty0 :: Branch0 m
empty0 =
  Branch0 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

isEmpty0 :: Branch0 m -> Bool
isEmpty0 = (== empty0)

isEmpty :: Branch m -> Bool
isEmpty = (== empty)

step :: Applicative m => (Branch0 m -> Branch0 m) -> Branch m -> Branch m
step f = over history (Causal.stepDistinct f)

stepM :: (Monad m, Monad n) => (Branch0 m -> n (Branch0 m)) -> Branch m -> n (Branch m)
stepM f = mapMOf history (Causal.stepDistinctM f)

cons :: Applicative m => Branch0 m -> Branch m -> Branch m
cons = step . const

isOne :: Branch m -> Bool
isOne (Branch Causal.One {}) = True
isOne _ = False

uncons :: Applicative m => Branch m -> m (Maybe (Branch0 m, Branch m))
uncons (Branch b) = go <$> Causal.uncons b
  where
    go = over (_Just . _2) Branch

-- Modify the branch0 at the head of at `path` with `f`,
-- after creating it if necessary.  Preserves history.
stepAt ::
  forall m.
  Applicative m =>
  Path ->
  (Branch0 m -> Branch0 m) ->
  Branch m ->
  Branch m
stepAt p f = modifyAt p g
  where
    g :: Branch m -> Branch m
    g (Branch b) = Branch . Causal.consDistinct (f (Causal.head b)) $ b

stepManyAt ::
  (Monad m, Foldable f) =>
  f (Path, Branch0 m -> Branch0 m) ->
  Branch m ->
  Branch m
stepManyAt actions = step (stepManyAt0 actions)

-- Modify the branch0 at the head of at `path` with `f`,
-- after creating it if necessary.  Preserves history.
stepAtM ::
  forall n m.
  (Functor n, Applicative m) =>
  Path ->
  (Branch0 m -> n (Branch0 m)) ->
  Branch m ->
  n (Branch m)
stepAtM p f = modifyAtM p g
  where
    g :: Branch m -> n (Branch m)
    g (Branch b) = do
      b0' <- f (Causal.head b)
      pure $ Branch . Causal.consDistinct b0' $ b

stepManyAtM ::
  (Monad m, Monad n, Foldable f) =>
  f (Path, Branch0 m -> n (Branch0 m)) ->
  Branch m ->
  n (Branch m)
stepManyAtM actions = stepM (stepManyAt0M actions)

-- starting at the leaves, apply `f` to every level of the branch.
stepEverywhere ::
  Applicative m => (Branch0 m -> Branch0 m) -> (Branch0 m -> Branch0 m)
stepEverywhere f Branch0 {..} = f (branch0 _terms _types children _edits)
  where
    children = fmap (step $ stepEverywhere f) _children

-- Creates a function to fix up the children field._1
-- If the action emptied a child, then remove the mapping,
-- otherwise update it.
-- Todo: Fix this in hashing & serialization instead of here?
getChildBranch :: NameSegment -> Branch0 m -> Branch m
getChildBranch seg b = fromMaybe empty $ Map.lookup seg (_children b)

setChildBranch :: NameSegment -> Branch m -> Branch0 m -> Branch0 m
setChildBranch seg b = over children (updateChildren seg b)

getPatch :: Applicative m => NameSegment -> Branch0 m -> m Patch
getPatch seg b = case Map.lookup seg (_edits b) of
  Nothing -> pure Patch.empty
  Just (_, p) -> p

getMaybePatch :: Applicative m => NameSegment -> Branch0 m -> m (Maybe Patch)
getMaybePatch seg b = case Map.lookup seg (_edits b) of
  Nothing -> pure Nothing
  Just (_, p) -> Just <$> p

modifyPatches ::
  Monad m => NameSegment -> (Patch -> Patch) -> Branch0 m -> m (Branch0 m)
modifyPatches seg f = mapMOf edits update
  where
    update m = do
      p' <- case Map.lookup seg m of
        Nothing -> pure $ f Patch.empty
        Just (_, p) -> f <$> p
      let h = H.accumulate' p'
      pure $ Map.insert seg (h, pure p') m

replacePatch :: Applicative m => NameSegment -> Patch -> Branch0 m -> Branch0 m
replacePatch n p = over edits (Map.insert n (H.accumulate' p, pure p))

deletePatch :: NameSegment -> Branch0 m -> Branch0 m
deletePatch n = over edits (Map.delete n)

updateChildren ::
  NameSegment ->
  Branch m ->
  Map NameSegment (Branch m) ->
  Map NameSegment (Branch m)
updateChildren seg updatedChild =
  if isEmpty updatedChild
    then Map.delete seg
    else Map.insert seg updatedChild

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAt ::
  Applicative m =>
  Path ->
  (Branch m -> Branch m) ->
  Branch m ->
  Branch m
modifyAt path f = runIdentity . modifyAtM path (pure . f)

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAtM ::
  forall n m.
  Functor n =>
  Applicative m => -- because `Causal.cons` uses `pure`
  Path ->
  (Branch m -> n (Branch m)) ->
  Branch m ->
  n (Branch m)
modifyAtM path f b = case Path.uncons path of
  Nothing -> f b
  Just (seg, path) -> do
    -- Functor
    let child = getChildBranch seg (head b)
    child' <- modifyAtM path f child
    -- step the branch by updating its children according to fixup
    pure $ step (setChildBranch seg child') b

-- stepManyAt0 consolidates several changes into a single step
stepManyAt0 ::
  forall f m.
  (Monad m, Foldable f) =>
  f (Path, Branch0 m -> Branch0 m) ->
  Branch0 m ->
  Branch0 m
stepManyAt0 actions =
  runIdentity . stepManyAt0M [(p, pure . f) | (p, f) <- toList actions]

stepManyAt0M ::
  forall m n f.
  (Monad m, Monad n, Foldable f) =>
  f (Path, Branch0 m -> n (Branch0 m)) ->
  Branch0 m ->
  n (Branch0 m)
stepManyAt0M actions b = go (toList actions) b
  where
    go :: [(Path, Branch0 m -> n (Branch0 m))] -> Branch0 m -> n (Branch0 m)
    go actions b =
      let -- combines the functions that apply to this level of the tree
          currentAction b = foldM (\b f -> f b) b [f | (Path.Empty, f) <- actions]

          -- groups the actions based on the child they apply to
          childActions :: Map NameSegment [(Path, Branch0 m -> n (Branch0 m))]
          childActions =
            List.multimap [(seg, (rest, f)) | (seg :< rest, f) <- actions]

          -- alters the children of `b` based on the `childActions` map
          stepChildren :: Map NameSegment (Branch m) -> n (Map NameSegment (Branch m))
          stepChildren children0 = foldM g children0 $ Map.toList childActions
            where
              g children (seg, actions) = do
                -- Recursively applies the relevant actions to the child branch
                -- The `findWithDefault` is important - it allows the stepManyAt
                -- to create new children at paths that don't previously exist.
                child <- stepM (go actions) (Map.findWithDefault empty seg children0)
                pure $ updateChildren seg child children
       in do
            c2 <- stepChildren (view children b)
            currentAction (set children c2 b)

instance Hashable (Branch0 m) where
  tokens b =
    [ H.accumulateToken (_terms b),
      H.accumulateToken (_types b),
      H.accumulateToken (headHash <$> _children b)
    ]

-- getLocalBranch :: Hash -> IO Branch
-- getGithubBranch :: RemotePath -> IO Branch
-- getLocalEdit :: GUID -> IO Patch

-- todo: consider inlining these into Actions2
addTermName ::
  Referent -> NameSegment -> Metadata.Metadata -> Branch0 m -> Branch0 m
addTermName r new md =
  over terms (Metadata.insertWithMetadata (r, md) . Star3.insertD1 (r, new))

addTypeName ::
  Reference -> NameSegment -> Metadata.Metadata -> Branch0 m -> Branch0 m
addTypeName r new md =
  over types (Metadata.insertWithMetadata (r, md) . Star3.insertD1 (r, new))

-- addTermNameAt :: Path.Split -> Referent -> Branch0 m -> Branch0 m
-- addTypeNameAt :: Path.Split -> Reference -> Branch0 m -> Branch0 m

deleteTermName :: Referent -> NameSegment -> Branch0 m -> Branch0 m
deleteTermName r n b
  | Star3.memberD1 (r, n) (view terms b) =
    over terms (Star3.deletePrimaryD1 (r, n)) b
deleteTermName _ _ b = b

deleteTypeName :: Reference -> NameSegment -> Branch0 m -> Branch0 m
deleteTypeName r n b
  | Star3.memberD1 (r, n) (view types b) =
    over types (Star3.deletePrimaryD1 (r, n)) b
deleteTypeName _ _ b = b

namesDiff :: Branch m -> Branch m -> Names.Diff
namesDiff b1 b2 = Names.diff0 (toNames0 (head b1)) (toNames0 (head b2))

lca :: Monad m => Branch m -> Branch m -> m (Maybe (Branch m))
lca (Branch a) (Branch b) = fmap Branch <$> Causal.lca a b

diff0 :: Monad m => Branch0 m -> Branch0 m -> m BranchDiff
diff0 old new = do
  newEdits <- sequenceA $ snd <$> _edits new
  oldEdits <- sequenceA $ snd <$> _edits old
  let diffEdits =
        Map.merge
          (Map.mapMissing $ \_ p -> Patch.diff p mempty)
          (Map.mapMissing $ \_ p -> Patch.diff mempty p)
          (Map.zipWithMatched (const Patch.diff))
          newEdits
          oldEdits
  pure $
    BranchDiff
      { addedTerms = Star3.difference (_terms new) (_terms old),
        removedTerms = Star3.difference (_terms old) (_terms new),
        addedTypes = Star3.difference (_types new) (_types old),
        removedTypes = Star3.difference (_types old) (_types new),
        changedPatches = diffEdits
      }

transform :: Functor m => (forall a. m a -> n a) -> Branch m -> Branch n
transform f b = case _history b of
  causal -> Branch . Causal.transform f $ transformB0s f causal
  where
    transformB0 :: Functor m => (forall a. m a -> n a) -> Branch0 m -> Branch0 n
    transformB0 f b =
      b
        { _children = transform f <$> _children b,
          _edits = second f <$> _edits b
        }

    transformB0s ::
      Functor m =>
      (forall a. m a -> n a) ->
      Causal m Raw (Branch0 m) ->
      Causal m Raw (Branch0 n)
    transformB0s f = Causal.unsafeMapHashPreserving (transformB0 f)

data BranchAttentions = BranchAttentions
  { -- Patches that were edited on the right but entirely removed on the left.
    removedPatchEdited :: [Name],
    -- Patches that were edited on the left but entirely removed on the right.
    editedPatchRemoved :: [Name]
  }

instance Semigroup BranchAttentions where
  BranchAttentions edited1 removed1 <> BranchAttentions edited2 removed2 =
    BranchAttentions (edited1 <> edited2) (removed1 <> removed2)

instance Monoid BranchAttentions where
  mempty = BranchAttentions [] []
  mappend = (<>)

data RefCollisions = RefCollisions
  { termCollisions :: Relation Name Name,
    typeCollisions :: Relation Name Name
  }
  deriving (Eq, Show)

instance Semigroup RefCollisions where
  (<>) = mappend

instance Monoid RefCollisions where
  mempty = RefCollisions mempty mempty
  mappend r1 r2 =
    RefCollisions
      (termCollisions r1 <> termCollisions r2)
      (typeCollisions r1 <> typeCollisions r2)
