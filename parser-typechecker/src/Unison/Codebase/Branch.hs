{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Branch where

import Unison.Prelude hiding (empty)

import           Prelude                  hiding (head,read,subtract)

import           Control.Lens            hiding ( children, cons, transform )
import qualified Control.Monad                 as Monad
import qualified Data.Map                      as Map
import qualified Data.Map.Merge.Lazy           as Map
import qualified Data.Set                      as Set
import qualified Unison.Codebase.Patch         as Patch
import           Unison.Codebase.Patch          ( Patch )
import qualified Unison.Codebase.Causal        as Causal
import           Unison.Codebase.Causal         ( Causal
                                                , pattern RawOne
                                                , pattern RawCons
                                                , pattern RawMerge
                                                )
import           Unison.Codebase.Path           ( Path(..) )
import qualified Unison.Codebase.Path          as Path
import           Unison.Codebase.NameSegment    ( NameSegment )
import qualified Unison.Codebase.NameSegment   as NameSegment
import qualified Unison.Codebase.Metadata      as Metadata
import qualified Unison.Hash                   as Hash
import           Unison.Hashable                ( Hashable )
import qualified Unison.Hashable               as H
import           Unison.Name                    ( Name, RelName )
import qualified Unison.Name                   as Name
import qualified Unison.Names2                 as Names
import qualified Unison.Names3                 as Names
import           Unison.Names2                  ( Names'(Names), Names0 )
import           Unison.Reference               ( Reference )
import           Unison.Referent                ( Referent )
import qualified Unison.Referent              as Referent
import qualified Unison.Reference              as Reference

import qualified Unison.Util.Relation         as R
import           Unison.Util.Relation           ( Relation )
import qualified Unison.Util.Star3             as Star3
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import qualified Unison.HashQualified as HQ
import Unison.HashQualified (HashQualified)
import qualified Unison.LabeledDependency as LD
import Unison.LabeledDependency (LabeledDependency)

newtype Branch m = Branch { _history :: Causal m Raw (Branch0 m) }
  deriving (Eq, Ord)

type Hash = Causal.RawHash Raw
type EditHash = Hash.Hash

-- Star3 r n Metadata.Type (Metadata.Type, Metadata.Value)
type Star r n = Metadata.Star r n

data Branch0 m = Branch0
  { _terms :: Star Referent NameSegment
  , _types :: Star Reference NameSegment
  , _children :: Map NameSegment (Branch m)
  , _edits :: Map NameSegment (EditHash, m Patch)
  -- names and metadata for this branch and its children
  , deepTerms :: Star Referent Name
  , deepTypes :: Star Reference Name
  , deepPaths :: Set Path
  , deepEdits :: Map Name EditHash
  }

-- Each of these `Star`s contain metadata as well, so an entry in
-- `added` or `removed` could be an update to the metadata.
data BranchDiff = BranchDiff
  { addedTerms :: Star Referent NameSegment
  , removedTerms :: Star Referent NameSegment
  , addedTypes :: Star Reference NameSegment
  , removedTypes :: Star Reference NameSegment
  , changedPatches :: Map NameSegment Patch.PatchDiff
  } deriving (Eq, Ord, Show)

instance Semigroup BranchDiff where
  left <> right = BranchDiff
    { addedTerms     = addedTerms left <> addedTerms right
    , removedTerms   = removedTerms left <> removedTerms right
    , addedTypes     = addedTypes left <> addedTypes right
    , removedTypes   = removedTypes left <> removedTypes right
    , changedPatches =
        Map.unionWith (<>) (changedPatches left) (changedPatches right)
    }

instance Monoid BranchDiff where
  mappend = (<>)
  mempty = BranchDiff mempty mempty mempty mempty mempty

-- The raw Branch
data Raw = Raw
  { _termsR :: Star Referent NameSegment
  , _typesR :: Star Reference NameSegment
  , _childrenR :: Map NameSegment Hash
  , _editsR :: Map NameSegment EditHash
  }

makeLenses ''Branch
makeLensesFor [("_edits", "edits")] ''Branch0
makeLenses ''Raw

-- | Like @deepTerms@, but returns relative names instead.
--
-- FIXME This should replace @deepTerms@ eventually, since they are all
-- relative.
relDeepTerms :: Branch0 m -> Star Referent RelName
relDeepTerms = Star3.mapD1 Name.unsafeAsRelName . deepTerms

-- | Like @deepTypes@, but returns relative names instead.
--
-- FIXME This should replace @deepTypes@ eventually, since they are all
-- relative.
relDeepTypes :: Branch0 m -> Star Reference RelName
relDeepTypes = Star3.mapD1 Name.unsafeAsRelName . deepTypes

toNames0 :: Branch0 m -> Names0
toNames0 b = Names (R.swap . Star3.d1 . deepTerms $ b)
                   (R.swap . Star3.d1 . deepTypes $ b)

-- This stops searching for a given ShortHash once it encounters
-- any term or type in any Branch0 that satisfies that ShortHash.
findHistoricalSHs
  :: Monad m => Set ShortHash -> Branch m -> m (Set ShortHash, Names0)
findHistoricalSHs = findInHistory
  (\sh r _n -> sh `SH.isPrefixOf` Referent.toShortHash r)
  (\sh r _n -> sh `SH.isPrefixOf` Reference.toShortHash r)

-- This stops searching for a given HashQualified once it encounters
-- any term or type in any Branch0 that satisfies that HashQualified.
findHistoricalHQs :: Monad m
                  => Set HashQualified
                  -> Branch m
                  -> m (Set HashQualified, Names0)
findHistoricalHQs = findInHistory
  (\hq r n -> HQ.matchesNamedReferent n r hq)
  (\hq r n -> HQ.matchesNamedReference n r hq)

findHistoricalRefs :: Monad m => Set LabeledDependency -> Branch m
                   -> m (Set LabeledDependency, Names0)
findHistoricalRefs = findInHistory
  (\query r _n -> LD.fold (const False) (==r) query)
  (\query r _n -> LD.fold (==r) (const False) query)

findInHistory :: forall m q. (Monad m, Ord q)
  => (q -> Referent -> Name -> Bool)
  -> (q -> Reference -> Name -> Bool)
  -> Set q -> Branch m -> m (Set q, Names0)
findInHistory termMatches typeMatches queries b =
  (Causal.foldHistoryUntil f (queries, mempty) . _history) b <&> \case
    -- could do something more sophisticated here later to report that some SH
    -- couldn't be found anywhere in the history.  but for now, I assume that
    -- the normal thing will happen when it doesn't show up in the namespace.
    Causal.Satisfied   (_, names)       -> (mempty, names)
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
      foldl' (doType sh) (foldl' (doTerm sh) acc
                            (R.toList . Metadata.toRelation $ deepTerms b0))
                          (R.toList . Metadata.toRelation $ deepTypes b0)
    doTerm q acc@(remainingSHs, names0) (r, n) = if termMatches q r n
      then (Set.delete q remainingSHs, Names.addTerm n r names0) else acc
    doType q acc@(remainingSHs, names0) (r, n) = if typeMatches q r n
      then (Set.delete q remainingSHs, Names.addType n r names0) else acc

deepReferents :: Branch0 m -> Set Referent
deepReferents = Star3.fact . deepTerms

deepTypeReferences :: Branch0 m -> Set Reference
deepTypeReferences = Star3.fact . deepTypes

terms :: Lens' (Branch0 m) (Star Referent NameSegment)
terms = lens _terms (\Branch0{..} x -> branch0 x _types _children _edits)

types :: Lens' (Branch0 m) (Star Reference NameSegment)
types = lens _types (\Branch0{..} x -> branch0 _terms x _children _edits)

children :: Lens' (Branch0 m) (Map NameSegment (Branch m))
children = lens _children (\Branch0{..} x -> branch0 _terms _types x _edits)

-- creates a Branch0 from the primary fields and derives the others.
branch0 :: Metadata.Star Referent NameSegment
        -> Metadata.Star Reference NameSegment
        -> Map NameSegment (Branch m)
        -> Map NameSegment (EditHash, m Patch)
        -> Branch0 m
branch0 terms types children edits =
  Branch0 terms types children edits
          deepTerms' deepTypes' deepPaths' deepEdits'
  where
  nameSegToName = Name.unsafeFromText . NameSegment.toText
  deepTerms' = Star3.mapD1 nameSegToName terms
    <> foldMap go (Map.toList children)
   where
    go (nameSegToName -> n, b) =
      Star3.mapD1 (Name.joinDot n) (deepTerms $ head b)
  deepTypes' = Star3.mapD1 nameSegToName types
    <> foldMap go (Map.toList children)
   where
    go (nameSegToName -> n, b) =
      Star3.mapD1 (Name.joinDot n) (deepTypes $ head b)
  deepPaths' = Set.map Path.singleton (Map.keysSet children)
    <> foldMap go (Map.toList children)
    where go (nameSeg, b) = Set.map (Path.cons nameSeg) (deepPaths $ head b)
  deepEdits' = Map.mapKeys nameSegToName (Map.map fst edits)
    <> foldMap go (Map.toList children)
   where
    go (nameSeg, b) =
      Map.mapKeys (nameSegToName nameSeg `Name.joinDot`) . deepEdits $ head b

head :: Branch m -> Branch0 m
head (Branch c) = Causal.head c

headHash :: Branch m -> Hash
headHash (Branch c) = Causal.currentHash c

merge :: forall m . Monad m => Branch m -> Branch m -> m (Branch m)
merge (Branch x) (Branch y) =
  Branch <$> Causal.threeWayMerge merge0 diff0 apply x y
 where
  apply :: Branch0 m -> BranchDiff -> m (Branch0 m)
  apply b0 BranchDiff {..} = do
    patches <- sequenceA
      $ Map.differenceWith patchMerge (pure @m <$> _edits b0) changedPatches
    let newPatches = makePatch <$> Map.difference changedPatches (_edits b0)
        makePatch Patch.PatchDiff {..} =
          let p = Patch.Patch _addedTermEdits _addedTypeEdits
           in (H.accumulate' p, pure p)
    pure $ branch0 (Star3.difference (_terms b0) removedTerms <> addedTerms)
                   (Star3.difference (_types b0) removedTypes <> addedTypes)
                   (_children b0)
                   (patches <> newPatches)
  patchMerge mhp Patch.PatchDiff {..} = Just $ do
    (_, mp) <- mhp
    p       <- mp
    let np = Patch.Patch
          { _termEdits = R.difference (Patch._termEdits p) _removedTermEdits
            <> _addedTermEdits
          , _typeEdits = R.difference (Patch._typeEdits p) _removedTypeEdits
            <> _addedTypeEdits
          }
    pure (H.accumulate' np, pure np)

-- `before b1 b2` is true if `b2` incorporates all of `b1`
before :: Monad m => Branch m -> Branch m -> m Bool
before (Branch x) (Branch y) = Causal.before x y

merge0 :: forall m. Monad m => Branch0 m -> Branch0 m -> m (Branch0 m)
merge0 b1 b2 = do
  c3 <- unionWithM merge (_children b1) (_children b2)
  e3 <- unionWithM g (_edits b1) (_edits b2)
  pure $ branch0 (_terms b1 <> _terms b2)
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

unionWithM :: forall m k a.
  (Monad m, Ord k) => (a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithM f m1 m2 = Monad.foldM go m1 $ Map.toList m2 where
  go :: Map k a -> (k, a) -> m (Map k a)
  go m1 (k, a2) = case Map.lookup k m1 of
    Just a1 -> do a <- f a1 a2; pure $ Map.insert k a m1
    Nothing -> pure $ Map.insert k a2 m1

pattern Hash h = Causal.RawHash h

toList0 :: Branch0 m -> [(Path, Branch0 m)]
toList0 = go Path.empty where
  go p b = (p, b) : (Map.toList (_children b) >>= (\(seg, cb) ->
    go (Path.snoc p seg) (head cb) ))

printDebugPaths :: Branch m -> String
printDebugPaths = unlines . map show . Set.toList . debugPaths

debugPaths :: Branch m -> Set (Path, Hash)
debugPaths = go Path.empty where
  go p b = Set.insert (p, headHash b) . Set.unions $
    [ go (Path.snoc p seg) b | (seg, b) <- Map.toList $ _children (head b) ]

data Target = TargetType | TargetTerm | TargetBranch
  deriving (Eq, Ord, Show)

instance Eq (Branch0 m) where
  a == b = view terms a == view terms b
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

-- Question: How does Deserialize throw a not-found error?
-- Question: What is the previous question?
read
  :: forall m
   . Monad m
  => Causal.Deserialize m Raw Raw
  -> (EditHash -> m Patch)
  -> Hash
  -> m (Branch m)
read deserializeRaw deserializeEdits h = Branch <$> Causal.read d h
 where
  fromRaw :: Raw -> m (Branch0 m)
  fromRaw Raw {..} = do
    children <- traverse go _childrenR
    edits <- for _editsR $ \hash -> (hash,) . pure <$> deserializeEdits hash
    pure $ branch0 _termsR _typesR children edits
  go = read deserializeRaw deserializeEdits
  d :: Causal.Deserialize m Raw (Branch0 m)
  d h = deserializeRaw h >>= \case
    RawOne raw      -> RawOne <$> fromRaw raw
    RawCons  raw h  -> flip RawCons h <$> fromRaw raw
    RawMerge raw hs -> flip RawMerge hs <$> fromRaw raw


-- serialize a `Branch m` indexed by the hash of its corresponding Raw
sync :: forall m. Monad m
     => (Hash -> m Bool)
     -> Causal.Serialize m Raw Raw
     -> (EditHash -> m Patch -> m ())
     -> Branch m
     -> m ()
sync exists serializeRaw serializeEdits b =
  Causal.sync exists serialize0 (view history b)
  where
  toRaw :: Branch0 m -> Raw
  toRaw Branch0{..} =
    Raw _terms _types (headHash <$> _children) (fst <$> _edits)
  serialize0 :: Causal.Serialize m Raw (Branch0 m)
  serialize0 h b0 =
    case b0 of
      RawOne b0 -> do
        writeB0 b0
        serializeRaw h $ RawOne (toRaw b0)
      RawCons b0 ht -> do
        writeB0 b0
        serializeRaw h $ RawCons (toRaw b0) ht
      RawMerge b0 hs -> do
        writeB0 b0
        serializeRaw h $ RawMerge (toRaw b0) hs
    where
      writeB0 b0 = do
        for_ (view children b0) (sync exists serializeRaw serializeEdits)
        for_ (view edits b0) (uncurry serializeEdits)

  -- this has to serialize the branch0 and its descendants in the tree,
  -- and then serialize the rest of the history of the branch as well

-- copy a path to another path
fork
  :: Applicative m
  => Path
  -> Path
  -> Branch m
  -> Either ForkFailure (Branch m)
fork src dest root = case getAt src root of
  Nothing -> Left SrcNotFound
  Just src' -> case setIfNotExists dest src' root of
    Nothing -> Left DestExists
    Just root' -> Right root'

-- Move the node at src to dest.
-- It's okay if `dest` is inside `src`, just create empty levels.
-- Try not to `step` more than once at each node.
move :: Applicative m
     => Path
     -> Path
     -> Branch m
     -> Either ForkFailure (Branch m)
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

setIfNotExists
  :: Applicative m => Path -> Branch m -> Branch m -> Maybe (Branch m)
setIfNotExists dest b root = case getAt dest root of
  Just _destExists -> Nothing
  Nothing -> Just $ setAt dest b root

setAt :: Applicative m => Path -> Branch m -> Branch m -> Branch m
setAt path b = modifyAt path (const b)

deleteAt :: Applicative m => Path -> Branch m -> Branch m
deleteAt path = setAt path empty

-- returns `Nothing` if no Branch at `path` or if Branch is empty at `path`
getAt :: Path
      -> Branch m
      -> Maybe (Branch m)
getAt path root = case Path.toList path of
  [] -> if isEmpty root then Nothing else Just root
  seg : path -> case Map.lookup seg (_children $ head root) of
    Just b -> getAt (Path.fromList path) b
    Nothing -> Nothing

getAt' :: Path -> Branch m -> Branch m
getAt' p b = fromMaybe empty $ getAt p b

getAt0 :: Path -> Branch0 m -> Branch0 m
getAt0 p b = case Path.toList p of
  [] -> b
  seg : path -> case Map.lookup seg (_children b) of
    Just c -> getAt0 (Path.fromList path) (head c)
    Nothing -> empty0

empty :: Branch m
empty = Branch $ Causal.one empty0

one :: Branch0 m -> Branch m
one = Branch . Causal.one

empty0 :: Branch0 m
empty0 = Branch0 mempty mempty mempty mempty mempty mempty mempty mempty

isEmpty0 :: Branch0 m -> Bool
isEmpty0 = (== empty0)

isEmpty :: Branch m -> Bool
isEmpty = (== empty)

step :: Applicative m => (Branch0 m -> Branch0 m) -> Branch m -> Branch m
step f = over history (Causal.stepDistinct f)

stepM :: Monad m => (Branch0 m -> m (Branch0 m)) -> Branch m -> m (Branch m)
stepM f = mapMOf history (Causal.stepDistinctM f)

cons :: Applicative m => Branch0 m -> Branch m -> Branch m
cons = step . const

isOne :: Branch m -> Bool
isOne (Branch Causal.One{}) = True
isOne _ = False

uncons :: Applicative m => Branch m -> m (Maybe (Branch0 m, Branch m))
uncons (Branch b) = go <$> Causal.uncons b where
  go = over (_Just . _2) Branch

-- Modify the branch0 at the head of at `path` with `f`,
-- after creating it if necessary.  Preserves history.
stepAt :: forall m. Applicative m
       => Path
       -> (Branch0 m -> Branch0 m)
       -> Branch m -> Branch m
stepAt p f = modifyAt p g where
  g :: Branch m -> Branch m
  g (Branch b) = Branch . Causal.consDistinct (f (Causal.head b)) $ b

stepManyAt :: (Applicative m, Foldable f)
           => f (Path, Branch0 m -> Branch0 m) -> Branch m -> Branch m
stepManyAt actions = step (stepManyAt0 actions)

-- Modify the branch0 at the head of at `path` with `f`,
-- after creating it if necessary.  Preserves history.
stepAtM :: forall n m. (Functor n, Applicative m)
        => Path -> (Branch0 m -> n (Branch0 m)) -> Branch m -> n (Branch m)
stepAtM p f = modifyAtM p g where
  g :: Branch m -> n (Branch m)
  g (Branch b) = do
    b0' <- f (Causal.head b)
    pure $ Branch . Causal.consDistinct b0' $ b

stepManyAtM :: (Monad m, Foldable f)
            => f (Path, Branch0 m -> m (Branch0 m)) -> Branch m -> m (Branch m)
stepManyAtM actions = stepM (stepManyAt0M actions)

-- starting at the leaves, apply `f` to every level of the branch.
stepEverywhere
  :: Applicative m => (Branch0 m -> Branch0 m) -> (Branch0 m -> Branch0 m)
stepEverywhere f Branch0 {..} = f (branch0 _terms _types children _edits)
  where children = fmap (step $ stepEverywhere f) _children

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

modifyPatches
  :: Monad m => NameSegment -> (Patch -> Patch) -> Branch0 m -> m (Branch0 m)
modifyPatches seg f = mapMOf edits update
 where
  update m = do
    p' <- case Map.lookup seg m of
      Nothing     -> pure $ f Patch.empty
      Just (_, p) -> f <$> p
    let h = H.accumulate' p'
    pure $ Map.insert seg (h, pure p') m

replacePatch :: Applicative m => NameSegment -> Patch -> Branch0 m -> Branch0 m
replacePatch n p = over edits (Map.insert n (H.accumulate' p, pure p))

deletePatch :: NameSegment -> Branch0 m -> Branch0 m
deletePatch n = over edits (Map.delete n)

updateChildren ::NameSegment
               -> Branch m
               -> Map NameSegment (Branch m)
               -> Map NameSegment (Branch m)
updateChildren seg updatedChild =
  if isEmpty0 (head updatedChild)
  then Map.delete seg
  else Map.insert seg updatedChild

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAt :: Applicative m
  => Path -> (Branch m -> Branch m) -> Branch m -> Branch m
modifyAt path f = runIdentity . modifyAtM path (pure . f)

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAtM
  :: forall n m
   . Functor n
  => Applicative m -- because `Causal.cons` uses `pure`
  => Path
  -> (Branch m -> n (Branch m))
  -> Branch m
  -> n (Branch m)
modifyAtM path f b = case Path.toList path of
  [] -> f b
  seg : path -> do -- Functor
    let child = getChildBranch seg (head b)
    child' <- modifyAtM (Path.fromList path) f child
    -- step the branch by updating its children according to fixup
    pure $ step (setChildBranch seg child') b

stepAt0 :: Applicative m => Path
                         -> (Branch0 m -> Branch0 m)
                         -> Branch0 m -> Branch0 m
stepAt0 p f = runIdentity . stepAt0M p (pure . f)

-- stepManyAt0 consolidates several changes into a single step
stepManyAt0 :: (Applicative m, Foldable f)
           => f (Path, Branch0 m -> Branch0 m)
           -> Branch0 m -> Branch0 m
stepManyAt0 actions b = foldl' (\b (p, f) -> stepAt0 p f b) b actions

stepManyAt0M :: (Monad m, Foldable f)
             => f (Path, Branch0 m -> m (Branch0 m))
             -> Branch0 m -> m (Branch0 m)
stepManyAt0M actions b = Monad.foldM (\b (p, f) -> stepAt0M p f b) b actions

stepAt0M :: forall n m. (Functor n, Applicative m)
         => Path
         -> (Branch0 m -> n (Branch0 m))
         -> Branch0 m -> n (Branch0 m)
stepAt0M p f b = case Path.uncons p of
  Nothing -> f b
  Just (seg, path) -> do
    let child = getChildBranch seg b
    child0' <- stepAt0M path f (head child)
    pure $ setChildBranch seg (cons child0' child) b

instance Hashable (Branch0 m) where
  tokens b =
    [ H.accumulateToken (_terms b)
    , H.accumulateToken (_types b)
    , H.accumulateToken (headHash <$> _children b)
    ]

-- getLocalBranch :: Hash -> IO Branch
-- getGithubBranch :: RemotePath -> IO Branch
-- getLocalEdit :: GUID -> IO Patch

-- todo: consider inlining these into Actions2
addTermName
  :: Referent -> NameSegment -> Metadata.Metadata -> Branch0 m -> Branch0 m
addTermName r new md =
  over terms (Metadata.insertWithMetadata (r, md) . Star3.insertD1 (r, new))

addTypeName
  :: Reference -> NameSegment -> Metadata.Metadata -> Branch0 m -> Branch0 m
addTypeName r new md =
  over types (Metadata.insertWithMetadata (r, md) . Star3.insertD1 (r, new))

-- addTermNameAt :: Path.Split -> Referent -> Branch0 m -> Branch0 m
-- addTypeNameAt :: Path.Split -> Reference -> Branch0 m -> Branch0 m

deleteTermName :: Referent -> NameSegment -> Branch0 m -> Branch0 m
deleteTermName r n b | Star3.memberD1 (r,n) (view terms b)
                     = over terms (Star3.deletePrimaryD1 (r,n)) b
deleteTermName _ _ b = b

deleteTypeName :: Reference -> NameSegment -> Branch0 m -> Branch0 m
deleteTypeName r n b | Star3.memberD1 (r,n) (view types b)
                     = over types (Star3.deletePrimaryD1 (r,n)) b
deleteTypeName _ _ b = b

namesDiff :: Branch m -> Branch m -> Names.Diff
namesDiff b1 b2 = Names.diff0 (toNames0 (head b1)) (toNames0 (head b2))

lca :: Monad m => Branch m -> Branch m -> m (Maybe (Branch m))
lca (Branch a) (Branch b) = fmap Branch <$> Causal.lca a b

diff0 :: Monad m => Branch0 m -> Branch0 m -> m BranchDiff
diff0 old new = do
  newEdits <- sequenceA $ snd <$> _edits new
  oldEdits <- sequenceA $ snd <$> _edits old
  let diffEdits = Map.merge (Map.mapMissing $ \_ p -> Patch.diff p mempty)
                            (Map.mapMissing $ \_ p -> Patch.diff mempty p)
                            (Map.zipWithMatched (const Patch.diff))
                            newEdits
                            oldEdits
  pure $ BranchDiff
    { addedTerms     = Star3.difference (_terms new) (_terms old)
    , removedTerms   = Star3.difference (_terms old) (_terms new)
    , addedTypes     = Star3.difference (_types new) (_types old)
    , removedTypes   = Star3.difference (_types old) (_types new)
    , changedPatches = diffEdits
    }

data BranchAttentions = BranchAttentions
  { -- Patches that were edited on the right but entirely removed on the left.
    removedPatchEdited :: [Name]
  -- Patches that were edited on the left but entirely removed on the right.
  , editedPatchRemoved :: [Name]
  }

instance Semigroup BranchAttentions where
  BranchAttentions edited1 removed1 <> BranchAttentions edited2 removed2
    = BranchAttentions (edited1 <> edited2) (removed1 <> removed2)

instance Monoid BranchAttentions where
  mempty = BranchAttentions [] []
  mappend = (<>)

data RefCollisions =
  RefCollisions { termCollisions :: Relation Name Name
                , typeCollisions :: Relation Name Name
                } deriving (Eq, Show)

instance Semigroup RefCollisions where
  (<>) = mappend
instance Monoid RefCollisions where
  mempty = RefCollisions mempty mempty
  mappend r1 r2 = RefCollisions (termCollisions r1 <> termCollisions r2)
                                (typeCollisions r1 <> typeCollisions r2)
