{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Branch
  ( -- * Branch types
    Branch(..)
  , BranchDiff(..)
  , UnwrappedBranch
  , BranchSnapshot(..)
  , Raw(..)
  , Star
  , Hash
  , EditHash
  , pattern Hash
  -- * Branch construction
  , branchSnapshot
  , one
  , cons
  , uncons
  , empty
  , empty0
  , discardHistory0
  , toCausalRaw
  , transform
  -- * Branch tests
  , isEmpty
  , isEmpty0
  , isOne
  , before
  , lca
  -- * diff
  , diff0
  -- * properties
  , head
  , headHash
  , children_
  , deepEdits'
  , toList0
  -- * step
  , stepManyAt
  , stepManyAtM
  , stepManyAt0
  , stepEverywhere
  -- *
  , addTermName
  , addTypeName
  , deleteTermName
  , deleteTypeName
  , setChildBranch
  , replacePatch
  , deletePatch
  , getMaybePatch
  , getPatch
  , modifyPatches
  -- ** Children queries
  , getAt
  , getAt'
  , getAt0
  , modifyAt
  , modifyAtM
  -- * Branch terms/types/edits
  -- ** Term/type/edits lenses
  , terms_
  , types_
  , edits_
    -- ** Term/type queries
  , deepReferents
  , deepTypeReferences
  -- * Branch serialization
  , cachedRead
  , Cache
  , sync
  ) where

import Unison.Prelude hiding (empty)

import           Prelude                  hiding (head,read,subtract)

import           Control.Lens as Lens          hiding ( children, cons, transform, uncons )
import qualified Control.Monad.State           as State
import           Control.Monad.State            ( StateT )
import           Data.Bifunctor                 ( second )
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
import           Unison.NameSegment             ( NameSegment )
import qualified Unison.NameSegment            as NameSegment
import qualified Unison.Codebase.Metadata      as Metadata
import qualified Unison.Hash                   as Hash
import           Unison.Hashable                ( Hashable )
import qualified Unison.Hashable               as H
import           Unison.Name                    ( Name(..) )
import qualified Unison.Name                   as Name
import           Unison.Reference               ( Reference )
import           Unison.Referent                ( Referent )

import qualified U.Util.Cache             as Cache
import qualified Unison.Util.Relation          as R
import           Unison.Util.Relation            ( Relation )
import qualified Unison.Util.Relation4         as R4
import qualified Unison.Util.Star3             as Star3
import qualified Unison.Util.List as List

-- | A node in the Unison namespace hierarchy
-- along with its history.
newtype Branch m = Branch { history :: UnwrappedBranch m }
  deriving (Eq, Ord)
type UnwrappedBranch m = Causal m Raw (BranchSnapshot m)

type Hash = Causal.RawHash Raw
type EditHash = Hash.Hash

type Star r n = Metadata.Star r n

-- | A node in the Unison namespace hierarchy.
--
-- '_terms' and '_types' are the declarations at this level.
-- '_children' are the nodes one level below us.
-- '_edits' are the 'Patch's stored at this node in the code.
--
-- The @deep*@ fields are derived from the four above.
data BranchSnapshot m = BranchSnapshot
  { terms :: Star Referent NameSegment
  , types :: Star Reference NameSegment
  , children :: Map NameSegment (Branch m)
    -- ^ Note the 'Branch' here, not 'BranchSnapshot'.
    -- Every level in the tree has a history.
  , edits :: Map NameSegment (EditHash, m Patch)
  -- names and metadata for this branch and its children
  -- (ref, (name, value)) iff ref has metadata `value` at name `name`
  , deepTerms :: Relation Referent Name
  , deepTypes :: Relation Reference Name
  , deepTermMetadata :: Metadata.R4 Referent Name
  , deepTypeMetadata :: Metadata.R4 Reference Name
  , deepPaths :: Set Path
  , deepEdits :: Map Name EditHash
  }

-- Represents a shallow diff of a BranchSnapshot.
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

Lens.makeLensesWith (Lens.defaultFieldRules & Lens.lensField .~ Lens.mappingNamer (\n -> [n <> "_"])) ''Branch
Lens.makeLensesWith (Lens.defaultFieldRules & Lens.lensField .~ Lens.mappingNamer (\n -> [n <> "_"])) ''BranchSnapshot

deepReferents :: BranchSnapshot m -> Set Referent
deepReferents = R.dom . deepTerms

deepTypeReferences :: BranchSnapshot m -> Set Reference
deepTypeReferences = R.dom . deepTypes

-- creates a BranchSnapshot from the primary fields and derives the others.
branchSnapshot :: Metadata.Star Referent NameSegment
               -> Metadata.Star Reference NameSegment
               -> Map NameSegment (Branch m)
               -> Map NameSegment (EditHash, m Patch)
               -> BranchSnapshot m
branchSnapshot terms types children edits =
  BranchSnapshot terms types children edits
          deepTerms' deepTypes'
          deepTermMetadata' deepTypeMetadata'
          deepPaths' deepEdits'
  where
  nameSegToName = Name.unsafeFromText . NameSegment.toText
  deepTerms' = (R.mapRan nameSegToName . Star3.d1) terms
    <> foldMap go (Map.toList children)
   where
    go (nameSegToName -> n, b) =
      R.mapRan (Name.joinDot n) (deepTerms $ head b) -- could use mapKeysMonotonic
  deepTypes' = (R.mapRan nameSegToName . Star3.d1) types
    <> foldMap go (Map.toList children)
   where
    go (nameSegToName -> n, b) =
      R.mapRan (Name.joinDot n) (deepTypes $ head b) -- could use mapKeysMonotonic
  deepTermMetadata' = R4.mapD2 nameSegToName (Metadata.starToR4 terms)
    <> foldMap go (Map.toList children)
   where
    go (nameSegToName -> n, b) =
      R4.mapD2 (Name.joinDot n) (deepTermMetadata $ head b)
  deepTypeMetadata' = R4.mapD2 nameSegToName (Metadata.starToR4 types)
    <> foldMap go (Map.toList children)
   where
    go (nameSegToName -> n, b) =
      R4.mapD2 (Name.joinDot n) (deepTypeMetadata $ head b)
  deepPaths' = Set.map Path.singleton (Map.keysSet children)
    <> foldMap go (Map.toList children)
    where go (nameSeg, b) = Set.map (Path.cons nameSeg) (deepPaths $ head b)
  deepEdits' = Map.mapKeys nameSegToName (Map.map fst edits)
    <> foldMap go (Map.toList children)
   where
    go (nameSeg, b) =
      Map.mapKeys (nameSegToName nameSeg `Name.joinDot`) . deepEdits $ head b

head :: Branch m -> BranchSnapshot m
head (Branch c) = Causal.head c

headHash :: Branch m -> Hash
headHash (Branch c) = Causal.currentHash c

-- | a version of `deepEdits` that returns the `m Patch` as well.
deepEdits' :: BranchSnapshot m -> Map Name (EditHash, m Patch)
deepEdits' b = go id b where
  -- can change this to an actual prefix once Name is a [NameSegment]
  go :: (Name -> Name) -> BranchSnapshot m -> Map Name (EditHash, m Patch)
  go addPrefix BranchSnapshot{..} =
    Map.mapKeysMonotonic (addPrefix . Name.fromSegment) edits
      <> foldMap f (Map.toList children)
    where
    f :: (NameSegment, Branch m) -> Map Name (EditHash, m Patch)
    f (c, b) =  go (addPrefix . Name.joinDot (Name.fromSegment c)) (head b)

-- Discards the history of a Branch0's children, recursively
discardHistory0 :: Applicative m => BranchSnapshot m -> BranchSnapshot m
discardHistory0 = over children_ (fmap tweak) where
  tweak b = cons (discardHistory0 (head b)) empty

-- `before b1 b2` is true if `b2` incorporates all of `b1`
before :: Monad m => Branch m -> Branch m -> m Bool
before (Branch b1) (Branch b2) = Causal.before b1 b2

pattern Hash h = Causal.RawHash h

-- | what does this do? —AI
toList0 :: BranchSnapshot m -> [(Path, BranchSnapshot m)]
toList0 = go Path.empty where
  go p b = (p, b) : (Map.toList (children b) >>= (\(seg, cb) ->
    go (Path.snoc p seg) (head cb) ))

instance Eq (BranchSnapshot m) where
  a == b = view terms_ a == view terms_ b
    && view types_ a == view types_ b
    && view children_ a == view children_ b
    && (fmap fst . view edits_) a == (fmap fst . view edits_) b

-- This type is a little ugly, so we wrap it up with a nice type alias for
-- use outside this module.
type Cache m = Cache.Cache (Causal.RawHash Raw) (UnwrappedBranch m)

-- Can use `Cache.nullCache` to disable caching if needed
cachedRead :: forall m . MonadIO m
           => Cache m
           -> Causal.Deserialize m Raw Raw
           -> (EditHash -> m Patch)
           -> Hash
           -> m (Branch m)
cachedRead cache deserializeRaw deserializeEdits h =
 Branch <$> Causal.cachedRead cache d h
 where
  fromRaw :: Raw -> m (BranchSnapshot m)
  fromRaw Raw {..} = do
    children <- traverse go _childrenR
    edits <- for _editsR $ \hash -> (hash,) . pure <$> deserializeEdits hash
    pure $ branchSnapshot _termsR _typesR children edits
  go = cachedRead cache deserializeRaw deserializeEdits
  d :: Causal.Deserialize m Raw (BranchSnapshot m)
  d h = deserializeRaw h >>= \case
    RawOne raw      -> RawOne <$> fromRaw raw
    RawCons  raw h  -> flip RawCons h <$> fromRaw raw
    RawMerge raw hs -> flip RawMerge hs <$> fromRaw raw

sync
  :: Monad m
  => (Hash -> m Bool)
  -> Causal.Serialize m Raw Raw
  -> (EditHash -> m Patch -> m ())
  -> Branch m
  -> m ()
sync exists serializeRaw serializeEdits b = do
  _written <- State.execStateT (sync' exists serializeRaw serializeEdits b) mempty
  -- traceM $ "Branch.sync wrote " <> show (Set.size written) <> " namespace files."
  pure ()

-- serialize a `Branch m` indexed by the hash of its corresponding Raw
sync'
  :: forall m
   . Monad m
  => (Hash -> m Bool)
  -> Causal.Serialize m Raw Raw
  -> (EditHash -> m Patch -> m ())
  -> Branch m
  -> StateT (Set Hash) m ()
sync' exists serializeRaw serializeEdits b = Causal.sync exists
                                                         serialize0
                                                         (view history_ b)
 where
  serialize0 :: Causal.Serialize (StateT (Set Hash) m) Raw (BranchSnapshot m)
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
    writeB0 :: BranchSnapshot m -> StateT (Set Hash) m ()
    writeB0 b0 = do
      for_ (view children_ b0) $ \c -> do
        queued <- State.get
        when (Set.notMember (headHash c) queued) $
          sync' exists serializeRaw serializeEdits c
      for_ (view edits_ b0) (lift . uncurry serializeEdits)

  -- this has to serialize the branchSnapshot and its descendants in the tree,
  -- and then serialize the rest of the history of the branch as well

toRaw :: BranchSnapshot m -> Raw
toRaw BranchSnapshot {..} =
  Raw terms types (headHash <$> children) (fst <$> edits)

toCausalRaw :: Branch m -> Causal.Raw Raw Raw
toCausalRaw = \case
  Branch (Causal.One _h e)           -> RawOne (toRaw e)
  Branch (Causal.Cons _h e (ht, _m)) -> RawCons (toRaw e) ht
  Branch (Causal.Merge _h e tls)     -> RawMerge (toRaw e) (Map.keysSet tls)

-- returns `Nothing` if no Branch at `path` or if Branch is empty at `path`
getAt :: Path
      -> Branch m
      -> Maybe (Branch m)
getAt path root = case Path.uncons path of
  Nothing -> if isEmpty root then Nothing else Just root
  Just (seg, path) -> case Map.lookup seg (children $ head root) of
    Just b -> getAt path b
    Nothing -> Nothing

getAt' :: Path -> Branch m -> Branch m
getAt' p b = fromMaybe empty $ getAt p b

getAt0 :: Path -> BranchSnapshot m -> BranchSnapshot m
getAt0 p b = case Path.uncons p of
  Nothing -> b
  Just (seg, path) -> case Map.lookup seg (children b) of
    Just c -> getAt0 path (head c)
    Nothing -> empty0

empty :: Branch m
empty = Branch $ Causal.one empty0

one :: BranchSnapshot m -> Branch m
one = Branch . Causal.one

empty0 :: BranchSnapshot m
empty0 =
  BranchSnapshot mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

isEmpty0 :: BranchSnapshot m -> Bool
isEmpty0 = (== empty0)

isEmpty :: Branch m -> Bool
isEmpty = (== empty)

step :: Applicative m => (BranchSnapshot m -> BranchSnapshot m) -> Branch m -> Branch m
step f = \case
  Branch (Causal.One _h e) | e == empty0 -> Branch (Causal.one (f empty0))
  b -> over history_ (Causal.stepDistinct f) b

stepM :: (Monad m, Monad n) => (BranchSnapshot m -> n (BranchSnapshot m)) -> Branch m -> n (Branch m)
stepM f = \case
  Branch (Causal.One _h e) | e == empty0 -> Branch . Causal.one <$> f empty0
  b -> mapMOf history_ (Causal.stepDistinctM f) b

cons :: Applicative m => BranchSnapshot m -> Branch m -> Branch m
cons = step . const

isOne :: Branch m -> Bool
isOne (Branch Causal.One{}) = True
isOne _ = False

uncons :: Applicative m => Branch m -> m (Maybe (BranchSnapshot m, Branch m))
uncons (Branch b) = go <$> Causal.uncons b where
  go = over (_Just . _2) Branch

stepManyAt :: (Monad m, Foldable f)
           => f (Path, BranchSnapshot m -> BranchSnapshot m) -> Branch m -> Branch m
stepManyAt actions = step (stepManyAt0 actions)

stepManyAtM :: (Monad m, Monad n, Foldable f)
            => f (Path, BranchSnapshot m -> n (BranchSnapshot m)) -> Branch m -> n (Branch m)
stepManyAtM actions = stepM (stepManyAt0M actions)

-- starting at the leaves, apply `f` to every level of the branch.
stepEverywhere
  :: Applicative m => (BranchSnapshot m -> BranchSnapshot m) -> (BranchSnapshot m -> BranchSnapshot m)
stepEverywhere f BranchSnapshot {..} = f (branchSnapshot terms types children edits)
  where children = fmap (step $ stepEverywhere f) children

-- Creates a function to fix up the children field._1
-- If the action emptied a child, then remove the mapping,
-- otherwise update it.
-- Todo: Fix this in hashing & serialization instead of here?
getChildBranch :: NameSegment -> BranchSnapshot m -> Branch m
getChildBranch seg b = fromMaybe empty $ Map.lookup seg (children b)

setChildBranch :: NameSegment -> Branch m -> BranchSnapshot m -> BranchSnapshot m
setChildBranch seg b = over children_ (updateChildren seg b)

getPatch :: Applicative m => NameSegment -> BranchSnapshot m -> m Patch
getPatch seg b = case Map.lookup seg (edits b) of
  Nothing -> pure Patch.empty
  Just (_, p) -> p

getMaybePatch :: Applicative m => NameSegment -> BranchSnapshot m -> m (Maybe Patch)
getMaybePatch seg b = case Map.lookup seg (edits b) of
  Nothing -> pure Nothing
  Just (_, p) -> Just <$> p

modifyPatches
  :: Monad m => NameSegment -> (Patch -> Patch) -> BranchSnapshot m -> m (BranchSnapshot m)
modifyPatches seg f = mapMOf edits_ update
 where
  update m = do
    p' <- case Map.lookup seg m of
      Nothing     -> pure $ f Patch.empty
      Just (_, p) -> f <$> p
    let h = H.accumulate' p'
    pure $ Map.insert seg (h, pure p') m

replacePatch :: Applicative m => NameSegment -> Patch -> BranchSnapshot m -> BranchSnapshot m
replacePatch n p = over edits_ (Map.insert n (H.accumulate' p, pure p))

deletePatch :: NameSegment -> BranchSnapshot m -> BranchSnapshot m
deletePatch n = over edits_ (Map.delete n)

updateChildren ::NameSegment
               -> Branch m
               -> Map NameSegment (Branch m)
               -> Map NameSegment (Branch m)
updateChildren seg updatedChild =
  if isEmpty updatedChild
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
modifyAtM path f b = case Path.uncons path of
  Nothing -> f b
  Just (seg, path) -> do -- Functor
    let child = getChildBranch seg (head b)
    child' <- modifyAtM path f child
    -- step the branch by updating its children according to fixup
    pure $ step (setChildBranch seg child') b

-- stepManyAt0 consolidates several changes into a single step
stepManyAt0 :: forall f m . (Monad m, Foldable f)
           => f (Path, BranchSnapshot m -> BranchSnapshot m)
           -> BranchSnapshot m -> BranchSnapshot m
stepManyAt0 actions =
  runIdentity . stepManyAt0M [ (p, pure . f) | (p,f) <- toList actions ]

stepManyAt0M :: forall m n f . (Monad m, Monad n, Foldable f)
             => f (Path, BranchSnapshot m -> n (BranchSnapshot m))
             -> BranchSnapshot m -> n (BranchSnapshot m)
stepManyAt0M actions b = go (toList actions) b where
  go :: [(Path, BranchSnapshot m -> n (BranchSnapshot m))] -> BranchSnapshot m -> n (BranchSnapshot m)
  go actions b = let
    -- combines the functions that apply to this level of the tree
    currentAction b = foldM (\b f -> f b) b [ f | (Path.Empty, f) <- actions ]

    -- groups the actions based on the child they apply to
    childActions :: Map NameSegment [(Path, BranchSnapshot m -> n (BranchSnapshot m))]
    childActions =
      List.multimap [ (seg, (rest,f)) | (seg :< rest, f) <- actions ]

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
      c2 <- stepChildren (view children_ b)
      currentAction (set children_ c2 b)

instance Hashable (BranchSnapshot m) where
  tokens b =
    [ H.accumulateToken (terms b)
    , H.accumulateToken (types b)
    , H.accumulateToken (headHash <$> children b)
    , H.accumulateToken (fst <$> edits b)
    ]

-- todo: consider inlining these into Actions2
addTermName
  :: Referent -> NameSegment -> Metadata.Metadata -> BranchSnapshot m -> BranchSnapshot m
addTermName r new md =
  over terms_ (Metadata.insertWithMetadata (r, md) . Star3.insertD1 (r, new))

addTypeName
  :: Reference -> NameSegment -> Metadata.Metadata -> BranchSnapshot m -> BranchSnapshot m
addTypeName r new md =
  over types_ (Metadata.insertWithMetadata (r, md) . Star3.insertD1 (r, new))

deleteTermName :: Referent -> NameSegment -> BranchSnapshot m -> BranchSnapshot m
deleteTermName r n b | Star3.memberD1 (r,n) (view terms_ b)
                     = over terms_ (Star3.deletePrimaryD1 (r,n)) b
deleteTermName _ _ b = b

deleteTypeName :: Reference -> NameSegment -> BranchSnapshot m -> BranchSnapshot m
deleteTypeName r n b | Star3.memberD1 (r,n) (view types_ b)
                     = over types_ (Star3.deletePrimaryD1 (r,n)) b
deleteTypeName _ _ b = b

lca :: Monad m => Branch m -> Branch m -> m (Maybe (Branch m))
lca (Branch a) (Branch b) = fmap Branch <$> Causal.lca a b

diff0 :: Monad m => BranchSnapshot m -> BranchSnapshot m -> m BranchDiff
diff0 old new = do
  newEdits <- sequenceA $ snd <$> edits new
  oldEdits <- sequenceA $ snd <$> edits old
  let diffEdits = Map.merge (Map.mapMissing $ \_ p -> Patch.diff p mempty)
                            (Map.mapMissing $ \_ p -> Patch.diff mempty p)
                            (Map.zipWithMatched (const Patch.diff))
                            newEdits
                            oldEdits
  pure $ BranchDiff
    { addedTerms     = Star3.difference (terms new) (terms old)
    , removedTerms   = Star3.difference (terms old) (terms new)
    , addedTypes     = Star3.difference (types new) (types old)
    , removedTypes   = Star3.difference (types old) (types new)
    , changedPatches = diffEdits
    }

transform :: Functor m => (forall a . m a -> n a) -> Branch m -> Branch n
transform f b = case history b of
  causal -> Branch . Causal.transform f $ transformB0s f causal
  where
  transformB0 :: Functor m => (forall a . m a -> n a) -> BranchSnapshot m -> BranchSnapshot n
  transformB0 f b =
    b { children = transform f <$> children b
      , edits    = second f    <$> edits b
      }

  transformB0s :: Functor m => (forall a . m a -> n a)
               -> Causal m Raw (BranchSnapshot m)
               -> Causal m Raw (BranchSnapshot n)
  transformB0s f = Causal.unsafeMapHashPreserving (transformB0 f)
