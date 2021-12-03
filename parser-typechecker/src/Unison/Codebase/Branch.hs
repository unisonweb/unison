{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Codebase.Branch
  ( -- * Branch types
    Branch(..)
  , UnwrappedBranch
  , Branch0(..)
  , Raw(..)
  , Star
  , Hash
  , EditHash
  , pattern Hash
  -- * Branch construction
  , branch0
  , one
  , cons
  , uncons
  , empty
  , empty0
  , discardHistory0
  , transform
  -- * Branch tests
  , isEmpty
  , isEmpty0
  , isOne
  , before
  , lca
  -- * properties
  , history
  , head
  , headHash
  , children
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
  , children0
  -- * Branch terms/types/edits
  -- ** Term/type/edits lenses
  , terms
  , types
  , edits
    -- ** Term/type queries
  , deepReferents
  , deepTypeReferences
  ) where

import Unison.Prelude hiding (empty)

import           Prelude                  hiding (head,read,subtract)

import Control.Lens hiding (children, cons, transform, uncons)
import Data.Bifunctor (second)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Unison.Codebase.Branch.Raw (Raw (Raw))
import Unison.Codebase.Branch.Type
  ( Branch (..),
    Branch0 (..),
    EditHash,
    Hash,
    Star,
    UnwrappedBranch,
    edits,
    head,
    headHash,
    history,
  )
import Unison.Codebase.Causal (Causal)
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Patch (Patch)
import qualified Unison.Codebase.Patch as Patch
import Unison.Codebase.Path (Path (..))
import qualified Unison.Codebase.Path as Path
import qualified Unison.Hashing.V2.Convert as H
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment)
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import qualified Unison.Util.List as List
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation4 as R4
import qualified Unison.Util.Star3 as Star3
import qualified Unison.Hashing.V2.Hashable as H

deepReferents :: Branch0 m -> Set Referent
deepReferents = R.dom . deepTerms

deepTypeReferences :: Branch0 m -> Set TypeReference
deepTypeReferences = R.dom . deepTypes

terms :: Lens' (Branch0 m) (Star Referent NameSegment)
terms =
  lens
    _terms
    \branch terms ->
      branch {_terms = terms}
        & deriveDeepTerms
        & deriveDeepTermMetadata

types :: Lens' (Branch0 m) (Star TypeReference NameSegment)
types =
  lens
    _types
    \branch types ->
      branch {_types = types}
        & deriveDeepTypes
        & deriveDeepTypeMetadata

children :: Lens' (Branch0 m) (Map NameSegment (Branch m))
children = lens _children (\Branch0{..} x -> branch0 _terms _types x _edits)

-- creates a Branch0 from the primary fields and derives the others.
branch0 ::
  forall m.
  Metadata.Star Referent NameSegment ->
  Metadata.Star TypeReference NameSegment ->
  Map NameSegment (Branch m) ->
  Map NameSegment (EditHash, m Patch) ->
  Branch0 m
branch0 terms types children edits =
  Branch0
    { _terms = terms,
      _types = types,
      _children = children,
      _edits = edits,
      -- These are all overwritten immediately
      deepTerms = R.empty,
      deepTypes = R.empty,
      deepTermMetadata = R4.empty,
      deepTypeMetadata = R4.empty,
      deepPaths = Set.empty,
      deepEdits = Map.empty
    }
    & deriveDeepTerms
    & deriveDeepTypes
    & deriveDeepTermMetadata
    & deriveDeepTypeMetadata
    & deriveDeepPaths
    & deriveDeepEdits

-- | Derive the 'deepTerms' field of a branch.
deriveDeepTerms :: Branch0 m -> Branch0 m
deriveDeepTerms branch =
  branch {deepTerms = makeDeepTerms (_terms branch) (_children branch)}
  where
    makeDeepTerms :: Metadata.Star Referent NameSegment -> Map NameSegment (Branch m) -> Relation Referent Name
    makeDeepTerms terms children =
      R.mapRanMonotonic Name.fromSegment (Star3.d1 terms) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Relation Referent Name
        go n b =
          R.mapRan (Name.cons n) (deepTerms $ head b)

-- | Derive the 'deepTypes' field of a branch.
deriveDeepTypes :: Branch0 m -> Branch0 m
deriveDeepTypes branch =
  branch {deepTypes = makeDeepTypes (_types branch) (_children branch)}
  where
    makeDeepTypes :: Metadata.Star TypeReference NameSegment -> Map NameSegment (Branch m) -> Relation TypeReference Name
    makeDeepTypes types children =
      R.mapRanMonotonic Name.fromSegment (Star3.d1 types) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Relation TypeReference Name
        go n b =
          R.mapRan (Name.cons n) (deepTypes $ head b)

-- | Derive the 'deepTermMetadata' field of a branch.
deriveDeepTermMetadata :: Branch0 m -> Branch0 m
deriveDeepTermMetadata branch =
  branch {deepTermMetadata = makeDeepTermMetadata (_terms branch) (_children branch)}
  where
    makeDeepTermMetadata :: Metadata.Star Referent NameSegment -> Map NameSegment (Branch m) -> Metadata.R4 Referent Name
    makeDeepTermMetadata terms children =
      R4.mapD2Monotonic Name.fromSegment (Metadata.starToR4 terms) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Metadata.R4 Referent Name
        go n b =
          R4.mapD2 (Name.cons n) (deepTermMetadata $ head b)

-- | Derive the 'deepTypeMetadata' field of a branch.
deriveDeepTypeMetadata :: Branch0 m -> Branch0 m
deriveDeepTypeMetadata branch =
  branch {deepTypeMetadata = makeDeepTypeMetadata (_types branch) (_children branch)}
  where
    makeDeepTypeMetadata :: Metadata.Star TypeReference NameSegment -> Map NameSegment (Branch m) -> Metadata.R4 TypeReference Name
    makeDeepTypeMetadata types children =
      R4.mapD2Monotonic Name.fromSegment (Metadata.starToR4 types) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Metadata.R4 TypeReference Name
        go n b =
          R4.mapD2 (Name.cons n) (deepTypeMetadata $ head b)

-- | Derive the 'deepPaths' field of a branch.
deriveDeepPaths :: Branch0 m -> Branch0 m
deriveDeepPaths branch =
  branch {deepPaths = makeDeepPaths (_children branch)}
  where
    makeDeepPaths :: Map NameSegment (Branch m) -> Set Path
    makeDeepPaths children =
      Set.mapMonotonic Path.singleton (Map.keysSet children) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Set Path
        go n b =
          Set.map (Path.cons n) (deepPaths $ head b)

-- | Derive the 'deepEdits' field of a branch.
deriveDeepEdits :: Branch0 m -> Branch0 m
deriveDeepEdits branch =
  branch {deepEdits = makeDeepEdits (_edits branch) (_children branch)}
  where
    makeDeepEdits :: Map NameSegment (EditHash, m Patch) -> Map NameSegment (Branch m) -> Map Name EditHash
    makeDeepEdits edits children =
      Map.mapKeysMonotonic Name.fromSegment (Map.map fst edits) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Map Name EditHash
        go n b =
          Map.mapKeys (Name.cons n) (deepEdits $ head b)

-- | a version of `deepEdits` that returns the `m Patch` as well.
deepEdits' :: Branch0 m -> Map Name (EditHash, m Patch)
deepEdits' = go id where
  -- can change this to an actual prefix once Name is a [NameSegment]
  go :: (Name -> Name) -> Branch0 m -> Map Name (EditHash, m Patch)
  go addPrefix Branch0{_children, _edits} =
    Map.mapKeys (addPrefix . Name.fromSegment) _edits
      <> foldMap f (Map.toList _children)
    where
    f :: (NameSegment, Branch m) -> Map Name (EditHash, m Patch)
    f (c, b) =  go (addPrefix . Name.cons c) (head b)

-- Discards the history of a Branch0's children, recursively
discardHistory0 :: Applicative m => Branch0 m -> Branch0 m
discardHistory0 = over children (fmap tweak) where
  tweak b = cons (discardHistory0 (head b)) empty

-- `before b1 b2` is true if `b2` incorporates all of `b1`
before :: Monad m => Branch m -> Branch m -> m Bool
before (Branch b1) (Branch b2) = Causal.before b1 b2

pattern Hash h = Causal.RawHash h

-- | what does this do? —AI
toList0 :: Branch0 m -> [(Path, Branch0 m)]
toList0 = go Path.empty where
  go p b = (p, b) : (Map.toList (_children b) >>= (\(seg, cb) ->
    go (Path.snoc p seg) (head cb) ))

-- returns `Nothing` if no Branch at `path` or if Branch is empty at `path`
getAt :: Path
      -> Branch m
      -> Maybe (Branch m)
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
step f = \case
  Branch (Causal.One _h e) | e == empty0 -> Branch (Causal.one (f empty0))
  b -> over history (Causal.stepDistinct f) b

stepM :: (Monad m, Monad n) => (Branch0 m -> n (Branch0 m)) -> Branch m -> n (Branch m)
stepM f = \case
  Branch (Causal.One _h e) | e == empty0 -> Branch . Causal.one <$> f empty0
  b -> mapMOf history (Causal.stepDistinctM f) b

cons :: Applicative m => Branch0 m -> Branch m -> Branch m
cons = step . const

isOne :: Branch m -> Bool
isOne (Branch Causal.One{}) = True
isOne _ = False

uncons :: Applicative m => Branch m -> m (Maybe (Branch0 m, Branch m))
uncons (Branch b) = go <$> Causal.uncons b where
  go = over (_Just . _2) Branch

stepManyAt :: (Monad m, Foldable f)
           => f (Path, Branch0 m -> Branch0 m) -> Branch m -> Branch m
stepManyAt actions = step (stepManyAt0 actions)

stepManyAtM :: (Monad m, Monad n, Foldable f)
            => f (Path, Branch0 m -> n (Branch0 m)) -> Branch m -> n (Branch m)
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
    let h = H.hashPatch p'
    pure $ Map.insert seg (h, pure p') m

replacePatch :: Applicative m => NameSegment -> Patch -> Branch0 m -> Branch0 m
replacePatch n p = over edits (Map.insert n (H.hashPatch p, pure p))

deletePatch :: NameSegment -> Branch0 m -> Branch0 m
deletePatch n = over edits (Map.delete n)

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
           => f (Path, Branch0 m -> Branch0 m)
           -> Branch0 m -> Branch0 m
stepManyAt0 actions =
  runIdentity . stepManyAt0M [ (p, pure . f) | (p,f) <- toList actions ]

stepManyAt0M :: forall m n f . (Monad m, Monad n, Foldable f)
             => f (Path, Branch0 m -> n (Branch0 m))
             -> Branch0 m -> n (Branch0 m)
stepManyAt0M actions b = go (toList actions) b where
  go :: [(Path, Branch0 m -> n (Branch0 m))] -> Branch0 m -> n (Branch0 m)
  go actions b = let
    -- combines the functions that apply to this level of the tree
    currentAction b = foldM (\b f -> f b) b [ f | (Path.Empty, f) <- actions ]

    -- groups the actions based on the child they apply to
    childActions :: Map NameSegment [(Path, Branch0 m -> n (Branch0 m))]
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
      c2 <- stepChildren (view children b)
      currentAction (set children c2 b)

-- todo: consider inlining these into Actions2
addTermName
  :: Referent -> NameSegment -> Metadata.Metadata -> Branch0 m -> Branch0 m
addTermName r new md =
  over terms (Metadata.insertWithMetadata (r, md) . Star3.insertD1 (r, new))

addTypeName
  :: TypeReference -> NameSegment -> Metadata.Metadata -> Branch0 m -> Branch0 m
addTypeName r new md =
  over types (Metadata.insertWithMetadata (r, md) . Star3.insertD1 (r, new))

deleteTermName :: Referent -> NameSegment -> Branch0 m -> Branch0 m
deleteTermName r n b | Star3.memberD1 (r,n) (view terms b)
                     = over terms (Star3.deletePrimaryD1 (r,n)) b
deleteTermName _ _ b = b

deleteTypeName :: TypeReference -> NameSegment -> Branch0 m -> Branch0 m
deleteTypeName r n b | Star3.memberD1 (r,n) (view types b)
                     = over types (Star3.deletePrimaryD1 (r,n)) b
deleteTypeName _ _ b = b

lca :: Monad m => Branch m -> Branch m -> m (Maybe (Branch m))
lca (Branch a) (Branch b) = fmap Branch <$> Causal.lca a b

transform :: Functor m => (forall a . m a -> n a) -> Branch m -> Branch n
transform f b = case _history b of
  causal -> Branch . Causal.transform f $ transformB0s f causal
  where
  transformB0 :: Functor m => (forall a . m a -> n a) -> Branch0 m -> Branch0 n
  transformB0 f b =
    b { _children = transform f <$> _children b
      , _edits    = second f    <$> _edits b
      }

  transformB0s :: Functor m => (forall a . m a -> n a)
               -> Causal m Raw (Branch0 m)
               -> Causal m Raw (Branch0 n)
  transformB0s f = Causal.unsafeMapHashPreserving (transformB0 f)

-- | Traverse the head branch of all direct children.
-- The index of the traversal is the name of that child branch according to the parent.
children0 :: IndexedTraversal' NameSegment (Branch0 m) (Branch0 m)
children0 = children .> itraversed <. (history . Causal.head_)

instance H.Hashable (Branch0 m) where
  hash = H.hashBranch0
