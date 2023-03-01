{-# LANGUAGE RecursiveDo #-}

module Unison.PatternMatchCoverage.UFMap
  ( UFMap,
    UFValue (..),
    empty,
    lookupCanon,
    insert,
    union,
    alterF,
    alter,
    keys,
    toClasses,
  )
where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Functor.Sum (Sum (..))
import Data.Map (Map)
import qualified Data.Map.Lazy as LazyMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A union-find structure. Used by
-- 'Unison.PatternMatchCoverage.NormalizedConstraints.NormalizedConstraints'
-- to provide efficient unification.
newtype UFMap k v = UFMap (Map k (UFValue k v))
  deriving stock (Eq, Ord, Show)

data UFValue k v
  = -- | This is not the canonical value, lookup k in the map to try again
    Indirection !k
  | -- | The number of elements in the equivalence class
    Canonical !Int !v
  deriving stock (Eq, Ord, Show)

empty :: UFMap k v
empty = UFMap Map.empty

insert :: (Ord k) => k -> v -> UFMap k v -> UFMap k v
insert k !v m =
  alter k (Just v) (\_ s _ -> Canonical s v) m

alterF' ::
  forall f k v.
  (Functor f, Ord k) =>
  -- | The key to lookup
  k ->
  -- | The canonical key (use laziness to supply if unknown)
  k ->
  -- | Return Just to short-circuit the indirection lookup loop
  (k -> UFMap k v -> Maybe (f (UFMap k v))) ->
  -- | Nothing case
  f (Maybe v) ->
  -- | Just case
  --
  -- @canonicalKey -> size -> value -> new value@
  --
  -- /N.B./ deleting a value is not supported
  (k -> Int -> v -> f (UFValue k v)) ->
  UFMap k v ->
  -- | Returns the canonical k, the size, the value, and the path
  -- compressed UFMap
  f (UFMap k v)
alterF' k0 kcanon loopGuard handleNothing handleJust map0 =
  let phi :: k -> Maybe (UFValue k v) -> Sum ((,) k) f (Maybe (UFValue k v))
      phi k =
        \case
          Nothing -> InR (fmap (Canonical 1) <$> handleNothing)
          Just alpha -> case alpha of
            Indirection k -> InL (k, Just (Indirection kcanon))
            Canonical sizeOrig v -> InR (Just <$> handleJust k sizeOrig v)
      go :: k -> UFMap k v -> f (UFMap k v)
      go k ufm@(UFMap m) = case loopGuard k ufm of
        Just short -> short
        Nothing -> case LazyMap.alterF (phi k) k m of
          InL (k, m') -> go k (UFMap m')
          InR res -> UFMap <$> res
   in go k0 map0
{-# INLINE alterF' #-}

alterFWithHalt ::
  forall f k v.
  (Functor f, Ord k) =>
  k ->
  (k -> UFMap k v -> Maybe (f (UFMap k v))) ->
  f (Maybe v) ->
  (k -> Int -> v -> f (UFValue k v)) ->
  UFMap k v ->
  f (UFMap k v)
alterFWithHalt k0 isCanonical handleNothing handleJust map0 =
  -- tie the canonicalK knot
  let (canonicalK, res) = getCompose (alterF' k0 canonicalK loopGuard handleNothing' handleJust' map0)
      handleNothing' = Compose (k0, handleNothing)
      handleJust' k s v = Compose (k, handleJust k s v)
      -- if the key is canonical then we halt and return it as the
      -- left element of the tuple
      loopGuard k m = Compose . (k,) <$> isCanonical k m
   in res
{-# INLINE alterFWithHalt #-}

alterF ::
  forall f k v.
  (Functor f, Ord k) =>
  k ->
  f (Maybe v) ->
  (k -> Int -> v -> f (UFValue k v)) ->
  UFMap k v ->
  f (UFMap k v)
alterF k = alterFWithHalt k (\_ _ -> Nothing)
{-# INLINE alterF #-}

alter ::
  forall k v.
  (Ord k) =>
  k ->
  Maybe v ->
  (k -> Int -> v -> UFValue k v) ->
  UFMap k v ->
  UFMap k v
alter k handleNothing handleJust map0 =
  runIdentity (alterF k (Identity handleNothing) (\k s v -> Identity (handleJust k s v)) map0)

-- | Lookup the canonical value
lookupCanon ::
  (Ord k) =>
  k ->
  UFMap k v ->
  -- | returns:
  --
  -- * the canonical member of the equivalence set
  -- * the size of the equivalence set
  -- * the associated value
  -- * the @UFMap@ after path compression
  Maybe (k, Int, v, UFMap k v)
lookupCanon k m =
  getCompose (alterF k nothing just m)
  where
    nothing = Compose Nothing
    just k s v = Compose (Just (k, s, v, Canonical s v))

data UnionHaltReason k v
  = KeyNotFound k
  | MergeFailed v v

data UnionValue k v a
  = UnionValue k Int v (UFValue k v) a
  deriving stock (Functor)

union ::
  forall m k v r.
  (MonadFix m, Ord k) =>
  k ->
  k ->
  UFMap k v ->
  (k -> v -> UFMap k v -> m (Maybe r)) ->
  m (Maybe r)
union k0 k1 mapinit mergeValues = toMaybe do
  rec let lu ::
            k ->
            UFMap k v ->
            (k -> UFMap k v -> Maybe (Compose (Either (UnionHaltReason k v)) (UnionValue k v) (UFMap k v))) ->
            Either (UnionHaltReason k v) (UnionValue k v (UFMap k v))
          lu k m loopGuard = getCompose (alterFWithHalt k loopGuard luNothing luJust m)
            where
              luNothing = Compose (Left (KeyNotFound k))
          luJust k s v =
            -- a final value thunk is inserted before it is resolved,
            -- as the final result cannot be known before we have
            -- looked up both values and merged them
            let newValue =
                  let newSize = case kcanon0 == kcanon1 of
                        True -> size0
                        False -> size0 + size1
                   in case chosenCanon == k of
                        True -> Canonical newSize canonValue
                        False -> Indirection chosenCanon
             in Compose (Right (UnionValue k s v newValue newValue))
      UnionValue kcanon0 size0 v0 vfinal0 map0 <- ExceptT $ pure $ lu k0 mapinit \_ _ -> Nothing
      UnionValue kcanon1 size1 v1 vfinal1 map1 <- ExceptT $
        pure $ lu k1 map0 \k m -> case k == kcanon0 of
          False -> Nothing
          True -> Just (Compose (Right (UnionValue k size0 v0 vfinal0 m)))
      -- Join the smaller equivalence class to the larger to bound
      -- worst case number of lookups to log(n). This is the same
      -- strategy as the weighted fast-union algorithm.
      let (chosenCanon, canonValue, nonCanonValue) = case size0 > size1 of
            True -> (kcanon0, v0, v1)
            False -> (kcanon1, v1, v0)
  map2 <-
    let res =
          ExceptT $
            mergeValues chosenCanon nonCanonValue map1 <&> \case
              Nothing -> Left (MergeFailed v0 v1)
              Just x -> Right x
     in -- Now that both lookups have completed we can safely force the
        -- final values
        vfinal0 `seq` vfinal1 `seq` res
  pure map2
  where
    toMaybe :: ExceptT (UnionHaltReason k v) m r -> m (Maybe r)
    toMaybe (ExceptT action) =
      action <&> \case
        Right m -> Just m
        Left r -> case r of
          KeyNotFound _k -> Nothing
          MergeFailed _v0 _v1 -> Nothing

toClasses :: forall k v. (Ord k) => UFMap k v -> [(k, Set k, v)]
toClasses m0 =
  let cmFinal :: Map k (k, Set k, v)
      (_mfinal, cmFinal) = foldl' phi (m0, Map.empty) keys
      keys = case m0 of
        UFMap m -> Map.keys m
      phi (m, cm) k =
        let (kcanon, _, v, m') = fromJust (lookupCanon k m)
            cm' =
              Map.insertWith
                (\(k0, s0, v0) (_k1, s1, _v1) -> (k0, s0 <> s1, v0))
                kcanon
                (k, Set.singleton k, v)
                cm
         in (m', cm')
   in Map.elems cmFinal

keys :: UFMap k v -> [k]
keys (UFMap m) = Map.keys m
