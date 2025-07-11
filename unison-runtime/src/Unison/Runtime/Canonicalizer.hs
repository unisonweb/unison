
module Unison.Runtime.Canonicalizer
  ( Canonicalizer
  , canonicalize
  , categorize
  , unsafeCategorize
  , Canonicity (..)
  , CanonMap (..)
  , empty
  , lookup
  , unsafeLookup
  , findWithDefault
  , fromListByIndex
  , fromList
  )
  where

import Control.Exception (evaluate)
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HM
import Data.Map.Lazy qualified as M
import System.IO.Unsafe
import System.Mem.StableName
import Prelude hiding (lookup)

-- A canonicalizer is a structure for mapping values to versions
-- that are unique in memory. This is accomplished via two
-- mappings. One maps stable names to a canonical value, which is
-- fast if we've seen the exact in-memory value before. A second
-- is just a normal hash map, which will canonicalize values by
-- hash code/equality, which allows us to add to the fast lookup.
data Canonicalizer a = Canon {
    stableMap :: !(HashMap (StableName a) a),
    _slowMap :: !(M.Map a a)
  }

empty :: Canonicalizer a
empty = Canon HM.empty M.empty

-- Result for categorizing a value with regard to a Canonicalizer
data Canonicity a
  -- the provided value is the known canonical one
  = Canonical
  -- the provided value is equivalent to this canonical one; updated
  -- canonicalizer
  | Equivalent a (Canonicalizer a)
  -- the provided value was not previously known, and added to the
  -- canonicalizer
  | Novel (Canonicalizer a)

categorize0 ::
  (Ord a) =>
  Canonicalizer a ->
  a ->
  StableName a ->
  IO (Canonicity a)
categorize0 cn@(Canon fast slow) x xname
  | Just !y <- HM.lookup xname fast = do
    yname <- makeStableName y
    if xname == yname
    then pure Canonical
    else pure (Equivalent y cn)
  | Just x <- M.lookup x slow = do
      cn <- evaluate cn { stableMap = HM.insert xname x fast }
      pure (Equivalent x cn)
  | otherwise = do
      cn <- evaluate (Canon (HM.insert xname x fast) (M.insert x x slow))
      pure (Novel cn)
{-# INLINE categorize0 #-}

-- Incorporates a value into a canonicalizer, giving a canonical
-- value, and also indicating whether the value has been seen before,
-- either up to in-memory or Eq equality.
categorize ::
  (Ord a) =>
  Canonicalizer a ->
  a ->
  IO (Canonicity a)
categorize cn !x = makeStableName x >>= categorize0 cn x
{-# INLINE categorize #-}

unsafeCategorize ::
  (Ord a) => Canonicalizer a -> a -> Canonicity a
unsafeCategorize cn a = unsafePerformIO $ categorize cn a

-- Produces a canonical value and an updated canonicalizer under
-- the assumption that the given stable name is the one for the
-- given value.
canonicalize0 ::
  (Ord a) =>
  Canonicalizer a ->
  a ->
  StableName a ->
  IO (a, Canonicalizer a)
canonicalize0 cn@(Canon fast slow) x name
  | Just x <- HM.lookup name fast = pure (x, cn)
  | Just x <- M.lookup x slow = do
      cn <- evaluate cn { stableMap = HM.insert name x fast }
      pure (x, cn)
  | otherwise = do
    cn <- evaluate (Canon (HM.insert name x fast) (M.insert x x slow))
    pure (x, cn)
{-# INLINE canonicalize0 #-}

-- Canonicalizes a value, giving an updated canonicalizer.
--
-- Note: uses `unsafePerformIO`. This should be fine, because we
-- are only using stable names to canonicalize values in memory,
-- always replacing values with other values that are identical
-- according to the `Eq` instance.
--
-- The API exposed only allows such canonicalization and building
-- opaque `Canonicalizer` values, so there should be no opportunity
-- for doing anything actually unsafe.
canonicalize :: (Ord a) => Canonicalizer a -> a -> (a, Canonicalizer a)
canonicalize cn !x =
  unsafePerformIO $ makeStableName x >>= canonicalize0 cn x
{-# INLINABLE canonicalize #-}

newtype CanonMap k v = CanonM (HashMap (StableName k) v)
  deriving (Functor)

lookup :: k -> CanonMap k v -> IO (Maybe v)
lookup !k (CanonM m) = flip HM.lookup m <$> makeStableName k
{-# INLINE lookup #-}

findWithDefault :: v -> k -> CanonMap k v -> IO v
findWithDefault d !k (CanonM m) =
  flip (HM.findWithDefault d) m <$> makeStableName k
{-# INLINE findWithDefault #-}

unsafeLookup :: k -> CanonMap k v -> Maybe v
unsafeLookup k m = unsafePerformIO $ lookup k m
{-# INLINE unsafeLookup #-}

fromListByIndex :: [k] -> CanonMap k Int
fromListByIndex l = unsafePerformIO do
  l <- traverse (\k -> makeStableName =<< evaluate k) l
  pure . CanonM $ HM.fromList (zip l [0..])

fromList :: [(k, v)] -> IO (CanonMap k v)
fromList = fmap (CanonM . HM.fromList) . traverse f
  where
    f (k, v) = (,v) <$> (makeStableName =<< evaluate k)
