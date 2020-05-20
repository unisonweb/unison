{-# language DeriveTraversable #-}
{-# language GeneralizedNewtypeDeriving #-}

module Unison.Util.WordContainers
  ( WordMap
  , WordSet
  , mapFromList
  , setFromList
  , mapSingleton
  , setSingleton
  , mapInsert
  , unionWith
  , keys
  , restrictKeys
  , withoutKeys
  , member
  , lookup
  , lookupWithDefault
  , foldMapWithKey
  , mapToList
  , (!)
  ) where

import Prelude hiding (lookup)

import Data.Bifunctor

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

import Data.Word (Word64)

newtype WordMap a
  = WM (IM.IntMap a)
  deriving
    ( Monoid
    , Semigroup
    , Functor
    , Foldable
    , Traversable
    , Show
    , Eq
    , Ord
    )
newtype WordSet
  = WS IS.IntSet
  deriving
    ( Monoid
    , Semigroup
    , Show
    , Eq
    , Ord
    )

mapFromList :: [(Word64, a)] -> WordMap a
mapFromList = WM . IM.fromList . fmap (first fromIntegral)

setFromList :: [Word64] -> WordSet
setFromList = WS . IS.fromList . fmap fromIntegral

mapSingleton :: Word64 -> a -> WordMap a
mapSingleton w a = WM $ IM.singleton (fromIntegral w) a

setSingleton :: Word64 -> WordSet
setSingleton w = WS . IS.singleton $ fromIntegral w

mapInsert :: Word64 -> a -> WordMap a -> WordMap a
mapInsert w x (WM m) = WM $ IM.insert (fromIntegral w) x m

unionWith :: (a -> a -> a) -> WordMap a -> WordMap a -> WordMap a
unionWith f (WM l) (WM r) = WM $ IM.unionWith f l r

keys :: WordMap a -> [Word64]
keys (WM m) = fmap fromIntegral . IM.keys $ m

restrictKeys :: WordMap a -> WordSet -> WordMap a
restrictKeys (WM m) (WS s) = WM $ IM.restrictKeys m s

withoutKeys :: WordMap a -> WordSet -> WordMap a
withoutKeys (WM m) (WS s) = WM $ IM.withoutKeys m s

member :: Word64 -> WordSet -> Bool
member w (WS s) = IS.member (fromIntegral w) s

lookup :: Word64 -> WordMap a -> Maybe a
lookup w (WM m) = IM.lookup (fromIntegral w) m

lookupWithDefault :: a -> Word64 -> WordMap a -> a
lookupWithDefault d w (WM m) = IM.findWithDefault d (fromIntegral w) m

foldMapWithKey :: Monoid m => (Word64 -> a -> m) -> WordMap a -> m
foldMapWithKey f (WM m) = IM.foldMapWithKey (f . fromIntegral) m

mapToList :: WordMap a -> [(Word64, a)]
mapToList (WM m) = first fromIntegral <$> IM.toList m

(!) :: WordMap a -> Word64 -> a
WM m ! w = m IM.! fromIntegral w
