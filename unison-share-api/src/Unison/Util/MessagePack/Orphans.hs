{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Util.MessagePack.Orphans where

import Control.Monad.Validate
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.NonEmpty as NEMap
import Data.MessagePack
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import U.Util.Base32Hex
import U.Util.Hash32
import Unison.Prelude

instance (Ord a, MessagePack a) => MessagePack (NESet.NESet a) where
  toObject config = toObject config . toList
  fromObjectWith config obj = do
    fromObjectWith config obj >>= \case
      [] -> refute "Expected non-empty set"
      (x : xs) -> pure $ NESet.fromList (x :| xs)

instance (Ord a, MessagePack a) => MessagePack (Set a) where
  toObject config = toObject config . toList
  fromObjectWith config obj = Set.fromList <$> fromObjectWith config obj

instance (Ord k, MessagePack k, MessagePack v) => MessagePack (NEMap.NEMap k v) where
  toObject config = toObject config . toList
  fromObjectWith config obj = do
    fromObjectWith config obj >>= \case
      [] -> refute "Expected non-empty map"
      (x : xs) -> pure $ NEMap.fromList (x :| xs)

instance MessagePack Hash32 where
  toObject config (UnsafeFromBase32Hex (UnsafeFromText txt)) = toObject config txt
  fromObjectWith config obj = UnsafeFromBase32Hex . UnsafeFromText <$> fromObjectWith config obj
