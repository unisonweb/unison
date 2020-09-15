{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
module Unison.Codebase.V1.Causal.Raw where

import U.Util.Hashable (Hashable)
import U.Util.Hash (Hash)
import Data.Set (Set)
import Data.Foldable (Foldable(toList))

newtype RawHash h = RawHash { unRawHash :: Hash }
  deriving (Eq, Ord, Hashable) via Hash

data Raw h e
  = RawOne e
  | RawCons e (RawHash h)
  | RawMerge e (Set (RawHash h))

rawHead :: Raw h e -> e
rawHead (RawOne e    ) = e
rawHead (RawCons  e _) = e
rawHead (RawMerge e _) = e

rawTails :: Raw h e -> [RawHash h]
rawTails (RawOne _)      = []
rawTails (RawCons _ h)   = [h]
rawTails (RawMerge _ hs) = toList hs
