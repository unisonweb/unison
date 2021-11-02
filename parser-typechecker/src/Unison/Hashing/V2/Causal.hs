{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Hashing.V2.Causal
  ( Causal (..),
    hashCausal,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Unison.Hash (Hash)
import Unison.Hashable (Hashable)
import qualified Unison.Hashable as H

hashCausal :: H.Accumulate h => Causal -> h
hashCausal = H.accumulate'

data Causal = Causal {branchHash :: Hash, parents :: Set Hash}

instance Hashable Causal where
  tokens c = H.tokens $ branchHash c : Set.toList (parents c)
