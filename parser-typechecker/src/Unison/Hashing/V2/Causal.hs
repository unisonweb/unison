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
import qualified Unison.Hashing.V2.BuildHashable as H

hashCausal :: Causal -> Hash
hashCausal = H.accumulate'

data Causal = Causal {branchHash :: Hash, parents :: Set Hash}

instance H.Hashable Causal where
  tokens c = H.tokens $ branchHash c : Set.toList (parents c)
