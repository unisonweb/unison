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

hashCausal :: H.Accumulate h => Causal -> [(H.Token h)]
hashCausal c =
  H.tokens $ [selfHash c, branchHash c] ++ (Set.toList $ parents c)

data Causal = Causal {selfHash :: Hash, branchHash :: Hash, parents :: Set Hash}

instance Hashable Causal where
  tokens c = hashCausal c
