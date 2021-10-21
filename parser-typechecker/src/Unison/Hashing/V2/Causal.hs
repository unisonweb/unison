{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Hashing.V2.Causal (Causal (..)) where

import Data.Set (Set)
import Unison.Hash (Hash)
import Unison.Hashable (Hashable)
import qualified Unison.Hashable as H

data Causal e = Causal {current :: e, parents :: Set Hash}

instance Hashable e => Hashable (Causal e) where
  tokens c = H.tokens (current c, parents c)
