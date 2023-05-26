module Unison.Hashing.V2.Causal
  ( Causal (..),
  )
where

import Data.Set qualified as Set
import Unison.Hash (Hash)
import Unison.Hashing.ContentAddressable (ContentAddressable (..))
import Unison.Hashing.V2.Tokenizable qualified as H
import Unison.Prelude

data Causal = Causal {branchHash :: Hash, parents :: Set Hash}

instance ContentAddressable Causal where
  contentHash = H.hashTokenizable

instance H.Tokenizable Causal where
  tokens c = H.tokens $ branchHash c : Set.toList (parents c)
