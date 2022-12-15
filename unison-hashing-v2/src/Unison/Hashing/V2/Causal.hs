module Unison.Hashing.V2.Causal
  ( Causal (..),
  )
where

import qualified Data.Set as Set
import Unison.Hash (Hash)
import Unison.Hashing.ContentAddressable (ContentAddressable (..))
import qualified Unison.Hashing.V2.Tokenizable as H
import Unison.Prelude

data Causal = Causal {branchHash :: Hash, parents :: Set Hash}

instance ContentAddressable Causal where
  contentHash = H.hashTokenizable

instance H.Tokenizable Causal where
  tokens c = H.tokens $ branchHash c : Set.toList (parents c)
