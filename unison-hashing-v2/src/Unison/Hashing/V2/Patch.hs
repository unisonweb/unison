module Unison.Hashing.V2.Patch
  ( Patch (..),
  )
where

import Unison.ContentAddressable (ContentAddressable (contentHash))
import Unison.Hashing.V2.Reference (Reference)
import Unison.Hashing.V2.Referent (Referent)
import Unison.Hashing.V2.TermEdit (TermEdit)
import Unison.Hashing.V2.Tokenizable (Tokenizable)
import qualified Unison.Hashing.V2.Tokenizable as H
import Unison.Hashing.V2.TypeEdit (TypeEdit)
import Unison.Prelude

data Patch = Patch
  { termEdits :: Map Referent (Set TermEdit),
    typeEdits :: Map Reference (Set TypeEdit)
  }

instance ContentAddressable Patch where
  contentHash = H.hashTokenizable

instance Tokenizable Patch where
  tokens p =
    [ H.accumulateToken (termEdits p),
      H.accumulateToken (typeEdits p)
    ]
