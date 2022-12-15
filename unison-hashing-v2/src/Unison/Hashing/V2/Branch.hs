module Unison.Hashing.V2.Branch
  ( Raw (..),
    MdValues (..),
  )
where

import Unison.ContentAddressable (ContentAddressable (..))
import Unison.Hash (Hash)
import Unison.Hashing.V2.NameSegment (NameSegment)
import Unison.Hashing.V2.Reference (Reference)
import Unison.Hashing.V2.Referent (Referent)
import Unison.Hashing.V2.Tokenizable (Tokenizable)
import qualified Unison.Hashing.V2.Tokenizable as H
import Unison.Prelude

type MetadataValue = Reference

newtype MdValues = MdValues (Set MetadataValue)
  deriving (Eq, Ord, Show)
  deriving (Tokenizable) via Set MetadataValue

data Raw = Raw
  { terms :: Map NameSegment (Map Referent MdValues),
    types :: Map NameSegment (Map Reference MdValues),
    patches :: Map NameSegment Hash,
    children :: Map NameSegment Hash -- the Causal Hash
  }

instance ContentAddressable Raw where
  contentHash = H.hashTokenizable

instance Tokenizable Raw where
  tokens b =
    [ H.accumulateToken (terms b),
      H.accumulateToken (types b),
      H.accumulateToken (children b),
      H.accumulateToken (patches b)
    ]
