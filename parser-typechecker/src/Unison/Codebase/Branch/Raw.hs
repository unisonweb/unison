module Unison.Codebase.Branch.Raw where

import Data.Map (Map)
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Hash (Hash)
import qualified Unison.Hash as Hash
import Unison.NameSegment (NameSegment)
import Unison.Reference (Reference)
import Unison.Referent (Referent)

type Star r n = Metadata.Star r n

type EditHash = Hash.Hash

-- The raw Branch
data Raw = Raw
  { _termsR :: Star Referent NameSegment,
    _typesR :: Star Reference NameSegment,
    _childrenR :: Map NameSegment Hash,
    _editsR :: Map NameSegment EditHash
  }