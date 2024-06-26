module Unison.Merge.PartialDeclNameLookup
  ( PartialDeclNameLookup (..),
  )
where

import Unison.Name (Name)
import Unison.Prelude

-- | Like a @DeclNameLookup@, but "partial" / more lenient - because we don't require the LCA of a merge to have a full
-- @DeclNameLookup@.
data PartialDeclNameLookup = PartialDeclNameLookup
  { constructorToDecl :: !(Map Name Name),
    declToConstructors :: !(Map Name [Maybe Name])
  }
  deriving stock (Generic)
