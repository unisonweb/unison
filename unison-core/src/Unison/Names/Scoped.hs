module Unison.Names.Scoped where

import Unison.Names (Names)
import qualified Unison.Names as Names

-- | Configure how names will be filtered.
--   this is typically used when fetching names for printing source code or when finding
--   definitions by name.
data NamesFilter
  = -- | Include all names
    -- otherwise leave them absolute.
    AllNames
  | -- | Filter returned names to only include names within this path.
    Scoped

-- | Contains all useful permutations of names scoped to a given branch.
data ScopedNames = ScopedNames
  { absoluteExternalNames :: Names,
    relativeScopedNames :: Names,
    absoluteRootNames :: Names
  }

-- scopedPrettyNames :: NamesFilter -> ScopedNames -> Names
-- scopedPrettyNames AllNames (ScopedNames {relativeScopedNames, absoluteExternalNames}) = relativeScopedNames `Names.unionLeft` absoluteExternalNames
-- scopedPrettyNames Scoped (ScopedNames {relativeScopedNames}) = relativeScopedNames

-- scopedParseNames :: NamesFilter -> ScopedNames -> Names
-- scopedParseNames AllNames (ScopedNames {relativeScopedNames, absoluteRootNames}) = relativeScopedNames <> absoluteRootNames
-- scopedParseNames Scoped (ScopedNames {relativeScopedNames}) = relativeScopedNames

-- | Return all names contained in the path, relative to that path.
namesAtPath :: ScopedNames -> Names
namesAtPath (ScopedNames {relativeScopedNames}) = relativeScopedNames

-- | Includes ALL absolute names AND includes relative names for anything in the path.
parseNames :: ScopedNames -> Names
parseNames (ScopedNames {relativeScopedNames, absoluteRootNames}) = relativeScopedNames <> absoluteRootNames

-- | Includes includes relative names for anything in the path, and absolute names for
-- everything else.
prettyNames :: ScopedNames -> Names
prettyNames (ScopedNames {relativeScopedNames, absoluteExternalNames}) = relativeScopedNames `Names.unionLeft` absoluteExternalNames
