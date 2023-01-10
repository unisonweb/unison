module Unison.Names.Scoped where

import Unison.Names (Names)

-- | Contains all useful permutations of names scoped to a given branch.
data ScopedNames = ScopedNames
  { relativeScopedNames :: Names,
    absoluteRootNames :: Names
  }

-- | Return all names contained in the path, relative to that path.
namesAtPath :: ScopedNames -> Names
namesAtPath (ScopedNames {relativeScopedNames}) = relativeScopedNames

-- | Includes ALL absolute names AND includes relative names for anything in the path.
parseNames :: ScopedNames -> Names
parseNames (ScopedNames {relativeScopedNames, absoluteRootNames}) = relativeScopedNames <> absoluteRootNames

-- | Includes includes relative names for anything in the path, and absolute names for
-- everything else.
prettyNames :: ScopedNames -> Names
prettyNames (ScopedNames {relativeScopedNames}) = relativeScopedNames
