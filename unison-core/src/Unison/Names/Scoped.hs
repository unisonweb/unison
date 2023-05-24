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
