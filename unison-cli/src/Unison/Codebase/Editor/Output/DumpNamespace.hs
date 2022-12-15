module Unison.Codebase.Editor.Output.DumpNamespace where

import U.Codebase.HashTags (CausalHash, PatchHash)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)

data DumpNamespace = DumpNamespace
  { terms :: Map Referent (Set NameSegment, Set Reference),
    types :: Map Reference (Set NameSegment, Set Reference),
    patches :: Map NameSegment PatchHash,
    children :: Map NameSegment CausalHash,
    causalParents :: Set CausalHash
  }
  deriving (Show)
