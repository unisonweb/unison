module Unison.Codebase.Editor.Output.DumpNamespace where

import Data.Map (Map)
import Data.Set (Set)
import qualified Unison.Codebase.Branch as Branch
import Unison.NameSegment (NameSegment)
import Unison.Reference (Reference)
import Unison.Referent (Referent)

data DumpNamespace = DumpNamespace
  { terms :: Map Referent (Set NameSegment, Set Reference),
    types :: Map Reference (Set NameSegment, Set Reference),
    patches :: Map NameSegment Branch.EditHash,
    children :: Map NameSegment Branch.Hash,
    causalParents :: Set Branch.Hash
  }
  deriving (Show)
