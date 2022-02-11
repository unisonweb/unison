{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Codebase.Editor.Output.DumpNamespace where

import Data.Map (Map)
import Unison.NameSegment (NameSegment)
import Unison.Referent (Referent)
import Data.Set (Set)
import Unison.Reference (Reference)
import qualified Unison.Codebase.Branch as Branch

data DumpNamespace = DumpNamespace {
  terms :: Map Referent (Set NameSegment, Set Reference),
  types :: Map Reference (Set NameSegment, Set Reference),
  patches :: Map NameSegment Branch.EditHash,
  children :: Map NameSegment Branch.Hash,
  causalParents :: Set Branch.Hash
} deriving Show

