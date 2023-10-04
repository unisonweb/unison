module Unison.Merge.NamespaceTypes
  ( Defns (..),
    DefnsA,
    DefnsB,
    NamespaceTree,
  )
where

import Control.Comonad.Cofree (Cofree)
import Unison.Name (Name)
import Unison.NameSegment
import Unison.Prelude
import Unison.Util.BiMultimap (BiMultimap)

-- | Definitions (terms and types) in a namespace.
data Defns terms types = Defns
  { terms :: terms,
    types :: types
  }
  deriving stock (Generic, Show)

-- haha rename or delete
type DefnsA terms types =
  Defns (BiMultimap terms Name) (BiMultimap types Name)

-- haha rename or delete
type DefnsB terms types =
  Defns (Map Name terms) (Map Name types)

-- | A namespace tree has values, and a collection of children namespace trees keyed by name segment.
type NamespaceTree a =
  Cofree (Map NameSegment) a
