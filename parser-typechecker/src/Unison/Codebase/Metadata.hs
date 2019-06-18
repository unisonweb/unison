module Unison.Codebase.Metadata where

import Data.Map (Map)
import Data.Set (Set)
import Unison.Referent (Referent)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Type = Referent
type Value = Referent

-- keys can be terms or types
type Metadata = Map Type (Set Value)

insert :: Type -> Referent -> Metadata -> Metadata
insert typ r = Map.insertWith (<>) typ (Set.singleton r)

delete :: Type -> Value -> Metadata -> Metadata
delete typ r md = case Map.lookup typ md of
  Just s | Set.member r s -> Map.insert typ (Set.delete r s) md
  _ -> md

-- parallel composition - commutative and associative
merge :: Metadata -> Metadata -> Metadata
merge = Map.unionWith (<>)

-- sequential composition, right-biased
append :: Metadata -> Metadata -> Metadata
append = Map.unionWith (flip const)
