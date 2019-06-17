module Unison.Codebase.Metadata where

import Data.Map (Map)
import Data.Set (Set)
import Unison.Referent (Referent)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Type = Referent
type Value = Referent

-- keys can be terms or types
type Metadata = Map Referent (Map Type (Set Value))

insert :: Referent -> Type -> Referent -> Metadata -> Metadata
insert src typ r =
  Map.insertWith collide src (Map.singleton typ (Set.singleton r))
  where
  collide = Map.unionWith (<>)

delete :: Referent -> Type -> Value -> Metadata -> Metadata
delete src typ r md = case Map.lookup src md of
  Nothing -> md
  Just m -> case Map.lookup typ m of
    Just s | Set.member r s ->
      Map.insert src (Map.insert typ (Set.delete r s) m) md
    _ -> md

-- parallel composition - commutative and associative
merge :: Metadata -> Metadata -> Metadata
merge = Map.unionWith (Map.unionWith (<>))

-- sequential composition, right-biased
append :: Metadata -> Metadata -> Metadata
append = Map.unionWith (Map.unionWith (flip const))
