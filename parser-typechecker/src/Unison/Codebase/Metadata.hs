module Unison.Codebase.Metadata where

import Data.Map (Map)
import Data.Set (Set)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Type = Reference

type Metadata = Map Reference (Map Type (Set Referent))

insert :: Reference -> Type -> Referent -> Metadata -> Metadata
insert src typ r =
  Map.insertWith collide src (Map.singleton typ (Set.singleton r))
  where
  collide = Map.unionWith (<>)

delete :: Reference -> Type -> Referent -> Metadata -> Metadata
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

includeOnly :: Set Reference -> Metadata -> Metadata
includeOnly s md =
  Map.fromList [ (r, m) | r <- Set.toList s, Just m <- [Map.lookup r md]]
