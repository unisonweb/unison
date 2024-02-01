module Unison.Codebase.Metadata where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Util.List qualified as List
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as R
import Unison.Util.Relation4 (Relation4)
import Unison.Util.Relation4 qualified as R4
import Unison.Util.Star3 (Star3)
import Unison.Util.Star3 qualified as Star3

type Type = Reference

type Value = Reference

-- keys can be terms or types
type Metadata = Map Type (Set Value)

-- `a` is generally the type of references or hashes
-- `n` is generally the the type of name associated with the references
-- `Type` is the type of metadata. Duplicate info to speed up certain queries.
-- `(Type, Value)` is the metadata value itself along with its type.
type Star a n = Star3 a n Type (Type, Value)

type R4 a n = R4.Relation4 a n Type Value

starToR4 :: (Ord r, Ord n) => Star r n -> Relation4 r n Type Value
starToR4 = R4.fromList . starToR4List

-- | Flattens a Metadata.Star into a 4-tuple.
starToR4List :: (Ord r) => Star r n -> [(r, n, Type, Value)]
starToR4List s =
  [ (f, x, y, z)
    | f <- Set.toList (Star3.fact s),
      x <- Set.toList (R.lookupDom f (Star3.d1 s)),
      (y, z) <- Set.toList (R.lookupDom f (Star3.d3 s))
  ]

hasMetadata :: (Ord a) => a -> Type -> Value -> Star a n -> Bool
hasMetadata a t v = Set.member (t, v) . R.lookupDom a . Star3.d3

hasMetadataWithType' :: (Ord a) => a -> Type -> R4 a n -> Bool
hasMetadataWithType' =
  R4.memberD13

hasMetadataWithType :: (Ord a) => a -> Type -> Star a n -> Bool
hasMetadataWithType a t = Set.member t . R.lookupDom a . Star3.d2

inserts :: (Ord a, Ord n) => [(a, Type, Value)] -> Star a n -> Star a n
inserts tups s = foldl' (flip insert) s tups

insert :: (Ord a, Ord n) => (a, Type, Value) -> Star a n -> Star a n
insert (a, ty, v) = Star3.insertD23 (a, ty, (ty, v))

delete :: (Ord a, Ord n) => (a, Type, Value) -> Star a n -> Star a n
delete (a, ty, v) s =
  let s' = Star3.deleteD3 (a, (ty, v)) s
      -- if (ty,v) is the last metadata of type ty
      -- we also delete (a, ty) from the d2 index
      metadataByType = List.multimap (toList (R.lookupDom a (Star3.d3 s)))
   in case Map.lookup ty metadataByType of
        Just vs | all (== v) vs -> Star3.deleteD2 (a, ty) s'
        _ -> s'

-- parallel composition - commutative and associative
merge :: Metadata -> Metadata -> Metadata
merge = Map.unionWith (<>)

-- sequential composition, right-biased
append :: Metadata -> Metadata -> Metadata
append = Map.unionWith (flip const)

empty :: Metadata
empty = mempty

singleton :: Type -> Value -> Metadata
singleton ty v = Map.singleton ty (Set.singleton v)

toRelation :: Star3 a n x y -> Relation a n
toRelation = Star3.d1
