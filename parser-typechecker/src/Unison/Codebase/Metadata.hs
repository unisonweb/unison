module Unison.Codebase.Metadata
  ( Star,
    Type,
    Value,
    insert,
    delete,
  )
where

import Data.Map qualified as Map
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Util.List qualified as List
import Unison.Util.Relation qualified as R
import Unison.Util.Star3 (Star3)
import Unison.Util.Star3 qualified as Star3

type Type = Reference

type Value = Reference

-- `a` is generally the type of references or hashes
-- `n` is generally the the type of name associated with the references
-- `Type` is the type of metadata. Duplicate info to speed up certain queries.
-- `(Type, Value)` is the metadata value itself along with its type.
type Star a n = Star3 a n Type (Type, Value)

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
