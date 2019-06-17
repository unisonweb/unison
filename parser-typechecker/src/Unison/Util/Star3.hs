module Unison.Util.Star3 where

import Data.Set (Set)
import qualified Data.Set as Set
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R

-- Star schema with 3 dimensions.
-- Can be efficiently queried from any of the dimensions.
data Star3 fact d1 d2 d3
  = Star3 { fact :: Set fact
          , d1 :: Relation fact d1
          , d2 :: Relation fact d2
          , d3 :: Relation fact d3 }

insert :: (Ord fact, Ord d1, Ord d2, Ord d3)
       => (fact, d1, d2, d3)
       -> Star3 fact d1 d2 d3
       -> Star3 fact d1 d2 d3
insert (f, d1i, d2i, d3i) s = Star3 fact' d1' d2' d3' where
  fact' = Set.insert f (fact s)
  d1'   = R.insert f d1i (d1 s)
  d2'   = R.insert f d2i (d2 s)
  d3'   = R.insert f d3i (d3 s)

instance (Ord fact, Ord d1, Ord d2, Ord d3) => Semigroup (Star3 fact d1 d2 d3) where
  (<>) = mappend

instance (Ord fact, Ord d1, Ord d2, Ord d3) => Monoid (Star3 fact d1 d2 d3) where
  mempty = Star3 mempty mempty mempty mempty
  s1 `mappend` s2 = Star3 fact' d1' d2' d3' where
    fact' = Set.intersection (fact s1) (fact s2)
    d1' = (fact' R.<| d1 s1) <> (fact' R.<| d1 s2)
    d2' = (fact' R.<| d2 s1) <> (fact' R.<| d2 s2)
    d3' = (fact' R.<| d3 s1) <> (fact' R.<| d3 s2)

-- Star Reference/Referent NameSegment Metadata.Type Metadata.Value
