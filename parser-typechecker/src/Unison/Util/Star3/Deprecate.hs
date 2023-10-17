{-# LANGUAGE RecordWildCards #-}

module Unison.Util.Star3.Deprecate
  ( module Unison.Util.Star3,
    deleteD1RanAndGC,
  )
where

import Unison.Util.Relation qualified as R
import Unison.Util.Star3

deleteD1RanAndGC :: (Ord fact, Ord d1, Ord x, Ord y) => d1 -> Star3 fact d1 d2 d3 -> Star3 fact d1 x y
deleteD1RanAndGC x star@Star3 {d1} =
  star
    { fact = R.dom d1',
      d1 = d1',
      d2 = mempty,
      d3 = mempty
    }
  where
    d1' = R.deleteRan x d1
