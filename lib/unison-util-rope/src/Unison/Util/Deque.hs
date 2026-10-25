{-# LANGUAGE PatternSynonyms #-}

-- | A strict finger tree: see "Unison.Util.Deque.Internal", which has the
-- implementation and also exports the parts that "Unison.Util.Rope" shares.
module Unison.Util.Deque
  ( -- * The type
    Deque,
    Seq,
    pattern Empty,
    pattern (:<|),
    pattern (:|>),

    -- * Construction
    empty,
    singleton,
    cons,
    snoc,
    append,
    (<|),
    (|>),
    (><),
    fromList,
    fromListN,
    unsafeFromListN,
    fromFunction,

    -- * Queries
    size,
    length,
    null,
    lookup,
    toList,

    -- * Taking apart
    uncons,
    unsnoc,
    take,
    drop,
    splitAt,

    -- * Whole-sequence transformations (these rebuild the sequence)
    sort,
    sortBy,
    unstableSort,
    unstableSortBy,
    intersperse,
    reverse,

    -- * For tests
    valid,
  )
where

import Unison.Util.Deque.Internal
import Prelude ()
