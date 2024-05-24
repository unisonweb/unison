module Unison.NameSegment
  ( NameSegment,

    -- * Sentinel name segments
    libSegment,
  )
where

import Unison.NameSegment.Internal (NameSegment (NameSegment))

-- |
--
--  __TODO__: This should live in "Unison.Syntax.NameSegment", but it’s currently used in unison-core.
libSegment :: NameSegment
libSegment = NameSegment "lib"
