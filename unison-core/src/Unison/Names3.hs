module Unison.Names3
  ( Names3(..),
    allLocals,
    temporarilyAllLocals
  )
where

import Unison.Names (Names)

-- | A @Names3@ is three disjoint sets of names:
--
--   1. "Local" names, none of which begin with a `lib` segment.
--   2. "Direct deps" names, which are all of the form `lib.X.Y`, where `Y` does not begin with a `lib` segment.
--   3. "Indirect deps" names, which are all of the form `lib.X.lib.Y.Z`
data Names3 = Names3
  { local :: Names,
    directDeps :: Names,
    indirectDeps :: Names
  }

allLocals :: Names -> Names3
allLocals local =
  Names3 {local, directDeps = mempty, indirectDeps = mempty}

-- A temporary way of making a Names3 from a Names that treats every name as a local.
--
-- This is intended only to exist as a means to bridge the old world of not distinguishing between kinds of names to the
-- new world (of distinguishing) - really, no caller should be using `allLocals`. So, it will be a useful thing to grep
-- for.
temporarilyAllLocals :: Names -> Names3
temporarilyAllLocals =
  allLocals
