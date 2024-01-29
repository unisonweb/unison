module Unison.Blank where

import Unison.Prelude

loc :: Recorded loc -> loc
loc (Placeholder loc _) = loc
loc (Resolve loc _) = loc
loc (MissingResultPlaceholder loc) = loc

nameb :: Blank loc -> Maybe String
nameb (Recorded (Placeholder _ n)) = Just n
nameb (Recorded (Resolve _ n)) = Just n
nameb _ = Nothing

data Recorded loc
  = -- A user-provided named placeholder
    Placeholder loc String
  | -- A name to be resolved with type-directed name resolution.
    Resolve
      loc
      String
  | -- A placeholder for a missing result at the end of a block
    MissingResultPlaceholder
      loc
  deriving (Show, Eq, Ord, Functor, Generic)

-- - Blank is just a dummy annotation.
-- - Recorded indicates that we want to remember the variable's solution
--   for some kind of
data Blank loc
  = Blank
  -- ^ just a dummy annotation
  | Recorded (Recorded loc)
  -- ^ indicates that we want to remember the variable's solution for
  -- some reason
  | Retain
  -- ^ indicates that we want to prefer keeping the variable in the
  -- context to better refine the above recorded solutions
  deriving (Show, Eq, Ord, Functor, Generic)
