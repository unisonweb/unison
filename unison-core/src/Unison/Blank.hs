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

data Blank loc = Blank | Recorded (Recorded loc)
  deriving (Show, Eq, Ord, Functor, Generic)
