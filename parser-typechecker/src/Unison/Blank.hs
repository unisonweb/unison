module Unison.Blank where

loc :: Recorded loc -> loc
loc (Placeholder loc _) = loc
loc (Resolve loc _) = loc

data Recorded loc
  -- A user-provided named placeholder
  = Placeholder loc String
  -- A name to be resolved with type-directed name resolution.
  | Resolve loc String
  deriving (Show, Eq, Ord)

data Blank loc = Blank | Recorded (Recorded loc)
  deriving (Show, Eq, Ord)


