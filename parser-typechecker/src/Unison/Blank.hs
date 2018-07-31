module Unison.Blank where

data Blank loc
  -- An expression that has not been filled in, has type `forall a . a`.
  = Placeholder
  -- A user-provided typed hole
  | Remember loc String
  -- A name to be resolved with type-directed name resolution.
  | Resolve loc String
  deriving (Show, Eq, Ord)
