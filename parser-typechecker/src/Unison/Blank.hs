module Unison.Blank where

data Blank
  -- An expression that has not been filled in, has type `forall a . a`.
  = Placeholder
  -- A user-provided typed hole
  | Remember String
  -- A name to be resolved with type-directed name resolution.
  | Resolve String
  deriving (Show, Eq, Ord)

