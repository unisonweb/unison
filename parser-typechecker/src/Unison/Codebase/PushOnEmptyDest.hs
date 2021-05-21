module Unison.Codebase.PushOnEmptyDest where

-- Whether `push` is allowed to create a new branch / push to an empty branch.
data PushOnEmptyDest
  = PushOnEmptyDest
  | AbortOnEmptyDest
  deriving (Eq, Show)