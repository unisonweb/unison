module Unison.Merge.DiffOp
  ( DiffOp (..),
  )
where

-- | A diff operation is one of:
--
--   * An add (where nothing was)
--   * A delete (of the thing that was)
--   * An update (from old to new)
data DiffOp a
  = Added !a
  | Deleted !a
  | Updated !a !a -- old, new
  deriving stock (Functor, Show)
