module Unison.PatternMatchCoverage.ListPat where

data ListPat
  = Cons
  | Snoc
  | Nil
  deriving stock (Show, Eq, Ord)
