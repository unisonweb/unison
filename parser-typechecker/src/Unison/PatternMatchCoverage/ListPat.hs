module Unison.PatternMatchCoverage.ListPat where

import Unison.Util.Pretty

data ListPat
  = Cons
  | Snoc
  | Nil
  deriving stock (Show, Eq, Ord)

prettyListPat :: ListPat -> Pretty ColorText
prettyListPat = \case
  Cons -> "Cons"
  Snoc -> "Snoc"
  Nil -> "Nil"
