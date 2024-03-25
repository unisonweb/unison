module Unison.PatternMatchCoverage.IsExhaustive
  ( IsExhaustive (..),
  )
where

data IsExhaustive
  = Exhaustive
  | NonExhaustive
  deriving stock (Show)
