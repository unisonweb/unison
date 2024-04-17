module Unison.Merge.AliceXorBob
  ( AliceXorBob (..),
    swap,
  )
where

-- | Alice exclusive-or Bob?
data AliceXorBob
  = Alice
  | Bob
  deriving stock (Eq, Show)

swap :: AliceXorBob -> AliceXorBob
swap Alice = Bob
swap Bob = Alice
