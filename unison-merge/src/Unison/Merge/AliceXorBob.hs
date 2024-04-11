module Unison.Merge.AliceXorBob
  ( AliceXorBob (..),
    swap,
  )
where

-- | Alice exclusive-or Bob?
data AliceXorBob
  = Alice
  | Bob

swap :: AliceXorBob -> AliceXorBob
swap Alice = Bob
swap Bob = Alice
