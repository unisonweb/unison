module Unison.Merge.AliceIorBob
  ( AliceIorBob (..),
  )
where

-- | Alice inclusive-or Bob?
data AliceIorBob
  = OnlyAlice
  | OnlyBob
  | AliceAndBob
