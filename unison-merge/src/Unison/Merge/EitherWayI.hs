module Unison.Merge.EitherWayI
  ( EitherWayI (..),
    value,
  )
where

-- | Alice inclusive-or Bob?
data EitherWayI a
  = OnlyAlice a
  | OnlyBob a
  | AliceAndBob a
  deriving stock (Functor, Show)

value :: EitherWayI a -> a
value = \case
  OnlyAlice x -> x
  OnlyBob x -> x
  AliceAndBob x -> x
