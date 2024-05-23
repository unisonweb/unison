module Unison.Merge.EitherWayI
  ( EitherWayI (..),
    includingAlice,
    excludingAlice,
    value,
  )
where

-- | Alice inclusive-or Bob?
data EitherWayI a
  = OnlyAlice a
  | OnlyBob a
  | AliceAndBob a
  deriving stock (Functor, Show)

includingAlice :: EitherWayI a -> Maybe a
includingAlice = \case
  OnlyAlice x -> Just x
  AliceAndBob x -> Just x
  OnlyBob _ -> Nothing

excludingAlice :: EitherWayI a -> Maybe a
excludingAlice = \case
  OnlyBob x -> Just x
  OnlyAlice _ -> Nothing
  AliceAndBob _ -> Nothing

value :: EitherWayI a -> a
value = \case
  OnlyAlice x -> x
  OnlyBob x -> x
  AliceAndBob x -> x
