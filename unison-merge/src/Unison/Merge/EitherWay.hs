module Unison.Merge.EitherWay
  ( EitherWay (..),
    swap,
    value,
  )
where

-- | Alice exclusive-or Bob?
data EitherWay a
  = Alice a
  | Bob a
  deriving stock (Eq, Functor, Show)

swap :: EitherWay a -> EitherWay a
swap = \case
  Alice x -> Bob x
  Bob x -> Alice x

value :: EitherWay a -> a
value = \case
  Alice x -> x
  Bob x -> x
