{-# Language ExistentialQuantification #-}
{-# Language Rank2Types #-}

module Unison.Runtime.Free where

import Control.Monad

data Free f a
  = Pure a
  | forall x . Bind (f x) (x -> Free f a)

instance Monad (Free f) where
  return = Pure
  Bind x f >>= g = Bind x ((g =<<) . f)
  Pure x >>= f = f x

instance Functor (Free f) where
  fmap = liftM

instance Applicative (Free f) where
  pure = return
  (<*>) = ap

eval :: f a -> Free f a
eval a = Bind a pure

translate :: (forall a . f a -> g a) -> Free f a -> Free g a
translate _ (Pure a) = Pure a
translate u (Bind x f) = Bind (u x) (translate u . f)

interpret :: Monad f => Free f a -> f a
interpret (Bind x f) = x >>= (interpret . f)
interpret (Pure a) = return a
