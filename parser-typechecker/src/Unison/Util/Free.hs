{-# Language ExistentialQuantification #-}

module Unison.Util.Free where

import Control.Monad (ap, liftM, (>=>))

-- We would use another package for this if we knew of one.
-- Neither http://hackage.haskell.org/package/free
--     nor http://hackage.haskell.org/package/free-functors
--     nor http://hackage.haskell.org/package/freer
--     appear to have this.

data Free f a = Pure a | forall x . Bind (f x) (x -> Free f a)

eval :: f a -> Free f a
eval fa = Bind fa Pure

instance Functor (Free f) where
  fmap = liftM

instance Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Bind fx f >>= g = Bind fx (f >=> g)

instance Applicative (Free f) where
  pure = Pure
  (<*>) = ap
