{-# LANGUAGE DefaultSignatures #-}

module Unison.Util.Tritraversable where

import Control.Applicative
import Data.Functor.Identity

class Trifoldable t where
  trifold :: Monoid r => (a -> r) -> (b -> r) -> (c -> r) -> t a b c -> r
  default trifold :: (Tritraversable t, Monoid r) => (a -> r) -> (b -> r) -> (c -> r) -> t a b c -> r
  trifold f g h = getConst . tritraverse (Const . f) (Const . g) (Const . h)

class Trifunctor t where
  trimap :: (a -> a') -> (b -> b') -> (c -> c') -> t a b c -> t a' b' c'
  default trimap :: Tritraversable t => (a -> a') -> (b -> b') -> (c -> c') -> t a b c -> t a' b' c'
  trimap f g h = runIdentity . tritraverse (Identity . f) (Identity . g) (Identity . h)

class (Trifoldable t, Trifunctor t) => Tritraversable t where
  tritraverse :: Applicative f => (a -> f a') -> (b -> f b') -> (c -> f c') -> t a b c -> f (t a' b' c')
