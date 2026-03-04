{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Util.Recursion
  ( Algebra,
    Steppable (..),
    Recursive (..),
    Corecursive (..),
    cataM,
    futu,
    para,
    Fix (..),
    Cofix (..),
    XNor (..),
    cycle,
    takeExactly,
  )
where

import Control.Arrow ((&&&))
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Comonad.Trans.Cofree (CofreeF)
import Control.Comonad.Trans.Cofree qualified as CofreeF
import Control.Monad (join, (<=<))
import Control.Monad.Trans.Free (Free, FreeF, free, runFree)
import Control.Monad.Trans.Free qualified as FreeF
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Sequence (Seq (Empty, (:<|)))
import Prelude hiding (cycle)

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

class Steppable t f | t -> f where
  project :: t -> f t
  default project :: (Recursive t f, Functor f) => t -> f t
  project = cata (fmap embed)
  embed :: f t -> t

class (Steppable t f) => Recursive t f where
  cata :: (Algebra f a) -> t -> a
  default cata :: (Functor f) => (f a -> a) -> t -> a
  cata φ = φ . fmap (cata φ) . project

cataM :: (Recursive t f, Traversable f, Monad m) => (f a -> m a) -> t -> m a
cataM φ = cata $ φ <=< sequenceA

para :: (Recursive t f, Functor f) => (f (t, a) -> a) -> t -> a
para φ = snd . cata (embed . fmap fst &&& φ)

newtype Fix f = Fix (f (Fix f))

deriving instance (forall a. (Show a) => Show (f a)) => Show (Fix f)

deriving instance (forall a. (Eq a) => Eq (f a)) => Eq (Fix f)

deriving instance (Eq (Fix f), forall a. (Ord a) => Ord (f a)) => Ord (Fix f)

instance (Functor f) => Steppable (Fix f) f where
  embed = Fix
  project (Fix f) = f

instance (Functor f) => Recursive (Fix f) f

class (Steppable t f) => Corecursive t f where
  ana :: (Coalgebra f a) -> a -> t
  default ana :: (Functor f) => (a -> f a) -> a -> t
  ana ψ = embed . fmap (ana ψ) . ψ

data Cofix f = Cofix (f (Cofix f))

instance (Functor f) => Steppable (Cofix f) f where
  embed = Cofix
  project (Cofix f) = f

instance (Functor f) => Corecursive (Cofix f) f

futu :: (Corecursive t f, Functor f) => (a -> f (Free f a)) -> a -> t
futu ψ =
  ana
    ( fmap join
        . cata
          ( \case
              FreeF.Pure a -> free . FreeF.Pure <$> a
              FreeF.Free ft -> free . FreeF.Free <$> ft
          )
        . fmap ψ
    )
    . pure

-- | Neither `cycle` in base encodes the proof that the result is infinite. The one on @[]@ isn’t even guaranteed to be
--   infinite – it could be empty.
cycle :: NonEmpty a -> (a, (Free ((,) a) (NonEmpty a)))
cycle ne@(h :| t) = (h, foldr (\a -> free . FreeF.Free . (a,)) (pure ne) t)

-- |
--
--  __NB__: `Cofree` from “free” is lazy, so this instance is technically partial.
instance (Functor f) => Steppable (Cofree f a) (CofreeF f a) where
  embed (a CofreeF.:< fco) = a :< fco
  project (a :< fco) = a CofreeF.:< fco

instance (Functor f) => Recursive (Cofree f a) (CofreeF f a)

instance (Functor f) => Corecursive (Cofree f a) (CofreeF f a)

instance (Functor f) => Steppable (Free f a) (FreeF f a) where
  embed = free
  project = runFree

-- |
--
--  __NB__: `Free` from “free” is lazy, so this instance is technically partial.
instance (Functor f) => Recursive (Free f a) (FreeF f a)

instance (Functor f) => Corecursive (Free f a) (FreeF f a)

-- | The pattern functor for sequences.
data XNor a b = Neither | Both a !b

instance Functor (XNor a) where
  fmap fn = \case
    Neither -> Neither
    Both a b -> Both a $ fn b

instance Steppable [a] (XNor a) where
  project = \case
    [] -> Neither
    a : b -> Both a b
  embed = \case
    Neither -> []
    Both a b -> a : b

-- |
--
--  __NB__: Lists are lazy, so this instance is technically partial.
instance Recursive [a] (XNor a)

instance Corecursive [a] (XNor a)

instance Steppable (Seq a) (XNor a) where
  project = \case
    Empty -> Neither
    a :<| b -> Both a b
  embed = \case
    Neither -> Empty
    Both a b -> a :<| b

instance Recursive (Seq a) (XNor a)

instance Steppable Word Maybe where
  project = \case
    0 -> Nothing
    n -> pure $ n - 1
  embed = maybe 0 (+ 1)

instance Recursive Word Maybe

takeNext :: (Steppable s ((,) a)) => Maybe (s -> [a]) -> s -> [a]
takeNext Nothing _ = []
takeNext (Just f) s = uncurry (:) $ f <$> project s

-- | Since this operates on infinite sequences, we can always take the requested number of elements.
takeExactly :: (Steppable s ((,) a)) => Word -> s -> [a]
takeExactly = cata takeNext
