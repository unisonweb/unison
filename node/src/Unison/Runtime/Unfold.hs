{-# Language DeriveFunctor #-}
{-# Language ExistentialQuantification #-}
{-# Language TupleSections #-}

module Unison.Runtime.Unfold where

import Control.Applicative
import Control.Monad
import Data.Maybe

data Unfold a = forall s . Unfold s (s -> Step s a)
data Step s a = Done | Cons a s | Skip s deriving Functor

uncons :: Unfold a -> Maybe (a, Unfold a)
uncons (Unfold s f) = case f s of
  Done -> Nothing
  Cons a s -> Just (a, Unfold s f)
  Skip s -> uncons (Unfold s f)

toList :: Unfold a -> [a]
toList (Unfold s f) = case f s of
  Done -> []
  Cons a s -> a : toList (Unfold s f)
  Skip s -> toList (Unfold s f)

fromList :: [a] -> Unfold a
fromList a = Unfold a step where
  step [] = Done
  step (hd:tl) = Cons hd tl

instance Functor Unfold where
  fmap f (Unfold s g) = Unfold s (fmap f . g)

mapState :: (s -> s2) -> Step s a -> Step s2 a
mapState _ Done = Done
mapState f (Cons a s) = Cons a (f s)
mapState f (Skip s) = Skip (f s)

columns :: [Unfold a] -> [[a]]
columns []   = []
columns rows = case unzip (catMaybes $ map uncons rows) of
  ([], []) -> []
  (hds, rows) -> hds : columns rows

instance Applicative Unfold where
  pure = return
  (<*>) = ap

instance Alternative Unfold where
  empty = mzero
  (<|>) = mplus

instance Monad Unfold where
  return a = Unfold False (\b -> if b then Done else Cons a True)
  a >>= f = Unfold (f <$> a, Nothing) step where
    step (a,Nothing) = case uncons a of
      Nothing -> Done
      Just (s1, a) -> step (a, Just s1)
    step (a, Just s1) = case uncons s1 of
      Nothing -> Skip (a, Nothing)
      Just (b, s1) -> Cons b (a, Just s1)

instance MonadPlus Unfold where
  mzero = Unfold () (const Done)
  mplus (Unfold s1 f) (Unfold s2 g) = Unfold (Just s1, s2) step where
    step (Nothing, s2) = mapState (Nothing,) (g s2)
    step (Just s1, s2) = mapState (\s1 -> (Just s1,s2)) (f s1)

instance Monoid (Unfold a) where
  mempty = empty
  mappend = mplus

instance Eq a => Eq (Unfold a) where
  u == u2 = toList u == toList u2

instance Ord a => Ord (Unfold a) where
  u `compare` u2 = toList u `compare` toList u2

instance Show a => Show (Unfold a) where
  show = show . toList
