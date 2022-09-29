{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.PatternMatchCoverage.Fix where

newtype Fix f = Fix {unFix :: f (Fix f)}

deriving instance (forall a. Show a => Show (f a)) => Show (Fix f)

deriving instance (forall a. Eq a => Eq (f a)) => Eq (Fix f)

deriving instance (Eq (Fix f), forall a. Ord a => Ord (f a)) => Ord (Fix f)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = let c = alg . fmap c . unFix in c

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para alg = let c = alg . fmap (\x -> (x, c x)) . unFix in c
