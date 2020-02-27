{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language DeriveFunctor #-}
{-# language PatternGuards #-}
{-# language DeriveFoldable #-}
{-# language PatternSynonyms #-}
{-# language DeriveTraversable #-}

module Unison.ABT.Normalized
  ( ABT(..)
  , Term(.., TVar, TAbs, TTm)
  , renames
  , rename
  , transform
  )
  where

import Data.Bifunctor
import Data.Bifoldable
-- import Data.Bitraversable

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Unison.ABT (Var(..))

-- ABTs with support for 'normalized' structure where only variables
-- may occur at some positions. This is accomplished by passing the
-- variable type to the base functor.
data ABT f v r
  = Var v
  | Abs v r
  | Tm (f v r)
  deriving (Functor, Foldable, Traversable)

data Term f v a = Term
  { freeVars :: Set v
  , annotation :: a
  , out :: ABT f v (Term f v a)
  }

pattern TVar a v <- Term _ a (Var v)
  where TVar a v = Term (Set.singleton v) a (Var v)

pattern TAbs :: Var v => a -> v -> Term f v a -> Term f v a
pattern TAbs a u bd <- Term _ a (Abs u bd)
  where TAbs a u bd = Term (Set.delete u (freeVars bd)) a (Abs u bd)

pattern TTm :: (Var v, Bifoldable f) => a -> f v (Term f v a) -> Term f v a
pattern TTm a bd <- Term _ a (Tm bd)
  where TTm a bd = Term (bifoldMap Set.singleton freeVars bd) a (Tm bd)

{-# complete TVar, TAbs, TTm #-}

instance Functor (f v) => Functor (Term f v) where
  fmap f (Term fvs a bnd) = Term fvs (f a) $ fmap f <$> bnd

-- Simultaneous variable renaming.
--
-- subvs0 counts the number of variables being renamed to a particular
-- variable
--
-- rnv0 is the variable renaming map.
renames
  :: (Var v, Ord v, Bifunctor f, Bifoldable f)
  => Map v Int -> Map v v -> Term f v a -> Term f v a
renames subvs0 rnv0 tm = case tm of
  TVar ann v | Just u <- Map.lookup v rnv0 -> TVar ann u

  TAbs ann u body
    | not $ Map.null rnv' -> TAbs ann u' (renames subvs' rnv' body)
   where
   rnv' = Map.alter (const $ adjustment) u rnv
   -- if u is in the set of variables we're substituting in, it
   -- needs to be renamed to avoid capturing things.
   u' | u `Map.member` subvs = freshIn (fvs `Set.union` Map.keysSet subvs) u
      | otherwise = u

   -- if u needs to be renamed to avoid capturing subvs
   -- and u actually occurs in the body, then add it to
   -- the substitutions
   (adjustment, subvs')
     | u /= u' && u `Set.member` fvs = (Just u', Map.insertWith (+) u' 1 subvs)
     | otherwise = (Nothing, subvs)

  TTm ann body
    | not $ Map.null rnv
    -> TTm ann $ bimap (\u -> Map.findWithDefault u u rnv) (renames subvs rnv) body

  _ -> tm
 where
 fvs = freeVars tm

 -- throw out irrelevant renamings
 rnv = Map.restrictKeys rnv0 fvs

 -- decrement the variable usage counts for the renamings we threw away
 subvs = Map.foldl' decrement subvs0 $ Map.withoutKeys rnv0 fvs
 decrement sv v = Map.update drop v sv
 drop n | n <= 1 = Nothing
        | otherwise = Just (n-1)

rename
  :: (Var v, Ord v, Bifunctor f, Bifoldable f)
  => v -> v -> Term f v a -> Term f v a
rename old new = renames (Map.singleton new 1) (Map.singleton old new)

transform
  :: (Var v, Bifunctor g, Bifoldable f, Bifoldable g)
  => (forall a b. f a b -> g a b)
  -> Term f v a -> Term g v a
transform phi (TTm  ann   body) = TTm ann . second (transform phi) $ phi body
transform phi (TAbs ann u body) = TAbs ann u $ transform phi body
transform _   (TVar ann v)      = TVar ann v
