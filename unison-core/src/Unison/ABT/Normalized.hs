{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ViewPatterns #-}
{-# language DeriveFunctor #-}
{-# language PatternGuards #-}
{-# language DeriveFoldable #-}
{-# language PatternSynonyms #-}
{-# language DeriveTraversable #-}

{-# language UndecidableInstances #-}
{-# language QuantifiedConstraints #-}

module Unison.ABT.Normalized
  ( ABT(..)
  , Term(.., TAbs, TTm, TAbss)
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
data ABT f v
  = Abs v (Term f v)
  | Tm (f v (Term f v))

data Term f v = Term
  { freeVars :: Set v
  , out :: ABT f v
  }

instance (forall a b. Show a => Show b => Show (f a b), Show v)
      => Show (ABT f v)
  where
  showsPrec p a = showParen (p >= 9) $ case a of
    Abs v tm
      -> showString "Abs " . showsPrec 10 v
       . showString " " . showsPrec 10 tm
    Tm e -> showString "Tm " . showsPrec 10 e

instance (forall a b. Show a => Show b => Show (f a b), Show v)
      => Show (Term f v)
  where
  showsPrec p (Term _ e)
    = showParen (p >= 9) $ showString "Term " . showsPrec 10 e

pattern TAbs :: Var v => v -> Term f v -> Term f v
pattern TAbs u bd <- Term _ (Abs u bd)
  where TAbs u bd = Term (Set.delete u (freeVars bd)) (Abs u bd)

pattern TTm :: (Var v, Bifoldable f) => f v (Term f v) -> Term f v
pattern TTm bd <- Term _ (Tm bd)
  where TTm bd = Term (bifoldMap Set.singleton freeVars bd) (Tm bd)

{-# complete TAbs, TTm #-}

unabss :: Var v => Term f v -> ([v], Term f v)
unabss (TAbs v (unabss -> (vs, bd))) = (v:vs, bd)
unabss bd = ([], bd)

pattern TAbss :: Var v => [v] -> Term f v -> Term f v
pattern TAbss vs bd <- (unabss -> (vs, bd))
  where TAbss vs bd = foldr TAbs bd vs

{-# complete TAbss #-}

-- Simultaneous variable renaming.
--
-- subvs0 counts the number of variables being renamed to a particular
-- variable
--
-- rnv0 is the variable renaming map.
renames
  :: (Var v, Ord v, Bifunctor f, Bifoldable f)
  => Map v Int -> Map v v -> Term f v -> Term f v
renames subvs0 rnv0 tm = case tm of
  TAbs u body
    | not $ Map.null rnv' -> TAbs u' (renames subvs' rnv' body)
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

  TTm body
    | not $ Map.null rnv
    -> TTm $ bimap (\u -> Map.findWithDefault u u rnv) (renames subvs rnv) body

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
  => v -> v -> Term f v -> Term f v
rename old new = renames (Map.singleton new 1) (Map.singleton old new)

transform
  :: (Var v, Bifunctor g, Bifoldable f, Bifoldable g)
  => (forall a b. f a b -> g a b)
  -> Term f v -> Term g v
transform phi (TTm    body) = TTm . second (transform phi) $ phi body
transform phi (TAbs u body) = TAbs u $ transform phi body
