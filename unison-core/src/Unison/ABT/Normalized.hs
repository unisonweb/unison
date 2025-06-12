{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.ABT.Normalized
  ( ABT (..),
    Term (.., TAbs, TTm, TAbss),
    Align (..),
    alpha,
    freshen,
    Renaming (..),
    isEmptyRenaming,
    freshenBinder,
    pruneRenaming,
    mapping,
    avoiding,
    mappingAndAvoiding,
    renames,
    renamesAvoiding,
    renamesAndFreshen0,
    rename,
    transform,
    visit,
    visitPure,
  )
where

import Data.Bifoldable
import Data.Bifunctor
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Unison.ABT (Var (..))

-- ABTs with support for 'normalized' structure where only variables
-- may occur at some positions. This is accomplished by passing the
-- variable type to the base functor.
data ABT f v
  = Abs v (Term f v)
  | Tm (f v (Term f v))

data Term f v = Term
  { freeVars :: Set v,
    out :: ABT f v
  }

instance
  (forall a b. (Show a) => (Show b) => Show (f a b), Show v) =>
  Show (ABT f v)
  where
  showsPrec p a = showParen (p >= 9) $ case a of
    Abs v tm ->
      showString "Abs "
        . showsPrec 10 v
        . showString " "
        . showsPrec 10 tm
    Tm e -> showString "Tm " . showsPrec 10 e

instance
  (forall a b. (Show a) => (Show b) => Show (f a b), Show v) =>
  Show (Term f v)
  where
  showsPrec p (Term _ e) =
    showParen (p >= 9) $ showString "Term " . showsPrec 10 e

instance
  (forall a b. (Eq a) => (Eq b) => Eq (f a b), Bifunctor f, Bifoldable f, Var v) =>
  Eq (ABT f v)
  where
  Abs v1 e1 == Abs v2 e2
    | v1 == v2 = e1 == e2
    | otherwise = e1 == rename v2 v1 e2
  Tm e1 == Tm e2 = e1 == e2
  _ == _ = False

instance
  (forall a b. (Eq a) => (Eq b) => Eq (f a b), Bifunctor f, Bifoldable f, Var v) =>
  Eq (Term f v)
  where
  Term _ abt1 == Term _ abt2 = abt1 == abt2

pattern TAbs :: (Var v) => v -> Term f v -> Term f v
pattern TAbs u bd <-
  Term _ (Abs u bd)
  where
    TAbs u bd = Term (Set.delete u (freeVars bd)) (Abs u bd)

pattern TTm :: (Var v, Bifoldable f) => f v (Term f v) -> Term f v
pattern TTm bd <-
  Term _ (Tm bd)
  where
    TTm bd = Term (bifoldMap Set.singleton freeVars bd) (Tm bd)

{-# COMPLETE TAbs, TTm #-}

class (Bifoldable f, Bifunctor f) => Align f where
  align ::
    (Applicative g) =>
    (vl -> vr -> g vs) ->
    (el -> er -> g es) ->
    f vl el ->
    f vr er ->
    Maybe (g (f vs es))

alphaErr ::
  (Align f) => (Var v) => Map v v -> Term f v -> Term f v -> Either (Term f v, Term f v) a
alphaErr un tml tmr = Left (tml, renamesAndFreshen0 rn tmr)
  where
    rn = mapping un

-- Checks if two terms are equal up to a given variable renaming. The
-- renaming should map variables in the right hand term to the
-- equivalent variable in the left hand term.
alpha :: (Align f) => (Var v) => Map v v -> Term f v -> Term f v -> Either (Term f v, Term f v) ()
alpha un (TAbs u tml) (TAbs v tmr) =
  alpha (Map.insert v u (Map.filter (/= u) un)) tml tmr
alpha un tml@(TTm bdl) tmr@(TTm bdr)
  | Just sub <- align av (alpha un) bdl bdr = () <$ sub
  where
    av u v
      | maybe False (== u) (Map.lookup v un) = pure ()
      | otherwise = alphaErr un tml tmr
alpha un tml tmr = alphaErr un tml tmr

unabss :: (Var v) => Term f v -> ([v], Term f v)
unabss (TAbs v (unabss -> (vs, bd))) = (v : vs, bd)
unabss bd = ([], bd)

pattern TAbss :: (Var v) => [v] -> Term f v -> Term f v
pattern TAbss vs bd <-
  (unabss -> (vs, bd))
  where
    TAbss vs bd = foldr TAbs bd vs

{-# COMPLETE TAbss #-}

-- Renaming data.
--
-- `renames` contains an actual mapping from old variables to new
-- variables, which is used for the actual substitution.
--
-- `conflicts` stores information about which variables should be
-- avoided. The value at a variable `u` should _at least_ be the
-- number of variables in `renames` that are being substituted _to_
-- `u`, because substituting under a binder for `u` will capture those
-- substitutions. However, `conflicts` can be bootstrapped with extra
-- counts to avoid ambient variables as well, so that it is possible
-- to substitute/rewrite expressions without introducing variable
-- captures.
data Renaming v = RN
  { conflicts :: Map v Int,
    renamings :: Map v v
  }

-- Creates a Renaming that will avoid the given set of variables.
avoiding :: Set v -> Renaming v
avoiding avoid = RN (Map.fromSet (const 1) avoid) Map.empty

mapping :: Var v => Map v v -> Renaming v
mapping rn = RN cf rn
  where
    cf = Map.fromListWith (+) . fmap (,1) $ Map.elems rn

mappingAndAvoiding :: Var v => Map v v -> Set v -> Renaming v
mappingAndAvoiding rn avoid = RN cf rn
  where
    cf = Map.unionWith (+)
          (Map.fromSet (const 1) avoid)
          (Map.fromListWith (+) . fmap (,1) $ Map.elems rn)

-- Adjusts a renaming with respect to a remaining set of free
-- variables. Unnecessary renamings are discarded.
pruneRenaming :: Var v => Set v -> Renaming v -> Renaming v
pruneRenaming fvs (RN cf rn) = RN
  { renamings = Map.restrictKeys rn fvs,
    conflicts = Map.foldl' decrement cf $ Map.withoutKeys rn fvs
  }
  where
    decrement sv v = Map.update drop v sv
    drop n
      | n <= 1 = Nothing
      | otherwise = Just (n - 1)

-- Tests if the renaming is empty in the sense that it will never
-- cause bound variables to be renamed. This is _not_ just a test of
-- whether the substitutions are empty, because the conflicts can
-- cause variables to need freshening even without variable
-- substitutions.
isEmptyRenaming :: Renaming v -> Bool
isEmptyRenaming = null . conflicts

-- Freshens a bound variable with regard to a renaming, yielding the
-- fresh variable and a renaming appropriate for the term within the
-- binder. The `Set` should be the free variables of the expression
-- within the binder, for proper freshening.
freshenBinder :: Var v => v -> Set v -> Renaming v -> (v, Renaming v)
freshenBinder u fvs rn0@(RN cf rn) = (u', rn')
  where
    -- if u conflicts with the renaming, freshen it
    u' | u `Map.member` cf = freshIn (fvs `Set.union` Map.keysSet cf) u
       | otherwise = u

    -- if u needs to be renamed, and it actually occurs in the body,
    -- add it to the Renaming.
    rn'
      | u /= u' && u `Set.member` fvs =
          RN { conflicts = Map.insertWith (+) u' 1 cf,
               renamings = Map.alter (const $ Just u') u rn
             }
      | otherwise = rn0

-- Simultaneous variable renaming and freshening implementation.
--
-- subvs0 is a count of the number of conflicts associated with a
-- variable. There are two sources of conflicts.
--
--   1. A variable is being renamed _to_ the given variable
--   2. We want to avoid capturing the variable for other reasons
--
-- So, if you initially call `renamesAndFreshen0` with a higher count
-- for `v` then there are variables being renamed to `v`, all bound
-- occurrences of `v` will be freshened regardless of whether any
-- actual renamings are left.
--
-- rnv0 is the variable renaming map.
renamesAndFreshen0 ::
  (Var v, Bifunctor f, Bifoldable f) =>
  Renaming v ->
  Term f v ->
  Term f v
renamesAndFreshen0 rn0 tm = case tm of
  TAbs u body
    | (u', rn) <- freshenBinder u (freeVars body) rn,
      u /= u' || not (isEmptyRenaming rn) ->
        TAbs u' (renamesAndFreshen0 rn body)
  TTm body
    | not $ isEmptyRenaming rn ->
        TTm $ bimap lkup (renamesAndFreshen0 rn) body
  _ -> tm
  where
    fvs = freeVars tm

    rn = pruneRenaming fvs rn0

    lkup u = Map.findWithDefault u u $ renamings rn

-- Freshens the bound variables in a term to avoid capturing variables
-- in the set.
freshen ::
  (Var v, Bifunctor f, Bifoldable f) =>
  Set v ->
  Term f v ->
  Term f v
freshen avoid = renamesAndFreshen0 (avoiding avoid)

-- Renames some variables while also avoiding a given set of variables
-- for any bindings in the term.
renamesAvoiding ::
  (Var v, Bifunctor f, Bifoldable f) =>
  Set v ->
  Map v v ->
  Term f v ->
  Term f v
renamesAvoiding avoid rnv =
  renamesAndFreshen0 (mappingAndAvoiding rnv avoid)

-- Simultaneous variable renaming.
renames ::
  (Var v, Bifunctor f, Bifoldable f) =>
  Map v v ->
  Term f v ->
  Term f v
renames rnv tm = renamesAndFreshen0 (mapping rnv) tm

rename ::
  (Var v, Ord v, Bifunctor f, Bifoldable f) =>
  v ->
  v ->
  Term f v ->
  Term f v
rename old new = renamesAndFreshen0 (mapping $ Map.singleton old new)

transform ::
  (Var v, Bifunctor g, Bifoldable f, Bifoldable g) =>
  (forall a b. f a b -> g a b) ->
  Term f v ->
  Term g v
transform phi (TTm body) = TTm . second (transform phi) $ phi body
transform phi (TAbs u body) = TAbs u $ transform phi body

visit ::
  (Applicative g, Bifoldable f, Traversable (f v), Var v) =>
  (Term f v -> Maybe (g (Term f v))) ->
  Term f v ->
  g (Term f v)
visit h t = flip fromMaybe (h t) $ case out t of
  Abs x e -> TAbs x <$> visit h e
  Tm body -> TTm <$> traverse (visit h) body

visitPure ::
  (Bifoldable f, Traversable (f v), Var v) =>
  (Term f v -> Maybe (Term f v)) ->
  Term f v ->
  Term f v
visitPure h = runIdentity . visit (fmap pure . h)
