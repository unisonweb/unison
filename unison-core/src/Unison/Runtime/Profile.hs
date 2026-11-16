{-# LANGUAGE ExistentialQuantification #-}

-- | What the runtime records when it profiles a program. The reports made
-- from it are in "Unison.Codebase.Runtime.Profile".
module Unison.Runtime.Profile
  ( ProfTrie (..),
    Profile (..),
    SomeProfile (..),
    ProfileSpec (..),
    emptyProfile,
    singlePath,
    addPath,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Unison.Reference

-- A `ProfTrie` maps non-empty paths to counts of occurrences of that
-- path. This allows tracking relative frequency of call stacks. The
-- path elements are parameterized to allow for more efficient `Map` keys
-- to be used during construction, and can be paired with a decoding to
-- `Reference` as below.
newtype ProfTrie k a = ProfT (Map k (a, ProfTrie k a))
  deriving (Functor)

-- A profile pairs the above arbitrary key based profile trie with a
-- decoding of the integers to references and a total sample count.
data Profile k
  = Prof !(Int, Int) !(ProfTrie k (Int, Int)) !(Map k Reference)

-- Abstracts over the exact key type used in a profile.
data SomeProfile = forall k. (Ord k) => SomeProf (Profile k)

data ProfileSpec = NoProf | MiniProf | FullProf String
  deriving (Eq, Ord, Show)

emptyProfile :: Profile k
emptyProfile = Prof zero (ProfT M.empty) M.empty

zero :: (Int, Int)
zero = (0, 0)

inc :: Bool -> (Int, Int) -> (Int, Int)
inc b (m, n) = pair (m + 1) (if b then n + 1 else n)
  where
    pair !x !y = (x, y)

-- Creates a singleton profile trie from a path.
singlePath ::
  (Ord k) =>
  Bool ->
  [k] ->
  ((Int, Int), ProfTrie k (Int, Int))
singlePath b [] = (inc b zero, ProfT M.empty)
singlePath b (i : is) =
  (zero,) . ProfT $! M.singleton i (singlePath b is)

addPath0 ::
  (Ord k) =>
  Bool ->
  [k] ->
  ((Int, Int), ProfTrie k (Int, Int)) ->
  ((Int, Int), ProfTrie k (Int, Int))
addPath0 b [] (t, p) = (,p) $! inc b t
addPath0 b (i : is) (m, ProfT p) = (m,) . ProfT $! M.alter f i p
  where
    f Nothing = Just $ singlePath b is
    f (Just q) = Just $ addPath0 b is q

-- Adds a path to a profile trie, incrementing the count for the given
-- path.
addPath ::
  (Ord k) =>
  Bool ->
  [k] ->
  ProfTrie k (Int, Int) ->
  ProfTrie k (Int, Int)
addPath _ [] p = p
addPath b (i : is) (ProfT m) = ProfT $ M.alter f i m
  where
    f Nothing = Just $ singlePath b is
    f (Just q) = Just $ addPath0 b is q
