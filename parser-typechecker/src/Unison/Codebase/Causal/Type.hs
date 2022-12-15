{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Causal.Type
  ( Causal (..),
    pattern One,
    pattern Cons,
    pattern Merge,
    before,
    predecessors,
    lca,
  )
where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import U.Codebase.HashTags (CausalHash)
import Unison.Hash (HashFor (..))
import Unison.Prelude
import Prelude hiding (head, read, tail)

{-
`Causal a` has 5 operations, specified algebraically here:

* `before : Causal m a -> Causal m a -> m Bool` defines a partial order on
            `Causal`.
* `head : Causal m a -> a`, which represents the "latest" `a` value in a causal
          chain.
* `one : a -> Causal m a`, satisfying `head (one hd) == hd`
* `cons : a -> Causal a -> Causal a`, satisfying `head (cons hd tl) == hd` and
          also `before tl (cons hd tl)`.
* `merge : CommutativeSemigroup a => Causal a -> Causal a -> Causal a`, which is
           commutative (but not associative) and satisfies:
  * `before c1 (merge c1 c2)`
  * `before c2 (merge c1 c2)`
* `sequence : Causal a -> Causal a -> Causal a`, which is defined as
              `sequence c1 c2 = cons (head c2) (merge c1 c2)`.
  * `before c1 (sequence c1 c2)`
  * `head (sequence c1 c2) == head c2`
-}

instance (Show e) => Show (Causal m e) where
  show = \case
    UnsafeOne h eh e -> "One " ++ (take 3 . show) h ++ " " ++ (take 3 . show) eh ++ " " ++ show e
    UnsafeCons h eh e t -> "Cons " ++ (take 3 . show) h ++ " " ++ (take 3 . show) eh ++ " " ++ show e ++ " " ++ (take 3 . show) (fst t)
    UnsafeMerge h eh e ts -> "Merge " ++ (take 3 . show) h ++ " " ++ (take 3 . show) eh ++ " " ++ show e ++ " " ++ (show . fmap (take 3 . show) . toList) (Map.keysSet ts)

-- h is the type of the pure data structure that will be hashed and used as
-- an index; e.g. h = Branch00, e = Branch0 m
data Causal m e
  = UnsafeOne
      { currentHash :: CausalHash,
        valueHash :: HashFor e,
        head :: e
      }
  | UnsafeCons
      { currentHash :: CausalHash,
        valueHash :: HashFor e,
        head :: e,
        tail :: (CausalHash, m (Causal m e))
      }
  | -- The merge operation `<>` flattens and normalizes for order
    UnsafeMerge
      { currentHash :: CausalHash,
        valueHash :: HashFor e,
        head :: e,
        tails :: Map CausalHash (m (Causal m e))
      }

pattern One :: CausalHash -> HashFor e -> e -> Causal m e
pattern One h eh e <- UnsafeOne h eh e

pattern Cons :: CausalHash -> HashFor e -> e -> (CausalHash, m (Causal m e)) -> Causal m e
pattern Cons h eh e tail <- UnsafeCons h eh e tail

pattern Merge :: CausalHash -> HashFor e -> e -> Map CausalHash (m (Causal m e)) -> Causal m e
pattern Merge h eh e tails <- UnsafeMerge h eh e tails

{-# COMPLETE One, Cons, Merge #-}

predecessors :: Causal m e -> Seq (m (Causal m e))
predecessors (UnsafeOne _ _ _) = Seq.empty
predecessors (UnsafeCons _ _ _ (_, t)) = Seq.singleton t
predecessors (UnsafeMerge _ _ _ ts) = Seq.fromList $ Map.elems ts

before :: Monad m => Causal m e -> Causal m e -> m Bool
before a b = (== Just a) <$> lca a b

-- Find the lowest common ancestor of two causals.
lca :: Monad m => Causal m e -> Causal m e -> m (Maybe (Causal m e))
lca a b =
  lca' (Seq.singleton $ pure a) (Seq.singleton $ pure b)

-- `lca' xs ys` finds the lowest common ancestor of any element of `xs` and any
-- element of `ys`.
-- This is a breadth-first search used in the implementation of `lca a b`.
lca' ::
  Monad m =>
  Seq (m (Causal m e)) ->
  Seq (m (Causal m e)) ->
  m (Maybe (Causal m e))
lca' = go Set.empty Set.empty
  where
    go seenLeft seenRight remainingLeft remainingRight =
      case Seq.viewl remainingLeft of
        Seq.EmptyL -> search seenLeft remainingRight
        a Seq.:< as -> do
          left <- a
          if Set.member (currentHash left) seenRight
            then pure $ Just left
            else -- Note: swapping position of left and right when we recurse so that
            -- we search each side equally. This avoids having to case on both
            -- arguments, and the order shouldn't really matter.

              go
                seenRight
                (Set.insert (currentHash left) seenLeft)
                remainingRight
                (as <> predecessors left)
    search seen remaining = case Seq.viewl remaining of
      Seq.EmptyL -> pure Nothing
      a Seq.:< as -> do
        current <- a
        if Set.member (currentHash current) seen
          then pure $ Just current
          else search seen (as <> predecessors current)

instance Eq (Causal m a) where
  a == b = currentHash a == currentHash b

instance Ord (Causal m a) where
  a <= b = currentHash a <= currentHash b
