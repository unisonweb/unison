-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Hashing.V2.ABT (Unison.ABT.Term, hash, hashComponents) where
import Unison.Prelude
import Unison.ABT

import Data.List hiding (cycle, find)
import Data.Vector ((!))
import Prelude hiding (abs, cycle)
import Unison.Hashing.V2.Tokenizable (Accumulate, Hashable1, hash1)
import qualified Unison.Hashing.V2.Tokenizable as Hashable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

-- Hash a strongly connected component and sort its definitions into a canonical order.
hashComponent ::
  (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Ord v, Ord h, Accumulate h)
  => Map.Map v (Term f v a) -> (h, [(v, Term f v a)])
hashComponent byName = let
  ts = Map.toList byName
  embeds = [ (v, void (transform Embed t)) | (v,t) <- ts ]
  vs = fst <$> ts
  tms = [ (v, absCycle vs (tm $ Component (snd <$> embeds) (var v))) | v <- vs ]
  hashed  = [ ((v,t), hash t) | (v,t) <- tms ]
  sortedHashed = sortOn snd hashed
  overallHash = Hashable.accumulate (Hashable.Hashed . snd <$> sortedHashed)
  in (overallHash, [ (v, t) | ((v, _),_) <- sortedHashed, Just t <- [Map.lookup v byName] ])

-- Group the definitions into strongly connected components and hash
-- each component. Substitute the hash of each component into subsequent
-- components (using the `termFromHash` function). Requires that the
-- overall component has no free variables.
hashComponents
  :: (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Var v, Ord h, Accumulate h)
  => (h -> Word64 -> Term f v ())
  -> Map.Map v (Term f v a)
  -> [(h, [(v, Term f v a)])]
hashComponents termFromHash termsByName = let
  bound = Set.fromList (Map.keys termsByName)
  escapedVars = Set.unions (freeVars <$> Map.elems termsByName) `Set.difference` bound
  sccs = components (Map.toList termsByName)
  go _ [] = []
  go prevHashes (component : rest) = let
    sub = substsInheritAnnotation (Map.toList prevHashes)
    (h, sortedComponent) = hashComponent $ Map.fromList [ (v, sub t) | (v, t) <- component ]
    curHashes = Map.fromList [ (v, termFromHash h i) | ((v, _),i) <- sortedComponent `zip` [0..]]
    newHashes = prevHashes `Map.union` curHashes
    newHashesL = Map.toList newHashes
    sortedComponent' = [ (v, substsInheritAnnotation newHashesL t) | (v, t) <- sortedComponent ]
    in (h, sortedComponent') : go newHashes rest
  in if Set.null escapedVars then go Map.empty sccs
     else error $ "can't hashComponents if bindings have free variables:\n  "
               ++ show (map show (Set.toList escapedVars))
               ++ "\n  " ++ show (map show (Map.keys termsByName))

-- Implementation detail of hashComponent
data Component f a = Component [a] a | Embed (f a) deriving (Functor, Traversable, Foldable)

instance (Hashable1 f, Functor f) => Hashable1 (Component f) where
  hash1 hashCycle hash c = case c of
    Component as a -> let
      (hs, hash) = hashCycle as
      toks = Hashable.Hashed <$> hs
      in Hashable.accumulate $ (Hashable.Tag 1 : toks) ++ [Hashable.Hashed (hash a)]
    Embed fa -> Hashable.hash1 hashCycle hash fa

-- | We ignore annotations in the `Term`, as these should never affect the
-- meaning of the term.
hash :: forall f v a h . (Functor f, Hashable1 f, Eq v, Show v, Ord h, Accumulate h)
     => Term f v a -> h
hash = hash' [] where
  hash' :: [Either [v] v] -> Term f v a -> h
  hash' env (Term _ _ t) = case t of
    Var v -> maybe die hashInt ind
      where lookup (Left cycle) = v `elem` cycle
            lookup (Right v') = v == v'
            ind = findIndex lookup env
            hashInt :: Int -> h
            hashInt i = Hashable.accumulate [Hashable.Nat $ fromIntegral i]
            die = error $ "unknown var in environment: " ++ show v
                        ++ " environment = " ++ show env
    Cycle (AbsN' vs t) -> hash' (Left vs : env) t
    -- Cycle t -> hash' env t
    Abs v t -> hash' (Right v : env) t
    Tm t -> Hashable.hash1 (hashCycle env) (hash' env) t

  hashCycle :: [Either [v] v] -> [Term f v a] -> ([h], Term f v a -> h)
  hashCycle env@(Left cycle : envTl) ts | length cycle == length ts =
    let
      permute p xs = case Vector.fromList xs of xs -> map (xs !) p
      hashed = map (\(i,t) -> ((i,t), hash' env t)) (zip [0..] ts)
      pt = fst <$> sortOn snd hashed
      (p,ts') = unzip pt
    in case map Right (permute p cycle) ++ envTl of
      env -> (map (hash' env) ts', hash' env)
  hashCycle env ts = (map (hash' env) ts, hash' env)
