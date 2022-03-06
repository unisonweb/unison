-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V2.ABT (Unison.ABT.Term, hash, hashComponents) where

import Data.List hiding (cycle, find)
import qualified Data.List as List (sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Unison.ABT
import Unison.Hashing.V2.Tokenizable (Accumulate, Hashable1, hash1)
import qualified Unison.Hashing.V2.Tokenizable as Hashable
import Unison.Prelude
import Prelude hiding (abs, cycle)

-- Hash a strongly connected component and sort its definitions into a canonical order.
hashComponent ::
  forall a f h v.
  (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Ord v, Ord h, Accumulate h) =>
  Map.Map v (Term f v a) ->
  (h, [(v, Term f v a)])
hashComponent byName =
  let ts = Map.toList byName
      -- First, compute a canonical hash ordering of the component, as well as an environment in which we can hash
      -- individual names.
      (hashes, env) = doHashCycle [] ts
      -- Construct a list of tokens that is shared by all members of the component. They are disambiguated only by their
      -- name that gets tumbled into the hash.
      commonTokens :: [Hashable.Token h]
      commonTokens = Hashable.Tag 1 : map Hashable.Hashed hashes
      -- Use a helper function that hashes a single term given its name, now that we have an environment in which we can
      -- look the name up, as well as the common tokens.
      hashName :: v -> h
      hashName v = Hashable.accumulate (commonTokens ++ [Hashable.Hashed (hash' env (var v :: Term f v ()))])
      (hashes', permutedTerms) =
        ts
          -- Pair each term with its hash
          & map (\t -> (hashName (fst t), t))
          -- Sort again to get the final canonical ordering
          & sortOn fst
          & unzip
      overallHash = Hashable.accumulate (map Hashable.Hashed hashes')
   in (overallHash, permutedTerms)

-- Group the definitions into strongly connected components and hash
-- each component. Substitute the hash of each component into subsequent
-- components (using the `termFromHash` function). Requires that the
-- overall component has no free variables.
hashComponents ::
  (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Var v, Ord h, Accumulate h) =>
  (h -> Word64 -> Term f v ()) ->
  Map.Map v (Term f v a) ->
  [(h, [(v, Term f v a)])]
hashComponents termFromHash termsByName =
  let bound = Set.fromList (Map.keys termsByName)
      escapedVars = Set.unions (freeVars <$> Map.elems termsByName) `Set.difference` bound
      sccs = components (Map.toList termsByName)
      go _ [] = []
      go prevHashes (component : rest) =
        let sub = substsInheritAnnotation (Map.toList prevHashes)
            (h, sortedComponent) = hashComponent $ Map.fromList [(v, sub t) | (v, t) <- component]
            curHashes = Map.fromList [(v, termFromHash h i) | ((v, _), i) <- sortedComponent `zip` [0 ..]]
            newHashes = prevHashes `Map.union` curHashes
            newHashesL = Map.toList newHashes
            sortedComponent' = [(v, substsInheritAnnotation newHashesL t) | (v, t) <- sortedComponent]
         in (h, sortedComponent') : go newHashes rest
   in if Set.null escapedVars
        then go Map.empty sccs
        else
          error $
            "can't hashComponents if bindings have free variables:\n  "
              ++ show (map show (Set.toList escapedVars))
              ++ "\n  "
              ++ show (map show (Map.keys termsByName))

-- | We ignore annotations in the `Term`, as these should never affect the
-- meaning of the term.
hash ::
  forall f v a h.
  (Functor f, Hashable1 f, Eq v, Show v, Ord h, Accumulate h) =>
  Term f v a ->
  h
hash = hash' [] where

hash' ::
  forall f v a h.
  (Functor f, Hashable1 f, Eq v, Show v, Ord h, Accumulate h) =>
  [Either [v] v] ->
  Term f v a ->
  h
hash' env = \case
  Var' v -> maybe die hashInt ind
    where
      lookup (Left cycle) = v `elem` cycle
      lookup (Right v') = v == v'
      ind = findIndex lookup env
      hashInt :: Int -> h
      hashInt i = Hashable.accumulate [Hashable.Nat $ fromIntegral i]
      die =
        error $
          "unknown var in environment: " ++ show v
            ++ " environment = "
            ++ show env
  Cycle' vs t -> hash1 (hashCycle vs env) undefined t
  Abs'' v t -> hash' (Right v : env) t
  Tm' t -> hash1 (\ts -> (List.sort (map (hash' env) ts), hash' env)) (hash' env) t
  where
    hashCycle :: [v] -> [Either [v] v] -> [Term f v a] -> ([h], Term f v a -> h)
    hashCycle cycle env ts =
      let (ts', env') = doHashCycle env (zip cycle ts)
       in (ts', hash' env')

-- | @doHashCycle env terms@ hashes cycle @terms@ in environment @env@, and returns the canonical ordering of the hashes
-- of those terms, as well as an updated environment with each of the terms' bindings in the canonical ordering.
doHashCycle ::
  forall a f h v.
  (Accumulate h, Eq v, Functor f, Hashable1 f, Ord h, Show v) =>
  [Either [v] v] ->
  [(v, Term f v a)] ->
  ([h], [Either [v] v])
doHashCycle env namedTerms =
  (map (hash' newEnv) permutedTerms, newEnv)
  where
    names = map fst namedTerms
    -- The environment in which we compute the canonical permutation of terms
    permutationEnv = Left names : env
    (permutedNames, permutedTerms) =
      namedTerms
        & sortOn @h (hash' permutationEnv . snd)
        & unzip
    -- The new environment, which includes the names of all of the terms in the cycle, now that we have computed their
    -- canonical ordering
    newEnv = map Right permutedNames ++ env
