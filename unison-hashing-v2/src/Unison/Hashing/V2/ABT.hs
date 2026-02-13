-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V2.ABT
  ( Unison.ABT.Term,
    HashingWarning (..),
    crashOnHashingWarning,
    hash,
    hashComponents,
  )
where

import Control.Exception (throw)
import Data.List hiding (cycle, find, unzip)
import Data.List qualified as List (sort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ABT
import Unison.Hash (Hash)
import Unison.Hashing.V2.Tokenizable (Hashable1, hash1)
import Unison.Hashing.V2.Tokenizable qualified as Hashable
import Unison.Prelude
import Prelude hiding (abs, cycle, unzip)

data HashingWarning
  = -- | two or more component elements can not be completely ordered with respect to one another
    -- https://github.com/unisonweb/unison/issues/2787
    IncompleteElementOrderingError (NonEmpty (NonEmpty String {- Each list is a set of structurally equivalent component elements -}))
  deriving stock (Eq, Ord)
  deriving anyclass (Exception)

instance Show HashingWarning where
  show hf = reportBug "E253299" (renderHashingFailure hf)
    where
      renderHashingFailure :: HashingWarning -> String
      renderHashingFailure = \case
        IncompleteElementOrderingError equivalenceSets ->
          unlines
            [ "🐞",
              "",
              "Sorry, you've encountered a weird situation that we are aware of and are currently working on a fix for.",
              "I'll explain what happened and how you can work around it.",
              "",
              "The following cyclic definition sets could not be completely ordered:",
              toList equivalenceSets
                <&> ( \vs ->
                        "  * " <> intercalate ", " (toList vs)
                    )
                & unlines,
              "",
              "This happens when multiple definitions in a mutually recursive cycle have a very similar structure.",
              "",
              "You can work around this by restructuring them to be less similar, e.g. by adding a pure expression to distinguish them, like:",
              "_ = \"this is the foo definition\""
            ]

-- | Crash if hashing produced any warnings.
--
-- In the future we will hopefully prevent this error entirely.
crashOnHashingWarning :: (HasCallStack) => ([HashingWarning], a) -> a
crashOnHashingWarning = \case
  ([], a) -> a
  (hf : _, _) -> throw hf

-- Hash a strongly connected component and sort its definitions into a canonical order.
hashComponent ::
  forall a f v.
  (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Ord v) =>
  Map.Map v (Term f v a) ->
  ([HashingWarning], (Hash, [(v, Term f v a)]))
hashComponent byName = do
  let ts = Map.toList byName
  -- First, compute a canonical hash ordering of the component, as well as an environment in which we can hash
  -- individual names.
  (hashes, env) <- doHashCycle [] ts
  -- Construct a list of tokens that is shared by all members of the component. They are disambiguated only by their
  -- name that gets tumbled into the hash.
  let commonTokens :: [Hashable.Token]
      commonTokens = Hashable.Tag 1 : map Hashable.Hashed hashes
      -- Use a helper function that hashes a single term given its name, now that we have an environment in which we can
      -- look the name up, as well as the common tokens.
      hashName :: v -> Hash
      hashName v = Hashable.accumulate (commonTokens ++ [Hashable.Hashed (hash' env (var v :: Term f v ()))])
      (hashes', permutedTerms) =
        ts
          -- Pair each term with its hash
          & map (\t -> (hashName (fst t), t))
          -- Sort again to get the final canonical ordering
          & sortOn fst
          & unzip
      overallHash = Hashable.accumulate (map Hashable.Hashed hashes')
  pure (overallHash, permutedTerms)

-- Group the definitions into strongly connected components and hash
-- each component. Substitute the hash of each component into subsequent
-- components (using the `termFromHash` function). Requires that the
-- overall component has no free variables.
hashComponents ::
  forall f v a.
  (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Var v) =>
  (Hash -> Word64 -> Term f v ()) ->
  Map.Map v (Term f v a) ->
  ([HashingWarning], [(Hash, [(v, Term f v a)])])
hashComponents termFromHash termsByName = do
  let bound = Set.fromList (Map.keys termsByName)
      escapedVars = Set.unions (freeVars <$> Map.elems termsByName) `Set.difference` bound
      sccs = components (Map.toList termsByName)
      go :: Map v (Term f v ()) -> [[(v, Term f v a)]] -> ([HashingWarning], [(Hash, [(v, Term f v a)])])
      go _ [] = pure $ []
      go prevHashes (component : rest) = do
        let sub = substsInheritAnnotation (Map.toList prevHashes)
        (h, sortedComponent) <- hashComponent $ Map.fromList [(v, sub t) | (v, t) <- component]
        let curHashes = Map.fromList [(v, termFromHash h i) | ((v, _), i) <- sortedComponent `zip` [0 ..]]
            newHashes = prevHashes `Map.union` curHashes
            newHashesL = Map.toList newHashes
            sortedComponent' = [(v, substsInheritAnnotation newHashesL t) | (v, t) <- sortedComponent]
        sortedRest <- go newHashes rest
        pure $ ((h, sortedComponent') : sortedRest)
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
  forall f v a.
  (Functor f, Hashable1 f, Eq v, Show v) =>
  Term f v a ->
  Hash
hash = hash' []

hash' ::
  forall f v a.
  (Functor f, Hashable1 f, Eq v, Show v) =>
  [Either [v] v] ->
  Term f v a ->
  Hash
hash' env = \case
  Var' v -> maybe die hashInt ind
    where
      lookup (Left cycle) = v `elem` cycle
      lookup (Right v') = v == v'
      ind = findIndex lookup env
      hashInt :: Int -> Hash
      hashInt i = Hashable.accumulate [Hashable.Nat $ fromIntegral i]
      die =
        error $
          "unknown var in environment: "
            ++ show v
            ++ " environment = "
            ++ show env
  Cycle' vs t -> hash1 (hashCycle vs env) undefined t
  Abs'' v t -> hash' (Right v : env) t
  Tm' t -> hash1 (\ts -> (List.sort (map (hash' env) ts), hash' env)) (hash' env) t
  where
    hashCycle :: [v] -> [Either [v] v] -> [Term f v a] -> (([Hash], Term f v a -> Hash))
    hashCycle cycle env ts =
      -- We ignore incomplete element ordering warnings when calling in from hash';
      -- we don't want to error on that when hashing internal let-bindings.
      let (_warnings, (ts', env')) = doHashCycle env (zip cycle ts)
       in (ts', hash' env')

-- | @doHashCycle env terms@ hashes cycle @terms@ in environment @env@, and returns the canonical ordering of the hashes
-- of those terms, as well as an updated environment with each of the terms' bindings in the canonical ordering.
doHashCycle ::
  forall a f v.
  (Eq v, Functor f, Hashable1 f, Show v) =>
  [Either [v] v] ->
  [(v, Term f v a)] ->
  -- Hashing always succeeds even if it generates warnings.
  ([HashingWarning], ([Hash], [Either [v] v]))
doHashCycle env namedTerms = do
  -- Ensure that all of the hashes we use for ordering components are unique;
  -- if not, we have an incomplete ordering of the elements in the cycle.
  -- Report a warning if there are any structurally equivalent elements,
  -- the caller can choose what to do with the warning.
  for_ structurallyEquivalentElements \vs ->
    -- Accumulate errors using the tuple monad.
    ([IncompleteElementOrderingError (vs <&> (NEL.sort . fmap show) & NEL.sort)], ())
  pure $ (map (hash' newEnv) permutedTerms, newEnv)
  where
    names = map fst namedTerms
    -- The environment in which we compute the canonical permutation of terms
    permutationEnv = Left names : env
    namedHashes :: [(v, Hash)]
    namedHashes = second (hash' permutationEnv) <$> namedTerms
    hashes :: [Hash]
    hashes = snd <$> namedHashes
    (permutedNames, permutedTerms) =
      zip namedTerms hashes
        & sortOn snd
        & fmap fst
        & unzip
    -- The new environment, which includes the names of all of the terms in the cycle, now that we have computed their
    -- canonical ordering
    newEnv = map Right permutedNames ++ env
    structurallyEquivalentElements :: Maybe (NonEmpty (NonEmpty v))
    structurallyEquivalentElements =
      namedHashes
        <&> (\(v, h) -> (h, [v]))
        & Map.fromListWith (<>)
        & mapMaybe (\xs -> guard (length xs > 1) *> NEL.nonEmpty xs)
        & Map.elems
        & NEL.nonEmpty
