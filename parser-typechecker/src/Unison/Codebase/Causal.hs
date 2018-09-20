module Unison.Codebase.Causal where

import Prelude hiding (head, sequence)
import Data.List (foldl1')
import Unison.Hash (Hash)
import qualified Unison.Hashable as Hashable
import Unison.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map

data Causal e
  = One { currentHash :: Hash, head :: e }
  | Cons { currentHash :: Hash, head :: e, tail :: Causal e }
  -- The merge operation `<>` flattens and normalizes for order
  | Merge { currentHash :: Hash, head :: e, tails :: Map Hash (Causal e) }
  deriving Eq

instance Semigroup e => Semigroup (Causal e) where
  a <> b | before a b = b
  a <> b | before b a = a
  Merge _ e tls <> Merge _ _ tls2 = merge0 e (Map.union tls tls2)
  Merge _ e tls <> b = merge0 e $ Map.insertWith (\_ old -> old) (currentHash b) b tls
  b <> Merge _ e tls = merge0 e $ Map.insertWith (\_ old -> old) (currentHash b) b tls
  a <> b = merge0 (head a) $ Map.fromList [(currentHash a, a), (currentHash b, b)]

mergePreferRight :: (Hashable e, Semigroup e) => Causal e -> Causal e -> Causal e
mergePreferRight a b | before a b = b
                     | before b a = a
                     | otherwise = cons (head b) (a <> b)

-- implementation detail, form a `Merge`
merge0 :: Semigroup e => e -> Map Hash (Causal e) -> Causal e
merge0 e0 m = let
  e = if Map.null m then e0 else foldl1' (<>) (head <$> Map.elems m)
  h = hash (Map.keys m) -- sorted order
  in Merge h e m

hash :: Hashable e => e -> Hash
hash = Hashable.accumulate'

one :: Hashable e => e -> Causal e
one e = One (hash e) e

cons :: Hashable e => e -> Causal e -> Causal e
cons e tl = Cons (hash [hash e, currentHash tl]) e tl

rebase :: (Semigroup e, Hashable e) => Causal e -> Causal e -> Causal e
rebase a b | a `before` b = a
rebase a b = go a b where
  go a (One _ e) = cons e a
  go a (Cons _ e tl) = cons e (rebase a tl)
  go a (Merge _ _ tls) = case Map.toList tls of
    -- note: if causal had a `split` operation, we'd need to sequence on all branches
    (_,c) : tls -> merge0 (head c) $ Map.fromList ((h',c') : tls)
      where c' = go a c
            h' = currentHash c'
    [] -> a -- shouldn't ever occur

-- Does `h2` incorporate all of `h1`?
before :: Causal e -> Causal e -> Bool
before h1 h2 = go h1 h2 where
  -- stopping condition if both are equal
  go h1 h2 | currentHash h1 == currentHash h2 = True
  -- otherwise look through tails if they exist
  go _  (One _ _) = False
  go h1 (Cons _ _ tl) = go h1 tl
  go (Merge _ _ m1) (Merge _ _ m2)
    -- `m1` is a submap of `m2`
    | all (`Map.member` m2) (Map.keys m1) = True
  -- if not, see if `h1` is a subgraph of one of the tails
  go h1 (Merge _ _ tls) =
    Map.member (currentHash h1) tls || any (go h1) (Map.elems tls)
  -- Exponential algorithm of checking that all paths are present
  -- in `h2` isn't necessary because of how merges are flattened
  --go (Merge _ _ m1) h2@(Merge _ _ _)
  --  all (\h1 -> go h1 h2) (Map.elems m1)
