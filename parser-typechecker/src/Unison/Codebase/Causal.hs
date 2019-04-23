{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Unison.Codebase.Causal where

import Prelude hiding (head, sequence)
import Data.List (foldl1')
import Unison.Hash (Hash)
import qualified Unison.Hashable as Hashable
import Unison.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map

{-
`Causal a` has 5 operations, specified algebraically here:

* `before : Causal a -> Causal a -> Bool` defines a partial order on `Causal`.
* `head : Causal a -> a`, which represents the "latest" `a` value in a causal chain.
* `one : a -> Causal a`, satisfying `head (one hd) == hd`
* `cons : a -> Causal a -> Causal a`, satisfying `head (cons hd tl) == hd` and also `before tl (cons hd tl)`.
* `merge : CommutativeSemigroup a => Causal a -> Causal a -> Causal a`, which is associative and commutative and satisfies:
  * `before c1 (merge c1 c2)`
  * `before c2 (merge c1 c2)`
* `sequence : Causal a -> Causal a -> Causal a`, which is defined as `sequence c1 c2 = cons (head c2) (merge c1 c2)`.
  * `before c1 (sequence c1 c2)`
  * `head (sequence c1 c2) == head c2`
-}

--newtype Causal' e = Causal' { unCausal :: Cofree (Hash, Causal :+: (Map Hash :.: Causal)) e }
-- data Causal e = { head :: e, currentHash :: Hash, tail :: Either (Causal e) (Map Hash (Causal e)) }
data Causal e
  = One { currentHash :: Hash, head :: e }
  | Cons { currentHash :: Hash, head :: e, tail :: Causal e }
  -- The merge operation `<>` flattens and normalizes for order
  | Merge { currentHash :: Hash, head :: e, tails :: Map Hash (Causal e) }
  deriving (Show)

pattern ConsN conss tail <- (uncons -> Just (conss,tail))

consN :: [(Hash, e)] -> Causal e -> Causal e
consN conss tail = foldr (\(h,e) t -> Cons h e t) tail conss

uncons :: Causal e -> Maybe ([(Hash,e)], Causal e)
uncons (One _ _) = Nothing
uncons (Merge _ _ _) = Nothing
uncons x = Just $ go [] x where
  go acc (Cons h e tail) = go ((h,e) : acc) tail
  go acc x = (reverse acc,x)


instance Eq (Causal a) where
  a == b = currentHash a == currentHash b

instance Ord (Causal a) where
  a <= b = currentHash a <= currentHash b

merge :: Semigroup e => Causal e -> Causal e -> Causal e
a `merge` b | before a b = b
a `merge` b | before b a = a
Merge _ _ tls `merge` Merge _ _ tls2 =
  merge0 (Map.union tls tls2)
Merge _ _ tls `merge` b =
  merge0 $ Map.insert (currentHash b) b tls
b `merge` Merge _ _ tls =
  merge0 $ Map.insert (currentHash b) b tls
a `merge` b =
  merge0 $ Map.fromList [(currentHash a, a), (currentHash b, b)]

-- Does `h2` incorporate all of `h1`?
before :: Causal e -> Causal e -> Bool
before h1 h2 = go h1 h2 where
  -- stopping condition if both are equal
  go h1 h2 | h1 == h2 = True
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

instance Semigroup e => Semigroup (Causal e) where
  (<>) = merge

mergePreferRight :: (Hashable e, Semigroup e) => Causal e -> Causal e -> Causal e
mergePreferRight a b | before a b = b
                     | before b a = a
                     | otherwise = cons (head b) (a <> b)

-- implementation detail, form a `Merge`
merge0 :: Semigroup e => Map Hash (Causal e) -> Causal e
merge0 m = let
  e = if Map.null m then error "Causal.merge0 empty map"
      else foldl1' (<>) (head <$> Map.elems m)
  h = hash (Map.keys m) -- sorted order
  in Merge h e m

hash :: Hashable e => e -> Hash
hash = Hashable.accumulate'

step :: Hashable e => (e -> e) -> Causal e -> Causal e
step f c = f (head c) `cons` c

stepIf :: Hashable e => (e -> Bool) -> (e -> e) -> Causal e -> Causal e
stepIf cond f c = if (cond $ head c) then step f c else c

stepM :: (Functor m, Hashable e) => (e -> m e) -> Causal e -> m (Causal e)
stepM f c = (`cons` c) <$> f (head c)

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
    (_,c) : tls -> merge0 $ Map.fromList ((h',c') : tls)
      where c' = go a c
            h' = currentHash c'
    [] -> a -- shouldn't ever occur
