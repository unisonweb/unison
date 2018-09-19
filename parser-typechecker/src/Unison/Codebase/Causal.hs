module Unison.Codebase.Causal where

import Prelude hiding (head, sequence)
import Data.List (sort)
import Unison.Hash (Hash)
import qualified Unison.Hashable as Hashable
import Unison.Hashable (Hashable)

data Causal e
  = One { currentHash :: Hash, head :: e }
  | Cons { currentHash :: Hash, head :: e, tail :: Causal e }
  | Merge { currentHash :: Hash, head :: e, tail1 :: Causal e, tail2 :: Causal e }

instance Semigroup e => Semigroup (Causal e) where
  a <> b
    | before a b = b
    | before b a = a
    | otherwise  = Merge (mixHashes [currentHash a, currentHash b])
                         (head a <> head b) a b

hash :: Hashable e => e -> Hash
hash = Hashable.accumulate'

-- commutative combine of a list of hashes
mixHashes :: [Hash] -> Hash
mixHashes = hash . sort

one :: Hashable e => e -> Causal e
one e = One (hash e) e

cons :: Hashable e => e -> Causal e -> Causal e
cons e tl = Cons (hash [hash e, currentHash tl]) e tl

sequence :: (Semigroup e, Hashable e) => Causal e -> Causal e -> Causal e
sequence a (One _ e) = cons e a
sequence a (Cons _ e tl) = cons e (sequence a tl)
sequence a (Merge _ _ l r) = sequence a l <> r
-- note: if causal had a `split` operation, we'd need to sequence on both sides

-- Does `h2` incorporate all of `h1`?
before :: Causal e -> Causal e -> Bool
before h1 h2 = go (currentHash h1) h2 where
  go h1 (One h _) = h == h1
  go h1 (Cons h _ tl) = h == h1 || go h1 tl
  go h1 (Merge h _ left right) = h == h1 || go h1 left || go h1 right
