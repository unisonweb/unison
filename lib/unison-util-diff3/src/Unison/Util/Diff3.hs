module Unison.Util.Diff3
  ( Hunk (..),
    diff3,
  )
where

import Data.Algorithm.Diff qualified as Diff

data Hunk a
  = Hunk [a]
  | Conflict [a] [a] [a]
  deriving stock (Show)

-- | Produce a 3-way diff, given lca, alice, bob. The returned list of hunks may have consecutive non-conflict hunks,
-- but won't have consecutive conflicts.
diff3 :: forall a. (Eq a) => [a] -> [a] -> [a] -> [Hunk a]
diff3 lca alice bob =
  diff3_ (Diff.getDiff lca alice) (Diff.getDiff lca bob)

diff3_ :: (Eq a) => [Diff.Diff a] -> [Diff.Diff a] -> [Hunk a]
diff3_ =
  diff3__ [] [] []

-- The meat of the implementation. There are 3 accumulators:
--
--   os - the lca lines (only ultimately relevant if this is a conflict)
--   ps - alice's lines
--   qs - bob's lines
--
-- We walk stepwise through alice and bobs' two-way diffs with the lca, adding to our accumulators as appropriate, until
-- we reach a "sync point" (where alice and bob both left a line of the lca untouched).
--
-- When we reach a "sync point", we decide whether what we walked past was an unconflicted or conflicted hunk, and
-- proceed to process the rest of the input.
--
-- This algorithm is pretty good, and simple! It doesn't quite handle some cases you might care about, like:
--
--   lca   = A B
--   alice = X B
--   bob   = A Y
--
-- will be classified as a conflict, not as two independent edits. But these cases are relatively rare, arguably
-- conflicts anyway (due to the proximity of the edits), and aren't hard to resolve manually.
diff3__ :: (Eq a) => [a] -> [a] -> [a] -> [Diff.Diff a] -> [Diff.Diff a] -> [Hunk a]
diff3__ os ps qs dx dy =
  case (dx, dy) of
    -- New lines - add them to their respective accumulators
    (Inserted x : xs, ys) -> diff3__ os (x : ps) qs xs ys
    (xs, Inserted y : ys) -> diff3__ os ps (y : qs) xs ys
    -- Sync point! both sides left the same lca line untouched. process the accumulators and also emit a tiny one-line
    -- hunk for the sync point itself
    (Unchanged x : xs, Unchanged _ : ys) -> resolveHunk os ps qs ++ Hunk [x] : diff3_ xs ys
    -- one side deleted this line and the other left it alone - accumulate it in the lca and side that didn't delete
    (Unchanged x : xs, Deleted _ : ys) -> diff3__ (x : os) (x : ps) qs xs ys
    (Deleted _ : xs, Unchanged y : ys) -> diff3__ (y : os) ps (y : qs) xs ys
    -- both sides deleted this line, accumulate it only in the lca
    (Deleted x : xs, Deleted _ : ys) -> diff3__ (x : os) ps qs xs ys
    -- pretty boring cases, similar to above, but one side ran out of lines. just accumulate in the obvious way
    (Unchanged x : xs, []) -> diff3__ (x : os) (x : ps) qs xs []
    (Deleted x : xs, []) -> diff3__ (x : os) ps qs xs []
    ([], Unchanged y : ys) -> diff3__ (y : os) ps (y : qs) [] ys
    ([], Deleted y : ys) -> diff3__ (y : os) ps qs [] ys
    ([], []) -> resolveHunk os ps qs

-- Process a collection of lca/alice/bob lines as either a conflict (because alice and bob are both different, and
-- both different from the lca, too) or not a conflict (because either bob and alice are equal to each other, or one
-- is equal to the lca and thus we should take the other)
resolveHunk :: (Eq a) => [a] -> [a] -> [a] -> [Hunk a]
resolveHunk _ [] [] = []
resolveHunk os ps qs
  | ps == qs = [Hunk (reverse ps)]
  | os == ps = [Hunk (reverse qs)]
  | os == qs = [Hunk (reverse ps)]
  | otherwise = [Conflict (reverse os) (reverse ps) (reverse qs)]

-- Pattern synonyms for readability

pattern Unchanged :: a -> Diff.Diff a
pattern Unchanged x <- Diff.Both x _

pattern Deleted :: a -> Diff.Diff a
pattern Deleted x <- Diff.First x

pattern Inserted :: a -> Diff.Diff a
pattern Inserted x <- Diff.Second x

{-# COMPLETE Unchanged, Deleted, Inserted #-}
