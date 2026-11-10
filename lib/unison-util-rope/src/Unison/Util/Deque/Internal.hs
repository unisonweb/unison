{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}

-- | A strict finger tree.
--
-- * amortized O(1) 'cons', 'snoc', 'uncons', 'unsnoc' when a sequence is used
--   once; O(log n) in the worst case, which a program can hit repeatedly only
--   by going back to the same old version
-- * O(1) 'size'
-- * O(log n) 'lookup', 'append', 'take', 'drop'
-- * no laziness anywhere: every field is strict, nothing is a thunk
--
-- The shape is Hinze and Paterson's: a prefix digit, a middle that is a
-- sequence of nodes, a suffix digit.  The differences are that the middle is
-- strict, digits hold up to 'maxD' items, and nodes are mostly eight wide.
-- The one invariant is that both digits of a tree with a middle have an item
-- (so the ends, and what is near them, are found without going down).  A
-- digit that fills up sheds a node of eight into the middle, and a digit
-- that empties takes a node out of it.
--
-- Digits are short lists, so that adding or removing an item at an end
-- allocates one cell or none.  A node's children are in an array (or, for the
-- nodes of eight leaves, in the node itself): a node is built once and then
-- only read.
module Unison.Util.Deque.Internal
  ( -- * The type
    Seq,
    pattern Empty,
    pattern (:<|),
    pattern (:|>),

    -- * Construction
    empty,
    singleton,
    cons,
    snoc,
    append,
    (<|),
    (|>),
    (><),
    fromList,
    fromListN,
    unsafeFromListN,
    fromFunction,

    -- * Queries
    size,
    length,
    null,
    lookup,
    toList,

    -- * Taking apart
    uncons,
    unsnoc,
    take,
    drop,
    splitAt,

    -- * Whole-sequence transformations (these rebuild the sequence)
    sort,
    sortBy,
    unstableSort,
    unstableSortBy,
    intersperse,
    reverse,

    -- * For tests
    valid,

    -- * The parts 'Unison.Util.Rope' is built from: digits, nodes, and the

    -- middle of a tree, which is a sequence of nodes that knows how many
    -- leaves are under each
    Deque (..),
    SList (..),
    Node (..),
    Mid (..),
    Arr,
    takeS,
    dropS,
    appendS,
    lenS,
    revS,
    revOnto,
    listS,
    arr8,
    fromSListA,
    nodeSize,
    nodeArity,
    nodeAt,
    nodeTakeRev,
    nodeDropFwd,
    maxD,
    mk,
    tsize,
    tpc,
    tsc,
    sizeM,
    consM,
    snocM,
    unconsM,
    unsnocM,
    scanFwd,
    scanBwd,
    scanKids,
    takeM,
    dropM,
    appM,
    foldrM,
    foldrS,
    foldrRev,
    foldrNode,
    foldlM,
    foldlS,
    foldlRev,
    foldlNode,
    validM,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad (unless, when)
import Control.Monad.ST (ST)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.Foldable qualified as F
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..))
import Data.List qualified as L
import Data.Primitive.SmallArray
  ( SmallArray,
    SmallMutableArray,
    cloneSmallArray,
    indexSmallArray,
    newSmallArray,
    runSmallArray,
    sizeofSmallArray,
    writeSmallArray,
  )
import GHC.Exts qualified as Exts
import Prelude hiding (drop, length, lookup, null, reverse, splitAt, take)

------------------------------------------------------------------------
-- Strict lists: the digits

data SList a = SNil | SCons !a !(SList a)

takeS :: Int -> SList a -> SList a
takeS 0 _ = SNil
takeS k (SCons x r) = SCons x (takeS (k - 1) r)
takeS _ SNil = SNil

dropS :: Int -> SList a -> SList a
dropS 0 l = l
dropS k (SCons _ r) = dropS (k - 1) r
dropS _ SNil = SNil

appendS :: SList a -> SList a -> SList a
appendS SNil ys = ys
appendS (SCons x r) ys = SCons x (appendS r ys)

lenS :: SList a -> Int
lenS = go 0
  where
    go !n SNil = n
    go !n (SCons _ r) = go (n + 1) r

revS :: SList a -> SList a
revS l = revOnto l SNil

-- the first list reversed, then the second
revOnto :: SList a -> SList a -> SList a
revOnto SNil acc = acc
revOnto (SCons x r) acc = revOnto r (SCons x acc)

nthS :: Int -> SList a -> a
nthS k (SCons x r)
  | k == 0 = x
  | otherwise = nthS (k - 1) r
nthS _ SNil = error "Deque: index past the end of a digit"

listS :: SList a -> [a]
listS SNil = []
listS (SCons x r) = x : listS r

------------------------------------------------------------------------
-- Arrays: the children of most nodes.  Every element is evaluated before
-- it is stored.  They are filled with single writes, which compile to a
-- store each; the bulk copy operations are calls into the runtime that cost
-- more than they save at these sizes.

type Arr = SmallArray

lenA :: Arr a -> Int
lenA = sizeofSmallArray
{-# INLINE lenA #-}

ixA :: Arr a -> Int -> a
ixA = indexSmallArray
{-# INLINE ixA #-}

hole :: a
hole = error "Deque: an array element that was never written"
{-# NOINLINE hole #-}

arr8 :: a -> a -> a -> a -> a -> a -> a -> a -> Arr a
arr8 !a !b !c !d !e !f !g !h = runSmallArray do
  m <- newSmallArray 8 a
  writeSmallArray m 1 b
  writeSmallArray m 2 c
  writeSmallArray m 3 d
  writeSmallArray m 4 e
  writeSmallArray m 5 f
  writeSmallArray m 6 g
  writeSmallArray m 7 h
  pure m

-- n elements starting at off
sliceA :: Arr a -> Int -> Int -> Arr a
sliceA = cloneSmallArray
{-# INLINE sliceA #-}

-- items of a list go into slots i, i + 1, ...
writeUp :: SmallMutableArray s a -> Int -> SList a -> ST s ()
writeUp m !i (SCons x r) = writeSmallArray m i x >> writeUp m (i + 1) r
writeUp _ _ SNil = pure ()

-- items of a list go into slots i, i - 1, ...
writeDown :: SmallMutableArray s a -> Int -> SList a -> ST s ()
writeDown m !i (SCons x r) = writeSmallArray m i x >> writeDown m (i - 1) r
writeDown _ _ SNil = pure ()

-- the first n items of a list that has at least n
fromSListA :: Int -> SList a -> Arr a
fromSListA n l = runSmallArray do
  m <- newSmallArray n hole
  let go !i (SCons x r) | i < n = writeSmallArray m i x >> go (i + 1) r
      go _ _ = pure ()
  go 0 l
  pure m

-- A list that runs back to front, then two that run front to back, as one
-- array of c items: c is the three lengths added up, and the first list has
-- nb items.
gather :: Int -> Int -> SList a -> SList a -> SList a -> Arr a
gather c nb back mid front = runSmallArray do
  m <- newSmallArray c hole
  writeDown m (nb - 1) back
  writeUp m nb mid
  writeUp m (nb + lenS mid) front
  pure m

mapA :: (a -> b) -> Arr a -> Arr b
mapA f a = runSmallArray do
  m <- newSmallArray n hole
  let go !i
        | i >= n = pure ()
        | otherwise = case f (ixA a i) of !x -> writeSmallArray m i x >> go (i + 1)
  go 0
  pure m
  where
    !n = lenA a
{-# INLINE mapA #-}

foldrA :: (a -> r -> r) -> r -> Arr a -> r
foldrA f z a = go 0
  where
    !n = lenA a
    go i
      | i >= n = z
      | otherwise = f (ixA a i) (go (i + 1))

foldlA :: (r -> a -> r) -> r -> Arr a -> r
foldlA f z0 a = go 0 z0
  where
    !n = lenA a
    go !i !z
      | i >= n = z
      | otherwise = go (i + 1) (f z (ixA a i))

------------------------------------------------------------------------
-- Nodes.  A node made by a top-level digit that filled up has eight leaves.
-- Every other node has two to eight children in an array, and the number of
-- leaves under it: the nodes below the first level of nodes, and the nodes
-- that 'append' makes.

data Node a
  = N8 !a !a !a !a !a !a !a !a
  | NA !Int !(Arr a)

nodeSize :: Node a -> Int
nodeSize (N8 {}) = 8
nodeSize (NA n _) = n
{-# INLINE nodeSize #-}

nodeArity :: Node a -> Int
nodeArity (N8 {}) = 8
nodeArity (NA _ a) = lenA a
{-# INLINE nodeArity #-}

-- the k-th child (0-based)
nodeAt :: Int -> Node a -> a
nodeAt k (N8 a b c d e f g h) = case k of
  0 -> a
  1 -> b
  2 -> c
  3 -> d
  4 -> e
  5 -> f
  6 -> g
  _ -> h
nodeAt k (NA _ a) = ixA a k

-- the first k children, back to front
nodeTakeRev :: Int -> Node a -> SList a
nodeTakeRev k nd = go 0 SNil
  where
    go !i acc
      | i == k = acc
      | otherwise = go (i + 1) (SCons (nodeAt i nd) acc)

-- all but the first k children, front to back
nodeDropFwd :: Int -> Node a -> SList a
nodeDropFwd k nd = go k
  where
    !n = nodeArity nd
    go !i
      | i >= n = SNil
      | otherwise = SCons (nodeAt i nd) (go (i + 1))

sumA :: Arr (Node a) -> Int
sumA a = go 0 0
  where
    !n = lenA a
    go !i !s
      | i >= n = s
      | otherwise = go (i + 1) (s + nodeSize (ixA a i))

sumS :: SList (Node a) -> Int
sumS = go 0
  where
    go !n SNil = n
    go !n (SCons x r) = go (n + nodeSize x) r

-- the index of the node holding leaf number i, and the leaves before it
scanA :: Int -> Arr (Node a) -> (Int, Int)
scanA i a = go 0 0
  where
    go !q !sb =
      let !s = nodeSize (ixA a q)
       in if i < sb + s
            then ((q, sb))
            else go (q + 1) (sb + s)
{-# INLINE scanA #-}

------------------------------------------------------------------------
-- The tree.
--
-- A prefix runs front to back and a suffix back to front, so the item at
-- the outer end of either is the head of its list.
--
-- The first field of 'Deep' and 'MDeep' packs three numbers: the number of
-- leaves in the tree (bits 8 and up), the number of items in the suffix (bits
-- 4 to 7) and in the prefix (bits 0 to 3).  'MDeep' also has the number of
-- leaves under its prefix, which 'lookup' needs to step over the prefix
-- without reading it.
--
-- 'Mid' is the same thing as 'Deque' one level down, where the items are
-- nodes.

data Deque a
  = Nil
  | Deep !Int !(SList a) !(Mid a) !(SList a)

data Mid a
  = MNil
  | MDeep !Int !Int !(SList (Node a)) !(Mid (Node a)) !(SList (Node a))

-- the most items a digit holds; it must fit in four bits and be at least nine
maxD :: Int
maxD = 10

mk :: Int -> Int -> Int -> Int -- leaves, prefix count, suffix count
mk n p s = (n `unsafeShiftL` 8) .|. (s `unsafeShiftL` 4) .|. p
{-# INLINE mk #-}

tsize, tpc, tsc :: Int -> Int
tsize t = t `unsafeShiftR` 8
tpc t = t .&. 15
tsc t = (t `unsafeShiftR` 4) .&. 15
{-# INLINE tsize #-}
{-# INLINE tpc #-}
{-# INLINE tsc #-}

sizeM :: Mid a -> Int
sizeM MNil = 0
sizeM (MDeep t _ _ _ _) = tsize t
{-# INLINE sizeM #-}

empty :: Deque a
empty = Nil

singleton :: a -> Deque a
singleton x = Deep 0x101 (SCons x SNil) MNil SNil

size :: Deque a -> Int
size Nil = 0
size (Deep t _ _ _) = tsize t
{-# INLINE size #-}

------------------------------------------------------------------------
-- Adding at the ends.  The wrappers are INLINE and the paths that touch the
-- middle are not, so that a call site gets only the common case.

cons :: a -> Deque a -> Deque a
cons x Nil = Deep 0x101 (SCons x SNil) MNil SNil
cons x (Deep t pr m sf)
  | t .&. 15 < maxD = Deep (t + 0x101) (SCons x pr) m sf
  | otherwise = consFull x t pr m sf
{-# INLINE cons #-}

snoc :: Deque a -> a -> Deque a
snoc Nil x = Deep 0x110 SNil MNil (SCons x SNil)
snoc (Deep t pr m sf) x
  | t .&. 0xF0 < maxD * 16 = Deep (t + 0x110) pr m (SCons x sf)
  | otherwise = snocFull x t pr m sf
{-# INLINE snoc #-}

-- A full prefix keeps its two outermost items and sheds the other eight.
-- If the suffix is empty there is no middle yet, and the far half of the
-- prefix becomes the suffix.
consFull :: a -> Int -> SList a -> Mid a -> SList a -> Deque a
consFull x !t pr m sf
  | t .&. 0xF0 == 0 = Deep (mk (tsize t + 1) 6 5) (SCons x (takeS 5 pr)) MNil (revS (dropS 5 pr))
  | otherwise = case pr of
      SCons p1 (SCons p2 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h _))))))))) ->
        Deep (t + 0x100 - 7) (SCons x (SCons p1 (SCons p2 SNil))) (consM (N8 a b c d e f g h) m) sf
      _ -> error "Deque.cons: short prefix"
{-# NOINLINE consFull #-}

snocFull :: a -> Int -> SList a -> Mid a -> SList a -> Deque a
snocFull x !t pr m sf
  | t .&. 15 == 0 = Deep (mk (tsize t + 1) 5 6) (revS (dropS 5 sf)) MNil (SCons x (takeS 5 sf))
  | otherwise = case sf of
      SCons s1 (SCons s2 (SCons h (SCons g (SCons f (SCons e (SCons d (SCons c (SCons b (SCons a _))))))))) ->
        Deep (t + 0x100 - 0x70) pr (snocM m (N8 a b c d e f g h)) (SCons x (SCons s1 (SCons s2 SNil)))
      _ -> error "Deque.snoc: short suffix"
{-# NOINLINE snocFull #-}

consM :: Node a -> Mid a -> Mid a
consM !n MNil = let !s = nodeSize n in MDeep (mk s 1 0) s (SCons n SNil) MNil SNil
consM n (MDeep t ps pr m sf)
  | t .&. 15 < maxD = MDeep (t + (s `unsafeShiftL` 8) + 1) (ps + s) (SCons n pr) m sf
  | t .&. 0xF0 == 0 =
      let !kept = takeS 5 pr
       in MDeep (mk (tsize t + s) 6 5) (s + sumS kept) (SCons n kept) MNil (revS (dropS 5 pr))
  | otherwise = case pr of
      SCons p1 (SCons p2 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h _))))))))) ->
        let !keep = nodeSize p1 + nodeSize p2
         in MDeep
              (t + (s `unsafeShiftL` 8) - 7)
              (s + keep)
              (SCons n (SCons p1 (SCons p2 SNil)))
              (consM (NA (ps - keep) (arr8 a b c d e f g h)) m)
              sf
      _ -> error "Deque.consM: short prefix"
  where
    !s = nodeSize n

snocM :: Mid a -> Node a -> Mid a
snocM MNil !n = MDeep (mk (nodeSize n) 0 1) 0 SNil MNil (SCons n SNil)
snocM (MDeep t ps pr m sf) n
  | t .&. 0xF0 < maxD * 16 = MDeep (t + (s `unsafeShiftL` 8) + 0x10) ps pr m (SCons n sf)
  | t .&. 15 == 0 =
      let !pr' = revS (dropS 5 sf)
       in MDeep (mk (tsize t + s) 5 6) (sumS pr') pr' MNil (SCons n (takeS 5 sf))
  | otherwise = case sf of
      SCons s1 (SCons s2 (SCons h (SCons g (SCons f (SCons e (SCons d (SCons c (SCons b (SCons a _))))))))) ->
        let !shed = tsize t - ps - sizeM m - nodeSize s1 - nodeSize s2
         in MDeep
              (t + (s `unsafeShiftL` 8) - 0x70)
              ps
              pr
              (snocM m (NA shed (arr8 a b c d e f g h)))
              (SCons n (SCons s1 (SCons s2 SNil)))
      _ -> error "Deque.snocM: short suffix"
  where
    !s = nodeSize n

------------------------------------------------------------------------
-- Removing at the ends

uncons :: Deque a -> Maybe (a, Deque a)
uncons Nil = Nothing
uncons (Deep t pr m sf) = case pr of
  SCons x pr'@(SCons _ _) -> Just (x, Deep (t - 0x101) pr' m sf)
  SCons x SNil -> Just (x, unconsLast t m sf)
  SNil -> unconsSlow t sf
{-# INLINE uncons #-}

unsnoc :: Deque a -> Maybe (Deque a, a)
unsnoc Nil = Nothing
unsnoc (Deep t pr m sf) = case sf of
  SCons x sf'@(SCons _ _) -> Just (Deep (t - 0x110) pr m sf', x)
  SCons x SNil -> Just (unsnocLast t pr m, x)
  SNil -> unsnocSlow t pr
{-# INLINE unsnoc #-}

-- The prefix's only item is being removed: the first node of the middle, if
-- there is one, becomes the prefix.
unconsLast :: Int -> Mid a -> SList a -> Deque a
unconsLast !t m sf
  | t < 0x200 = Nil
  | otherwise = case m of
      MNil -> Deep (t - 0x101) SNil MNil sf
      _ -> case unconsM m of
        (nd, m') -> case nd of
          N8 a b c d e f g h ->
            Deep (t - 0x101 + 8) (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h SNil)))))))) m' sf
          _ -> Deep (t - 0x101 + nodeArity nd) (nodeDropFwd 0 nd) m' sf
{-# NOINLINE unconsLast #-}

unsnocLast :: Int -> SList a -> Mid a -> Deque a
unsnocLast !t pr m
  | t < 0x200 = Nil
  | otherwise = case m of
      MNil -> Deep (t - 0x110) pr MNil SNil
      _ -> case unsnocM m of
        (m', nd) -> case nd of
          N8 a b c d e f g h ->
            Deep (t - 0x110 + 0x80) pr m' (SCons h (SCons g (SCons f (SCons e (SCons d (SCons c (SCons b (SCons a SNil))))))))
          _ -> let !k = nodeArity nd in Deep (t - 0x110 + (k `unsafeShiftL` 4)) pr m' (nodeTakeRev k nd)
{-# NOINLINE unsnocLast #-}

-- The prefix is empty, so there is no middle and everything is in the
-- suffix: half of it moves over.
unconsSlow :: Int -> SList a -> Maybe (a, Deque a)
unconsSlow !t sf
  | sc == 1 = case sf of
      SCons x _ -> Just (x, Nil)
      SNil -> error "Deque.uncons: empty"
  | otherwise =
      let !q = (sc - 1) `div` 2
          !p = sc - 1 - q
       in case revS (dropS q sf) of
            SCons x pr -> Just (x, Deep (mk (sc - 1) p q) pr MNil (takeS q sf))
            SNil -> error "Deque.uncons: empty"
  where
    !sc = tsc t
{-# NOINLINE unconsSlow #-}

unsnocSlow :: Int -> SList a -> Maybe (Deque a, a)
unsnocSlow !t pr
  | pc == 1 = case pr of
      SCons x _ -> Just (Nil, x)
      SNil -> error "Deque.unsnoc: empty"
  | otherwise =
      let !p = (pc - 1) `div` 2
          !q = pc - 1 - p
       in case revS (dropS p pr) of
            SCons x sf -> Just (Deep (mk (pc - 1) p q) (takeS p pr) MNil sf, x)
            SNil -> error "Deque.unsnoc: empty"
  where
    !pc = tpc t
{-# NOINLINE unsnocSlow #-}

-- the first node and the rest; the argument is not empty
unconsM :: Mid a -> (Node a, Mid a)
unconsM MNil = error "Deque.unconsM: empty"
unconsM (MDeep t ps pr m sf) = case pr of
  SCons n SNil | MDeep {} <- m -> case unconsM m of
    (nn, m') ->
      (n, MDeep (t - (nodeSize n `unsafeShiftL` 8) - 1 + nodeArity nn) (nodeSize nn) (nodeDropFwd 0 nn) m' sf)
  SCons n pr' ->
    let !s = nodeSize n
        !t' = t - (s `unsafeShiftL` 8) - 1
     in if t' < 0x100 then (n, MNil) else (n, MDeep t' (ps - s) pr' m sf)
  SNil ->
    let !sc = tsc t
        !q = (sc - 1) `div` 2
        !p = sc - 1 - q
     in case revS (dropS q sf) of
          SCons n pr'
            | sc == 1 -> (n, MNil)
            | otherwise -> (n, MDeep (mk (tsize t - nodeSize n) p q) (sumS pr') pr' MNil (takeS q sf))
          SNil -> error "Deque.unconsM: empty"

unsnocM :: Mid a -> (Mid a, Node a)
unsnocM MNil = error "Deque.unsnocM: empty"
unsnocM (MDeep t ps pr m sf) = case sf of
  SCons n SNil | MDeep {} <- m -> case unsnocM m of
    (m', nn) ->
      let !k = nodeArity nn
       in (MDeep (t - (nodeSize n `unsafeShiftL` 8) - 0x10 + (k `unsafeShiftL` 4)) ps pr m' (nodeTakeRev k nn), n)
  SCons n sf' ->
    let !t' = t - (nodeSize n `unsafeShiftL` 8) - 0x10
     in if t' < 0x100 then (MNil, n) else (MDeep t' ps pr m sf', n)
  SNil ->
    let !pc = tpc t
        !p = (pc - 1) `div` 2
        !q = pc - 1 - p
     in case revS (dropS p pr) of
          SCons n sf'
            | pc == 1 -> (MNil, n)
            | otherwise ->
                let !pr' = takeS p pr
                 in (MDeep (mk (tsize t - nodeSize n) p q) (sumS pr') pr' MNil sf', n)
          SNil -> error "Deque.unsnocM: empty"

-- A tree whose suffix would be empty: the suffix gets the last node of the
-- middle, if there is one.  And the same for a prefix.
deepR :: Int -> Int -> SList a -> Mid a -> Deque a
deepR n pc pr MNil = Deep (mk n pc 0) pr MNil SNil
deepR n pc pr m = case unsnocM m of
  (m', nd) -> let !k = nodeArity nd in Deep (mk n pc k) pr m' (nodeTakeRev k nd)

deepL :: Int -> Int -> Mid a -> SList a -> Deque a
deepL n sc MNil sf = Deep (mk n 0 sc) SNil MNil sf
deepL n sc m sf = case unconsM m of
  (nd, m') -> Deep (mk n (nodeArity nd) sc) (nodeDropFwd 0 nd) m' sf

mdeepR :: Int -> Int -> Int -> SList (Node a) -> Mid (Node a) -> Mid a
mdeepR n pc ps pr MNil = MDeep (mk n pc 0) ps pr MNil SNil
mdeepR n pc ps pr m = case unsnocM m of
  (m', nn) -> let !k = nodeArity nn in MDeep (mk n pc k) ps pr m' (nodeTakeRev k nn)

mdeepL :: Int -> Int -> Mid (Node a) -> SList (Node a) -> Mid a
mdeepL n sc MNil sf = MDeep (mk n 0 sc) 0 SNil MNil sf
mdeepL n sc m sf = case unconsM m of
  (nn, m') -> MDeep (mk n (nodeArity nn) sc) (nodeSize nn) (nodeDropFwd 0 nn) m' sf

------------------------------------------------------------------------
-- Lookup.  Results flow upward in unboxed tuples: a level hands back the
-- node holding the leaf and the leaf's offset in it, and the level above
-- picks the child.

lookup :: Int -> Deque a -> Maybe a
lookup !_ Nil = Nothing
lookup i (Deep t pr m sf)
  | i < 0 || i >= n = Nothing
  | i < pc = let !r = nthS i pr in Just r
  | j < tsc t = let !r = nthS j sf in Just r
  | otherwise = case lookM 0 (i - pc) m of
      (nd, !off) -> let !r = nodeAt off nd in Just r
  where
    !n = tsize t
    !pc = tpc t
    !j = n - 1 - i

-- sh: a full child of this level's nodes has 2^sh leaves
lookM :: Int -> Int -> Mid a -> (Node a, Int)
lookM !_ !_ MNil = error "Deque.lookup: empty middle"
lookM sh i (MDeep t ps pr m sf)
  | i < ps = scanFwd i pr
  | i' < ms = case lookM (sh + 3) i' m of
      (nn, !off) -> case nn of
        NA s kids
          | s == 8 `unsafeShiftL` (sh + 3),
            lenA kids == 8 ->
              case off .&. ((1 `unsafeShiftL` (sh + 3)) - 1) of
                o -> (ixA kids (off `unsafeShiftR` (sh + 3)), o)
          | otherwise -> case scanA off kids of
              (!q, !sb) -> let !o = off - sb in (ixA kids q, o)
        N8 {} -> error "Deque.lookup: a node of leaves below the first level"
  | otherwise = scanBwd (tsize t - 1 - i) sf
  where
    !i' = i - ps
    !ms = sizeM m

scanFwd :: Int -> SList (Node a) -> (Node a, Int)
scanFwd !i (SCons n r)
  | i < s = (n, i)
  | otherwise = scanFwd (i - s) r
  where
    !s = nodeSize n
scanFwd _ SNil = error "Deque.lookup: past the end of a prefix"

-- r: the number of leaves after the one looked for
scanBwd :: Int -> SList (Node a) -> (Node a, Int)
scanBwd !r (SCons n rest)
  | r < s = let !i = s - 1 - r in (n, i)
  | otherwise = scanBwd (r - s) rest
  where
    !s = nodeSize n
scanBwd _ SNil = error "Deque.lookup: past the end of a suffix"

-- the index of the child holding leaf number i, and the leaves before it
scanKids :: Int -> Node (Node a) -> (Int, Int)
scanKids i (NA _ kids) = scanA i kids
scanKids _ (N8 {}) = error "Deque: a node of leaves below the first level"
{-# INLINE scanKids #-}

------------------------------------------------------------------------
-- take and drop.  The cut falls in a prefix, in a suffix, or in the middle;
-- in the middle, the level below hands back the node the cut falls in and
-- the part of itself that is wholly on the side being kept, and the kept
-- children of that node become this level's digit on the cut side.

take :: Int -> Deque a -> Deque a
take !_ Nil = Nil
take i d@(Deep t pr m sf)
  | i <= 0 = Nil
  | i >= n = d
  | i <= pc = Deep (mk i i 0) (takeS i pr) MNil SNil
  | i >= n - sc = let !k = i - (n - sc) in if k == 0 then deepR i pc pr m else Deep (mk i pc k) pr m (dropS (sc - k) sf)
  | otherwise = case takeM (i - pc) m of
      (m', nd, k) -> Deep (mk i pc k) pr m' (nodeTakeRev k nd)
  where
    !n = tsize t
    !pc = tpc t
    !sc = tsc t

drop :: Int -> Deque a -> Deque a
drop !_ Nil = Nil
drop i d@(Deep t pr m sf)
  | i <= 0 = d
  | i >= n = Nil
  | i >= n - sc = let !r = n - i in Deep (mk r 0 r) SNil MNil (takeS r sf)
  | i <= pc = if i == pc then deepL (n - i) sc m sf else Deep (mk (n - i) (pc - i) sc) (dropS i pr) m sf
  | otherwise = case dropM (i - pc) m of
      (nd, !k, m') ->
        Deep (mk (n - i) (nodeArity nd - k) sc) (nodeDropFwd k nd) m' sf
  where
    !n = tsize t
    !pc = tpc t
    !sc = tsc t

-- | @splitAt i d = (take i d, drop i d)@
splitAt :: Int -> Deque a -> (Deque a, Deque a)
splitAt i d = (take i d, drop i d)

-- The first j leaves, 0 < j <= size: the node holding the last of them, the
-- number of its leaves that are among them, and everything before that node.
takeM :: Int -> Mid a -> (Mid a, Node a, Int)
takeM !_ MNil = error "Deque.take: empty middle"
takeM j (MDeep t ps pr m sf)
  | j <= ps = goP 0 0 pr
  | j <= ps + ms = case takeM (j - ps) m of
      (m', nn, !k) -> case scanKids (k - 1) nn of
        -- the children of the node the level below cut in: those before the
        -- one holding the cut make the new suffix
        (!q, !sb) ->
          let !total = ps + sizeM m' + sb
           in case k - sb of
                !k'
                  | total == 0 -> (MNil, nodeAt q nn, k')
                  | q == 0 -> (mdeepR total pc ps pr m', nodeAt q nn, k')
                  | otherwise -> (MDeep (mk total pc q) ps pr m' (nodeTakeRev q nn), nodeAt q nn, k')
  | otherwise = goS (n - j) sc sf
  where
    !n = tsize t
    !pc = tpc t
    !sc = tsc t
    !ms = sizeM m
    -- q whole nodes of the prefix, with sb leaves, come before the cut
    goP !q !sb (SCons nd r)
      | j <= sb + nodeSize nd =
          case j - sb of
            k
              | q == 0 -> (MNil, nd, k)
              | otherwise -> (MDeep (mk sb q 0) sb (takeS q pr) MNil SNil, nd, k)
      | otherwise = goP (q + 1) (sb + nodeSize nd) r
    goP _ _ SNil = error "Deque.take: past the end of a prefix"
    -- r leaves are to go from the back; cnt nodes of the suffix are left
    goS !r !cnt (SCons nd rest)
      | r >= s = goS (r - s) (cnt - 1) rest
      | otherwise =
          let !keep = s - r
              !total = j - keep
           in case keep of
                k
                  | total == 0 -> (MNil, nd, k)
                  | cnt == 1 -> (mdeepR total pc ps pr m, nd, k)
                  | otherwise -> (MDeep (mk total pc (cnt - 1)) ps pr m rest, nd, k)
      where
        !s = nodeSize nd
    goS _ _ SNil = error "Deque.take: past the end of a suffix"

-- All but the first j leaves, 0 <= j < size: the node holding leaf j, the
-- number of its leaves to drop, and everything after that node.
dropM :: Int -> Mid a -> (Node a, Int, Mid a)
dropM !_ MNil = error "Deque.drop: empty middle"
dropM j (MDeep t ps pr m sf)
  | j < ps = goP j pc ps n pr
  | j < ps + ms = case dropM (j - ps) m of
      (nn, !k, m') -> case scanKids k nn of
        -- the children of the node the level below cut in: those after the
        -- one holding the cut make the new prefix
        (!q, !sb) ->
          let !nd = nodeAt q nn
              !c = nodeArity nn - q - 1
              !sa = nodeSize nn - sb - nodeSize nd
              !total = sa + sizeM m' + (n - ps - ms)
           in case k - sb of
                !k'
                  | total == 0 -> (nd, k', MNil)
                  | c == 0 -> (nd, k', mdeepL total sc m' sf)
                  | otherwise -> (nd, k', MDeep (mk total c sc) sa (nodeDropFwd (q + 1) nn) m' sf)
  | otherwise = goS (n - 1 - j) 0 0 sf
  where
    !n = tsize t
    !pc = tpc t
    !sc = tsc t
    !ms = sizeM m
    -- k leaves still to drop; the prefix has cnt nodes and psz leaves left,
    -- the tree tot leaves
    goP !k !cnt !psz !tot (SCons nd r)
      | k >= s = goP (k - s) (cnt - 1) (psz - s) (tot - s) r
      | otherwise =
          case k of
            k
              | tot == s -> (nd, k, MNil)
              | cnt == 1 -> (nd, k, mdeepL (tot - s) sc m sf)
              | otherwise -> (nd, k, MDeep (mk (tot - s) (cnt - 1) sc) (psz - s) r m sf)
      where
        !s = nodeSize nd
    goP _ _ _ _ SNil = error "Deque.drop: past the end of a prefix"
    -- r leaves come after leaf j; c nodes of the suffix, with sa leaves, are
    -- wholly after it
    goS !r !c !sa (SCons nd rest)
      | r >= s = goS (r - s) (c + 1) (sa + s) rest
      | otherwise =
          case s - 1 - r of
            !k
              | c == 0 -> (nd, k, MNil)
              | otherwise -> (nd, k, MDeep (mk sa 0 c) 0 SNil MNil (takeS c sf))
      where
        !s = nodeSize nd
    goS _ _ _ SNil = error "Deque.drop: past the end of a suffix"

------------------------------------------------------------------------
-- Append.  The digits that end up inside (the left side's suffix and the
-- right side's prefix) either join the outer digit of a side that has no
-- middle, when they fit there, or are packed into nodes and handed down
-- between the two middles.

append :: Deque a -> Deque a -> Deque a
append Nil b = b
append a Nil = a
append a@(Deep t1 pr1 m1 sf1) b@(Deep t2 pr2 m2 sf2)
  | MNil <- m1,
    tpc t1 + c <= maxD =
      Deep (mk n (tpc t1 + c) (tsc t2)) (appendS pr1 (revOnto sf1 pr2)) m2 sf2
  | MNil <- m2,
    c + tsc t2 <= maxD =
      Deep (mk n (tpc t1) (c + tsc t2)) pr1 m1 (appendS sf2 (revOnto pr2 sf1))
  | c >= 2,
    t1 .&. 15 /= 0,
    t2 .&. 0xF0 /= 0 =
      Deep (mk n (tpc t1) (tsc t2)) pr1 (appM m1 c (packLeaves c (tsc t1) sf1 pr2) m2) sf2
  -- what is left: a side with no middle whose outer digit is empty, or a
  -- single item inside
  | MNil <- m1 = consEach (revS pr1) (consEach sf1 b)
  | otherwise = snocEach (snocEach a pr2) (revS sf2)
  where
    !n = tsize t1 + tsize t2
    !c = tsc t1 + tpc t2

-- cons each item, in list order (so the list runs back to front)
consEach :: SList a -> Deque a -> Deque a
consEach SNil d = d
consEach (SCons x r) d = consEach r (cons x d)

snocEach :: Deque a -> SList a -> Deque a
snocEach d SNil = d
snocEach d (SCons x r) = snocEach (snoc d x) r

-- A node of the first k items of a list, which are leaves.
leafNode :: Int -> SList a -> Node a
leafNode 8 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h _)))))))) = N8 a b c d e f g h
leafNode k l = NA k (fromSListA k l)

-- The leaves between two middles, as nodes: a suffix (back to front, nb
-- items) and then a prefix, c items in all.  c is 0 or 2 to 2 * maxD.  The
-- nodes have eight leaves while that leaves none or at least two for the
-- next (nine make a five and a four).
packLeaves :: Int -> Int -> SList a -> SList a -> SList (Node a)
packLeaves c nb back front
  | c == 0 = SNil
  | c < 8 = SCons (NA c (gather c nb back SNil front)) SNil
  | otherwise = go c (revOnto back front)
  where
    go !left l
      | left == 0 = SNil
      | left <= 8 = SCons (leafNode left l) SNil
      | left == 9 = SCons (leafNode 5 l) (SCons (leafNode 4 (dropS 5 l)) SNil)
      | otherwise = SCons (leafNode 8 l) (go (left - 8) (dropS 8 l))

-- The same one level down: a suffix, the nodes handed down, a prefix; c
-- nodes in all, at least 2, with sz leaves under them.
packNodes :: Int -> Int -> Int -> SList (Node a) -> SList (Node a) -> SList (Node a) -> SList (Node (Node a))
packNodes c sz nb back mid front
  | c <= 8 = SCons (NA sz items) SNil
  | otherwise =
      -- two or three nodes of about the same number of children
      let !q = (c + 7) `unsafeShiftR` 3
          !k1 = (c + q - 1) `div` q
          !x = sliceA items 0 k1
          !sx = sumA x
       in if q == 2
            then SCons (NA sx x) (SCons (NA (sz - sx) (sliceA items k1 (c - k1))) SNil)
            else
              let !k2 = (c - k1 + 1) `div` 2
                  !y = sliceA items k1 k2
                  !sy = sumA y
               in SCons (NA sx x) (SCons (NA sy y) (SCons (NA (sz - sx - sy) (sliceA items (k1 + k2) (c - k1 - k2))) SNil))
  where
    !items = gather c nb back mid front

consEachM :: SList (Node a) -> Mid a -> Mid a
consEachM SNil d = d
consEachM (SCons x r) d = consEachM r (consM x d)

snocEachM :: Mid a -> SList (Node a) -> Mid a
snocEachM d SNil = d
snocEachM d (SCons x r) = snocEachM (snocM d x) r

-- The nodes between the two middles run front to back and hold sns leaves.
appM :: Mid a -> Int -> SList (Node a) -> Mid a -> Mid a
appM MNil _ ns b = consEachM (revS ns) b
appM a _ ns MNil = snocEachM a ns
appM a@(MDeep t1 ps1 pr1 m1 sf1) sns ns b@(MDeep t2 ps2 pr2 m2 sf2)
  | MNil <- m1,
    tpc t1 + c <= maxD =
      MDeep (mk n (tpc t1 + c) (tsc t2)) (tsize t1 + sns + ps2) (appendS pr1 (revOnto sf1 (appendS ns pr2))) m2 sf2
  | MNil <- m2,
    c + tsc t2 <= maxD =
      MDeep (mk n (tpc t1) (c + tsc t2)) ps1 pr1 m1 (appendS sf2 (revOnto pr2 (revOnto ns sf1)))
  | c >= 2,
    t1 .&. 15 /= 0,
    t2 .&. 0xF0 /= 0 =
      let !sz = (tsize t1 - ps1 - sizeM m1) + sns + ps2
       in MDeep (mk n (tpc t1) (tsc t2)) ps1 pr1 (appM m1 sz (packNodes c sz (tsc t1) sf1 ns pr2) m2) sf2
  | MNil <- m1 = consEachM (revS pr1) (consEachM sf1 (consEachM (revS ns) b))
  | otherwise = snocEachM (snocEachM (snocEachM a ns) pr2) (revS sf2)
  where
    !n = tsize t1 + sns + tsize t2
    !c = tsc t1 + lenS ns + tpc t2

------------------------------------------------------------------------
-- Lists

-- | The elements in order.  Lazy: the list is produced as it is consumed.
toList :: Deque a -> [a]
toList = foldrI (:) []

-- | O(n).  A fold, so that it fuses with a list that is being produced.
fromList :: [a] -> Deque a
fromList = L.foldl' snoc Nil
{-# INLINE fromList #-}

-- | 'fromList' for a list whose length is known.  O(n).  Exactly @n@
-- elements are taken; a shorter list is an error.  ('unsafeFromListN' is
-- the same without the check, for a caller that built the list to length.)
fromListN :: Int -> [a] -> Deque a
fromListN n xs
  | n <= 0 = Nil
  | L.length front < n = error ("Deque.fromListN: " ++ show n ++ " elements asked for, " ++ show (L.length front) ++ " given")
  | otherwise = unsafeFromListN n front
  where
    front = L.take n xs

-- | 'fromListN' for a list known to have exactly @n@ elements.  O(n).  Builds
-- the levels directly: each gets a prefix of two to nine items and a suffix
-- of five, and what is between them goes, in nodes of eight, to the level
-- below.  The count is trusted: with more elements than @n@ the suffix
-- holds them all while the packed count says five, and a later append
-- writes outside an array.
unsafeFromListN :: Int -> [a] -> Deque a
unsafeFromListN n xs
  | n <= 0 = Nil
  | n <= maxD =
      let !p = (n + 1) `div` 2
       in case takeL p xs of
            (pl, rest) -> Deep (mk n p (n - p)) pl MNil (revL rest)
  | otherwise =
      let !k = (n - 7) `div` 8
          !p = n - 5 - 8 * k
       in case takeL p xs of
            (pl, rest) -> case leafNodes k rest of
              (nodes, rest') -> Deep (mk n p 5) pl (buildMid 8 k nodes) (revL rest')

-- the first k elements, in order, and the rest (k is small)
takeL :: Int -> [a] -> (SList a, [a])
takeL 0 ys = (SNil, ys)
takeL k (y : ys) = case takeL (k - 1) ys of (l, r) -> (SCons y l, r)
takeL _ [] = (SNil, [])

revL :: [a] -> SList a
revL = go SNil
  where
    go acc [] = acc
    go acc (y : ys) = go (SCons y acc) ys

-- k nodes of eight elements each from the front of a list, in order
leafNodes :: Int -> [a] -> (SList (Node a), [a])
leafNodes = go SNil
  where
    go acc 0 ys = (revS acc, ys)
    go acc k (a : b : c : d : e : f : g : h : ys) = go (SCons (N8 a b c d e f g h) acc) (k - 1) ys
    go _ _ _ = error "Deque.fromList: the list is shorter than its length"

-- c nodes of s leaves each, in order
buildMid :: Int -> Int -> SList (Node a) -> Mid a
buildMid !s !c ns
  | c == 0 = MNil
  | c <= maxD =
      let !p = (c + 1) `div` 2
       in MDeep (mk (c * s) p (c - p)) (p * s) (takeS p ns) MNil (revS (dropS p ns))
  | otherwise =
      let !k = (c - 7) `div` 8
          !p = c - 5 - 8 * k
       in case innerNodes (8 * s) SNil k (dropS p ns) of
            (nodes, rest) -> MDeep (mk (c * s) p 5) (p * s) (takeS p ns) (buildMid (8 * s) k nodes) (revS rest)

innerNodes :: Int -> SList (Node (Node a)) -> Int -> SList (Node a) -> (SList (Node (Node a)), SList (Node a))
innerNodes !_ acc 0 ys = (revS acc, ys)
innerNodes s acc k (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h ys)))))))) =
  innerNodes s (SCons (NA s (arr8 a b c d e f g h)) acc) (k - 1) ys
innerNodes _ _ _ _ = error "Deque.fromList: short list of nodes"

-- | @fromFunction n f@ is @f 0, f 1, ..., f (n - 1)@.
fromFunction :: Int -> (Int -> a) -> Deque a
fromFunction n f
  | n < 0 = error "Deque.fromFunction called with negative len"
  | otherwise = unsafeFromListN n (L.map f [0 .. n - 1])

------------------------------------------------------------------------
-- Folds.  Each walks the prefix, then the middle, then the suffix.  Below
-- the top level an item is a node, so the function handed down folds over a
-- node's children.

-- | Right fold, lazy in the accumulator.
foldrD :: (a -> r -> r) -> r -> Deque a -> r
foldrD _ z Nil = z
foldrD f z (Deep _ pr m sf) = foldrS f (foldrM (foldrNode f) (foldrRev f z sf) m) pr

-- The same, inlined where it is used, so that a known function is applied
-- directly to the children of the leaf nodes, where nearly all the elements
-- are.  With (:) that builds a leaf's eight cells with no calls in between.
foldrI :: (a -> r -> r) -> r -> Deque a -> r
foldrI _ z Nil = z
foldrI f z (Deep _ pr m sf) = foldrS f (foldrM (\nd acc -> foldrNodeI f nd acc) (foldrRev f z sf) m) pr
{-# INLINE foldrI #-}

foldrNodeI :: (a -> r -> r) -> Node a -> r -> r
foldrNodeI fn (N8 a b c d e f g h) z = fn a (fn b (fn c (fn d (fn e (fn f (fn g (fn h z)))))))
foldrNodeI fn (NA _ a) z = foldrA fn z a
{-# INLINE foldrNodeI #-}

foldrM :: (Node a -> r -> r) -> r -> Mid a -> r
foldrM _ z MNil = z
foldrM f z (MDeep _ _ pr m sf) = foldrS f (foldrM (foldrNode f) (foldrRev f z sf) m) pr

foldrS :: (a -> r -> r) -> r -> SList a -> r
foldrS f z = go
  where
    go SNil = z
    go (SCons x r) = f x (go r)

-- a right fold over a list that runs back to front
foldrRev :: (a -> r -> r) -> r -> SList a -> r
foldrRev f = go
  where
    go acc SNil = acc
    go acc (SCons x r) = go (f x acc) r

foldrNode :: (a -> r -> r) -> Node a -> r -> r
foldrNode fn (N8 a b c d e f g h) z = fn a (fn b (fn c (fn d (fn e (fn f (fn g (fn h z)))))))
foldrNode fn (NA _ a) z = foldrA fn z a

-- | Strict left fold.
foldlD' :: (r -> a -> r) -> r -> Deque a -> r
foldlD' _ !z Nil = z
foldlD' f z (Deep _ pr m sf) = foldlRev f (foldlM (foldlNode f) (foldlS f z pr) m) sf

foldlM :: (r -> Node a -> r) -> r -> Mid a -> r
foldlM _ !z MNil = z
foldlM f z (MDeep _ _ pr m sf) = foldlRev f (foldlM (foldlNode f) (foldlS f z pr) m) sf

foldlS :: (r -> a -> r) -> r -> SList a -> r
foldlS f = go
  where
    go !z SNil = z
    go !z (SCons x r) = go (f z x) r

-- a strict left fold over a list that runs back to front
foldlRev :: (r -> a -> r) -> r -> SList a -> r
foldlRev f !z = go
  where
    go SNil = z
    go (SCons x r) = let !a = go r in f a x

foldlNode :: (r -> a -> r) -> r -> Node a -> r
foldlNode fn !z (N8 a b c d e f g h) =
  let !z1 = fn z a; !z2 = fn z1 b; !z3 = fn z2 c; !z4 = fn z3 d; !z5 = fn z4 e; !z6 = fn z5 f; !z7 = fn z6 g
   in fn z7 h
foldlNode fn !z (NA _ a) = foldlA fn z a

------------------------------------------------------------------------
-- Mapping keeps the shape

mapD :: (a -> b) -> Deque a -> Deque b
mapD _ Nil = Nil
mapD f (Deep t pr m sf) = Deep t (mapS f pr) (mapM' (mapNode f) m) (mapS f sf)

mapM' :: (Node a -> Node b) -> Mid a -> Mid b
mapM' _ MNil = MNil
mapM' f (MDeep t ps pr m sf) = MDeep t ps (mapS f pr) (mapM' (mapNode f) m) (mapS f sf)

mapS :: (a -> b) -> SList a -> SList b
mapS f = go
  where
    go SNil = SNil
    go (SCons x r) = SCons (f x) (go r)

mapNode :: (a -> b) -> Node a -> Node b
mapNode fn (N8 a b c d e f g h) = N8 (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g) (fn h)
mapNode fn (NA n a) = NA n (mapA fn a)

------------------------------------------------------------------------
-- Invariant checker, for tests

valid :: Deque a -> Either String ()
valid Nil = Right ()
valid (Deep t pr m sf) = do
  let n = tsize t
  when (n <= 0) $ Left "a Deep with no leaves"
  unless (lenS pr == tpc t) $ Left "top prefix count"
  unless (lenS sf == tsc t) $ Left "top suffix count"
  when (tpc t > maxD || tsc t > maxD) $ Left "top digit too long"
  ms <- validM (1 :: Int) (\_ -> Right 1) m
  when (ms > 0 && (tpc t == 0 || tsc t == 0)) $ Left "top: an empty digit beside a middle"
  unless (n == tpc t + ms + tsc t) $ Left ("top size: " ++ show n ++ " /= " ++ show (tpc t, ms, tsc t))

validM :: Int -> (a -> Either String Int) -> Mid a -> Either String Int
validM _ _ MNil = Right 0
validM lvl chk (MDeep t ps pr m sf) = do
  let n = tsize t
      at s = "level " ++ show lvl ++ ": " ++ s
  when (n <= 0) $ Left (at "an MDeep with no leaves")
  unless (lenS pr == tpc t) $ Left (at "prefix count")
  unless (lenS sf == tsc t) $ Left (at "suffix count")
  when (tpc t > maxD || tsc t > maxD) $ Left (at "digit too long")
  psz <- sum <$> traverse (checkNode lvl chk) (listS pr)
  unless (psz == ps) $ Left (at "prefix size")
  ssz <- sum <$> traverse (checkNode lvl chk) (listS sf)
  ms <- validM (lvl + 1) (checkNode lvl chk) m
  when (ms > 0 && (tpc t == 0 || tsc t == 0)) $ Left (at "an empty digit beside a middle")
  unless (n == psz + ms + ssz) $ Left (at ("size: " ++ show n ++ " /= " ++ show (psz, ms, ssz)))
  pure n

-- lvl: the level of the digit the node is in; only at the first are a
-- node's children leaves
checkNode :: Int -> (a -> Either String Int) -> Node a -> Either String Int
checkNode lvl chk nd = do
  let kids = [nodeAt i nd | i <- [0 .. nodeArity nd - 1]]
  s <- sum <$> traverse chk kids
  unless (s == nodeSize nd) $ Left "node size"
  when (L.length kids < 2 || L.length kids > 8) $ Left "node arity"
  case nd of
    N8 {} | lvl /= 1 -> Left "a node of eight leaves below the first level"
    _ -> pure ()
  pure s

------------------------------------------------------------------------
-- The names Data.Sequence uses, so that this module can stand in for it.

type Seq = Deque

length :: Deque a -> Int
length = size
{-# INLINE length #-}

null :: Deque a -> Bool
null Nil = True
null _ = False
{-# INLINE null #-}

infixr 5 <|

infixl 5 |>

infixr 5 ><

(<|) :: a -> Deque a -> Deque a
(<|) = cons
{-# INLINE (<|) #-}

(|>) :: Deque a -> a -> Deque a
(|>) = snoc
{-# INLINE (|>) #-}

(><) :: Deque a -> Deque a -> Deque a
(><) = append
{-# INLINE (><) #-}

infixr 5 :<|

infixl 5 :|>

pattern Empty :: Deque a
pattern Empty <- (null -> True)
  where
    Empty = Nil

pattern (:<|) :: a -> Deque a -> Deque a
pattern x :<| xs <- (uncons -> Just (x, xs))
  where
    x :<| xs = cons x xs

pattern (:|>) :: Deque a -> a -> Deque a
pattern xs :|> x <- (unsnoc -> Just (xs, x))
  where
    xs :|> x = snoc xs x

{-# COMPLETE (:<|), Empty #-}

{-# COMPLETE (:|>), Empty #-}

-- | A stable sort.  O(n log n); goes through a list.
sort :: (Ord a) => Deque a -> Deque a
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> Deque a -> Deque a
sortBy cmp d = unsafeFromListN (size d) (L.sortBy cmp (toList d))

-- | The same as 'sort' here; Data.Sequence has both.
unstableSort :: (Ord a) => Deque a -> Deque a
unstableSort = sort

unstableSortBy :: (a -> a -> Ordering) -> Deque a -> Deque a
unstableSortBy = sortBy

intersperse :: a -> Deque a -> Deque a
intersperse sep = fromList . L.intersperse sep . toList

reverse :: Deque a -> Deque a
reverse = foldlD' (flip cons) empty

------------------------------------------------------------------------
-- Instances

instance Semigroup (Deque a) where
  (<>) = append

instance Monoid (Deque a) where
  mempty = empty

instance Foldable Deque where
  foldr = foldrD
  foldl' = foldlD'
  toList = toList
  length = size
  null = null

instance Functor Deque where
  fmap = mapD

-- | Rebuilds the sequence.  O(n).
instance Traversable Deque where
  traverse f d = fromListN (size d) <$> traverse f (toList d)

instance (Eq a) => Eq (Deque a) where
  a == b = size a == size b && toList a == toList b

-- | Lexicographic, as for lists.
instance (Ord a) => Ord (Deque a) where
  compare a b = compare (toList a) (toList b)

instance Eq1 Deque where
  liftEq eq a b = size a == size b && liftEq eq (toList a) (toList b)

instance Ord1 Deque where
  liftCompare cmp a b = liftCompare cmp (toList a) (toList b)

instance (Show a) => Show (Deque a) where
  showsPrec p d = showParen (p > 10) $ showString "fromList " . shows (toList d)

instance Show1 Deque where
  liftShowsPrec _ sl p d = showParen (p > 10) $ showString "fromList " . sl (toList d)

-- | The structure is always evaluated; this evaluates the elements fully.
instance (NFData a) => NFData (Deque a) where
  rnf = foldlD' (\() x -> rnf x) ()

instance Exts.IsList (Deque a) where
  type Item (Deque a) = a
  fromList = fromList
  toList = toList
