{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- The JIT's C helpers read and build these constructors (unison-runtime's
-- cbits/jit_rt.c), so their layout must be the same in every build: fields are
-- unpacked only when optimizing.
{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}

-- | A rope: a sequence of chunks that knows its size in elements (the
-- characters of a text, the bytes of a byte string).
--
-- * O(1) 'size'
-- * amortized O(1) 'cons', 'snoc', 'uncons', 'unsnoc' of a chunk, and so of a
--   short piece of text at either end
-- * O(log n) 'index', 'take', 'drop', append; close to O(1) near either end
--
-- It is the finger tree of "Unison.Util.Deque" with chunks for elements and
-- sizes counted in what the chunks hold.  The middle of the tree is the
-- Deque's own middle, a sequence of nodes that knows the size under each; only
-- the top level, whose items are chunks, is written here.
--
-- Invariants:
--
-- * no chunk is empty
-- * a rope of one chunk is 'One'; a 'Deep' has a chunk in each digit
-- * two chunks next to each other hold more than 'threshold' elements
--   between them, so a rope of n elements has at most @2n / threshold + 1@
--   chunks however it was built
module Unison.Util.Rope
  ( chunks,
    fromChunks,
    singleton,
    one,
    map,
    traverse,
    traverseWithPos_,
    null,
    flatten,
    two,
    cons,
    uncons,
    snoc,
    unsnoc,
    index,
    debugDepth,
    extractChunk,
    threshold,
    valid,
    Sized (..),
    Take (..),
    Drop (..),
    Reverse (..),
    Index (..),
    Rope (..),
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad (unless, when)
import Data.Bits (unsafeShiftL, (.&.))
import Data.Foldable qualified as F
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import GHC.Exts (Int (I#), Int#)
import Unison.Util.Deque.Internal
  ( Mid (..),
    Node (..),
    SList (..),
    appM,
    arr8,
    consM,
    dropM,
    dropS,
    foldlM,
    foldlNode,
    foldlRev,
    foldlS,
    foldrM,
    foldrNode,
    foldrRev,
    foldrS,
    fromSListA,
    lenS,
    listS,
    maxD,
    mk,
    nodeArity,
    nodeAt,
    nodeDropFwd,
    nodeSize,
    nodeTakeRev,
    revOnto,
    revS,
    scanBwd,
    scanFwd,
    scanKids,
    sizeM,
    snocM,
    takeM,
    takeS,
    tpc,
    tsc,
    tsize,
    unconsM,
    unsnocM,
    validM,
  )
import Prelude hiding (drop, map, null, reverse, take, traverse)
import Prelude qualified as P

-- typeclasses used for abstracting over the chunk type
class Sized a where size :: a -> Int

class Take a where take :: Int -> a -> a

class Drop a where drop :: Int -> a -> a

class Index a elem where unsafeIndex :: Int -> a -> elem

class Reverse a where reverse :: a -> a

-- | The first 'Int' of 'Deep' packs three numbers, as in the Deque: the size
-- of the rope (bits 8 and up), the number of chunks in the suffix (bits 4 to
-- 7) and in the prefix (bits 0 to 3).  The second is the size of the prefix.
-- The prefix runs front to back and the suffix back to front, so the first
-- and last chunks of the rope are the heads of the two lists.
data Rope a
  = Empty
  | One !a
  | Deep !Int !Int !(SList a) !(Mid a) !(SList a)

-- | Two chunks next to each other with this many elements or fewer between
-- them are made one chunk.
--
-- See https://github.com/unisonweb/unison/pull/1899#discussion_r742953469
threshold :: Int
threshold = 64

instance (Sized a) => Sized (Rope a) where
  size = \case
    Empty -> 0
    One a -> size a
    Deep t _ _ _ _ -> tsize t
  {-# INLINE size #-}

null :: Rope a -> Bool
null Empty = True
null _ = False
{-# INLINE null #-}

singleton, one :: (Sized a) => a -> Rope a
one a | size a == 0 = Empty
one a = One a
singleton = one
{-# INLINE one #-}
{-# INLINE singleton #-}

sumC :: (Sized a) => SList a -> Int
sumC = go 0
  where
    go !n SNil = n
    go !n (SCons x r) = go (n + size x) r
{-# INLINE sumC #-}

------------------------------------------------------------------------
-- Building a level whose digits may be short

-- chunks in order, at most a digit's worth
fromFwd :: (Sized a) => Int -> Int -> SList a -> Rope a
fromFwd !n !cnt l = case l of
  SNil -> Empty
  SCons x SNil -> One x
  _ ->
    let !p = (cnt + 1) `div` 2
        !pr = takeS p l
     in Deep (mk n p (cnt - p)) (sumC pr) pr MNil (revS (dropS p l))
{-# INLINEABLE fromFwd #-}

-- chunks back to front
fromBwd :: (Sized a) => Int -> Int -> SList a -> Rope a
fromBwd !n !cnt l = case l of
  SNil -> Empty
  SCons x SNil -> One x
  _ ->
    let !q = cnt `div` 2
        !pr = revS (dropS q l)
     in Deep (mk n (cnt - q) q) (sumC pr) pr MNil (takeS q l)
{-# INLINEABLE fromBwd #-}

-- A rope of n elements from the parts of a 'Deep', either of whose digits
-- may be empty: an empty digit takes a node from the middle, or half of the
-- other digit when there is no middle.
build :: (Sized a) => Int -> Int -> Int -> SList a -> Mid a -> Int -> SList a -> Rope a
build !n !pc !ps pr m !sc sf
  | pc == 0 = case m of
      MNil -> fromBwd n sc sf
      _ -> case unconsM m of
        (# nd, m' #) -> build n (nodeArity nd) (nodeSize nd) (nodeDropFwd 0 nd) m' sc sf
  | sc == 0 = case m of
      MNil -> fromFwd n pc pr
      _ -> case unsnocM m of
        (# m', nd #) -> let !k = nodeArity nd in Deep (mk n pc k) ps pr m' (nodeTakeRev k nd)
  | otherwise = Deep (mk n pc sc) ps pr m sf
{-# INLINEABLE build #-}

------------------------------------------------------------------------
-- Adding a chunk at an end.  A chunk that fits in 'threshold' together with
-- the chunk already at that end is joined to it.

cons :: (Sized a, Semigroup a) => a -> Rope a -> Rope a
cons c r = case size c of
  0 -> r
  s -> cons' s c r
{-# INLINE cons #-}

snoc :: (Sized a, Semigroup a) => Rope a -> a -> Rope a
snoc r c = case size c of
  0 -> r
  s -> snoc' r s c
{-# INLINE snoc #-}

cons' :: (Sized a, Semigroup a) => Int -> a -> Rope a -> Rope a
cons' !_ c Empty = One c
cons' s c (One a)
  | s + sa <= threshold = One (c <> a)
  | otherwise = Deep (mk (s + sa) 1 1) s (SCons c SNil) MNil (SCons a SNil)
  where
    !sa = size a
cons' s c (Deep t ps pr m sf) = case pr of
  SCons f pr'
    | s + size f <= threshold -> Deep (t + (s `unsafeShiftL` 8)) (ps + s) (SCons (c <> f) pr') m sf
    | t .&. 15 < maxD -> Deep (t + (s `unsafeShiftL` 8) + 1) (ps + s) (SCons c pr) m sf
  -- a full prefix keeps its two outermost chunks and sheds the other eight
  SCons p1 (SCons p2 (SCons a (SCons b (SCons c' (SCons d (SCons e (SCons f (SCons g (SCons h _))))))))) ->
    let !keep = size p1 + size p2
     in Deep
          (t + (s `unsafeShiftL` 8) - 7)
          (s + keep)
          (SCons c (SCons p1 (SCons p2 SNil)))
          (consM (NA (ps - keep) (arr8 a b c' d e f g h)) m)
          sf
  _ -> error "Rope.cons: short prefix"
{-# INLINEABLE cons' #-}

snoc' :: (Sized a, Semigroup a) => Rope a -> Int -> a -> Rope a
snoc' Empty !_ c = One c
snoc' (One a) s c
  | sa + s <= threshold = One (a <> c)
  | otherwise = Deep (mk (sa + s) 1 1) sa (SCons a SNil) MNil (SCons c SNil)
  where
    !sa = size a
snoc' (Deep t ps pr m sf) s c = case sf of
  SCons l sf'
    | size l + s <= threshold -> Deep (t + (s `unsafeShiftL` 8)) ps pr m (SCons (l <> c) sf')
    | t .&. 0xF0 < maxD * 16 -> Deep (t + (s `unsafeShiftL` 8) + 0x10) ps pr m (SCons c sf)
  SCons s1 (SCons s2 (SCons h (SCons g (SCons f (SCons e (SCons d (SCons c' (SCons b (SCons a _))))))))) ->
    let !shed = tsize t - ps - sizeM m - size s1 - size s2
     in Deep
          (t + (s `unsafeShiftL` 8) - 0x70)
          ps
          pr
          (snocM m (NA shed (arr8 a b c' d e f g h)))
          (SCons c (SCons s1 (SCons s2 SNil)))
  _ -> error "Rope.snoc: short suffix"
{-# INLINEABLE snoc' #-}

fromChunks :: (Sized a, Semigroup a) => [a] -> Rope a
fromChunks = F.foldl' snoc Empty
{-# INLINE fromChunks #-}

------------------------------------------------------------------------
-- Removing a chunk at an end

uncons :: (Sized a) => Rope a -> Maybe (a, Rope a)
uncons = \case
  Empty -> Nothing
  One a -> Just (a, Empty)
  Deep t ps pr m sf -> case pr of
    SCons x pr' ->
      let !s = size x
          !rest = case pr' of
            SCons _ _ -> Deep (t - (s `unsafeShiftL` 8) - 1) (ps - s) pr' m sf
            SNil -> build (tsize t - s) 0 0 SNil m (tsc t) sf
       in Just (x, rest)
    SNil -> error "Rope.uncons: empty prefix"
{-# INLINEABLE uncons #-}

unsnoc :: (Sized a) => Rope a -> Maybe (Rope a, a)
unsnoc = \case
  Empty -> Nothing
  One a -> Just (Empty, a)
  Deep t ps pr m sf -> case sf of
    SCons x sf' ->
      let !s = size x
          !rest = case sf' of
            SCons _ _ -> Deep (t - (s `unsafeShiftL` 8) - 0x10) ps pr m sf'
            SNil -> build (tsize t - s) (tpc t) ps pr m 0 SNil
       in Just (rest, x)
    SNil -> error "Rope.unsnoc: empty suffix"
{-# INLINEABLE unsnoc #-}

------------------------------------------------------------------------
-- Finding the chunk that holds an element

-- The first-level node holding element i of a middle, and i's offset in it.
lookR :: Int -> Mid a -> (# Node a, Int# #)
lookR !_ MNil = error "Rope.index: empty middle"
lookR i (MDeep t ps pr m sf)
  | i < ps = scanFwd i pr
  | i' < sizeM m = case lookR i' m of
      (# nn, off# #) -> case scanKids (I# off#) nn of
        (# q#, sb# #) -> case I# off# - I# sb# of I# o# -> (# nodeAt (I# q#) nn, o# #)
  | otherwise = scanBwd (tsize t - 1 - i) sf
  where
    !i' = i - ps

-- The chunk holding element i, 0 <= i < size, and i's offset in it.
chunkAt :: (Sized a) => Int -> Rope a -> (# a, Int# #)
chunkAt !_ Empty = error "Rope.index: empty"
chunkAt (I# i#) (One a) = (# a, i# #)
chunkAt i (Deep t ps pr m sf)
  | i < ps = fwd i pr
  | i' < sizeM m = case lookR i' m of (# nd, off# #) -> inNode 0 (I# off#) nd
  | otherwise = bwd (tsize t - 1 - i) sf
  where
    !i' = i - ps
    fwd !k (SCons c r)
      | k < s = case k of I# k# -> (# c, k# #)
      | otherwise = fwd (k - s) r
      where
        !s = size c
    fwd _ SNil = error "Rope.index: past the end of the prefix"
    bwd !r (SCons c rest)
      | r < s = case s - 1 - r of I# k# -> (# c, k# #)
      | otherwise = bwd (r - s) rest
      where
        !s = size c
    bwd _ SNil = error "Rope.index: past the end of the suffix"
    inNode !q !k nd
      | k < s = case k of I# k# -> (# c, k# #)
      | otherwise = inNode (q + 1) (k - s) nd
      where
        !c = nodeAt q nd
        !s = size c
{-# INLINEABLE chunkAt #-}

index :: (Sized a, Index a ch) => Int -> Rope a -> Maybe ch
index i r
  | i >= 0 && i < size r = Just (unsafeIndex i r)
  | otherwise = Nothing
{-# INLINE index #-}

instance (Sized a, Index a ch) => Index (Rope a) ch where
  unsafeIndex i (One a) = unsafeIndex i a
  unsafeIndex i r = case chunkAt i r of (# c, o# #) -> unsafeIndex (I# o#) c
  {-# INLINE unsafeIndex #-}

-- Extracts a chunk from a rope that
--
--   1. Begins at the specified position in the rope.
--   2. Is at least the size specified.
--
-- When the chunk holding the position has that much after it, the result is a
-- slice of it; otherwise chunks are concatenated, so it is advisable not to
-- use a very large size.
--
-- This function *assumes* that the rope actually contains the necessary
-- elements. If that is not the case, #2 above will certainly not be
-- satisfied, but the exact behavior should not be relied upon.
extractChunk :: (Monoid a, Sized a, Take a, Drop a) => Int -> Int -> Rope a -> a
extractChunk ix ln r
  | ix < 0 || ix >= size r = mempty
  | otherwise = case chunkAt ix r of
      (# c, o# #)
        | size c - I# o# >= ln -> drop (I# o#) c
        | otherwise -> mconcat (chunks (take ln (drop ix r)))
{-# INLINEABLE extractChunk #-}

------------------------------------------------------------------------
-- take and drop.  The rope is cut between two chunks, by the Deque's code
-- when the cut is in the middle, and then the part of the chunk the cut falls
-- in is added back with 'snoc' or 'cons', which joins it to its neighbour if
-- the two are small.

instance (Sized a, Semigroup a, Take a) => Take (Rope a) where
  take = takeR
  {-# INLINE take #-}

instance (Sized a, Semigroup a, Drop a) => Drop (Rope a) where
  drop = dropR
  {-# INLINE drop #-}

takeR :: (Sized a, Semigroup a, Take a) => Int -> Rope a -> Rope a
takeR !i r = case r of
  Empty -> Empty
  One a
    | i <= 0 -> Empty
    | i >= size a -> r
    | otherwise -> One (take i a)
  Deep t ps pr m sf
    | i <= 0 -> Empty
    | i >= n -> r
    | i <= ps -> inPrefix 0 0 pr
    | i <= ps + sizeM m -> case takeM (i - ps) m of
        (# m', nd, k# #) -> inNode m' nd (I# k#) 0 0
    | otherwise -> inSuffix (n - i) sc sf
    where
      !n = tsize t
      !pc = tpc t
      !sc = tsc t
      -- q whole chunks of the prefix, with sb elements, come before the cut
      inPrefix !q !sb (SCons c rest)
        | sb + s < i = inPrefix (q + 1) (sb + s) rest
        | sb + s == i = fromFwd i (q + 1) (takeS (q + 1) pr)
        | otherwise = snoc (fromFwd sb q (takeS q pr)) (take (i - sb) c)
        where
          !s = size c
      inPrefix _ _ SNil = error "Rope.take: past the end of the prefix"
      -- the first k elements of the node are kept
      inNode m' nd !k !q !sb
        | sb + s < k = inNode m' nd k (q + 1) (sb + s)
        | sb + s == k = build i pc ps pr m' (q + 1) (nodeTakeRev (q + 1) nd)
        | otherwise = snoc (build (i - (k - sb)) pc ps pr m' q (nodeTakeRev q nd)) (take (k - sb) c)
        where
          !c = nodeAt q nd
          !s = size c
      -- d elements are to go from the back; cnt chunks of the suffix are left
      inSuffix !d !cnt l@(SCons c rest)
        | d >= s = inSuffix (d - s) (cnt - 1) rest
        | d == 0 = build i pc ps pr m cnt l
        | otherwise = snoc (build (i - (s - d)) pc ps pr m (cnt - 1) rest) (take (s - d) c)
        where
          !s = size c
      inSuffix _ _ SNil = error "Rope.take: past the end of the suffix"
{-# INLINEABLE takeR #-}

dropR :: (Sized a, Semigroup a, Drop a) => Int -> Rope a -> Rope a
dropR !i r = case r of
  Empty -> Empty
  One a
    | i <= 0 -> r
    | i >= size a -> Empty
    | otherwise -> One (drop i a)
  Deep t ps pr m sf
    | i <= 0 -> r
    | i >= n -> Empty
    | i >= ps + ms -> inSuffix 0 0 sf
    | i >= ps -> case dropM (i - ps) m of
        (# nd, k#, m' #) -> inNode nd (I# k#) m' 0 0
    | otherwise -> inPrefix i pc ps pr
    where
      !n = tsize t
      !pc = tpc t
      !sc = tsc t
      !ms = sizeM m
      !left = n - i
      -- q whole chunks of the suffix, with sa elements, come after the cut
      inSuffix !q !sa (SCons c rest)
        | sa + s < left = inSuffix (q + 1) (sa + s) rest
        | sa + s == left = fromBwd left (q + 1) (takeS (q + 1) sf)
        | otherwise = cons (drop (s - (left - sa)) c) (fromBwd sa q (takeS q sf))
        where
          !s = size c
      inSuffix _ _ SNil = error "Rope.drop: past the end of the suffix"
      -- the first k elements of the node go
      inNode nd !k m' !q !sb
        | k >= sb + s = inNode nd k m' (q + 1) (sb + s)
        | k == sb = build left (c - q) (nodeSize nd - sb) (nodeDropFwd q nd) m' sc sf
        | otherwise =
            cons
              (drop (k - sb) x)
              (build (left - (sb + s - k)) (c - q - 1) (nodeSize nd - sb - s) (nodeDropFwd (q + 1) nd) m' sc sf)
        where
          !x = nodeAt q nd
          !s = size x
          !c = nodeArity nd
      -- k elements still to drop; the prefix has cnt chunks and psz elements left
      inPrefix !k !cnt !psz l@(SCons c rest)
        | k >= s = inPrefix (k - s) (cnt - 1) (psz - s) rest
        | k == 0 = build left cnt psz l m sc sf
        | otherwise = cons (drop k c) (build (left - (s - k)) (cnt - 1) (psz - s) rest m sc sf)
        where
          !s = size c
      inPrefix _ _ _ SNil = error "Rope.drop: past the end of the prefix"
{-# INLINEABLE dropR #-}

------------------------------------------------------------------------
-- Append.  If the two chunks that meet are small they are joined; then the
-- digits that end up inside are packed into nodes and handed to the Deque's
-- append of two middles.

instance (Sized a, Semigroup a) => Semigroup (Rope a) where
  (<>) = append
  {-# INLINE (<>) #-}

instance (Sized a, Semigroup a) => Monoid (Rope a) where
  mempty = Empty

two :: (Sized a, Semigroup a) => Rope a -> Rope a -> Rope a
two = append
{-# INLINE two #-}

append :: (Sized a, Semigroup a) => Rope a -> Rope a -> Rope a
append Empty b = b
append a Empty = a
append (One a) b = cons' (size a) a b
append a (One c) = snoc' a (size c) c
append (Deep t1 ps1 pr1 m1 sf1) (Deep t2 ps2 pr2 m2 sf2) = case sf1 of
  SCons l sf1'
    | SCons f pr2' <- pr2,
      size l + size f <= threshold ->
        -- the two chunks that meet become one, the last of the left side
        inner (SCons (l <> f) sf1') pr2' (tpc t2 - 1) (size f)
  _ -> inner sf1 pr2 (tpc t2) 0
  where
    !n = tsize t1 + tsize t2
    !pc1 = tpc t1
    !sc1 = tsc t1
    !sc2 = tsc t2
    flat MNil = True
    flat _ = False
    -- isf: the left side's suffix; ipr: the ipc chunks of the right side's
    -- prefix that are still its own (d characters of it went to the left).
    -- The digits that end up inside join the outer digit of a side that has
    -- no middle, if they fit there (the shorter side's, if they fit in
    -- either: that copies fewer cells), or are packed into nodes and handed
    -- down between the two middles.
    inner isf ipr !ipc !d
      | flat m1,
        pc1 + c <= maxD,
        not (flat m2 && c + sc2 <= maxD && sc2 + ipc < pc1 + sc1) =
          Deep (mk n (pc1 + c) sc2) (tsize t1 + ps2) (appendS' pr1 (revOnto isf ipr)) m2 sf2
      | flat m2,
        c + sc2 <= maxD =
          Deep (mk n pc1 (c + sc2)) ps1 pr1 m1 (appendS' sf2 (revOnto ipr isf))
      | c >= 2 =
          let !sns = (tsize t1 - ps1 - sizeM m1) + ps2
           in Deep (mk n pc1 sc2) ps1 pr1 (appM m1 sns (packChunks c (revOnto isf ipr)) m2) sf2
      | otherwise =
          -- a single chunk between two sides that can't take it: the right
          -- side gets a prefix again, from its middle or its suffix
          append
            (Deep (t1 + (d `unsafeShiftL` 8)) ps1 pr1 m1 isf)
            (build (tsize t2 - d) 0 0 SNil m2 sc2 sf2)
      where
        !c = sc1 + ipc
{-# INLINEABLE append #-}

appendS' :: SList a -> SList a -> SList a
appendS' SNil ys = ys
appendS' (SCons x r) ys = SCons x (appendS' r ys)

-- c chunks, 2 to 2 * maxD of them, as nodes of eight while that leaves none
-- or at least two for the next (nine make a five and a four)
packChunks :: (Sized a) => Int -> SList a -> SList (Node a)
packChunks !c l
  | c <= 8 = SCons (chunkNode c l) SNil
  | c == 9 = SCons (chunkNode 5 l) (SCons (chunkNode 4 (dropS 5 l)) SNil)
  | otherwise = SCons (chunkNode 8 l) (packChunks (c - 8) (dropS 8 l))
{-# INLINEABLE packChunks #-}

-- a node of the first k chunks of a list
chunkNode :: (Sized a) => Int -> SList a -> Node a
chunkNode k l = NA (go 0 k l) (fromSListA k l)
  where
    go !n 0 _ = n
    go !n j (SCons x r) = go (n + size x) (j - 1 :: Int) r
    go !n _ SNil = n
{-# INLINE chunkNode #-}

------------------------------------------------------------------------
-- Folds over the chunks

-- A right fold, inlined where it is used so that a known function is applied
-- directly to the chunks of the first-level nodes, where nearly all of them
-- are.
foldrChunks :: (a -> r -> r) -> r -> Rope a -> r
foldrChunks f z = \case
  Empty -> z
  One a -> f a z
  Deep _ _ pr m sf -> foldrS f (foldrM (\nd acc -> foldrKids f nd acc) (foldrRev f z sf) m) pr
{-# INLINE foldrChunks #-}

foldrKids :: (a -> r -> r) -> Node a -> r -> r
foldrKids f nd z = case nd of
  NA _ kids ->
    let !n = sizeofSmallArray kids
        go i
          | i >= n = z
          | otherwise = f (indexSmallArray kids i) (go (i + 1))
     in go 0
  _ -> foldrNode f nd z
{-# INLINE foldrKids #-}

instance Foldable Rope where
  foldr f z = foldrChunks f z
  {-# INLINE foldr #-}
  foldl' f !z = \case
    Empty -> z
    One a -> f z a
    Deep _ _ pr m sf -> foldlRev f (foldlM (foldlNode f) (foldlS f z pr) m) sf
  null = null

chunks :: Rope a -> [a]
chunks = foldrChunks (:) []

flatten :: (Monoid a) => Rope a -> a
flatten = mconcat . chunks

-- | The result is rebuilt from the chunks the function returns, so it is a
-- rope whatever their sizes are.
map :: (Sized b, Semigroup b) => (a -> b) -> Rope a -> Rope b
map f = \case
  Empty -> Empty
  One a -> one (f a)
  r -> F.foldl' (\acc c -> snoc acc (f c)) Empty r
{-# INLINEABLE map #-}

traverse :: (Applicative f, Sized b, Semigroup b) => (a -> f b) -> Rope a -> f (Rope b)
traverse f = \case
  Empty -> pure Empty
  One a -> one <$> f a
  r -> fromChunks <$> P.traverse f (chunks r)
{-# INLINEABLE traverse #-}

-- Traverses the chunks of a rope with the position of each chunk.
traverseWithPos_ :: (Applicative f, Sized a) => (Int -> a -> f ()) -> (Rope a -> f ())
traverseWithPos_ f = \case
  Empty -> pure ()
  One a -> f 0 a
  r -> foldr (\c k !o -> f o c *> k (o + size c)) (\_ -> pure ()) r 0
{-# INLINE traverseWithPos_ #-}

instance (Sized a, Semigroup a, Reverse a) => Reverse (Rope a) where
  reverse = \case
    Empty -> Empty
    One a -> One (reverse a)
    r -> F.foldl' (\acc c -> cons (reverse c) acc) Empty r

------------------------------------------------------------------------
-- Comparison

-- Produces two lists of chunks where the chunks have the same length
alignChunks :: (Sized a, Take a, Drop a) => [a] -> [a] -> ([a], [a])
alignChunks bs1 bs2 = (cs1, cs2)
  where
    cs1 = alignTo bs1 bs2
    cs2 = alignTo bs2 cs1
    alignTo bs1 [] = bs1
    alignTo [] _ = []
    alignTo (hd1 : tl1) (hd2 : tl2)
      | len1 == len2 = hd1 : alignTo tl1 tl2
      | len1 < len2 = hd1 : alignTo tl1 (drop len1 hd2 : tl2)
      | otherwise -- len1 > len2
        =
          let (hd1', hd1rem) = (take len2 hd1, drop len2 hd1)
           in hd1' : alignTo (hd1rem : tl1) tl2
      where
        len1 = size hd1
        len2 = size hd2

instance (Sized a, Take a, Drop a, Eq a) => Eq (Rope a) where
  One l == One r = l == r
  b1 == b2
    | size b1 == size b2 =
        uncurry (==) (alignChunks (chunks b1) (chunks b2))
  _ == _ = False
  {-# INLINE (==) #-}

-- Lexicographical ordering
instance (Sized a, Take a, Drop a, Ord a) => Ord (Rope a) where
  One l `compare` One r = compare l r
  b1 `compare` b2 = uncurry compare (alignChunks (chunks b1) (chunks b2))
  {-# INLINE compare #-}

instance (NFData a) => NFData (Rope a) where
  rnf = F.foldl' (\() c -> rnf c) ()

------------------------------------------------------------------------
-- For tests

-- | The number of levels below the top one.
debugDepth :: Rope a -> Int
debugDepth = \case
  Deep _ _ _ m _ -> go m
  _ -> 0
  where
    go :: Mid b -> Int
    go MNil = 0
    go (MDeep _ _ _ m _) = 1 + go m

-- | Checks the invariants.
valid :: (Sized a) => Rope a -> Either String ()
valid = \case
  Empty -> Right ()
  One a -> when (size a <= 0) $ Left "an empty chunk"
  r@(Deep t ps pr m sf) -> do
    let n = tsize t
        chunk c = if size c <= 0 then Left "an empty chunk" else Right (size c)
    unless (lenS pr == tpc t) $ Left "prefix count"
    unless (lenS sf == tsc t) $ Left "suffix count"
    when (tpc t > maxD || tsc t > maxD) $ Left "digit too long"
    when (tpc t == 0 || tsc t == 0) $ Left "an empty digit"
    psz <- sum <$> P.traverse chunk (listS pr)
    unless (psz == ps) $ Left "prefix size"
    ssz <- sum <$> P.traverse chunk (listS sf)
    ms <- validM 1 chunk m
    unless (n == psz + ms + ssz) $ Left ("size: " ++ show n ++ " /= " ++ show (psz, ms, ssz))
    let sizes = fmap size (chunks r)
    unless (and (zipWith (\a b -> a + b > threshold) sizes (P.drop 1 sizes))) $
      Left "two small chunks next to each other"
