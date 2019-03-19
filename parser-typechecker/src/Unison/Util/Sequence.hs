{-# Language Strict, StrictData #-}

module Unison.Util.Sequence where

-- |
-- Fast sequence type based on skewed array-indexed tries
-- with large branching factor, `B`.
--
-- Asymptotics (assuming vector of size `N`):
--   O(1) worst-case access to indices [0,B)
--   O(1) average-case access to indices [N-B,N)
--   O(log_B(i)) worst-case access to index i
--   O(1) amortized snoc, O(log_B(N)) worst case
--
-- Some inspiration stolen from: http://julesjacobs.github.io/2014/11/11/immutable-vectors-csharp.html

import Control.Monad (join)
import Data.Foldable
import Data.List hiding (length, reverse)
import Prelude hiding (length, reverse)
import qualified Data.List as DL
import qualified Data.Vector as V

arity :: Int
arity = 32

data Seq0 a = Seq0 Int ~(Seq0 (V.Vector a)) (V.Vector a)

size0 :: Seq0 a -> Int
size0 (Seq0 n _ _) = n

empty0 :: Seq0 a
empty0 = Seq0 0 empty0 V.empty

snoc0 :: Seq0 a -> a -> Seq0 a
snoc0 (Seq0 n s buf) a = case buf `V.snoc` a of
  buf | V.length buf /= arity -> Seq0 (n+1) s buf
      | otherwise             -> Seq0 (n+1) (s `snoc0` buf) V.empty

reverse0 :: Seq0 a -> Seq0 a
reverse0 s = fromList0 [ unsafeIndex0 s i | i <- [0..size0 s - 1]]

unsafeIndex0 :: Seq0 a -> Int -> a
unsafeIndex0 (Seq0 _ s buf) i = let
  i' = i - size0 s * arity
  in if i' >= 0 then buf `V.unsafeIndex` i'
     else case i `divMod` arity of
       (bucket, offset) -> s `unsafeIndex0` bucket `V.unsafeIndex` offset

unsnoc0 :: Seq0 a -> Maybe (Seq0 a, a)
unsnoc0 (Seq0 0 _ _) = Nothing
unsnoc0 (Seq0 n s buf) =
  if V.null buf then case unsnoc0 s of
    Nothing -> Nothing
    Just (s, buf) -> unsnoc0 (Seq0 n s buf)
  else Just (Seq0 (n-1) s (V.take (V.length buf - 1) buf), V.last buf)

--take0 :: Int -> Seq0 a -> Seq0 a
--take0 k s0@(Seq n s buf) =
--  if k > n then s0
--  else let
--    k' = k -  s * arity
--    in if k' > 0 then Seq k empty (V.take k' buf)
--       else case (k-1) `divMod` arity of
--         (bucket, offset) -> Seq k (take0 bucketV.take s `take`
--         Seq k (V.take
--  else
-- reverse0 :: Seq0 a -> Sequence a
-- reverse0 (Seq0 n s buf) =

toList0 :: Seq0 a -> [a]
toList0 v = map (unsafeIndex0 v) [0 .. size0 v - 1]

fromList0 :: [a] -> Seq0 a
fromList0 = foldl' snoc0 empty0

instance Functor Seq0 where
  fmap f (Seq0 n s buf) = Seq0 n (fmap (fmap f) s) (fmap f buf)

instance Foldable Seq0 where
  foldMap f = foldl' (\acc a -> acc `mappend` f a) mempty
  foldl' f z v = foldl' f z (toList0 v)

instance Traversable Seq0 where
  traverse f v = fromList0 <$> traverse f (toList0 v)

data Sequence a =
  Sequence { size  :: Int
           , left :: Seq0 a
           , mid :: ~(Sequence (Seq0 a))
           , sizeMid :: Int --  left + size mid
           , right :: Seq0 a }

empty :: Sequence a
empty = Sequence 0 empty0 empty 0 empty0

snoc :: Sequence a -> a -> Sequence a
snoc (Sequence n l m sm r) a = Sequence (n+1) l m sm (r `snoc0` a)

cons :: a -> Sequence a -> Sequence a
cons a (Sequence n l m sm r) = Sequence (n+1) (l `snoc0` a) m (sm+1) r

conc :: Sequence a -> Sequence a -> Sequence a
conc s1 s2 =
  if size s1 * 2 < size s2 then foldl' (flip cons) s2 (toList s1)
  else if size s2 * 2 < size s1 then foldl' snoc s1 (toList s2)
  else Sequence (size s1 + size s2) (left s1) mid' sizeMid' (right s2)
  where
  snocSeq :: Sequence (Seq0 a) -> Seq0 a -> Sequence (Seq0 a)
  snocSeq s s0 = if size0 s0 == 0 then s else s `snoc` s0
  mid' = mid s1 `snocSeq` right s1 `snocSeq` reverse0 (left s2) `conc` (mid s2)
  sizeMid' = sizeMid s1 + size0 (right s1) + sizeMid s2

reverse :: Sequence a -> Sequence a
reverse (Sequence n l m sm r) = Sequence n r (reverse0 <$> reverse m) sm' l
  where sm' = size0 r + (sm - size0 l)

unsafeIndex :: Sequence a -> Int -> a
unsafeIndex (Sequence _ l m sm r) i
  | i < size0 l = unsafeIndex0 l (size0 l - i - 1)
  | i < sm        = case i `divMod` arity of
     (bucket, offset) -> unsafeIndex m bucket `unsafeIndex0` offset
  | otherwise     = unsafeIndex0 r (i - sm)

instance Semigroup (Sequence a) where (<>) = mappend
instance Monoid (Sequence a) where
  mempty = empty
  mappend = conc

instance Functor Sequence where
  fmap f (Sequence n l mid mr r) =
    Sequence n (f <$> l) (fmap f <$> mid) mr (f <$> r)

instance Foldable Sequence where
  foldMap f s = foldMap f (toList s)
  toList (Sequence _ l mid _ r) =
    DL.reverse (toList l) ++
    (if size mid == 0 then [] else join (toList <$> toList mid)) ++
    toList r

instance Show a => Show (Sequence a) where
  show s = show $ toList s

fromList :: [a] -> Sequence a
fromList = foldl' snoc empty

