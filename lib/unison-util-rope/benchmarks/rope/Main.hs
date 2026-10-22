{-# LANGUAGE RecordWildCards #-}

-- Unison.Util.Rope, operation by operation, on chunks of text like those of
-- Unison.Util.Text.
--
--   stack bench --work-dir .stack-work-opt --flag unison-runtime:jit unison-util-rope:bench:rope
--
-- Each benchmark does a whole loop of operations on a text of n characters.
-- (The rope is reached through a record of functions so that another
-- implementation can be put beside it: that is how this one was compared with
-- the size-balanced tree it replaced.)
module Main (main) where

import Data.List qualified as L
import Data.Text qualified as T
import Test.Tasty.Bench
import Unison.Util.Rope qualified as R

data Chunk = Chunk {-# UNPACK #-} !Int {-# UNPACK #-} !T.Text

chunk :: T.Text -> Chunk
chunk t = Chunk (T.length t) t

instance Eq Chunk where (Chunk n a) == (Chunk n2 a2) = n == n2 && a == a2

instance Ord Chunk where (Chunk _ a) `compare` (Chunk _ a2) = compare a a2

instance Semigroup Chunk where
  Chunk n a <> Chunk n2 a2 = Chunk (n + n2) (a <> a2)

instance Monoid Chunk where
  mempty = Chunk 0 mempty

instance R.Sized Chunk where size (Chunk n _) = n

instance R.Drop Chunk where
  drop k c@(Chunk n t)
    | k >= n = mempty
    | k <= 0 = c
    | otherwise = Chunk (n - k) (T.drop k t)

instance R.Take Chunk where
  take k c@(Chunk n t)
    | k >= n = c
    | k <= 0 = mempty
    | otherwise = Chunk k (T.take k t)

instance R.Index Chunk Char where
  unsafeIndex i (Chunk _ t) = T.index t i

-- What Unison.Util.Text does with a rope.
data Ops r = Ops
  { emptyR :: r,
    oneR :: Chunk -> r,
    snocR :: r -> Chunk -> r,
    consR :: Chunk -> r -> r,
    appR :: r -> r -> r,
    sizeR :: r -> Int,
    takeR :: Int -> r -> r,
    dropR :: Int -> r -> r,
    atR :: Int -> r -> Maybe Char,
    unconsR :: r -> Maybe (Chunk, r),
    unsnocR :: r -> Maybe (r, Chunk),
    eqR :: r -> r -> Bool,
    cmpR :: r -> r -> Ordering,
    chunksR :: r -> [Chunk]
  }

rope :: Ops (R.Rope Chunk)
rope =
  Ops
    { emptyR = mempty,
      oneR = R.one,
      snocR = R.snoc,
      consR = R.cons,
      appR = (<>),
      sizeR = R.size,
      takeR = R.take,
      dropR = R.drop,
      atR = R.index,
      unconsR = R.uncons,
      unsnocR = R.unsnoc,
      eqR = (==),
      cmpR = compare,
      chunksR = R.chunks
    }

main :: IO ()
main =
  defaultMain
    [ bgroup (show n) (group rope n)
    | n <- [20, 1000, 100000, 1000000]
    ]

-- n characters
sample :: Int -> T.Text
sample n = T.take n (T.concat (L.replicate (n `div` 60 + 1) "the quick brown fox jumps over the lazy dog, 0123456789 times; "))

-- pieces of k characters
pieces :: Int -> T.Text -> [Chunk]
pieces k = L.map chunk . T.chunksOf k

-- positions to index or cut at: 1000 of them, spread over 0..n-1
positions :: Int -> [Int]
positions n = [(i * 7919 + 13) `mod` n | i <- [1 .. 1000]]

group :: forall r. Ops r -> Int -> [Benchmark]
group Ops {..} n =
  [ -- building a text a piece at a time
    bench "snoc 1 char" $ whnf (L.foldl' snocR emptyR) ones,
    bench "snoc 5 chars" $ whnf (L.foldl' snocR emptyR) fives,
    bench "cons 1 char" $ whnf (L.foldl' (flip consR) emptyR) ones,
    bench "append 3 chars" $ whnf (L.foldl' (\acc c -> appR acc (oneR c)) emptyR) threes,
    bench "append 40 chars" $ whnf (L.foldl' (\acc c -> appR acc (oneR c)) emptyR) forties,
    -- taking a text apart a character at a time, as Text.uncons and Text.unsnoc do
    let x = loaded in bench "uncons char" $ whnf unconsChars x,
    let x = loaded in bench "unsnoc char" $ whnf unsnocChars x,
    let x = built in bench "uncons char, built" $ whnf unconsChars x,
    -- taking it apart ten characters at a time, as a parser does
    let x = loaded in bench "drop 10 repeatedly" $ whnf dropTens x,
    let x = loaded in bench "index" $ whnf (L.foldl' (\acc i -> acc + maybe 0 fromEnum (atR i x)) 0) ps,
    let x = built in bench "index, built" $ whnf (L.foldl' (\acc i -> acc + maybe 0 fromEnum (atR i x)) 0) ps,
    let x = loaded in bench "take" $ whnf (L.foldl' (\acc i -> acc + sizeR (takeR i x)) 0) ps,
    let x = loaded in bench "drop" $ whnf (L.foldl' (\acc i -> acc + sizeR (dropR i x)) 0) ps,
    let x = built in bench "take, built" $ whnf (L.foldl' (\acc i -> acc + sizeR (takeR i x)) 0) ps,
    let x = built in bench "drop, built" $ whnf (L.foldl' (\acc i -> acc + sizeR (dropR i x)) 0) ps,
    let x = loaded in bench "split near ends" $ whnf (L.foldl' (\acc i -> acc + sizeR (dropR i x) + sizeR (takeR i x) + sizeR (dropR (n - i) x) + sizeR (takeR (n - i) x)) 0) nearEnds,
    let x = loaded in bench "append" $ whnf (L.foldl' (\acc (a, b) -> acc + sizeR (appR a b)) 0) (pairs x),
    let x = built in bench "append, built" $ whnf (L.foldl' (\acc (a, b) -> acc + sizeR (appR a b)) 0) (pairs x),
    let x = loaded; y = built in bench "==" $ whnf (\(a, b) -> eqR a b) (x, y),
    let x = loaded; y = built in bench "compare" $ whnf (\(a, b) -> cmpR a b) (x, y),
    let x = built in bench "uncons chunk" $ whnf unconsChunks x,
    let x = built in bench "unsnoc chunk" $ whnf unsnocChunks x,
    let x = built in bench "chunks" $ whnf (L.foldl' (\acc (Chunk k _) -> acc + k) 0 . chunksR) x
  ]
  where
    txt = sample n
    ones = pieces 1 txt
    fives = pieces 5 txt
    threes = pieces 3 txt
    forties = pieces 40 txt
    ps = positions n
    nearEnds = concat (replicate 250 [1, 2, 3, 4 :: Int])
    -- as Text.fromText makes it: chunks of 512 characters
    loaded = L.foldl' snocR emptyR (pieces 512 txt)
    -- as a program that appends short pieces makes it
    built = L.foldl' snocR emptyR threes
    -- 100 pairs of pieces to append
    pairs :: r -> [(r, r)]
    pairs x = [(takeR i x, dropR i x) | i <- L.take 100 ps]

    unconsChars :: r -> Int
    unconsChars = go 0
      where
        go !acc x = case atR 0 x of
          Nothing -> acc
          Just c -> go (acc + fromEnum c) (dropR 1 x)

    unsnocChars :: r -> Int
    unsnocChars = go 0
      where
        go !acc x = case sizeR x of
          0 -> acc
          k -> case atR (k - 1) x of
            Nothing -> acc
            Just c -> go (acc + fromEnum c) (takeR (k - 1) x)

    dropTens :: r -> Int
    dropTens = go 0
      where
        go !acc x = case sizeR x of
          0 -> acc
          k -> go (acc + k) (dropR 10 x)

    unconsChunks :: r -> Int
    unconsChunks = go 0
      where
        go !acc x = case unconsR x of
          Nothing -> acc
          Just (Chunk k _, x') -> go (acc + k) x'

    unsnocChunks :: r -> Int
    unsnocChunks = go 0
      where
        go !acc x = case unsnocR x of
          Nothing -> acc
          Just (x', Chunk k _) -> go (acc + k) x'
