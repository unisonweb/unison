-- Unison.Util.Deque against Data.Sequence, operation by operation.
--
--   stack build --work-dir .stack-work-opt --flag unison-runtime:jit --bench unison-util-rope
--
-- Each benchmark does a whole loop of operations on a sequence of n elements;
-- divide by the count in its name's group for the time of one operation.
module Main (main) where

import Data.Foldable (toList)
import Data.List qualified as L
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Test.Tasty.Bench
import Unison.Util.Deque (Deque)
import Unison.Util.Deque qualified as D

-- positions to index or cut at: 1000 of them, spread over 0..n-1
positions :: Int -> [Int]
positions n = [(i * 7919 + 13) `mod` n | i <- [1 .. 1000]]

main :: IO ()
main =
  defaultMain
    [ bgroup (show n) (group n)
    | n <- [10, 100, 10000, 1000000]
    ]

group :: Int -> [Benchmark]
group n =
  [ bgroup
      "deque"
      [ bench "snoc" $ whnf (L.foldl' D.snoc D.empty) xs,
        bench "cons" $ whnf (L.foldl' (flip D.cons) D.empty) xs,
        bench "fromList" $ whnf D.fromList xs,
        bench "fromList small" $ whnf (L.foldl' (\acc i -> acc + D.size (D.fromList [1 .. i])) 0) nearEnds,
        let x = d in bench "uncons" $ whnf unconsAllD x,
        let x = d in bench "unsnoc" $ whnf unsnocAllD x,
        bench "snoc then uncons" $ whnf (unconsAllD . L.foldl' D.snoc D.empty) xs,
        bench "cons then unsnoc" $ whnf (unsnocAllD . L.foldl' (flip D.cons) D.empty) xs,
        let x = d in bench "queue" $ whnf (queueD n) x,
        let x = d in bench "index" $ whnf (\ps -> L.foldl' (\acc i -> acc + maybe 0 id (D.lookup i x)) 0 ps) ps,
        let x = d in bench "take" $ whnf (\ps -> L.foldl' (\acc i -> acc + D.size (D.take i x)) 0 ps) ps,
        let x = d in bench "drop" $ whnf (\ps -> L.foldl' (\acc i -> acc + D.size (D.drop i x)) 0 ps) ps,
        let x = d in bench "split near ends" $ whnf (\ks -> L.foldl' (\acc i -> acc + D.size (D.drop i x) + D.size (D.take i x) + D.size (D.drop (n - i) x) + D.size (D.take (n - i) x)) 0 ks) nearEnds,
        let x = d in bench "append" $ whnf (L.foldl' (\acc (a, b) -> acc + D.size (D.append a b)) 0) (pairsD x),
        let x = d in bench "append small" $ whnf (\ks -> L.foldl' (\acc i -> acc + D.size (D.append x (D.fromList [1 .. i])) + D.size (D.append (D.fromList [1 .. i]) x)) 0 ks) nearEnds,
        let x = d in bench "sum" $ whnf sum x,
        let x = d in bench "toList" $ whnf (L.foldl' (+) 0 . D.toList) x,
        let x = d in bench "==" $ whnf (\(y, z) -> y == z) (x, L.foldl' (flip D.cons) D.empty (reverse xs))
      ],
    bgroup
      "seq"
      [ bench "snoc" $ whnf (L.foldl' (Seq.|>) Seq.empty) xs,
        bench "cons" $ whnf (L.foldl' (flip (Seq.<|)) Seq.empty) xs,
        bench "fromList" $ whnf Seq.fromList xs,
        bench "fromList small" $ whnf (L.foldl' (\acc i -> acc + Seq.length (Seq.fromList [1 .. i])) 0) nearEnds,
        let x = s in bench "uncons" $ whnf unconsAllS x,
        let x = s in bench "unsnoc" $ whnf unsnocAllS x,
        bench "snoc then uncons" $ whnf (unconsAllS . L.foldl' (Seq.|>) Seq.empty) xs,
        bench "cons then unsnoc" $ whnf (unsnocAllS . L.foldl' (flip (Seq.<|)) Seq.empty) xs,
        let x = s in bench "queue" $ whnf (queueS n) x,
        let x = s in bench "index" $ whnf (\ps -> L.foldl' (\acc i -> acc + maybe 0 id (Seq.lookup i x)) 0 ps) ps,
        let x = s in bench "take" $ whnf (\ps -> L.foldl' (\acc i -> acc + Seq.length (Seq.take i x)) 0 ps) ps,
        let x = s in bench "drop" $ whnf (\ps -> L.foldl' (\acc i -> acc + Seq.length (Seq.drop i x)) 0 ps) ps,
        let x = s in bench "split near ends" $ whnf (\ks -> L.foldl' (\acc i -> acc + Seq.length (Seq.drop i x) + Seq.length (Seq.take i x) + Seq.length (Seq.drop (n - i) x) + Seq.length (Seq.take (n - i) x)) 0 ks) nearEnds,
        let x = s in bench "append" $ whnf (L.foldl' (\acc (a, b) -> acc + Seq.length (a Seq.>< b)) 0) (pairsS x),
        let x = s in bench "append small" $ whnf (\ks -> L.foldl' (\acc i -> acc + Seq.length (x Seq.>< Seq.fromList [1 .. i]) + Seq.length (Seq.fromList [1 .. i] Seq.>< x)) 0 ks) nearEnds,
        let x = s in bench "sum" $ whnf sum x,
        let x = s in bench "toList" $ whnf (L.foldl' (+) 0 . toList) x,
        let x = s in bench "==" $ whnf (\(y, z) -> y == z) (x, L.foldl' (flip (Seq.<|)) Seq.empty (reverse xs))
      ]
  ]
  where
    xs = [1 .. n]
    ps = positions n
    -- 100 pairs of pieces to append
    pairsS x = [(Seq.take i x, Seq.drop i x) | i <- L.take 100 ps]
    -- cuts one to four elements from an end, as list patterns make: 250 rounds of four
    nearEnds = concat (replicate 250 [1, 2, 3, 4 :: Int])
    d = D.fromList xs
    pairsD x = [(D.take i x, D.drop i x) | i <- L.take 100 ps]
    s = Seq.fromList xs

unconsAllD :: Deque Int -> Int
unconsAllD = go 0
  where
    go !acc x = case D.uncons x of
      Nothing -> acc
      Just (e, x') -> go (acc + e) x'

unsnocAllD :: Deque Int -> Int
unsnocAllD = go 0
  where
    go !acc x = case D.unsnoc x of
      Nothing -> acc
      Just (x', e) -> go (acc + e) x'

unconsAllS :: Seq Int -> Int
unconsAllS = go 0
  where
    go !acc x = case x of
      Seq.Empty -> acc
      e Seq.:<| x' -> go (acc + e) x'

unsnocAllS :: Seq Int -> Int
unsnocAllS = go 0
  where
    go !acc x = case x of
      Seq.Empty -> acc
      x' Seq.:|> e -> go (acc + e) x'

-- n rounds of: add one at the back, remove one from the front
queueD :: Int -> Deque Int -> Int
queueD n = go 0 0
  where
    go !i !acc x
      | i == n = acc
      | otherwise = case D.uncons (D.snoc x i) of
          Just (e, x') -> go (i + 1) (acc + e) x'
          Nothing -> acc

queueS :: Int -> Seq Int -> Int
queueS n = go 0 0
  where
    go !i !acc x
      | i == n = acc
      | otherwise = case x Seq.|> i of
          e Seq.:<| x' -> go (i + 1) (acc + e) x'
          Seq.Empty -> acc
