module Main (main) where

import Control.DeepSeq (rnf)
import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad
import Data.Foldable qualified as F
import Data.Functor.Classes (liftCompare, liftEq)
import Data.List qualified as L
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import EasyTest
import GHC.Exts qualified as Exts
import GHC.Stack (HasCallStack)
import RopeTests qualified
import Unison.Util.Deque (Deque)
import Unison.Util.Deque qualified as D

main :: IO ()
main = run (tests [scope "util.deque" test, scope "util.rope" RopeTests.test])

-- The deque's invariants hold and it has the same elements as the model.
same :: (HasCallStack) => Deque Int -> Seq Int -> Test ()
same d s = do
  either crash pure (D.valid d)
  expectEqual' (D.size d) (Seq.length s)
  expectEqual' (D.toList d) (F.toList s)

-- The same sequence built in different ways, so with different shapes inside.
shapes :: [Int] -> [(String, Deque Int)]
shapes xs =
  [ ("snoc", L.foldl' D.snoc D.empty xs),
    ("fromList", D.fromList xs),
    ("cons", foldr D.cons D.empty xs),
    ("outward", outward),
    ("append left", L.foldl' D.append D.empty (map D.fromList (chunks [1, 2, 5, 9, 14, 23, 40, 77] xs))),
    ("append right", foldr D.append D.empty (map D.fromList (chunks [64, 31, 12, 7, 3, 1, 100] xs))),
    ("append balanced", balanced xs),
    ("drop", D.drop 37 (D.fromList ([1 .. 37] ++ xs))),
    ("take", D.take n (D.fromList (xs ++ [1 .. 91]))),
    ("slice", D.take n (D.drop 500 (foldr D.cons D.empty ([1 .. 500] ++ xs ++ [1 .. 500]))))
  ]
  where
    n = length xs
    outward =
      let (front, back) = splitAt (n `div` 2) xs
       in L.foldl' D.snoc (L.foldl' (flip D.cons) D.empty (reverse front)) back
    balanced ys
      | length ys <= 5 = D.fromList ys
      | otherwise = let (a, b) = splitAt (length ys `div` 2) ys in D.append (balanced a) (balanced b)

-- Cut a list into pieces whose sizes cycle through the given ones.
chunks :: [Int] -> [a] -> [[a]]
chunks sizes = go (cycle sizes)
  where
    go _ [] = []
    go (k : ks) xs = let (a, b) = splitAt k xs in a : go ks b
    go [] _ = []

sizes :: [Int]
sizes = [0 .. 100] ++ [127, 128, 129, 255, 256, 257, 511, 512, 513, 700, 1000, 4095, 4096, 4097, 10000]

forShapes :: [Int] -> (Int -> Seq Int -> Deque Int -> Test ()) -> Test ()
forShapes ns f =
  forM_ ns \n -> do
    let xs = [1 .. n]
    forM_ (shapes xs) \(name, d) -> scope (name ++ " " ++ show n) (f n (Seq.fromList xs) d)

test :: Test ()
test =
  tests
    [ scope "fromListN takes exactly n" do
        expectEqual [1 .. 25] (D.toList (D.fromListN 25 [1 .. 100 :: Int]))
        expectEqual (Right ()) (D.valid (D.fromListN 25 [1 .. 100 :: Int]))
        expectEqual [1, 2, 3] (D.toList (D.fromListN 3 [1 .. 10 :: Int]))
        -- the appends that once walked an oversized suffix
        expectEqual (Right ()) (D.valid (D.fromListN 20 [1 .. 1000 :: Int] D.>< D.fromList [1, 2 :: Int]))
        expectEqual 22 (D.size (D.fromListN 20 [1 .. 1000 :: Int] D.>< D.fromList [1, 2 :: Int]))
        short <- io (try (evaluate (D.size (D.fromListN 5 [1, 2 :: Int]))))
        expect (either (const True) (const False) (short :: Either ErrorCall Int)),
      scope "empty" do
        same D.empty Seq.empty
        expect' (D.size (D.empty :: Deque Int) == 0)
        expect' (null (D.empty :: Deque Int))
        expect' (D.lookup 0 (D.empty :: Deque Int) == Nothing)
        expect' (fmap fst (D.uncons (D.empty :: Deque Int)) == Nothing)
        expect' (fmap snd (D.unsnoc (D.empty :: Deque Int)) == Nothing)
        same (D.take 3 D.empty) Seq.empty
        same (D.drop 3 D.empty) Seq.empty
        same (D.append D.empty D.empty) Seq.empty
        same (D.fromList []) Seq.empty
        ok,
      scope "singleton" do
        same (D.cons 1 D.empty) (Seq.singleton 1)
        same (D.snoc D.empty 1) (Seq.singleton 1)
        case D.uncons (D.snoc D.empty (1 :: Int)) of
          Just (x, d) -> expect' (x == 1) >> same d Seq.empty
          Nothing -> crash "uncons of a singleton"
        case D.unsnoc (D.cons (1 :: Int) D.empty) of
          Just (d, x) -> expect' (x == 1) >> same d Seq.empty
          Nothing -> crash "unsnoc of a singleton"
        ok,
      scope "shapes" do
        forShapes sizes \_ s d -> same d s
        ok,
      scope "uncons to empty" do
        forShapes ([0 .. 70] ++ [513, 3000]) \_ s0 d0 ->
          let go d s = case (D.uncons d, Seq.viewl s) of
                (Nothing, Seq.EmptyL) -> pure ()
                (Just (x, d'), y Seq.:< s') -> do
                  expectEqual' x y
                  either crash pure (D.valid d')
                  expectEqual' (D.size d') (Seq.length s')
                  go d' s'
                _ -> crash "uncons disagrees with the model"
           in go d0 s0
        ok,
      scope "unsnoc to empty" do
        forShapes ([0 .. 70] ++ [513, 3000]) \_ s0 d0 ->
          let go d s = case (D.unsnoc d, Seq.viewr s) of
                (Nothing, Seq.EmptyR) -> pure ()
                (Just (d', x), s' Seq.:> y) -> do
                  expectEqual' x y
                  either crash pure (D.valid d')
                  expectEqual' (D.size d') (Seq.length s')
                  go d' s'
                _ -> crash "unsnoc disagrees with the model"
           in go d0 s0
        ok,
      scope "alternating ends to empty" do
        forShapes [0, 1, 2, 9, 10, 11, 64, 100, 513, 3000] \_ s0 d0 ->
          let go front d s
                | Seq.null s = same d s
                | front = case D.uncons d of
                    Just (x, d') -> expectEqual' (Just x) (Seq.lookup 0 s) >> either crash pure (D.valid d') >> go False d' (Seq.drop 1 s)
                    Nothing -> crash "uncons of a non-empty deque"
                | otherwise = case D.unsnoc d of
                    Just (d', x) -> expectEqual' (Just x) (Seq.lookup (Seq.length s - 1) s) >> either crash pure (D.valid d') >> go True d' (Seq.take (Seq.length s - 1) s)
                    Nothing -> crash "unsnoc of a non-empty deque"
           in go True d0 s0
        ok,
      -- Pushing and popping around every size, at both ends: each repair is
      -- done and undone repeatedly.
      scope "seesaw" do
        forM_ [0 .. 700] \n -> do
          let d0 = D.fromList [1 .. n]
              s0 = Seq.fromList [1 .. n]
              step (d, s) i = case i `mod` 6 of
                0 -> (D.cons i d, i Seq.<| s)
                1 -> (maybe d snd (D.uncons d), Seq.drop 1 s)
                2 -> (maybe d snd (D.uncons d), Seq.drop 1 s)
                3 -> (D.snoc d i, s Seq.|> i)
                4 -> (maybe d fst (D.unsnoc d), Seq.take (Seq.length s - 1) s)
                _ -> (D.cons i d, i Seq.<| s)
              states = scanl step (d0, s0) [0 .. 40 :: Int]
          forM_ states (uncurry same)
        ok,
      scope "index" do
        forShapes sizes \n s d -> do
          forM_ [0 .. n - 1] \i -> expectEqual' (D.lookup i d) (Seq.lookup i s)
          expect' (D.lookup (-1) d == Nothing)
          expect' (D.lookup n d == Nothing)
          expect' (D.lookup (n + 1) d == Nothing)
          expect' (D.lookup minBound d == Nothing)
          expect' (D.lookup maxBound d == Nothing)
        ok,
      scope "take and drop" do
        scope "every position" $ forShapes ([0 .. 40] ++ [63, 64, 65, 100, 200]) \n s d ->
          forM_ [-1 .. n + 1] \k -> do
            same (D.take k d) (Seq.take k s)
            same (D.drop k d) (Seq.drop k s)
        scope "some positions" $ forShapes [513, 1000, 4097, 10000] \n s d ->
          replicateM_ 20 do
            k <- int' 0 n
            same (D.take k d) (Seq.take k s)
            same (D.drop k d) (Seq.drop k s)
        scope "slices" $ forShapes [100, 1000, 10000] \n s d ->
          replicateM_ 20 do
            i <- int' 0 n
            j <- int' 0 (n - i)
            same (D.take j (D.drop i d)) (Seq.take j (Seq.drop i s))
            same (D.drop i (D.take (i + j) d)) (Seq.take j (Seq.drop i s))
        -- what take and drop return can be used like any other deque
        scope "then used" $ forShapes [30, 100, 1000] \n s d ->
          replicateM_ 10 do
            k <- int' 0 n
            usable (D.take k d) (Seq.take k s)
            usable (D.drop k d) (Seq.drop k s)
        ok,
      scope "append" do
        scope "every pair of small sizes" $
          forM_ [0 .. 45] \n -> forM_ [0 .. 45] \m -> do
            let xs = [1 .. n]
                ys = [n + 1 .. n + m]
            same (D.append (D.fromList xs) (D.fromList ys)) (Seq.fromList (xs ++ ys))
            same (D.append (foldr D.cons D.empty xs) (foldr D.cons D.empty ys)) (Seq.fromList (xs ++ ys))
        scope "every pair of shapes" $
          forM_ [(n, m) | n <- ns, m <- ns] \(n, m) -> do
            let xs = [1 .. n]
                ys = [n + 1 .. n + m]
                s = Seq.fromList (xs ++ ys)
            forM_ (shapes xs) \(nameA, a) -> forM_ (shapes ys) \(nameB, b) ->
              scope (nameA ++ " " ++ show n ++ ", " ++ nameB ++ " " ++ show m) (same (D.append a b) s)
        scope "associative" $ replicateM_ 50 do
          [a, b, c] <- replicateM 3 randomDeque
          let s = Seq.fromList (D.toList a ++ D.toList b ++ D.toList c)
          same (D.append a (D.append b c)) s
          same (D.append (D.append a b) c) s
        scope "then used" $ replicateM_ 50 do
          a <- randomDeque
          b <- randomDeque
          usable (D.append a b) (Seq.fromList (D.toList a ++ D.toList b))
        -- A drained from the back and B drained from the front, a step at a
        -- time: every combination of digit sizes next to the seam, with the
        -- levels below in the states (red ones included) that draining leaves.
        scope "drained" $
          forM_ [(na, nb) | na <- [12, 30, 150, 700], nb <- [12, 30, 150, 700]] \(na, nb) -> do
            let drainB (d, s) = (maybe d fst (D.unsnoc d), Seq.take (Seq.length s - 1) s)
                drainF (d, s) = (maybe d snd (D.uncons d), Seq.drop 1 s)
                built n = [(D.fromList [1 .. n], Seq.fromList [1 .. n]), (foldr D.cons D.empty [1 .. n], Seq.fromList [1 .. n])]
                as = concatMap (take 100 . iterate drainB) (built na)
                bs = concatMap (take 100 . iterate drainF) (built nb)
            forM_ as \(a, sa) -> forM_ bs \(b, sb) -> do
              let ab = D.append a b
              either crash pure (D.valid ab)
              expectEqual' (D.size ab) (Seq.length sa + Seq.length sb)
              -- (comparing every element of every pair would take too long)
              when ((D.size a + D.size b) `mod` 7 == 0) (same ab (sa <> sb))
        scope "doubling" do
          let go d s i = when (i < (14 :: Int)) do
                same d s
                expectEqual' (D.lookup (Seq.length s `div` 3) d) (Seq.lookup (Seq.length s `div` 3) s)
                go (D.append d d) (s <> s) (i + 1)
          go (D.fromList [1 .. 7]) (Seq.fromList [1 .. 7]) 0
        ok,
      scope "random operations" do
        replicateM_ 20 do
          steps <- int' 100 3000
          let go :: Int -> Deque Int -> Seq Int -> Test ()
              go i d s = when (i < steps) do
                -- lean towards growing, then towards shrinking
                op <- if i < steps `div` 2 then pick [0, 0, 0, 1, 1, 1, 2, 3, 4, 5, 6, 7] else pick [0, 1, 2, 2, 2, 3, 3, 3, 4, 5, 6, 7 :: Int]
                (d', s') <- case op of
                  0 -> pure (D.cons i d, i Seq.<| s)
                  1 -> pure (D.snoc d i, s Seq.|> i)
                  2 -> case D.uncons d of
                    Nothing -> expect' (Seq.null s) >> pure (d, s)
                    Just (x, r) -> expectEqual' (Just x) (Seq.lookup 0 s) >> pure (r, Seq.drop 1 s)
                  3 -> case D.unsnoc d of
                    Nothing -> expect' (Seq.null s) >> pure (d, s)
                    Just (r, x) -> expectEqual' (Just x) (Seq.lookup (Seq.length s - 1) s) >> pure (r, Seq.take (Seq.length s - 1) s)
                  4 -> do
                    k <- int' 0 (Seq.length s)
                    keep <- int' 0 3 -- mostly cut a little
                    let k' = if keep == 0 then k else Seq.length s - k `div` 16
                    pure (D.take k' d, Seq.take k' s)
                  5 -> do
                    k <- int' 0 (Seq.length s)
                    keep <- int' 0 3
                    let k' = if keep == 0 then k else k `div` 16
                    pure (D.drop k' d, Seq.drop k' s)
                  6 -> do
                    other <- randomDeque
                    front <- bool
                    let o = Seq.fromList (D.toList other)
                    pure if front then (D.append other d, o <> s) else (D.append d other, s <> o)
                  _ -> do
                    k <- int' (-1) (Seq.length s)
                    expectEqual' (D.lookup k d) (Seq.lookup k s)
                    pure (d, s)
                expectEqual' (D.size d') (Seq.length s')
                if Seq.length s' <= 300 || i `mod` 50 == 0 then same d' s' else either crash pure (D.valid d')
                go (i + 1) d' s'
          d0 <- randomDeque
          go 0 d0 (Seq.fromList (D.toList d0))
        ok,
      scope "queue" do
        -- a long run of snoc at one end and uncons at the other
        let go :: Int -> Deque Int -> Seq Int -> Test ()
            go i d s = when (i < 30000) do
              let (d1, s1) = (D.snoc (D.snoc d i) (-i), s Seq.|> i Seq.|> (-i))
              (d2, s2) <-
                if i `mod` 3 == 0
                  then pure (d1, s1)
                  else case D.uncons d1 of
                    Just (x, r) -> expectEqual' (Just x) (Seq.lookup 0 s1) >> pure (r, Seq.drop 1 s1)
                    Nothing -> crash "uncons of a non-empty deque"
              when (i `mod` 997 == 0) (same d2 s2)
              go (i + 1) d2 s2
        go 0 D.empty Seq.empty
        ok,
      scope "folds" do
        forShapes ([0 .. 30] ++ [64, 100, 513, 1000, 4097]) \n s d -> do
          expectEqual' (F.toList d) (F.toList s)
          expectEqual' (length d) n
          expectEqual' (null d) (n == 0)
          expectEqual' (sum d) (sum s)
          expectEqual' (foldr (:) [] d) (foldr (:) [] s)
          expectEqual' (F.foldl' (flip (:)) [] d) (F.foldl' (flip (:)) [] s)
          expectEqual' (foldl (flip (:)) [] d) (foldl (flip (:)) [] s)
          expectEqual' (foldMap (\x -> [x, x]) d) (foldMap (\x -> [x, x]) s)
          expectEqual' (elem n d) (elem n s)
          expectEqual' (maximum (0 : F.toList d)) (maximum (0 : F.toList s))
          -- foldr is lazy: a prefix of the list needs only a prefix of the fold
          expectEqual' (take 3 (foldr (:) [] d)) (take 3 (F.toList s))
          expectEqual' (foldr (\x _ -> x) 0 d) (foldr (\x _ -> x) 0 s)
        ok,
      scope "instances" do
        forShapes [0, 1, 5, 64, 1000] \n s d -> do
          same (d <> d) (s <> s)
          same (mempty <> d <> mempty) s
          same (mconcat [d, d, d]) (mconcat [s, s, s])
          same (fmap (* 3) d) (fmap (* 3) s)
          r <- io (traverse (\x -> pure (x + 1)) d)
          same r (fmap (+ 1) s)
          expectEqual' (traverse (\x -> if x == 3 then Nothing else Just x) d == Nothing) (n >= 3)
          expectEqual' (show d) (show s)
          expectEqual' (showsPrec 11 d "") (showsPrec 11 s "")
          expect' (rnf d == ())
          same (Exts.fromList (Exts.toList d)) s
          -- equality and order agree with the model, whatever the shapes
          forM_ (shapes [1 .. n]) \(_, d2) -> do
            expect' (d == d2)
            expectEqual' (compare d d2) EQ
            expect' (liftEq (==) d d2)
            expectEqual' (liftCompare compare d d2) EQ
          forM_ [(D.snoc d 0, s Seq.|> 0), (D.cons 0 d, 0 Seq.<| s), (D.drop 1 d, Seq.drop 1 s), (D.take (n - 1) d, Seq.take (n - 1) s), (fmap negate d, fmap negate s)] \(d2, s2) -> do
            expectEqual' (d == d2) (s == s2)
            expectEqual' (compare d d2) (compare s s2)
            expectEqual' (compare d2 d) (compare s2 s)
            expectEqual' (liftCompare (flip compare) d d2) (liftCompare (flip compare) s s2)
        ok,
      scope "the names Data.Sequence uses" do
        forShapes [0, 1, 2, 10, 64, 300] \n s d -> do
          same (0 D.<| d) (0 Seq.<| s)
          same (d D.|> 0) (s Seq.|> 0)
          same (d D.>< d) (s Seq.>< s)
          expectEqual' (D.length d) (Seq.length s)
          expectEqual' (D.null d) (Seq.null s)
          case (d, s) of
            (D.Empty, Seq.Empty) -> pure ()
            (x D.:<| d', y Seq.:<| s') -> expectEqual' x y >> same d' s'
            _ -> crash "the front patterns disagree with the model"
          case (d, s) of
            (D.Empty, Seq.Empty) -> pure ()
            (d' D.:|> x, s' Seq.:|> y) -> expectEqual' x y >> same d' s'
            _ -> crash "the back patterns disagree with the model"
          same (0 D.:<| d) (0 Seq.:<| s)
          same (d D.:|> 0) (s Seq.:|> 0)
          forM_ [-1, 0, 1, n `div` 2, n - 1, n, n + 1] \k -> do
            let (a, b) = D.splitAt k d
                (a', b') = Seq.splitAt k s
            same a a'
            same b b'
          same (D.reverse d) (Seq.reverse s)
          same (D.intersperse 0 d) (Seq.intersperse 0 s)
          let mixed = fmap (\x -> (x * 7919) `mod` 101) d
              mixed' = fmap (\x -> (x * 7919) `mod` 101) s
          same (D.sort mixed) (Seq.sort mixed')
          same (D.unstableSort mixed) (Seq.unstableSort mixed')
          same (D.sortBy (flip compare) mixed) (Seq.sortBy (flip compare) mixed')
          same (D.fromFunction n (* 2)) (Seq.fromFunction n (* 2))
        same (D.Empty :: Deque Int) Seq.empty
        same (D.singleton 7) (Seq.singleton 7)
        -- sortBy is stable
        let pairs = [(x `mod` 5, x) | x <- [1 .. 200 :: Int]]
            byFst a b = compare (fst a) (fst b)
        expectEqual' (F.toList (D.sortBy byFst (D.fromList pairs))) (F.toList (Seq.sortBy byFst (Seq.fromList pairs)))
        ok,
      scope "elements are evaluated" do
        let throws :: Deque Int -> Test ()
            throws d = do
              r <- io (try (evaluate d))
              case r of
                Left (_ :: ErrorCall) -> pure ()
                Right _ -> crash "an unevaluated element was stored"
        throws (D.cons (error "element") D.empty)
        throws (D.snoc (D.fromList [1 .. 100]) (error "element"))
        throws (D.cons (error "element") (D.fromList [1 .. 100]))
        throws (D.fromList [1, 2, error "element", 4])
        ok,
      scope "old versions are unchanged" do
        let d = D.fromList [1 .. 1000 :: Int]
            s = Seq.fromList [1 .. 1000]
            _used = [D.toList (D.cons 0 d), D.toList (D.snoc d 0), D.toList (D.drop 500 d), D.toList (D.append d d)]
        expect' (sum (map length _used) > 0)
        same d s
        ok
    ]
  where
    ns = [0, 1, 2, 3, 4, 8, 9, 10, 11, 20, 21, 64, 73, 100, 513]

-- A deque built in one of several ways, up to a few thousand elements.
randomDeque :: Test (Deque Int)
randomDeque = do
  small <- bool
  n <- if small then int' 0 30 else int' 0 3000
  base <- int' 0 1000000
  snd <$> pick (shapes [base .. base + n - 1])

-- Push and pop at both ends, comparing with the model along the way.
usable :: (HasCallStack) => Deque Int -> Seq Int -> Test ()
usable d0 s0 = do
  same d0 s0
  let pushed = (D.snoc (D.cons 0 d0) 0, (0 Seq.<| s0) Seq.|> 0)
      drain front k (d, s)
        | k == (0 :: Int) = same d s
        | front = case D.uncons d of
            Nothing -> expect' (Seq.null s)
            Just (x, d') -> expectEqual' (Just x) (Seq.lookup 0 s) >> either crash pure (D.valid d') >> drain front (k - 1) (d', Seq.drop 1 s)
        | otherwise = case D.unsnoc d of
            Nothing -> expect' (Seq.null s)
            Just (d', x) -> expectEqual' (Just x) (Seq.lookup (Seq.length s - 1) s) >> either crash pure (D.valid d') >> drain front (k - 1) (d', Seq.take (Seq.length s - 1) s)
  uncurry same pushed
  drain True 40 pushed
  drain False 40 pushed
  forM_ [0, Seq.length s0 `div` 2, Seq.length s0 - 1] \i -> expectEqual' (D.lookup i d0) (Seq.lookup i s0)
