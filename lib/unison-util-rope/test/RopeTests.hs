-- Unison.Util.Rope against a model: a rope of string chunks is its string.
module RopeTests (test) where

import Control.Monad
import Data.Bits (shiftR, xor)
import Data.Functor.Identity (Identity (..))
import Data.IORef
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import EasyTest
import GHC.Stack (HasCallStack)
import Unison.Util.Rope (Rope)
import Unison.Util.Rope qualified as R

newtype C = C String
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

instance R.Sized C where size (C s) = length s

instance R.Take C where take n (C s) = C (take n s)

instance R.Drop C where drop n (C s) = C (drop n s)

instance R.Index C Char where unsafeIndex i (C s) = s !! i

instance R.Reverse C where reverse (C s) = C (reverse s)

str :: Rope C -> String
str r = concat [s | C s <- R.chunks r]

-- The rope's invariants hold and it holds the model's characters.
same :: (HasCallStack) => Rope C -> String -> Test ()
same r s = do
  either crash pure (R.valid r)
  expectEqual' (R.size r) (length s)
  expectEqual' (str r) s

-- n characters that differ from their neighbours and from what is far away
text :: Int -> String
text n = take n (concatMap (\i -> show (i * i + 7 :: Int) ++ "abcdefghijklmnopqrstuvwxyz" !! (i `mod` 26) : []) [0 :: Int ..])

-- Cut a string into chunks whose sizes cycle through the given ones.
pieces :: [Int] -> String -> [C]
pieces sizes = go (cycle sizes)
  where
    go _ [] = []
    go (k : ks) xs = let (a, b) = splitAt k xs in C a : go ks b
    go [] _ = []

-- the smallest size of chunk that is never joined to another of that size
big :: Int
big = R.threshold `div` 2 + 1

-- The same text built in different ways, so with different chunks and shapes.
shapes :: String -> [(String, Rope C)]
shapes s =
  [ ("snoc chars", L.foldl' R.snoc mempty (pieces [1] s)),
    ("cons chars", foldr R.cons mempty (pieces [1] s)),
    ("snoc big", L.foldl' R.snoc mempty (pieces [big] s)),
    ("cons big", foldr R.cons mempty (pieces [big] s)),
    ("snoc mixed", R.fromChunks (pieces [1, 2, 5, 40, 3, 100, 33, 17, 64, 9] s)),
    ("cons mixed", foldr R.cons mempty (pieces [70, 1, 1, 31, 32, 33, 4, 200] s)),
    ("outward", outward),
    ("append left", L.foldl' (<>) mempty (map (R.fromChunks . pieces [big]) (cut [1, 20, 50, 190, 400, 1700] s))),
    ("append right", foldr (<>) mempty (map (R.fromChunks . pieces [big, 3]) (cut [900, 310, 120, 70, 30, 1] s))),
    ("append balanced", balanced s),
    ("drop", R.drop 371 (R.fromChunks (pieces [big] (text 371 ++ s)))),
    ("take", R.take n (R.fromChunks (pieces [big, 50] (s ++ text 913)))),
    ("slice", R.take n (R.drop 5000 (foldr R.cons mempty (pieces [big] (text 5000 ++ s ++ text 5000)))))
  ]
  where
    n = length s
    cut sizes xs = [a | C a <- pieces sizes xs]
    outward =
      let (front, back) = splitAt (n `div` 2) s
       in L.foldl' R.snoc (L.foldl' (flip R.cons) mempty (reverse (pieces [big] front))) (pieces [big] back)
    balanced ys
      | length ys <= 3 * big = R.fromChunks (pieces [big] ys)
      | otherwise = let (a, b) = splitAt (length ys `div` 2) ys in balanced a <> balanced b

sizes :: [Int]
sizes = [0 .. 70] ++ [100, 127, 128, 129, 255, 256, 257, 500, 1000, 3000, 10000, 40000, 200000]

-- positions to look at in a text of n characters: all of them if it is short
spots :: Int -> [Int]
spots n
  | n <= 300 = [-1 .. n + 1]
  | otherwise = L.nub ([-1 .. 40] ++ [n - 40 .. n + 1] ++ [(i * 7919 + 13) `mod` n | i <- [1 .. 150]])

forShapes :: [Int] -> (Int -> String -> Rope C -> Test ()) -> Test ()
forShapes ns f =
  forM_ ns \n -> do
    let s = text n
    forM_ (shapes s) \(name, r) -> scope (name ++ " " ++ show n) (f n s r)

test :: Test ()
test =
  tests
    [ scope "empty" do
        same mempty ""
        same (R.one (C "")) ""
        same (R.cons (C "") mempty) ""
        same (R.snoc mempty (C "")) ""
        same (R.take 3 mempty) ""
        same (R.drop 3 mempty) ""
        expect' (R.null (mempty :: Rope C))
        expect' (R.index 0 (mempty :: Rope C) == (Nothing :: Maybe Char))
        expect' (fmap fst (R.uncons (mempty :: Rope C)) == Nothing)
        expect' (fmap snd (R.unsnoc (mempty :: Rope C)) == Nothing)
        ok,
      scope "shapes" do
        forShapes sizes \_ s r -> same r s
        ok,
      scope "deep" do
        -- a text of 200000 characters in chunks of 'big' is several levels deep
        let r = L.foldl' R.snoc mempty (pieces [big] (text 200000))
        expect' (R.debugDepth r >= 3)
        ok,
      scope "one" do
        -- a text that fits in a chunk is one chunk however it is built
        forM_ [1 .. R.threshold] \n ->
          forM_ (take 4 (shapes (text n))) \(_, r) -> case r of
            R.One _ -> pure ()
            _ -> crash ("not One at " ++ show n)
        ok,
      scope "index" do
        forShapes small \n s r ->
          forM_ (spots n) \i ->
            expectEqual' (R.index i r) (if i >= 0 && i < n then Just (s !! i) else Nothing)
        ok,
      scope "take" do
        forShapes small \n s r -> forM_ (spots n) \i -> same (R.take i r) (take i s)
        ok,
      scope "drop" do
        forShapes small \n s r -> forM_ (spots n) \i -> same (R.drop i r) (drop i s)
        ok,
      scope "slice" do
        forShapes [0, 1, 5, 33, 64, 257, 3000, 40000] \n s r ->
          forM_ (take 30 (spots n)) \i -> forM_ [0, 1, 7, 31, 32, 33, 100, 5000] \len -> do
            same (R.take len (R.drop i r)) (take len (drop i s))
            same (R.drop i (R.take (i + len) r)) (drop i (take (i + len) s))
        ok,
      scope "uncons" do
        forShapes (filter (<= 10000) sizes) \_ s r -> do
          let go x acc = case R.uncons x of
                Nothing -> pure (reverse acc)
                Just (C c, x') -> do
                  either crash pure (R.valid x')
                  expect' (not (null c))
                  expectEqual' (R.size x') (R.size x - length c)
                  go x' (c : acc)
          cs <- go r []
          expectEqual' (concat cs) s
        ok,
      scope "unsnoc" do
        forShapes (filter (<= 10000) sizes) \_ s r -> do
          let go x acc = case R.unsnoc x of
                Nothing -> pure acc
                Just (x', C c) -> do
                  either crash pure (R.valid x')
                  expectEqual' (R.size x') (R.size x - length c)
                  go x' (c : acc)
          cs <- go r []
          expectEqual' (concat cs) s
        ok,
      scope "alternating ends" do
        -- take a chunk from each end in turn, putting a character back now and then
        forShapes [100, 1000, 10000] \_ s r -> do
          let go :: Int -> Rope C -> String -> Test ()
              go k x m
                | R.null x = expect' (null m)
                | k `mod` 5 == 0 = step k (R.cons (C "<") x) ('<' : m)
                | k `mod` 7 == 0 = step k (R.snoc x (C ">")) (m ++ ">")
                | even k, Just (C c, x') <- R.uncons x = step k x' (drop (length c) m)
                | Just (x', C c) <- R.unsnoc x = step k x' (take (length m - length c) m)
                | otherwise = crash "nothing to remove"
              step k x m = do
                either crash pure (R.valid x)
                when (k `mod` 16 == 0) (expectEqual' (str x) m)
                go (k + 1) x m
          go 1 r s
        ok,
      scope "append" do
        forM_ [0, 1, 2, 16, 17, 31, 32, 33, 40, 64, 100, 257, 1000, 5000] \a ->
          forM_ [0, 1, 3, 15, 16, 17, 32, 33, 50, 129, 700, 3000, 40000] \b -> do
            let sa = text a
                sb = reverse (text b)
            forM_ (pick (shapes sa)) \(na, ra) -> forM_ (pick (shapes sb)) \(nb, rb) ->
              scope (na ++ " " ++ show a ++ " ++ " ++ nb ++ " " ++ show b) do
                same (ra <> rb) (sa ++ sb)
                same (R.two ra rb) (sa ++ sb)
        ok,
      scope "append many" do
        -- the builder pattern, with pieces of every small size
        forM_ [1, 2, 3, 5, 8, 15, 16, 17, 31, 32, 33, 50] \k -> do
          let s = text 6000
              r = L.foldl' (\acc c -> acc <> R.one c) mempty (pieces [k] s)
              l = foldr (\c acc -> R.one c <> acc) mempty (pieces [k] s)
          same r s
          same l s
        ok,
      scope "equality and order" do
        forM_ [0, 1, 5, 32, 33, 100, 1000, 10000] \n -> do
          let s = text n
              rs = shapes s
              other = if n == 0 then "x" else take (n - 1) s ++ [succ (last s)]
              shorter = take (n - 1) s
              early = if n < 3 then "~" else take (n `div` 2) s ++ "~" ++ drop (n `div` 2 + 1) s
          forM_ rs \(_, a) -> forM_ rs \(_, b) -> do
            expect' (a == b)
            expectEqual' (compare a b) EQ
          forM_ (pick rs) \(_, a) -> forM_ [other, shorter, early, s ++ "z"] \t ->
            forM_ (pick (shapes t)) \(_, b) -> do
              expectEqual' (a == b) (s == t)
              expectEqual' (compare a b) (compare s t)
              expectEqual' (compare b a) (compare t s)
        ok,
      scope "reverse" do
        forShapes (filter (<= 10000) sizes) \_ s r -> do
          same (R.reverse r) (reverse s)
          same (R.reverse (R.reverse r)) s
        ok,
      scope "map and traverse" do
        forShapes [0, 1, 20, 33, 500, 10000] \_ s r -> do
          let up (C c) = C (map succ c)
              -- a function that changes sizes, some of them to nothing
              thin (C c) = C (filter (/= 'a') (take 3 c))
          same (R.map up r) (map succ s)
          same (runIdentity (R.traverse (Identity . up) r)) (map succ s)
          same (R.map thin r) (concat [t | c <- R.chunks r, let C t = thin c])
          expectEqual' (R.flatten r) (C s)
        ok,
      scope "positions" do
        forShapes [0, 1, 20, 33, 500, 10000] \_ s r -> do
          let visit o (C c) = ([(o, c)], ())
              (seen, ()) = R.traverseWithPos_ visit r
          forM_ seen \(o, c) -> expectEqual' c (take (length c) (drop o s))
          expectEqual' (concatMap snd seen) s
        ok,
      scope "extractChunk" do
        forShapes [1, 20, 33, 500, 10000] \n s r ->
          forM_ (filter (\i -> i >= 0 && i < n) (spots n)) \i -> forM_ [1, 2, 8, 40] \len ->
            when (i + len <= n) do
              let C c = R.extractChunk i len r
              expect' (length c >= len)
              expectEqual' c (take (length c) (drop i s))
        ok,
      scope "random" (randomOps 60000)
    ]
  where
    -- a few of the shapes: the pairs of all of them are too many
    small = filter (<= 40000) sizes
    pick xs = [x | (i, x) <- zip [0 :: Int ..] xs, i `elem` [0, 2, 4, 9, 12]]

-- Random operations on a pool of ropes, each on the results of earlier ones.
randomOps :: Int -> Test ()
randomOps steps = do
  seed <- io (newIORef (0x9E3779B97F4A7C15 :: Word))
  let next :: Int -> Test Int
      next bound = io do
        x0 <- readIORef seed
        let x1 = x0 `xor` (x0 `shiftR` 12)
            x2 = x1 `xor` (x1 * 33554432)
            x3 = x2 `xor` (x2 `shiftR` 27)
        writeIORef seed x3
        pure (fromIntegral ((x3 * 2685821657736338717) `shiftR` 33) `mod` max 1 bound)
      slots = 8 :: Int
      start = Map.fromList [(i, (mempty :: Rope C, "")) | i <- [0 .. slots - 1]]
      loop :: Int -> Map.Map Int (Rope C, String) -> Test ()
      loop k pool
        | k >= steps = pure ()
        | otherwise = do
            i <- next slots
            j <- next slots
            dst <- next slots
            op <- next 12
            let (r, s) = pool Map.! i
                (r2, s2) = pool Map.! j
                n = length s
            len <- next 80
            let new = text (k + 200)
                piece = take (if len < 60 then len `mod` 6 else len) (drop (k `mod` 100) new)
            pos <- next (n + 1)
            (r', s') <- case op of
              0 -> pure (R.snoc r (C piece), s ++ piece)
              1 -> pure (R.cons (C piece) r, piece ++ s)
              2 -> pure (R.take pos r, take pos s)
              3 -> pure (R.drop pos r, drop pos s)
              4 | n + length s2 <= 60000 -> pure (r <> r2, s ++ s2)
              5 | n + length s2 <= 60000 -> pure (r2 <> r, s2 ++ s)
              6 -> pure (maybe (r, s) (\(C c, x) -> (x, drop (length c) s)) (R.uncons r))
              7 -> pure (maybe (r, s) (\(x, C c) -> (x, take (n - length c) s)) (R.unsnoc r))
              8 -> pure (R.take len (R.drop pos r), take len (drop pos s))
              9 -> pure (R.drop 1 r, drop 1 s)
              10 -> pure (R.take (n - 1) r, take (n - 1) s)
              _ -> pure (r <> R.one (C piece) <> r, s ++ piece ++ s)
            let (r'', s'') = if length s' > 60000 then (R.take 1000 r', take 1000 s') else (r', s')
            either (\e -> crash ("step " ++ show k ++ ", op " ++ show op ++ ": " ++ e)) pure (R.valid r'')
            unless (R.size r'' == length s'') $ crash ("step " ++ show k ++ ", op " ++ show op ++ ": size")
            when (k `mod` 20 == 0 || length s'' < 300) $
              unless (str r'' == s'') $
                crash ("step " ++ show k ++ ", op " ++ show op ++ ": content")
            when (n > 0) do
              let at = R.index pos r :: Maybe Char
              unless (at == (if pos < n then Just (s !! pos) else Nothing)) $ crash ("step " ++ show k ++ ": index")
            loop (k + 1) (Map.insert dst (r'', s'') pool)
  loop 0 start
  -- at the end every rope in the pool has been compared in full at least once
  ok
