{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Util.Text where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad
import Data.List (unfoldr)
import Data.Text qualified as T
import EasyTest
import Unison.Util.Bytes qualified as B
import Unison.Util.Rope qualified as R
import Unison.Util.Text qualified as Text
import Unison.Util.Text.Pattern qualified as P

test :: Test ()
test =
  scope "util.text" . tests $
    [ scope "empty ==" $ expectEqual Text.empty Text.empty,
      -- a case mapping that changes the character count: the size follows
      -- the result, not the input
      scope "toUppercase changes the size" $ do
        let up = Text.toUppercase (Text.pack "straße")
        expectEqual 7 (Text.size up)
        expectEqual "STRASSE" (Text.unpack up)
        expectEqual (Just 'E') (Text.at 6 up),
      scope "toLowercase changes the size" $ do
        let down = Text.toLowercase (Text.pack "İ")
        expectEqual (T.length (T.toLower (T.pack "İ"))) (Text.size down),
      scope "empty `compare`" . expectEqual EQ $ Text.empty `compare` Text.empty,
      scope "==" . expect $
        let a = join (replicate 100 ['a' .. 'z'])
            b = join (replicate 45 ['A' .. 'Z'])
         in (Text.pack a <> Text.pack b) == Text.pack (a ++ b),
      scope "at" $ do
        expectEqual' (Just 'a') $ Text.at 0 (Text.pack "abc")
        expectEqual' Nothing $ Text.at 0 mempty
        ok,
      scope "at.cornerCases" $ do
        let b = Text.drop 3 $ "abc" <> "def"
        expectEqual' (Just 'd') $ Text.at 0 b
        expectEqual' (Just 'a') $ Text.at 0 (mempty <> "abc")
        ok,
      scope "consistency with Text" $ do
        forM_ [(1 :: Int) .. 100] $ \_ -> do
          n <- int' 0 50
          m <- int' 0 50
          k <- int' 0 (n + m)
          o <- int' 0 50
          let ch = pick (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])
          t1 <- T.pack <$> replicateM n ch
          t2 <- T.pack <$> replicateM m ch
          t3 <- T.pack <$> replicateM o ch
          let (t1s, t2s, t3s) = (Text.fromText t1, Text.fromText t2, Text.fromText t3)
          scope "associativity" $ do
            -- note $ show (t1s, t2s, t3s)
            expect' $ t1s <> (t2s <> t3s) == (t1s <> t2s) <> t3s
          scope "<>" . expect' $
            Text.toText (t1s <> t2s <> t3s) == t1 <> t2 <> t3
          scope "Ord" . expect' $
            (t1 <> t2 <> t3) `compare` t3
              == (t1s <> t2s <> t3s) `compare` t3s
          scope "take" . expect' $
            Text.toText (Text.take k (t1s <> t2s)) == T.take k (t1 <> t2)
          scope "drop" . expect' $
            Text.toText (Text.drop k (t1s <> t2s)) == T.drop k (t1 <> t2)
          scope "uncons" . expect' $
            let ts = t1s <> t2s <> t3s
             in unfoldr Text.uncons ts == Text.unpack ts
          scope "unsnoc" . expect' $
            let ts = t1s <> t2s <> t3s
             in unfoldr (\t -> (\(ts, ch) -> (ch, ts)) <$> Text.unsnoc t) ts == reverse (Text.unpack ts)
          scope "at" $
            let bs = t1s <> t2s <> t3s
                b = t1 <> t2 <> t3
             in forM_ [0 .. (T.length b - 1)] $ \ind ->
                  expectEqual' (Just (T.index b ind)) $ Text.at ind bs
        ok,
      scope "lots of chunks" $ do
        forM_ [(0 :: Int) .. 25] $ \_ -> do
          n <- int' 0 50
          k <- int' 200 600
          chunks <- replicateM n (replicateM k char)
          let b1 = foldMap Text.pack chunks
              b2 = foldr (<>) mempty (Text.pack <$> chunks)
              b3 = foldl' (<>) mempty (Text.pack <$> chunks)
              b = T.concat (T.pack <$> chunks)
          expect' $ b1 == b2 && b2 == b3
          expectEqual' b $ Text.toText b1
          expectEqual' b $ Text.toText b2
          expectEqual' b $ Text.toText b3
        ok,
      scope "depth checks" $ do
        chunk <- Text.pack <$> replicateM 1000 char
        forM_ [100, 200, 400] $ \i0 -> do
          n <- int' 200 400
          i <- (i0 +) <$> int' (-10) 10
          let chunks = replicate i chunk
              t1 = foldMap id chunks
              t2 = foldr (<>) mempty chunks
              t3 = foldl' (<>) mempty chunks
              moarChunks = join (replicate n chunks)
              ts =
                [ t1,
                  t2,
                  t3,
                  foldMap id (replicate n t3),
                  foldr (<>) mempty moarChunks,
                  foldl' (<>) mempty moarChunks
                ]
              maxDepth = maximum depths
              depths = map depth ts
          note ("maximum depth for tree with " <> show (i * n) <> " chunks was " <> show maxDepth)
          expect' (maxDepth < log2 (i * n) * 2)
        ok,
      scope "patterns" $ do
        expectEqual' (Just ([], "")) $ P.run P.Eof ""
        expectEqual' (Just ([], "")) $ P.run (P.Char P.Any) "a"
        expectEqual' (Just ([], "")) $ P.run (P.Char (P.CharRange 'a' 'z')) "a"
        expectEqual' Nothing $ P.run (P.Char (P.Not (P.CharRange 'a' 'z'))) "a"
        expectEqual' (Just ([], "bc")) $ P.run (P.Or (P.Char (P.Not (P.CharRange 'a' 'z'))) (P.Char P.Any)) "abc"
        -- this shows that we ignore subcaptures
        expectEqual' (Just (["ab"], "def")) $
          P.run
            (P.Join [P.Capture (P.Join [P.Capture (P.Char P.Any), P.Capture (P.Char P.Any)]), P.Char P.Any])
            "abcdef"
        expectEqual' (Just ([], "ab")) $ P.run (P.Char (P.CharSet "0123")) "3ab"
        expectEqual' (Just ([], "3b")) $ P.run (P.Char (P.Not (P.CharSet "0123"))) "a3b"
        expectEqual' (Just (["a"], "3b")) $ P.run (P.Capture (P.Char (P.Not (P.CharSet "0123")))) "a3b"
        expectEqual' (Just ([], "123")) $ P.run (P.Many True (P.Char (P.CharSet "abcd"))) "babbababac123"
        expectEqual' (Just (["babbababac"], "123")) $
          P.run (P.Capture (P.Many True (P.Char (P.CharSet "abcd")))) "babbababac123"
        expectEqual' (Just (["012345"], "abc")) $
          P.run (P.Capture (P.Many True (P.Char (P.CharClass P.Number)))) "012345abc"
        expectEqual' (Just (["012345", "abc"], "")) $
          P.run
            ( P.Join
                [ P.Capture (P.Many True (P.Char (P.CharClass P.Number))),
                  P.Literal ",",
                  P.Capture (P.Many True (P.Char P.Any))
                ]
            )
            "012345,abc"
        expectEqual' (Just (["01", "10", "20", "1123", "292", "110", "10"], "")) $
          P.run
            ( P.Many
                True
                ( P.Join
                    [ P.Capture (P.Many True (P.Char (P.CharClass P.Number))),
                      P.Many True (P.Char (P.CharClass P.Whitespace))
                    ]
                )
            )
            "01 10 20 1123 292 110 10"
        expectEqual' (Just (["127", "0", "0", "1"], "")) $
          let part = P.Capture (P.Replicate 1 3 (P.Char (P.CharClass P.Number)))
              dpart = P.Join [P.Literal ".", part]
              ip = P.Join [part, P.Replicate 3 3 dpart, P.Eof]
           in P.run ip "127.0.0.1"
        expectEqual' (Just (["1", "2", "3", "4", "5"], "")) $
          let p = P.Replicate 5 8 (P.Capture (P.Char (P.CharClass P.Number)))
           in P.run p "12345"
        expectEqual' (Just ([], "1234")) $
          let p = P.Replicate 5 8 (P.Capture (P.Char (P.CharClass P.Number))) `P.Or` P.Join []
           in P.run p "1234"
        expectEqual' (Just (["1z", "2z", "3z", "4z", "5z"], "6a")) $
          let p = P.Replicate 5 8 (P.Capture (P.Join [P.Char (P.CharClass P.Number), P.Literal "z"])) `P.Or` P.Join []
           in P.run p "1z2z3z4z5z6a"
        -- https://github.com/unisonweb/unison/issues/3530
        expectEqual Nothing $
          let p =
                P.Or
                  (P.Join [P.Literal "a", P.Literal "b"])
                  (P.Join [P.Literal "a", P.Literal "c"])
           in P.run p "aac"
        expectEqual (Just ([], "")) $
          let p =
                P.Or
                  ( P.Capture $
                      ( P.Or
                          (P.Join [P.Literal "a", P.Literal "b"])
                          (P.Join [P.Literal "a", P.Literal "c"])
                      )
                  )
                  (P.Join [P.Literal "aa", P.Literal "cd"])
           in P.run p "aacd"
        -- this is just making sure we don't duplicate captures to our left
        -- when entering an `Or` node
        expectEqual (Just (["@"], "")) $
          let p = P.Join [P.Capture (P.Char P.Any), P.Or (P.Literal "c") (P.Join []), P.Literal "d"]
           in P.run p "@cd"
        expectEqual (Just (["%", "c"], "")) $
          let p = P.Join [P.Capture (P.Char P.Any), (P.Or (P.Capture (P.Literal "c")) (P.Join [])), P.Literal "d"]
           in P.run p "%cd"
        expectEqual (Just ([""], "ac")) $
          let p = P.Capture (P.Or (P.Join [P.Literal "a", P.Literal "b"]) (P.Join []))
           in P.run p "ac"
        expectEqual (Just ([""], "ac")) $
          let p = P.Capture (P.Replicate 0 1 (P.Join [P.Literal "a", P.Literal "b"]))
           in P.run p "ac"
        -- nested or tests
        expectEqual (Just (["zzzaaa", "!"], "!!")) $
          let p =
                P.Or
                  ( P.Or
                      (P.Literal "a")
                      (P.Join [P.Literal "z", P.Replicate 3 5 (P.Literal "z")])
                  )
                  (P.Join [P.Capture (P.Literal "zzzaaa"), P.Capture (P.Literal "!")])
           in P.run p "zzzaaa!!!"
        ok,
      scope "slack" slackTests,
      scope "ordinal" do
        let expectOrdinal = \ord -> expectEqual @String ord . Text.ordinal
        expectOrdinal "1st" 1
        expectOrdinal "2nd" 2
        expectOrdinal "3rd" 3
        expectOrdinal "4th" 4
        expectOrdinal "5th" 5
        expectOrdinal "10th" 10
        expectOrdinal "11th" 11
        expectOrdinal "12th" 12
        expectOrdinal "13th" 13
        expectOrdinal "14th" 14
        expectOrdinal "21st" 21
        expectOrdinal "22nd" 22
        expectOrdinal "23rd" 23
        expectOrdinal "24th" 24
        expectOrdinal "111th" 111
        expectOrdinal "112th" 112
        expectOrdinal "113th" 113
        expectOrdinal "121st" 121
        expectOrdinal "122nd" 122
        expectOrdinal "123rd" 123
    ]
  where
    log2 :: Int -> Int
    log2 n
      | n <= 1 = 0
      | otherwise = 1 + log2 (div n 2)
    depth (Text.Text t) = R.debugDepth t

-- The chunks with slack (see Chunk in Unison.Util.Text and Unison.Util.Bytes):
-- an append writes into the array behind the last chunk when it owns the
-- array's tail. These check that every earlier version of a value is
-- unchanged by later appends to it, that two appends to the same value both
-- see the original, and that appends from many threads at once do too.
slackTests :: Test ()
slackTests =
  tests
    [ scope "text: every version intact" $ do
        forM_ [(1 :: Int) .. 50] $ \_ -> do
          steps <- int' 1 200
          let go :: Int -> [(Text.Text, T.Text)] -> Text.Text -> T.Text -> Test ()
              go 0 _ _ _ = ok
              go i old t m = do
                op <- int' 0 9
                piece <- T.pack <$> (int' 1 4 >>= \n -> replicateM n (pick "ab\x00e9\x4e2d\x1F600z"))
                k <- int' 0 (T.length m + 1)
                let (t', m') = case op of
                      0 -> (Text.one (T.head piece) <> t, T.cons (T.head piece) m)
                      1 -> (Text.take k t, T.take k m)
                      2 -> (Text.drop k t, T.drop k m)
                      3 -> (Text.fromText piece <> t, piece <> m)
                      _ -> (t <> Text.fromText piece, m <> piece)
                expect' (Text.toText t' == m')
                -- every earlier version still reads as it did
                forM_ old $ \(ot, om) -> expect' (Text.toText ot == om)
                go (i - 1) ((t', m') : old) t' m'
          go steps [] mempty mempty,
      scope "text: two appends to one value" $ do
        forM_ [(1 :: Int) .. 200] $ \_ -> do
          n <- int' 0 70
          base <- T.pack <$> replicateM n (pick "ab\x00e9z")
          let t = foldl (\acc c -> acc <> Text.singleton c) mempty (T.unpack base)
              a = t <> "x"
              b = t <> "yy"
              a2 = a <> "1"
              b2 = b <> "22"
          expect' (Text.toText t == base)
          -- the value built by appends ends in a chunk with slack (until the first chunk fills at 64), so the in-place path is what is tested
          when (n >= 2 && n <= 64) $ expect' (maybe False ((> 0) . Text.chunkCapacity . snd) (Text.unsnocChunk t))
          expect' (Text.toText a == base <> "x")
          expect' (Text.toText b == base <> "yy")
          expect' (Text.toText a2 == base <> "x1")
          expect' (Text.toText b2 == base <> "yy22")
          expect' (Text.toText t == base)
        ok,
      scope "text: appends from many threads" $ do
        let base = foldl (\acc c -> acc <> Text.singleton c) mempty (replicate 20 'a')
            suffixes = [T.pack (show i) | i <- [1 .. 64 :: Int]]
        results <- io $ mapConcurrently (\sfx -> pure $! base <> Text.fromText sfx) suffixes
        forM_ (zip suffixes results) $ \(sfx, r) -> expect' (Text.toText r == T.replicate 20 "a" <> sfx)
        expect' (Text.toText base == T.replicate 20 "a")
        ok,
      scope "text: many threads hammering one value" $ do
        -- 8 threads each append to the same shared value thousands of times at once, so
        -- their compare-and-swaps collide; every result must be the base plus the piece,
        -- and the base must still read as it did
        let base = foldl (\acc c -> acc <> Text.singleton c) mempty ("sh\x00e9red v\x4e2dlue" :: String)
            baseT = Text.toText base
            pieces = [T.pack (show i ++ "\x1F600") | i <- [1 .. 8 :: Int]]
            hammer piece = do
              let go :: Int -> Int -> IO Int
                  go 0 !bad = pure bad
                  go k !bad = do
                    let !r = base <> Text.fromText piece
                        !r2 = r <> Text.fromText piece
                    go (k - 1) (bad + fromEnum (Text.toText r /= baseT <> piece || Text.toText r2 /= baseT <> piece <> piece))
              go 5000 0
        bad <- io $ mapConcurrently hammer pieces
        expect' (sum bad == 0)
        expect' (Text.toText base == baseT)
        ok,
      scope "bytes: every version intact" $ do
        forM_ [(1 :: Int) .. 50] $ \_ -> do
          steps <- int' 1 200
          let go :: Int -> [(B.Bytes, [Int])] -> B.Bytes -> [Int] -> Test ()
              go 0 _ _ _ = ok
              go i old b m = do
                op <- int' 0 9
                piece <- int' 1 4 >>= \n -> replicateM n (int' 0 255)
                k <- int' 0 (length m + 1)
                let bs = B.fromWord8s (map fromIntegral piece)
                    (b', m') = case op of
                      1 -> (B.take k b, take k m)
                      2 -> (B.drop k b, drop k m)
                      3 -> (bs <> b, piece <> m)
                      _ -> (b <> bs, m <> piece)
                expect' (map fromIntegral (B.toWord8s b') == m')
                forM_ old $ \(ob, om) -> expect' (map fromIntegral (B.toWord8s ob) == om)
                go (i - 1) ((b', m') : old) b' m'
          go steps [] mempty mempty,
      scope "bytes: many threads hammering one value" $ do
        let one i = B.fromWord8s [fromIntegral i]
            base = foldl (\acc i -> acc <> one i) mempty [1 .. 30 :: Int]
            baseL = map fromIntegral (B.toWord8s base) :: [Int]
            hammer i = do
              let go :: Int -> Int -> IO Int
                  go 0 !bad = pure bad
                  go k !bad = do
                    let !r = base <> one i
                        !r2 = r <> one (i + 1)
                    go (k - 1) (bad + fromEnum (map fromIntegral (B.toWord8s r) /= baseL ++ [i] || map fromIntegral (B.toWord8s r2) /= baseL ++ [i, i + 1]))
              go 5000 0
        bad <- io $ mapConcurrently hammer [100, 110 .. 170 :: Int]
        expect' (sum bad == 0)
        expect' (map fromIntegral (B.toWord8s base) == baseL)
        ok,
      scope "bytes: appends from many threads" $ do
        let one i = B.fromWord8s [fromIntegral i]
            base = foldl (\acc i -> acc <> one i) mempty [1 .. 20 :: Int]
        expect' (maybe False ((> 0) . B.chunkCapacity . snd) (R.unsnoc (B.underlying base)))
        results <- io $ mapConcurrently (\i -> pure $! base <> one i) [100 .. 163 :: Int]
        forM_ (zip [100 .. 163 :: Int] results) $ \(i, r) -> expect' (map fromIntegral (B.toWord8s r) == [1 .. 20] ++ [i])
        expect' (map fromIntegral (B.toWord8s base) == [1 .. 20 :: Int])
        ok
    ]
