{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Util.Text where

import Control.Monad
import Data.List (foldl', unfoldr)
import qualified Data.Text as T
import EasyTest
import qualified Unison.Util.Rope as R
import qualified Unison.Util.Text as Text
import qualified Unison.Util.Text.Pattern as P

test :: Test ()
test =
  scope "util.text" . tests $
    [ scope "empty ==" . expect $ Text.empty == Text.empty,
      scope "empty `compare`" . expect $ Text.empty `compare` Text.empty == EQ,
      scope "==" . expect $
        let a = join (replicate 100 ['a' .. 'z'])
            b = join (replicate 45 ['A' .. 'Z'])
         in (Text.pack a <> Text.pack b) == Text.pack (a ++ b),
      scope "at" $ do
        expect' (Text.at 0 (Text.pack "abc") == Just 'a')
        expect' (Text.at 0 mempty == Nothing)
        ok,
      scope "at.cornerCases" $ do
        let b = Text.drop 3 $ "abc" <> "def"
        expect' (Text.at 0 b == Just 'd')
        expect' (Text.at 0 (mempty <> "abc") == Just 'a')
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
                  expect' $ Just (T.index b ind) == Text.at ind bs
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
          expect' $ Text.toText b1 == b
          expect' $ Text.toText b2 == b
          expect' $ Text.toText b3 == b
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
        expect' (P.run P.Eof "" == Just ([], ""))
        expect' (P.run P.AnyChar "a" == Just ([], ""))
        expect' (P.run (P.CharRange 'a' 'z') "a" == Just ([], ""))
        expect' (P.run (P.NotCharRange 'a' 'z') "a" == Nothing)
        expect' (P.run (P.Or (P.NotCharRange 'a' 'z') P.AnyChar) "abc" == Just ([], "bc"))
        -- this shows that we ignore subcaptures
        expect' (P.run (P.Join [P.Capture (P.Join [P.Capture P.AnyChar, P.Capture P.AnyChar]), P.AnyChar]) "abcdef" == Just (["ab"], "def"))
        expect' (P.run (P.CharIn "0123") "3ab" == Just ([], "ab"))
        expect' (P.run (P.NotCharIn "0123") "a3b" == Just ([], "3b"))
        expect' (P.run (P.Capture (P.NotCharIn "0123")) "a3b" == Just (["a"], "3b"))
        expect' (P.run (P.Many (P.CharIn "abcd")) "babbababac123" == Just ([], "123"))
        expect' (P.run (P.Capture (P.Many (P.CharIn "abcd"))) "babbababac123" == Just (["babbababac"], "123"))
        expect' (P.run (P.Capture (P.Many (P.Digit))) "012345abc" == Just (["012345"], "abc"))
        expect' (P.run (P.Join [P.Capture (P.Many (P.Digit)), P.Literal ",", P.Capture (P.Many P.AnyChar)]) "012345,abc" == Just (["012345", "abc"], ""))
        expect'
          ( P.run (P.Many (P.Join [P.Capture (P.Many (P.Digit)), P.Many P.Space])) "01 10 20 1123 292 110 10"
              == Just (["01", "10", "20", "1123", "292", "110", "10"], "")
          )
        expect' $
          let part = P.Capture (P.Replicate 1 3 (P.Digit))
              dpart = P.Join [P.Literal ".", part]
              ip = P.Join [part, P.Replicate 3 3 dpart, P.Eof]
           in P.run ip "127.0.0.1" == Just (["127", "0", "0", "1"], "")
        expect' $
          let p = P.Replicate 5 8 (P.Capture P.Digit)
           in P.run p "12345" == Just (["1", "2", "3", "4", "5"], "")
        expect' $
          let p = P.Replicate 5 8 (P.Capture P.Digit) `P.Or` P.Join []
           in P.run p "1234" == Just ([], "1234")
        expect' $
          let p = P.Replicate 5 8 (P.Capture (P.Join [P.Digit, P.Literal "z"])) `P.Or` P.Join []
           in P.run p "1z2z3z4z5z6a" == Just (["1z", "2z", "3z", "4z", "5z"], "6a")
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
          let p = P.Join [P.Capture P.AnyChar, P.Or (P.Literal "c") (P.Join []), P.Literal "d"]
           in P.run p "@cd"
        expectEqual (Just (["%", "c"], "")) $
          let p = P.Join [P.Capture P.AnyChar, (P.Or (P.Capture (P.Literal "c")) (P.Join [])), P.Literal "d"]
           in P.run p "%cd"
        expectEqual (Just ([""], "ac")) $
          let p = P.Capture (P.Or (P.Join [P.Literal "a", P.Literal "b"]) (P.Join []))
           in P.run p "ac"
        expectEqual (Just ([""], "ac")) $
          let p = P.Capture (P.Replicate 0 1 (P.Join [P.Literal "a", P.Literal "b"]))
           in P.run p "ac"
        ok
    ]
  where
    log2 :: Int -> Int
    log2 n
      | n <= 1 = 0
      | otherwise = 1 + log2 (div n 2)
    depth (Text.Text t) = R.debugDepth t
