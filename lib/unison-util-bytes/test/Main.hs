module Main (main) where

import qualified Data.ByteString as BS
import EasyTest
import System.IO.CodePage (withCP65001)
import Unison.Prelude
import qualified Unison.Util.Bytes as Bytes

main :: IO ()
main =
  withCP65001 (run (scope "util.bytes" test))

test :: Test ()
test =
  tests $
    [ scope "empty ==" . expect $ Bytes.empty == Bytes.empty,
      scope "empty `compare`" . expect $ Bytes.empty `compare` Bytes.empty == EQ,
      scope "==" . expect $
        Bytes.fromWord8s [0, 1, 2, 3, 4, 5] <> Bytes.fromWord8s [6, 7, 8, 9]
          == Bytes.fromWord8s [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
      scope "at" $ do
        expect' (Bytes.at 0 (Bytes.fromWord8s [77, 13, 12]) == Just 77)
        expect' (Bytes.at 0 (Bytes.fromWord8s []) == Nothing)
        ok,
      scope "at.cornerCases" $ do
        let b = Bytes.drop 3 $ Bytes.fromWord8s [77, 13, 12] <> Bytes.fromWord8s [9, 10, 11]
        expect' (Bytes.at 0 b == Just 9)
        expect' (Bytes.at 0 (mempty <> Bytes.fromWord8s [1, 2, 3]) == Just 1)
        ok,
      scope "consistency with ByteString" $ do
        forM_ [(1 :: Int) .. 100] $ \_ -> do
          n <- int' 0 50
          m <- int' 0 50
          k <- int' 0 (n + m)
          o <- int' 0 50
          b1 <- BS.pack <$> replicateM n word8
          b2 <- BS.pack <$> replicateM m word8
          b3 <- BS.pack <$> replicateM o word8
          let (b1s, b2s, b3s) = (Bytes.fromArray b1, Bytes.fromArray b2, Bytes.fromArray b3)
          scope "associtivity" . expect' $
            b1s <> (b2s <> b3s) == (b1s <> b2s) <> b3s
          scope "<>" . expect' $
            Bytes.toArray (b1s <> b2s <> b3s) == b1 <> b2 <> b3
          scope "Ord" . expect' $
            (b1 <> b2 <> b3)
              `compare` b3
              == (b1s <> b2s <> b3s)
              `compare` b3s
          scope "take" . expect' $
            Bytes.toArray (Bytes.take k (b1s <> b2s)) == BS.take k (b1 <> b2)
          scope "drop" . expect' $
            Bytes.toArray (Bytes.drop k (b1s <> b2s)) == BS.drop k (b1 <> b2)
          scope "at" $
            let bs = b1s <> b2s <> b3s
                b = b1 <> b2 <> b3
             in forM_ [0 .. (BS.length b - 1)] $ \ind ->
                  expect' $ Just (BS.index b ind) == Bytes.at ind bs
        ok,
      scope "lots of chunks" $ do
        forM_ [(0 :: Int) .. 100] $ \i -> do
          n <- int' 0 50
          k <- int' 0 i
          chunks <- replicateM n (replicateM k word8)
          let b1 = foldMap Bytes.fromWord8s chunks
              b2 = foldr (<>) mempty (Bytes.fromWord8s <$> chunks)
              b3 = foldl' (<>) mempty (Bytes.fromWord8s <$> chunks)
              b = BS.concat (BS.pack <$> chunks)
          expect' $ b1 == b2 && b2 == b3
          expect' $ Bytes.toArray b1 == b
          expect' $ Bytes.toArray b2 == b
          expect' $ Bytes.toArray b3 == b
        ok
    ]
