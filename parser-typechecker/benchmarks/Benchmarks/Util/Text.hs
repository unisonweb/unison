{-# Language OverloadedStrings #-}

module Benchmarks.Util.Text where

import Criterion.Main
import GHC.Base ( join ) 
import qualified Data.Text as T
import qualified Data.ByteString as ByteString


import qualified Unison.Util.Text as UT
import qualified Unison.Util.Bytes as UB


getString :: Int -> String
getString n = join $ Prelude.replicate n "a"

getText :: Int -> T.Text
getText n = T.pack $ getString n

getChunk :: Int -> UT.Chunk 
getChunk n = UT.chunk $ getText n

getUtf8 :: Int -> UB.Bytes
getUtf8 n = UB.fromByteString $ ByteString.replicate n 1

getUText :: Int -> UT.Text
getUText n = UT.replicate n $ UT.one 'a'

run :: Benchmark
run = bgroup "Text" [ 
    bgroup "Unison.Util.Text" [
          bgroup "pack (fromString)" [
               bench "100"     $ whnf UT.pack $ getString 100
             , bench "10000"   $ whnf UT.pack $ getString 10000
             , bench "1000000" $ whnf UT.pack $ getString 1000000
          ] 
          , bgroup "replicate" [
               bench "100"     $ whnf (UT.replicate 100    ) $ UT.one 'a'
             , bench "10000"   $ whnf (UT.replicate 10000  ) $ UT.one 'a'
             , bench "1000000" $ whnf (UT.replicate 1000000) $ UT.one 'a'
          ]
          , bgroup "take" [
               bench "100"     $ whnf (UT.take 100    ) $ getUText 1000000
             , bench "10000"   $ whnf (UT.take 10000  ) $ getUText 1000000
             , bench "1000000" $ whnf (UT.take 1000000) $ getUText 1000000
          ]
          , bgroup "drop" [
               bench "100"     $ whnf (UT.drop 100    ) $ getUText 1000000
             , bench "10000"   $ whnf (UT.drop 10000  ) $ getUText 1000000
             , bench "1000000" $ whnf (UT.drop 1000000) $ getUText 1000000
          ]
          , bgroup "uncons" [
               bench "100"     $ whnf UT.uncons $ getUText 100
             , bench "10000"   $ whnf UT.uncons $ getUText 10000
             , bench "1000000" $ whnf UT.uncons $ getUText 1000000
          ]
          , bgroup "unsnoc" [
               bench "100"     $ whnf UT.unsnoc $ getUText 100
             , bench "10000"   $ whnf UT.unsnoc $ getUText 10000
             , bench "1000000" $ whnf UT.unsnoc $ getUText 1000000
          ]
          , bgroup "at" [
               bench "100"     $ whnf (UT.at 100          ) $ getUText 1000000
             , bench "10000"   $ whnf (UT.at 10000        ) $ getUText 1000000
             , bench "1000000" $ whnf (UT.at $ 1000000 - 1) $ getUText 1000000
          ]
          , bgroup "size" [
               bench "100"     $ whnf UT.size $ getUText 100
             , bench "10000"   $ whnf UT.size $ getUText 10000
             , bench "1000000" $ whnf UT.size $ getUText 1000000
          ]
          , bgroup "reverse" [
               bench "100"     $ whnf UT.reverse $ getUText 100
             , bench "10000"   $ whnf UT.reverse $ getUText 10000
             , bench "1000000" $ whnf UT.reverse $ getUText 1000000
          ]
          , bgroup "unpack (toString)" [
               bench "100"     $ whnf UT.unpack $ getUText 100
             , bench "10000"   $ whnf UT.unpack $ getUText 10000
             , bench "1000000" $ whnf UT.unpack $ getUText 1000000
          ]

          -- Unison Text only
          , bgroup "chunk" [
               bench "100"     $ whnf UT.chunk $ getText 100
             , bench "10000"   $ whnf UT.chunk $ getText 10000
             , bench "1000000" $ whnf UT.chunk $ getText 1000000
          ]
          , bgroup "chunkToText" [
               bench "100"     $ whnf UT.chunkToText $ getChunk 100
             , bench "10000"   $ whnf UT.chunkToText $ getChunk 10000
             , bench "1000000" $ whnf UT.chunkToText $ getChunk 1000000
          ]
          , bgroup "fromUtf8" [
               bench "100"     $ whnf UT.fromUtf8 $ getUtf8 100
             , bench "10000"   $ whnf UT.fromUtf8 $ getUtf8 10000
             , bench "1000000" $ whnf UT.fromUtf8 $ getUtf8 1000000
          ]
          , bgroup "toUtf8" [
               bench "100"     $ whnf UT.toUtf8 $ getUText 100
             , bench "10000"   $ whnf UT.toUtf8 $ getUText 10000
             , bench "1000000" $ whnf UT.toUtf8 $ getUText 1000000
          ]
          , bgroup "toText" [
               bench "100"     $ whnf UT.toText $ getUText 100
             , bench "10000"   $ whnf UT.toText $ getUText 10000
             , bench "1000000" $ whnf UT.toText $ getUText 1000000
          ]
    ] 
    , bgroup "Data.Text" [
          bgroup "pack (fromString)" [
               bench "100"     $ whnf T.pack $ getString 100
             , bench "10000"   $ whnf T.pack $ getString 10000
             , bench "1000000" $ whnf T.pack $ getString 1000000
          ] 
          , bgroup "replicate" [
               bench "100"     $ whnf (T.replicate 100    ) $ T.singleton 'a'
             , bench "10000"   $ whnf (T.replicate 10000  ) $ T.singleton 'a'
             , bench "1000000" $ whnf (T.replicate 1000000) $ T.singleton 'a'
          ]
          , bgroup "take" [
               bench "100"     $ whnf (T.take 100    ) $ getText 1000000
             , bench "10000"   $ whnf (T.take 10000  ) $ getText 1000000
             , bench "1000000" $ whnf (T.take 1000000) $ getText 1000000
          ]
          , bgroup "drop" [
               bench "100"     $ whnf (T.drop 100    ) $ getText 1000000
             , bench "10000"   $ whnf (T.drop 10000  ) $ getText 1000000
             , bench "1000000" $ whnf (T.drop 1000000) $ getText 1000000
          ]
          , bgroup "uncons" [
               bench "100"     $ whnf T.uncons $ getText 100
             , bench "10000"   $ whnf T.uncons $ getText 10000
             , bench "1000000" $ whnf T.uncons $ getText 1000000
          ]
          , bgroup "unsnoc" [
               bench "100"     $ whnf T.unsnoc $ getText 100
             , bench "10000"   $ whnf T.unsnoc $ getText 10000
             , bench "1000000" $ whnf T.unsnoc $ getText 1000000
          ]
          , bgroup "index" [
               bench "100"     $ whnf (flip T.index 100          ) $ getText 1000000
             , bench "10000"   $ whnf (flip T.index 10000        ) $ getText 1000000
             , bench "1000000" $ whnf (flip T.index $ 1000000 - 1) $ getText 1000000
          ]
          , bgroup "lengh" [
               bench "100"     $ whnf T.length $ getText 100
             , bench "10000"   $ whnf T.length $ getText 10000
             , bench "1000000" $ whnf T.length $ getText 1000000
          ]
          , bgroup "reverse" [
               bench "100"     $ whnf T.reverse $ getText 100
             , bench "10000"   $ whnf T.reverse $ getText 10000
             , bench "1000000" $ whnf T.reverse $ getText 1000000
          ]
          , bgroup "unpack" [
               bench "100"     $ whnf T.unpack $ getText 100
             , bench "10000"   $ whnf T.unpack $ getText 10000
             , bench "1000000" $ whnf T.unpack $ getText 1000000
          ]
        ] 
    ]