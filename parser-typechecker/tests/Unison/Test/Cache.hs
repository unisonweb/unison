module Unison.Test.Cache where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import EasyTest
import qualified U.Util.Cache as Cache

test :: Test ()
test =
  scope "util.cache" $
    tests
      [ scope "ex1" $ fits Cache.cache,
        scope "ex2" $ fits (Cache.semispaceCache n),
        scope "ex3" $ doesn'tFit (Cache.semispaceCache n),
        scope "ex4" $ do
          replicateM_ 10 $ concurrent (Cache.semispaceCache n)
          ok
      ]
  where
    n :: Word
    n = 1000

    -- This checks that items properly expire from the cache
    doesn'tFit mkCache = do
      cache <- io $ mkCache
      misses <- io $ newTVarIO 0
      let f x = do
            atomically $ modifyTVar misses (+ 1)
            pure x
      -- populate the cache, all misses (n*2), but first 1-n will have expired by the end
      results1 <- io $ traverse (Cache.apply cache f) [1 .. n * 2]
      -- should be half hits, so an additional `n` misses
      results2 <- io $ traverse (Cache.apply cache f) (reverse [1 .. n * 2])
      misses <- io $ readTVarIO misses
      expect' (results1 == [1 .. n * 2])
      expect' (results2 == reverse [1 .. n * 2])
      expect (misses == n * 3)

    -- This checks the simple case that everything fits in the cache
    fits mkCache = do
      cache <- io $ mkCache
      misses <- io $ newTVarIO 0
      let f x = do
            atomically $ modifyTVar misses (+ 1)
            pure x
      -- populate the cache
      results1 <- io $ traverse (Cache.apply cache f) [1 .. n]
      -- should be all hits
      results2 <- io $ traverse (Cache.apply cache f) [1 .. n]
      misses <- io $ readTVarIO misses
      expect' (results1 == [1 .. n])
      expect' (results2 == [1 .. n])
      expect (misses == n)

    -- A simple smoke test of concurrent access. The cache doesn't
    -- try to linearize all reads / writes so the number of misses
    -- during concurrent access is unpredictable, but once the cache is
    -- fully populated, concurrent reads should generate no further misses
    concurrent mkCache = do
      cache <- io $ mkCache
      misses <- io $ newTVarIO 0
      let f x = do
            atomically $ modifyTVar misses (+ 1)
            pure x
      -- we're populating the cache in parallel
      results1 <- io $ async $ traverse (Cache.apply cache f) [1 .. (n `div` 2)]
      results2 <- io $ async $ traverse (Cache.apply cache f) [(n `div` 2 + 1) .. n]
      (results1, results2) <- io $ waitBoth results1 results2
      -- now the cache should be fully populated, so no further misses
      misses1 <- io $ readTVarIO misses

      -- these should be all hits
      results3 <- io $ async $ traverse (Cache.apply cache f) [1 .. (n `div` 2)]
      results4 <- io $ async $ traverse (Cache.apply cache f) [(n `div` 2 + 1) .. n]
      (results3, results4) <- io $ waitBoth results3 results4

      misses2 <- io $ readTVarIO misses
      expect' (results1 ++ results2 == [1 .. n])
      expect' (results3 ++ results4 == [1 .. n])
      expect' (misses1 == misses2)
