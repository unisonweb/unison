{-# LANGUAGE OverloadedStrings #-}

-- | The process-wide type registry (Unison.Runtime.TypeNumbers) under
-- concurrent assignment: every reference gets one number, every number
-- resolves to its reference, and the builtins keep theirs.
module Unison.Test.Runtime.TypeNumbers (test) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import EasyTest
import Unison.Reference (Reference, Reference' (Builtin))
import Unison.Runtime.Builtin.Types (builtinTypeNumbering)
import Unison.Runtime.TypeNumbers
import Unison.Util.EnumContainers qualified as EC

ref :: Int -> Reference
ref i = Builtin ("test.TypeNumbers." <> Text.pack (show i))

test :: Test ()
test =
  scope "typeNumbers" . tests $
    [ scope "the builtins keep their numbers" $ do
        m <- io typeNumberMap
        expectEqual builtinTypeNumbering (Map.restrictKeys m (Map.keysSet builtinTypeNumbering)),
      scope "a reference numbered twice has one number, which resolves" $ do
        n1 <- io (typeNumber (ref 0))
        n2 <- io (typeNumber (ref 0))
        expectEqual n1 n2
        expectEqual (ref 0) (typeReference n1),
      scope "eight threads numbering overlapping sets agree" $ do
        -- thread t numbers the references 100 t .. 100 t + 599 in batches
        -- of ten and one at a time, so every reference is contested by
        -- several threads
        results <- io . forConcurrently [0 .. 7 :: Int] $ \t -> do
          let rs = [ref (100 * t + i) | i <- [0 .. 599]]
              batches = chunks 10 rs
          fromBatches <- traverse (typeNumbers . Set.fromList) batches
          singles <- traverse (\r -> (r,) <$> typeNumber r) rs
          pure (Map.unions fromBatches, Map.fromList singles)
        let allRefs = Set.fromList [ref i | i <- [0 .. 1299]]
        final <- io typeNumberMap
        back <- io typeReferences
        -- every thread's view agrees with the final one
        for_ results \(batched, single) -> do
          expectEqual batched (Map.restrictKeys final (Map.keysSet batched))
          expectEqual single (Map.restrictKeys final (Map.keysSet single))
        -- one number per reference, distinct, and each resolves both ways
        let numbered = Map.restrictKeys final allRefs
        expectEqual (Set.size allRefs) (Map.size numbered)
        expectEqual (Map.size numbered) (Set.size (Set.fromList (Map.elems numbered)))
        for_ (Map.toList numbered) \(r, n) -> do
          expectEqual r (typeReference n)
          expectEqual (Just r) (EC.lookup n back),
      scope "a seed that agrees is accepted and a new one is taken on" $ do
        n <- io (typeNumber (ref 5000))
        io (seedTypeNumbers (EC.mapFromList [(n, ref 5000), (1000000, ref 5001)]))
        expectEqual (ref 5001) (typeReference 1000000)
        n' <- io (typeNumber (ref 5001))
        expectEqual 1000000 n'
        -- numbers assigned after the seed are above it
        n'' <- io (typeNumber (ref 5002))
        expect (n'' > 1000000)
    ]
  where
    chunks _ [] = []
    chunks k xs = let (a, b) = splitAt k xs in a : chunks k b
    forConcurrently xs f = do
      vars <- traverse (\x -> do v <- newEmptyMVar; _ <- forkIO (f x >>= putMVar v); pure v) xs
      traverse takeMVar vars
