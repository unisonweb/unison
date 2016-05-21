module Unison.Test.ResourcePool where

import Control.Concurrent
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.IORef (IORef)
import Data.Maybe
import Data.Time (UTCTime,getCurrentTime, addUTCTime)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Map as M
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Monad.STM as STM
import qualified Data.Hashable as H
import qualified Data.IORef as IORef
import qualified Unison.Runtime.ResourcePool as RP

type Resource = String
type Params = String
type TestState = M.Map String String

increment :: IORef Int -> IO Int
increment ref = IORef.atomicModifyIORef' ref (\n -> (n+1, n))

decrement :: IORef Int -> IO Int
decrement ref = IORef.atomicModifyIORef' ref (\n -> (n-1, n))

makePool :: RP.MaxPoolSize -> IO (RP.ResourcePool Int Int, Assertion)
makePool maxSize = do
  rs <- IORef.newIORef 0
  nonce <- IORef.newIORef 0
  pool <- RP.make 1 maxSize (\_ -> increment rs >> increment nonce) (\_ -> decrement rs $> ())
  pure (pool, IORef.readIORef rs >>= \n -> if n == 0 then pure () else fail $ "count nonzero: " ++ show n)

acquireSequential1 :: Assertion
acquireSequential1 = do
  (pool, ok) <- makePool 10
  replicateM_ 100 (RP.acquire pool 42 >>= \(n, release) -> assertEqual "resource not recycled" 0 n >> release)
  eventually ok

acquireSequential2 :: Assertion
acquireSequential2 = do
  (pool, ok) <- makePool 100
  -- let check n = putStrLn $ "got: " ++ show n
  let check n = assertEqual "resource not recycled" True (n <= 100)
  let acquire i = RP.acquire pool i >>= \(n, release) -> check n >> release
  mapM_ acquire ([1..100] ++ [1..100]) -- second 100 acquires should get cached
  eventually ok

parallelTraverse :: (a -> IO b) -> [a] -> IO [b]
parallelTraverse f xs = sequence =<< mapM spawn xs where
  spawn a = do
    r <- MVar.newEmptyMVar
    _ <- CC.forkIO $ f a >>= \b -> MVar.putMVar r b
    pure (MVar.takeMVar r)

parallelSequence :: [IO a] -> IO [a]
parallelSequence = parallelTraverse id

parallelSequence_ :: [IO a] -> IO ()
parallelSequence_ ios = void $ parallelSequence ios

acquireConcurrent1 :: Assertion
acquireConcurrent1 = do
  (pool, ok) <- makePool 10
  parallelSequence_ $ replicate 100 (RP.acquire pool 42 >>= \(n, release) -> release)
  eventually ok

acquireConcurrent2 :: Assertion
acquireConcurrent2 = do
  (pool, ok) <- makePool 100
  let acquire i = RP.acquire pool i >>= \(n, release) -> release
  _ <- parallelTraverse acquire ([1..100] ++ [1..100])
  eventually ok

delaySeconds μs = threadDelay (1000000 * μs)

eventually :: Assertion -> Assertion
eventually cond = go 1 where
  go n | n > 8 = cond
  go n = cond <|> (delaySeconds n >> go (n*2))

tests :: TestTree
tests = testGroup "ResourcePool"
  [ testCase "acquireSequential1" acquireSequential1
  , testCase "acquireSequential2" acquireSequential2
  , testCase "acquireConcurrent1" acquireConcurrent1
  , testCase "acquireConcurrent2" acquireConcurrent2 ]

main = defaultMain tests
