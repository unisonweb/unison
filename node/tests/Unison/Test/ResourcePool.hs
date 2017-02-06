module Unison.Test.ResourcePool where

import Control.Concurrent
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.IORef (IORef)
import Data.Maybe
import Data.Time (UTCTime,getCurrentTime, addUTCTime)
import EasyTest
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

makePool :: RP.MaxPoolSize -> IO (RP.ResourcePool Int Int, IO Bool)
makePool maxSize = do
  rs <- IORef.newIORef 0
  nonce <- IORef.newIORef 0
  pool <- RP.make 3 maxSize (\_ -> increment rs >> increment nonce) (\_ -> decrement rs $> ())
  pure (pool, IORef.readIORef rs >>= \n -> pure (n == 0))

acquireSequential1 :: Test ()
acquireSequential1 = do
  (pool, ok) <- io (makePool 10)
  let check n = scope "resource recycled" (expect (0 == n))
  replicateM_ 100 (io (RP.acquire pool 42) >>= \(n, release) -> check n >> io release)
  eventually ok

acquireSequential2 :: Test ()
acquireSequential2 = do
  (pool, ok) <- io (makePool 100)
  -- let check n = putStrLn $ "got: " ++ show n
  let check n = scope "resource recycled" (expect (n <= 100))
  let acquire i = io (RP.acquire pool i) >>= \(n, release) -> check n >> io release
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

acquireConcurrent1 :: Test ()
acquireConcurrent1 = do
  (pool, ok) <- io (makePool 10)
  io . parallelSequence_ $ replicate 100 (RP.acquire pool 42 >>= \(n, release) -> release)
  eventually ok

acquireConcurrent2 :: Test ()
acquireConcurrent2 = do
  (pool, ok) <- io (makePool 100)
  let acquire i = RP.acquire pool i >>= \(n, release) -> release
  _ <- io (parallelTraverse acquire ([1..100] ++ [1..100]))
  eventually ok

delaySeconds μs = threadDelay (1000000 * μs)

eventually :: IO Bool -> Test ()
eventually cond = go 1 where
  go n | n > 8 = expect =<< io cond
  go n = do
    c <- io cond
    case c of
      False -> io (delaySeconds n) >> go (n*2)
      True -> ok

test :: Test ()
test = scope "ResourcePool" . tests $
  [ scope "acquireSequential1" acquireSequential1
  , scope "acquireSequential2" acquireSequential2
  , scope "acquireConcurrent1" acquireConcurrent1
  , scope "acquireConcurrent2" acquireConcurrent2 ]
