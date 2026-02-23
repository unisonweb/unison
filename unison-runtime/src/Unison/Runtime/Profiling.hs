module Unison.Runtime.Profiling where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Map.Strict qualified as M
import Data.Word
import Unison.Codebase.Runtime.Profile
import Unison.Runtime.MCode
import Unison.Runtime.Stack

addSample :: Bool -> CombIx -> K -> Profile Word64 -> Profile Word64
addSample wait c k (Prof count trie refs) =
  Prof
    (inc wait count)
    (addPath wait (fst <$> cmbs) trie)
    (M.union refs $ M.fromList cmbs)
  where
    inc b (m, n) = pair (m + 1) (if b then n + 1 else n)
    pair !m !n = (m, n)

    cixToPair (CIx r i _) = (i, r)

    cmbs = combs [cixToPair c] k

    combs acc KE = acc
    combs acc (CB _) = acc
    combs acc (AMark _ _ _ k) = combs acc k
    combs acc (Mark _ _ _ k) = combs acc k
    combs acc (Local _ _ k) = combs acc k
    combs acc (Keep _ _ k) = combs acc k
    combs acc (Push _ _ c _ _ k) = combs (cixToPair c : acc) k

addSamples :: [(Bool, CombIx, K)] -> Profile Word64 -> Profile Word64
addSamples ts p = foldl' (flip . uncurry3 $ addSample) p ts
  where
    uncurry3 f (x, y, z) = f x y z

-- For communication between execution and a profiling thread. `Final`
-- indicates that execution is complete and the profiling thread should
-- write a final result to annother channel.
data TickComm
  = Empty
  | Finished
  | Ticks [(Bool, CombIx, K)]
  | Final [(Bool, CombIx, K)]

readInput :: TVar TickComm -> IO (Bool, [(Bool, CombIx, K)])
readInput input =
  atomically $
    readTVar input >>= \case
      Empty -> retry
      Finished -> pure (True, [])
      Ticks ts -> (False, ts) <$ writeTVar input Empty
      Final ts -> (True, ts) <$ writeTVar input Finished

profileLoop ::
  TVar TickComm ->
  TMVar (Profile Word64) ->
  Profile Word64 ->
  IO ()
profileLoop input output prof = do
  (finish, ts) <- readInput input
  prof <- pure $ addSamples ts prof
  if not finish
    then profileLoop input output prof
    else atomically $ putTMVar output prof

enqueue :: TVar TickComm -> Bool -> CombIx -> K -> IO ()
enqueue comm b c k = atomically $
  modifyTVar comm \case
    Empty -> Ticks [(b, c, k)]
    Finished -> Final [(b, c, k)]
    Ticks ts -> Ticks ((b, c, k) : ts)
    Final ts -> Final ((b, c, k) : ts)

finish :: TVar TickComm -> IO ()
finish comm = atomically $
  modifyTVar comm \case
    Empty -> Finished
    Finished -> Finished
    Ticks ts -> Final ts
    Final ts -> Final ts

data ProfileComm
  = PC
      (Bool -> CombIx -> K -> IO ())
      (IO ())
      (IO (Profile Word64))

spawnProfiler :: IO ProfileComm
spawnProfiler = do
  input <- newTVarIO Empty
  output <- newEmptyTMVarIO
  _ <- forkIO $ profileLoop input output emptyProfile
  pure $ PC (enqueue input) (finish input) (atomically $ takeTMVar output)
