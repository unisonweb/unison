
module Unison.Runtime.Profiling where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Word

import Unison.Codebase.Runtime.Profile
import Unison.Runtime.MCode
import Unison.Runtime.Stack

addSample :: CombIx -> K -> Profile Word64 -> Profile Word64
addSample c k (Prof count trie refs) =
  Prof
    (1+count)
    (addPath (fst <$> cmbs) trie)
    (M.union refs $ M.fromList cmbs)
  where
  cixToPair (CIx r i _) = (i, r)

  cmbs = combs [cixToPair c] k

  combs acc KE = acc
  combs acc (CB _) = acc
  combs acc (AMark _ _ _ k) = combs acc k
  combs acc (Mark _ _ _ k) = combs acc k
  combs acc (Local _ _ k) = combs acc k
  combs acc (Push _ _ c _ _ k) = combs (cixToPair c : acc) k

addSamples :: [(CombIx, K)] -> Profile Word64 -> Profile Word64
addSamples ts p = foldl' (flip . uncurry $ addSample) p ts

-- For communication between execution and a profiling thread. `Final`
-- indicates that execution is complete and the profiling thread should
-- write a final result to annother channel.
data TickComm
  = Empty
  | Finished
  | Ticks [(CombIx, K)]
  | Final [(CombIx, K)]

readInput :: TVar TickComm -> IO (Bool, [(CombIx, K)])
readInput input = atomically $
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

enqueue :: TVar TickComm -> CombIx -> K -> IO ()
enqueue comm c k = atomically $
  modifyTVar comm \case
    Empty -> Ticks [(c, k)]
    Finished -> Final [(c, k)]
    Ticks ts -> Ticks ((c, k):ts)
    Final ts -> Final ((c, k):ts)

finish :: TVar TickComm -> IO ()
finish comm = atomically $
  modifyTVar comm \case
    Empty -> Finished
    Finished -> Finished
    Ticks ts -> Final ts
    Final ts -> Final ts

data ProfileComm =
  PC (CombIx -> K -> IO ())
     (IO ())
     (IO (Profile Word64))

spawnProfiler :: IO ProfileComm
spawnProfiler = do
  input <- newTVarIO Empty
  output <- newEmptyTMVarIO
  _ <- forkIO $ profileLoop input output emptyProfile
  pure $ PC (enqueue input) (finish input) (atomically $ takeTMVar output)
