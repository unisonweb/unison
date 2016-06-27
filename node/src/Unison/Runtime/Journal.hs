module Unison.Runtime.Journal where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem
import Data.Functor
import Unison.BlockStore (BlockStore)
import Unison.Runtime.Block (Block)
import qualified Unison.Runtime.Block as Block

data Updates u = Updates { flush :: IO (), append :: u -> IO (), currentLog :: IO [u] }

data Journal a u = Journal { apply :: a -> [u] -> a, checkpoint :: IO a, updates :: Updates u, record :: IO () }

fromBlocks :: Eq h => BlockStore h -> (a -> [u] -> a) -> Block a -> Block u -> IO (Journal a u)
fromBlocks bs apply checkpoint us = do
  sem <- atomically $ newTSem 1
  let j = Journal apply (Block.get bs checkpoint) (Updates (pure ()) append gets) (record j sem)
  pure j
  where
  append u = void $ Block.append bs us u
  gets = sequenceA =<< Block.gets bs us
  record j sem = bracket (atomically $ waitTSem sem) (\_ -> atomically $ signalTSem sem) $ \_ -> do
    aOld <- Block.get bs checkpoint
    log <- currentLog (updates j)
    when (not . null $ log) $ do
      let commitUpdates = init log
          stickBack = last log
      _ <- Block.modify' bs checkpoint (const $ apply aOld commitUpdates)
      _ <- Block.modify' bs us (const stickBack)
      pure ()

get :: Journal a u -> IO a
get j = do
  c <- checkpoint j
  u <- currentLog (updates j)
  pure $ apply j c u

update :: u -> Journal a u -> IO ()
update u j = append (updates j) u

-- buffer :: Journal a u -> IO (Journal a u)
