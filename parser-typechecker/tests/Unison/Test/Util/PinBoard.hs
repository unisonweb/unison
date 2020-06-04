{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Unison.Test.Util.PinBoard
  ( test,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.ByteString as ByteString
import EasyTest
import GHC.Exts (touch#)
import GHC.IO (IO (IO))
import qualified GHC.Stats as GHC
import System.Mem (performGC)
import Unison.Prelude
import qualified Unison.Util.PinBoard as PinBoard

test :: Test ()
test =
  scope "util.pinboard" . tests $
    [ scope "pinning equal values stores only one" $ do
        -- Allocate 2*1mb bytestrings, pin them both, and assert less than 2mb live memory
        w0 <- getLiveHeapSize
        bytes <- io $ evaluate (force (take 2 (iterate ByteString.copy (ByteString.replicate (2 ^ 20) 0))))
        board <- PinBoard.new
        for_ bytes (PinBoard.pin board)
        w1 <- getLiveHeapSize
        expect' (w1 - w0 < 2 ^ 20)
        io (touch board)
        ok
    ]

getLiveHeapSize :: MonadIO m => m Word64
getLiveHeapSize = liftIO do
  performGC
  stats <- GHC.getRTSStats
  pure (GHC.gcdetails_live_bytes (GHC.gc stats))

touch :: a -> IO ()
touch x =
  IO \s -> (# touch# x s, () #)
