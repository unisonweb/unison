{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Unison.Test.Util.PinBoard
  ( test,
  )
where

import Data.ByteString qualified as ByteString
import EasyTest
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#, touch#)
import GHC.IO (IO (IO))
import System.Mem (performGC)
import Unison.Util.PinBoard qualified as PinBoard

test :: Test ()
test =
  scope "util.pinboard" . tests $
    [ scope "pinning equal values stores only one" $ do
        let b0 = ByteString.singleton 0
        let b1 = ByteString.copy b0

        board <- PinBoard.new

        -- pinning a thing for the first time returns it
        b0' <- PinBoard.pin board b0
        expectSamePointer b0 b0'

        -- pinning an equal thing returns the first
        b1' <- PinBoard.pin board b1
        expectSamePointer b0 b1'

        -- the board should only have one value in it
        expect' . (== 1) <$> io (PinBoard.debugSize board)

        -- keep b0 alive until here
        touch b0

        -- observe that the board doesn't keep its value alive
        io performGC
        expect' . (== 0) <$> io (PinBoard.debugSize board)

        ok
    ]

expectSamePointer :: a -> a -> Test ()
expectSamePointer x y =
  expect' (isTrue# (reallyUnsafePtrEquality# x y))

touch :: a -> Test ()
touch x =
  io (IO \s -> (# touch# x s, () #))
