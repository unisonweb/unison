{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Unison.Test.Util.PinBoard
  ( test,
  )
where

import qualified Data.ByteString as ByteString
import EasyTest
import GHC.Exts (reallyUnsafePtrEquality#, isTrue#)
import qualified Unison.Util.PinBoard as PinBoard

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
        n <- io (PinBoard.debugSize board)
        expect' (n == 1)

        ok
    ]

expectSamePointer :: a -> a -> Test ()
expectSamePointer x y =
  expect' (isTrue# (reallyUnsafePtrEquality# x y))
