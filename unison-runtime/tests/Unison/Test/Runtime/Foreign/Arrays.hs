{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The array builtins' checks that only ill-typed code can reach, which no
-- transcript can exercise: the typechecker keeps a PinnedByteArray and a
-- ByteArray apart, but at run time they are the same object, so the builtin
-- has to check pinnedness itself (security review, 2026-10-24).
module Unison.Test.Runtime.Foreign.Arrays (test) where

import Control.Monad.Primitive (RealWorld)
import Data.Primitive.ByteArray qualified as PA
import EasyTest
import Unison.Runtime.Foreign.Function (ForeignConvention (..), foreignCall)
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (..))
import Unison.Runtime.MCode (Args (..))
import Unison.Runtime.Stack

-- | Runs a foreign function on a stack holding just its one argument and
-- says whether it raised (the Left of mkForeignExn).
callRaises :: ForeignFunc -> PA.MutableByteArray RealWorld -> IO Bool
callRaises ff ba = do
  stk0 <- alloc defaultStackSlots
  stk <- bump stk0
  poke stk (encodeVal ba)
  (raised, _) <- exStackIOToIO (foreignCall ff (VArg1 0) (unpackXStack stk))
  pure raised

test :: Test ()
test =
  scope "Foreign.Arrays" . tests $
    [ scope "PinnedByteArray.contents accepts a pinned array" do
        ba <- io $ PA.newPinnedByteArray 32
        raised <- io $ callRaises PinnedByteArray_contents ba
        expectEqual False raised,
      scope "PinnedByteArray.contents refuses an unpinned array" do
        ba <- io $ PA.newByteArray 32
        raised <- io $ callRaises PinnedByteArray_contents ba
        expectEqual True raised,
      scope "PinnedByteArray.contents refuses a large unpinned array" do
        -- large objects don't move, but the primop (GHC 9.10) and the C copy
        -- both count only arrays made pinned
        ba <- io $ PA.newByteArray 1000000
        raised <- io $ callRaises PinnedByteArray_contents ba
        expectEqual True raised,
      scope "PinnedByteArray.contents accepts a large pinned array" do
        ba <- io $ PA.newPinnedByteArray 1000000
        raised <- io $ callRaises PinnedByteArray_contents ba
        expectEqual False raised
    ]
