{-# LANGUAGE ExistentialQuantification #-}

module Unison.Test.Runtime.Foreign.Dynamic (test) where

import Control.Exception (try)
import Control.Monad (replicateM_, void)
import EasyTest
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import System.Mem (performGC)
import Unison.Runtime.Foreign.Dynamic

foreign import ccall unsafe "&snprintf" snprintfPtr :: FunPtr ()

foreign import ccall unsafe "&strlen" strlenPtr :: FunPtr ()

data Argument = forall a. (Storable a) => Argument a

withArguments :: [Argument] -> (Ptr (Ptr ()) -> IO a) -> IO a
withArguments args action = go args []
  where
    go [] ptrs = withArray (reverse ptrs) action
    go (Argument x : xs) ptrs = with x \p -> go xs (castPtr p : ptrs)

sizeType :: FFType
sizeType = if sizeOf (undefined :: CSize) == 8 then U64 else U32

-- Reuse a prepared interface after other preparations and GC. In particular,
-- the argument-type array must not be a temporary alloca in prepareSpec.
formatWith :: [FFType] -> [Argument] -> String -> IO (String, CInt)
formatWith types values format = do
  spec <- prepareSpec $ FFSpec ([MBArr, sizeType, Ptr] ++ types) I32 (Just 3)
  let fun = CDynFunc "snprintf" spec snprintfPtr
  replicateM_ 100 $ void $ prepareSpec $ FFSpec [D64, U64, I32, Ptr] D64 Nothing
  performGC
  allocaBytes 256 \buffer ->
    withCString format \fmt ->
      withArguments ([Argument buffer, Argument (256 :: CSize), Argument fmt] ++ values) \args ->
        allocaBytes 8 \result -> do
          replicateM_ 20 $ callForeign fun args result
          (,) <$> peekCString buffer <*> peek (castPtr result)

test :: Test ()
test =
  scope "ffi.dynamic" $
    tests
      [ scope "fixed calls" do
          actual <- io do
            spec <- prepareSpec $ FFSpec [Ptr] sizeType Nothing
            performGC
            withCString "fixed FFI" \str ->
              withArguments [Argument str] \args ->
                allocaBytes 8 \result -> do
                  callForeign (CDynFunc "strlen" spec strlenPtr) args result
                  peek (castPtr result) :: IO CSize
          expectEqual 9 actual,
        scope "no optional arguments" do
          actual <- io $ formatWith [] [] "no extras"
          expectEqual ("no extras", 9) actual,
        scope "mixed variadic arguments and register overflow" do
          actual <- io $ withCString "hello" \str ->
            formatWith
              [I32, Ptr, D64, U32, I64, U64, D64, I32, D64, I32, D64, I32, D64, I32, D64]
              [ Argument (-7 :: CInt),
                Argument str,
                Argument (2.5 :: CDouble),
                Argument (99 :: CUInt),
                Argument (-1234567890123 :: CLLong),
                Argument (12345678901234 :: CULLong),
                Argument (3.5 :: CDouble),
                Argument (4 :: CInt),
                Argument (4.5 :: CDouble),
                Argument (5 :: CInt),
                Argument (5.5 :: CDouble),
                Argument (6 :: CInt),
                Argument (6.5 :: CDouble),
                Argument (7 :: CInt),
                Argument (7.5 :: CDouble)
              ]
              "%d %s %.1f %u %lld %llu %.1f %d %.1f %d %.1f %d %.1f %d %.1f"
          let expected = "-7 hello 2.5 99 -1234567890123 12345678901234 3.5 4 4.5 5 5.5 6 6.5 7 7.5"
          expectEqual (expected, fromIntegral $ length expected) actual,
        scope "fixed prefix is not promoted" do
          actual <- io $ try $ void $ prepareSpec $ FFSpec [F32, I8, U16, D64, I32] Void (Just 3)
          expectEqual (Right () :: Either PrepException ()) actual,
        scope "void placeholder for fixed no-argument function" do
          actual <- io $ ffArgs . ffSpec <$> prepareSpec (FFSpec [Void] I32 Nothing)
          expectEqual [] actual,
        scope "reject invalid fixed counts" $
          tests
            [ rejects (BadFixedArgs n 2) (FFSpec [I32, D64] I32 (Just n))
            | n <- [0, 3, 4294967296, maxBound]
            ],
        scope "reject unpromoted optional arguments" $
          tests
            [ rejects (BadVariadicArg 2 t) (FFSpec [I32, t] I32 (Just 1))
            | t <- [F32, I8, U8, I16, U16]
            ],
        scope "reject void arguments" $
          tests
            [ rejects BadVoid (FFSpec [I32, Void] I32 (Just 1)),
              rejects BadVoid (FFSpec [Void] I32 (Just 1)),
              rejects BadVoid (FFSpec [I32, Void] I32 Nothing)
            ],
        scope "reject array results" $
          rejects BadResult (FFSpec [I32] MBArr (Just 1))
      ]
  where
    rejects expected spec = do
      actual <- io $ try $ void $ prepareSpec spec
      expectEqual (Left expected) actual
