module Unison.Runtime.Exception
  ( RuntimeExn (BU, PE),
    die,
    exn,
  )
where

import Control.Exception (throw, throwIO)
import GHC.Stack (CallStack, callStack)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Runtime.Stack (Val)
import Unison.Util.Pretty as P

data RuntimeExn
  = -- | pretty exception
    PE CallStack [Word] (P.Pretty P.ColorText)
  | -- | a failure in Unison code
    BU
      -- | Unison stack
      [(Reference, Int)]
      -- | message
      Text
      -- | Unison value
      Val
  deriving (Show)

instance Exception RuntimeExn

peStr :: (HasCallStack) => [Word] -> String -> RuntimeExn
peStr issues = PE callStack issues . P.lit . fromString
{-# INLINE peStr #-}

die :: (HasCallStack) => [Word] -> String -> IO a
die issues s = do
  void . throwIO $ peStr issues s
  -- This is unreachable, but we need it to fix some quirks in GHC's
  -- worker/wrapper optimization, specifically, it seems that when throwIO's polymorphic return
  -- value is specialized to a type like 'Stack' which we want GHC to unbox, it will sometimes
  -- fail to unbox it, possibly because it can't unbox it when it's strictly a type application.
  -- For whatever reason, this seems to fix it while still allowing us to throw exceptions in IO
  -- like we prefer.
  error "unreachable"
{-# INLINE die #-}

exn :: (HasCallStack) => [Word] -> String -> a
exn issues = throw . peStr issues
{-# INLINE exn #-}
