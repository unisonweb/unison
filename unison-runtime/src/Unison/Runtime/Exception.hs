module Unison.Runtime.Exception
  ( module InternalError,
    RuntimeExn (BU, PE),
    die,
    dieP,
    exn,
  )
where

import Control.Exception (throw, throwIO)
import GHC.Stack (CallStack, callStack)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Runtime.InternalError as InternalError
import Unison.Runtime.Stack (Val)
import Unison.Util.Pretty as P

data RuntimeExn
  = -- | pretty exception
    PE CallStack (P.Pretty P.ColorText)
  | -- | __TODO__: What is `BU`? Boxed/Unboxed?
    BU [(Reference, Int)] Text Val
  deriving (Show)

instance Exception RuntimeExn

die :: (HasCallStack) => String -> IO a
die = dieP . P.lit . fromString
{-# INLINE die #-}

dieP :: (HasCallStack) => P.Pretty P.ColorText -> IO a
dieP = throwIO . PE callStack
{-# INLINE dieP #-}

exn :: (HasCallStack) => String -> a
exn = throw . PE callStack . P.lit . fromString
{-# INLINE exn #-}
