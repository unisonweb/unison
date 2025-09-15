{-# LANGUAGE OverloadedStrings #-}

-- | This module is distinct from "Unison.Runtime.Exception" because that depends on "Unison.Runtime.Stack", which would
--   cause an import cycle.
module Unison.Runtime.InternalError
  ( CompileExn (CE),
    internalBug,
  )
where

import Control.Exception (throw)
import GHC.Stack (CallStack, callStack)
import Unison.Prelude

data CompileExn = CE CallStack [Word] String
  deriving (Show)

instance Exception CompileExn

internalBug :: (HasCallStack) => [Word] -> String -> a
internalBug issues = throw . CE callStack issues
