{-# LANGUAGE TemplateHaskell #-}

-- | The LLVM IR of the helpers that generated code calls as IR rather than as
-- C (@cbits/jit_helpers.c@, compiled to @cbits/jit_helpers.ll@ by
-- @cbits/gen-helpers.sh@). The text is embedded in the executable at build
-- time and added to the JIT as a module at startup ("The IR helpers" in
-- design.md). This module holds nothing else, because every change to the
-- file recompiles it.
module Unison.Runtime.JIT.Helpers (helpersIR) where

import Data.Text (Text)
import Unison.Util.FileEmbed (embedProjectTextFile)

helpersIR :: Text
helpersIR = $(embedProjectTextFile "cbits/jit_helpers.ll")
