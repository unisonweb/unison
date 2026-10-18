{-# LANGUAGE TemplateHaskell #-}

-- | The LLVM IR the JIT embeds and adds as modules of its own at startup
-- (both produced by @cbits/gen-helpers.sh@, "The IR helpers" in
-- design.md): the helpers that generated code calls as IR rather than as
-- C (@cbits/jit_helpers.c@, as @jit_helpers.ll@) and the write-back
-- routine every exit calls (@cbits/jit_exit.c@, as @jit_exit.ll@). The
-- text is embedded in the executable at build time. This module holds
-- nothing else, because every change to either file recompiles it.
module Unison.Runtime.JIT.Helpers (helpersIR, exitIR) where

import Data.Text (Text)
import Unison.Util.FileEmbed (embedProjectTextFile)

helpersIR :: Text
helpersIR = $(embedProjectTextFile "cbits/jit_helpers.ll")

exitIR :: Text
exitIR = $(embedProjectTextFile "cbits/jit_exit.ll")
