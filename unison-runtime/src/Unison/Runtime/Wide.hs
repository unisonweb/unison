-- | The wide calling convention: whether it is on, and how many field slots
-- a wide variable has. See delayed-data-construction.local.md for the design.
--
-- A wide variable occupies @wideK + 1@ stack slots: its value slot on top (the
-- lowest index in the emitter's context) and @wideK@ field slots below it.
-- Every yield delivers @wideK + 1@ slots, and every variable bound by an
-- indirect let is wide, since its value arrives by a yield. Whether a
-- variable is wide is a function of the code alone, so processes in either
-- mode agree on the layout of a serialized continuation.
--
-- The convention is a property of the process, decided once from
-- @UNISON_WIDE@ (@on@ turns it on; the default is off while it is being
-- built), like the replacements mode in JIT/Config.hs. The JIT does not yet
-- generate code for wide frames, so it is off whenever this is on.
module Unison.Runtime.Wide
  ( wideOn,
    wideK,
    wideWidth,
    widePad,
  )
where

import Data.Char (toLower)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | Whether the process uses the wide calling convention.
wideOn :: Bool
wideOn = unsafePerformIO $ do
  v <- lookupEnv "UNISON_WIDE"
  pure (fmap (map toLower) v == Just "on")
{-# NOINLINE wideOn #-}

-- | The number of field slots of a wide variable.
wideK :: Int
wideK = 2

-- | The number of slots a wide variable occupies: 1 when the convention is
-- off, so that the layout is unchanged.
wideWidth :: Int
wideWidth
  | wideOn = wideK + 1
  | otherwise = 1

-- | The slots below a wide variable's value slot: 'wideWidth' - 1.
widePad :: Int
widePad = wideWidth - 1
