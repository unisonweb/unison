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
-- The convention is a property of the process, decided once, like the
-- replacements mode in JIT/Config.hs. The interpreter alone does not gain
-- from it (JIT/benchmarks/2026-10-27-wide-interpreter.md), so it is on
-- exactly when the JIT is (@UNISON_JIT=on@ or @eager@) unless
-- @UNISON_WIDE=on|off@ overrides that, for testing either combination. A
-- compiled program's MCode has the convention of the mode it was compiled
-- under baked in (its frames and the instructions that write them), so its
-- file carries the mode and 'setWide' applies it before anything loads.
module Unison.Runtime.Wide
  ( wideOn,
    wideArgsOn,
    setWide,
    wideK,
    wideWidth,
    widePad,
  )
where

import Control.Exception (throwIO)
import Data.Char (toLower)
import Data.IORef
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | Whether the process uses the wide calling convention.
wideOn :: Bool
wideOn = unsafePerformIO $ do
  chosen <- readIORef wideRef
  w <- maybe wideFromEnvironment pure chosen
  writeIORef wideRef (Just w)
  writeIORef wideUsed True
  pure w
{-# NOINLINE wideOn #-}

wideFromEnvironment :: IO Bool
wideFromEnvironment = do
  override <- lookupEnv "UNISON_WIDE"
  mode <- lookupEnv "UNISON_JIT"
  pure $ case (map toLower <$> override, map toLower (fromMaybe "off" mode)) of
    (Just "on", _) -> True
    (Just "off", _) -> False
    (_, "on") -> True
    (_, "eager") -> True
    _ -> False

-- | Whether known functions take the arguments they only match on as
-- slots (MCode's 'argWidths'): on with the convention unless
-- @UNISON_WIDE_ARGS=off@, which is for measuring the two apart. A
-- compiled program's file does not carry it: its workers are in its MCode
-- either way, and code it loads later only consults the widths stored
-- with it.
wideArgsOn :: Bool
wideArgsOn = unsafePerformIO $ do
  override <- lookupEnv "UNISON_WIDE_ARGS"
  pure $ wideOn && (map toLower <$> override) /= Just "off"
{-# NOINLINE wideArgsOn #-}

wideRef :: IORef (Maybe Bool)
wideRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE wideRef #-}

wideUsed :: IORef Bool
wideUsed = unsafePerformIO (newIORef False)
{-# NOINLINE wideUsed #-}

-- | Chooses the convention, for a compiled program from the mode stored in
-- its file. It has to happen before any code has used the setting: code
-- already loaded under the other convention can't be redone, so a late
-- choice that differs is an error.
setWide :: Bool -> IO ()
setWide w = do
  used <- readIORef wideUsed
  chosen <- readIORef wideRef
  case chosen of
    Just w' | used && w' /= w -> throwIO (userError ("the wide calling convention is " ++ (if w' then "on" else "off") ++ " and already in use; it cannot be changed to " ++ (if w then "on" else "off")))
    _ -> writeIORef wideRef (Just w)

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
