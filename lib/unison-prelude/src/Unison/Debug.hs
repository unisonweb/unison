module Unison.Debug (debug, debugM, whenDebug, DebugFlag (..)) where

import Control.Monad (when)
import Data.Maybe (isJust)
import Debug.Pretty.Simple (pTrace, pTraceM, pTraceShowId, pTraceShowM)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Environment (lookupEnv)

data DebugFlag
  = Git
  | Sqlite
  | Codebase

debugAll :: Bool
debugAll =
  isJust (unsafePerformIO (lookupEnv "UNISON_DEBUG"))
{-# NOINLINE debugAll #-}

debugGit :: Bool
debugGit =
  isJust (unsafePerformIO (lookupEnv "UNISON_DEBUG_GIT"))
{-# NOINLINE debugGit #-}

debugSqlite :: Bool
debugSqlite =
  isJust (unsafePerformIO (lookupEnv "UNISON_DEBUG_SQLITE"))
{-# NOINLINE debugSqlite #-}

debugCodebase :: Bool
debugCodebase =
  isJust (unsafePerformIO (lookupEnv "UNISON_DEBUG_CODEBASE"))
{-# NOINLINE debugCodebase #-}

-- | Use for trace-style selective debugging.
-- E.g. 1 + (debug Git "The second number" 2)
--
-- Or, use in pattern matching to view arguments.
-- E.g.
-- myFunc (debug Git "argA" -> argA) = ...
debug :: Show a => DebugFlag -> String -> a -> a
debug flag msg a =
  if shouldDebug flag
    then pTrace (msg <> ":\n") $ pTraceShowId a
    else a

-- | Use for selective debug logging in monadic contexts.
-- E.g.
-- do
--   debugM Git "source repo" srcRepo
--   ...
debugM :: (Show a, Monad m) => DebugFlag -> String -> a -> m ()
debugM flag msg a =
  when (shouldDebug flag) $ do
    pTraceM (msg <> ":\n")
    pTraceShowM a

-- | A 'when' block which is triggered if the given flag is being debugged.
whenDebug :: Monad m => DebugFlag -> m () -> m ()
whenDebug flag action = do
  when (shouldDebug flag) action

shouldDebug :: DebugFlag -> Bool
shouldDebug flag =
  debugAll || case flag of
    Git -> debugGit
    Sqlite -> debugSqlite
    Codebase -> debugCodebase
