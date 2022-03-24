{-# LANGUAGE OverloadedStrings #-}

module Unison.Debug
  ( debug,
    debugM,
    whenDebug,
    shouldDebug,
    debugLog,
    debugLogM,
    useStaging,
    DebugFlag (..),
  )
where

import Control.Applicative (empty)
import Control.Monad (when)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Debug.Pretty.Simple (pTrace, pTraceM, pTraceShowId, pTraceShowM)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Environment (lookupEnv)

data DebugFlag
  = Git
  | Sqlite
  | Codebase
  | UseStaging
  deriving (Eq, Ord, Show, Bounded, Enum)

debugFlags :: Set DebugFlag
debugFlags = pTraceShowId $ case pTraceShowId $ (unsafePerformIO (lookupEnv "UNISON_DEBUG")) of
  Nothing -> Set.empty
  -- Enable all debugging flags for bare UNISON_DEBUG declarations like:
  -- UNISON_DEBUG= ucm
  Just "" -> Set.fromList [minBound .. maxBound]
  Just s -> Set.fromList $ do
    w <- (Text.splitOn "," . Text.pack $ s)
    case Text.toUpper . Text.strip $ w of
      "GIT" -> pure Git
      "SQLITE" -> pure Sqlite
      "CODEBASE" -> pure Codebase
      "STAGING" -> pure UseStaging
      _ -> empty
{-# NOINLINE debugFlags #-}

debugGit :: Bool
debugGit = Git `Set.member` debugFlags
{-# NOINLINE debugGit #-}

debugSqlite :: Bool
debugSqlite = Sqlite `Set.member` debugFlags
{-# NOINLINE debugSqlite #-}

debugCodebase :: Bool
debugCodebase = Codebase `Set.member` debugFlags
{-# NOINLINE debugCodebase #-}

useStaging :: Bool
useStaging = UseStaging `Set.member` debugFlags
{-# NOINLINE useStaging #-}

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

debugLog :: DebugFlag -> String -> a -> a
debugLog flag msg =
  if (shouldDebug flag)
    then pTrace msg
    else id

debugLogM :: (Monad m) => DebugFlag -> String -> m ()
debugLogM flag msg =
  when (shouldDebug flag) $ pTraceM msg

-- | A 'when' block which is triggered if the given flag is being debugged.
whenDebug :: Monad m => DebugFlag -> m () -> m ()
whenDebug flag action = do
  when (shouldDebug flag) action

shouldDebug :: DebugFlag -> Bool
shouldDebug = \case
  Git -> debugGit
  Sqlite -> debugSqlite
  Codebase -> debugCodebase
  UseStaging -> useStaging
