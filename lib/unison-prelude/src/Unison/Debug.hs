{-# LANGUAGE OverloadedStrings #-}

module Unison.Debug (debug, debugM, whenDebug, debugLogM, DebugFlag (..)) where

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
  deriving (Eq, Ord, Bounded, Enum)

debugFlags :: Set DebugFlag
debugFlags = case unsafePerformIO (lookupEnv "UNISON_DEBUG") of
  Nothing -> Set.fromList [minBound .. maxBound]
  Just s -> Set.fromList $ do
    w <- (Text.splitOn "," . Text.pack $ s)
    case Text.toUpper . Text.strip $ w of
      "GIT" -> pure Git
      "SQLITE" -> pure Sqlite
      "CODEBASE" -> pure Codebase
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
