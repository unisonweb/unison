module Unison.CommandLine.Types (ShouldWatchFiles (..)) where

data ShouldWatchFiles
  = ShouldWatchFiles
  | ShouldNotWatchFiles
  deriving (Show, Eq)
