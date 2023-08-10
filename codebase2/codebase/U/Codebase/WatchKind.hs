module U.Codebase.WatchKind where

data WatchKind
  = RegularWatch
  | TestWatch
  | IOWatch
  deriving (Eq, Ord, Show)
