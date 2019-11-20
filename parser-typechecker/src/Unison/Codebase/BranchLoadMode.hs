module Unison.Codebase.BranchLoadMode where

-- When loading a nonexistent branch, what should happen?
-- Could bomb (`FailIfMissing`) or return the empty branch (`EmptyIfMissing`).
--
-- `EmptyIfMissing` mode is used when attempting to load a user-specified
-- branch. `FailIfMissing` is used when loading the root branch - if the root
-- does not exist, that's a serious problem.
data BranchLoadMode = FailIfMissing | EmptyIfMissing deriving (Eq, Show)
