module Unison.Codebase.SqliteCodebase.Branch.Cache () where

import Data.Map qualified as Map
import System.Mem.Weak
import U.Codebase.HashTags qualified as V2
import Unison.Codebase.Branch qualified as V1.Branch
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import UnliftIO.STM

type BranchCache m = InternCache m V2.CausalHash (V1.Branch.Branch m)
