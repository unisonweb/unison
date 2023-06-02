module U.Codebase.Projects
  ( inferDependencyMounts,
  )
where

import Control.Lens (ifoldMap)
import U.Codebase.Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..))
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.Name (libSegment)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Monoid (ifoldMapM)

-- | Find all dependency mounts within a branch and the path to those mounts.
-- For a typical project this will return something like:
-- @[(lib.base, #abc), (lib.distributed, #def)]@
--
-- For a user codebase it will return something like:
-- @[(public.nested.namespace.lib.base, #abc), (public.other.namespace.lib.distributed, #def)]@
inferDependencyMounts :: Branch Sqlite.Transaction -> Sqlite.Transaction [(Path, BranchHash)]
inferDependencyMounts Branch {children} =
  do
    children
    & ifoldMapM \segment child -> do
      case segment of
        seg
          | seg == libSegment -> do
              Branch {children = deps} <- Causal.value child
              deps
                & ( ifoldMap \depName depBranch ->
                      [(Path.fromList [seg, depName], Causal.valueHash depBranch)]
                  )
                & pure
          | otherwise -> do
              childBranch <- Causal.value child
              inferDependencyMounts childBranch
                <&> map (first (Path.cons seg))
