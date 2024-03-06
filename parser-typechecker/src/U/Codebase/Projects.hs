module U.Codebase.Projects
  ( inferDependencyMounts,
  )
where

import Control.Lens (ifoldMap)
import Data.Map qualified as Map
import U.Codebase.Branch
import U.Codebase.Branch qualified as Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..))
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.NameSegment (libSegment)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Monoid (ifoldMapM)

-- | Find all dependency mounts within a branch and the path to those mounts.
--
-- For a typical project this will return something like:
-- @[(lib.base, #abc), (lib.distributed, #def)]@
--
-- For the top-level name lookup of a user codebase it returns the project roots, and will return something like:
-- @[(public.nested.myproject.latest, #abc), (public.other.namespace.otherproject.main, #def)]@
inferDependencyMounts :: Branch Sqlite.Transaction -> Sqlite.Transaction [(Path, BranchHash)]
inferDependencyMounts branch = do
  children <- Branch.nonEmptyChildren branch
  do
    children
    & ifoldMapM \segment child -> do
      case segment of
        seg
          | seg == libSegment -> do
              childBranch <- Causal.value child
              deps <- Branch.nonEmptyChildren childBranch
              deps
                & ( ifoldMap \depName depBranch ->
                      [(Path.fromList [seg, depName], Causal.valueHash depBranch)]
                  )
                & pure
          | otherwise -> do
              childBranch <- Causal.value child
              nestedChildren <- Branch.nonEmptyChildren childBranch
              -- If a given child has a lib child, then it's inferred to be a project root.
              -- This allows us to detect most project roots in loose code.
              -- Note, we only do this on children nested at least one level deep
              -- to avoid treating project roots as their own self-referential dependency
              -- mounts. Mount paths must not be empty.
              case Map.member libSegment nestedChildren of
                True -> pure [(Path.fromList [seg], Causal.valueHash child)]
                False -> inferDependencyMounts childBranch <&> map (first (Path.cons seg))
