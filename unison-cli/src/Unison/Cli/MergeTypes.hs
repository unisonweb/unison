-- | Common types related to merge, pulled down far enough to be imported by all interested parties.
module Unison.Cli.MergeTypes
  ( MergeSource (..),
    MergeTarget,
    MergeSourceAndTarget (..),
    MergeSourceOrTarget (..),
  )
where

import U.Codebase.Sqlite.Project (Project)
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch)
import Unison.Cli.Share.Projects.Types (RemoteProjectBranch)
import Unison.Codebase.Editor.RemoteRepo (ReadShareLooseCode)
import Unison.Project (ProjectAndBranch, ProjectBranchName, ProjectName)

-- | What are we merging in?
data MergeSource
  = MergeSource'LocalProjectBranch !(ProjectAndBranch Project ProjectBranch)
  | MergeSource'RemoteProjectBranch !RemoteProjectBranch
  | MergeSource'RemoteLooseCode !ReadShareLooseCode

type MergeTarget =
  ProjectAndBranch ProjectName ProjectBranchName

-- | "Alice and Bob"
data MergeSourceAndTarget = MergeSourceAndTarget
  { alice :: !MergeTarget,
    bob :: !MergeSource
  }

-- | "Either Alice Bob"
data MergeSourceOrTarget
  = MergeSourceOrTarget'Source !MergeSource
  | MergeSourceOrTarget'Target !MergeTarget
