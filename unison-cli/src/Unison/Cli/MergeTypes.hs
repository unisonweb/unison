-- | Common types related to merge, pulled down far enough to be imported by all interested parties.
module Unison.Cli.MergeTypes
  ( MergeSource (..),
    MergeTarget,
    MergeSourceAndTarget (..),
    MergeSourceOrTarget (..),
  )
where

import Unison.Codebase.Editor.RemoteRepo (ReadGitRemoteNamespace, ReadShareLooseCode)
import Unison.Project (ProjectAndBranch, ProjectBranchName, ProjectName)

-- | What are we merging in?
data MergeSource
  = MergeSource'LocalProjectBranch !(ProjectAndBranch ProjectName ProjectBranchName)
  | MergeSource'RemoteProjectBranch !(ProjectAndBranch ProjectName ProjectBranchName)
  | MergeSource'RemoteLooseCode !ReadShareLooseCode
  | MergeSource'RemoteGitRepo !ReadGitRemoteNamespace

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
