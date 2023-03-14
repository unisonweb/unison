-- | Project-related types.
--
-- A larger API, including orphan instances for parsing from 'Text', is available in "Unison.Project". Here, we just
-- define the types, which are shared among the low-level database layer (which assumes without verifying that project
-- names and such are syntactically valid) and the higher-level project manipulation exposed by UCM.
module Unison.Core.Project
  ( ProjectName (..),
    ProjectBranchName (..),
    ProjectAndBranch (..),
  )
where

import Unison.Prelude

-- | The name of a project.
newtype ProjectName
  = UnsafeProjectName Text
  deriving stock (Eq, Ord, Show)

-- | The name of a branch of a project.
newtype ProjectBranchName
  = ProjectBranchName Text
  deriving stock (Eq, Ord, Show)

-- | A generic data structure that contains information about a project and a branch in that project.
data ProjectAndBranch a b = ProjectAndBranch
  { project :: a,
    branch :: b
  }
  deriving stock (Eq, Generic, Show)
