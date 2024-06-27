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

import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Unison.Prelude

-- | The name of a project.
newtype ProjectName
  = UnsafeProjectName Text
  deriving stock (Eq, Ord, Show, Generic)

-- | The name of a branch of a project.
newtype ProjectBranchName
  = UnsafeProjectBranchName Text
  deriving stock (Eq, Ord, Show, Generic)

-- | A generic data structure that contains information about a project and a branch in that project.
data ProjectAndBranch a b = ProjectAndBranch
  { project :: a,
    branch :: b
  }
  deriving stock (Eq, Generic, Show, Functor)

instance Bifunctor ProjectAndBranch where
  bimap f g (ProjectAndBranch a b) = ProjectAndBranch (f a) (g b)

instance Bifoldable ProjectAndBranch where
  bifoldMap f g (ProjectAndBranch a b) = f a <> g b

instance Bitraversable ProjectAndBranch where
  bitraverse f g (ProjectAndBranch a b) = ProjectAndBranch <$> f a <*> g b
