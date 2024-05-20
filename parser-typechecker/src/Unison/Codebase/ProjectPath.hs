module Unison.Codebase.ProjectPath
  ( ProjectPath (..),
    ProjectPathIds,
    ProjectPathNames,
    ProjectPathCtx,
    absPath_,
    path_,
    projectAndBranch_,
    toText,
    ctxAsIds_,
    ctxAsNames_,
    project_,
    branch_,
  )
where

import Control.Lens
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import U.Codebase.Sqlite.DbId (ProjectBranchId, ProjectId)
import Unison.Codebase.Path qualified as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)

data ProjectPath proj branch = ProjectPath
  { projPathProject :: proj,
    projPathBranch :: branch,
    projPathPath :: Path.Absolute
  }
  deriving stock (Eq, Ord, Show)

type ProjectPathIds = ProjectPath ProjectId ProjectBranchId

type ProjectPathNames = ProjectPath ProjectName ProjectBranchName

type ProjectPathCtx = ProjectPath (ProjectId, ProjectName) (ProjectBranchId, ProjectBranchName)

project_ :: Lens' (ProjectPath p b) p
project_ = lens go set
  where
    go (ProjectPath p _ _) = p
    set (ProjectPath _ b path) p = ProjectPath p b path

branch_ :: Lens' (ProjectPath p b) b
branch_ = lens go set
  where
    go (ProjectPath _ b _) = b
    set (ProjectPath p _ path) b = ProjectPath p b path

-- | Project a project context into a project path of just IDs
ctxAsIds_ :: Lens' ProjectPathCtx ProjectPathIds
ctxAsIds_ = lens go set
  where
    go (ProjectPath (pid, _) (bid, _) p) = ProjectPath pid bid p
    set (ProjectPath (_, pName) (_, bName) _) (ProjectPath pid bid p) = ProjectPath (pid, pName) (bid, bName) p

-- | Project a project context into a project path of just names
ctxAsNames_ :: Lens' ProjectPathCtx ProjectPathNames
ctxAsNames_ = lens go set
  where
    go (ProjectPath (_, pName) (_, bName) path) = ProjectPath pName bName path
    set (ProjectPath (pId, _) (bId, _) _) (ProjectPath pName bName path) = ProjectPath (pId, pName) (bId, bName) path

instance Bifunctor ProjectPath where
  bimap f g (ProjectPath p b path) = ProjectPath (f p) (g b) path

instance Bifoldable ProjectPath where
  bifoldMap f g (ProjectPath p b _) = f p <> g b

instance Bitraversable ProjectPath where
  bitraverse f g (ProjectPath p b path) = ProjectPath <$> f p <*> g b <*> pure path

toText :: ProjectPath ProjectName ProjectBranchName -> Text
toText (ProjectPath projName branchName path) =
  into @Text projName <> "/" <> into @Text branchName <> ":" <> Path.absToText path

absPath_ :: Lens' (ProjectPath p b) Path.Absolute
absPath_ = lens go set
  where
    go (ProjectPath _ _ p) = p
    set (ProjectPath n b _) p = ProjectPath n b p

path_ :: Lens' (ProjectPath p b) Path.Path
path_ = absPath_ . Path.absPath_

projectAndBranch_ :: Lens' (ProjectPath p b) (ProjectAndBranch p b)
projectAndBranch_ = lens go set
  where
    go (ProjectPath proj branch _) = ProjectAndBranch proj branch
    set (ProjectPath _ _ p) (ProjectAndBranch proj branch) = ProjectPath proj branch p
