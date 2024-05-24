module Unison.Codebase.ProjectPath
  ( ProjectPathG (..),
    ProjectPathIds,
    ProjectPathNames,
    ProjectPath,
    fromProjectAndBranch,
    absPath_,
    path_,
    projectAndBranch_,
    toText,
    toIds,
    toNames,
    asProjectAndBranch_,
  )
where

import Control.Lens
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import U.Codebase.Sqlite.DbId (ProjectBranchId, ProjectId)
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import Unison.Codebase.Path qualified as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)

data ProjectPathG proj branch = ProjectPath
  { project :: proj,
    branch :: branch,
    absPath :: Path.Absolute
  }
  deriving stock (Eq, Ord, Show, Generic)

type ProjectPathIds = ProjectPathG ProjectId ProjectBranchId

type ProjectPathNames = ProjectPathG ProjectName ProjectBranchName

type ProjectPath = ProjectPathG Project ProjectBranch

fromProjectAndBranch :: ProjectAndBranch Project ProjectBranch -> Path.Absolute -> ProjectPath
fromProjectAndBranch (ProjectAndBranch proj branch) path = ProjectPath proj branch path

-- | Project a project context into a project path of just IDs
toIds :: ProjectPath -> ProjectPathIds
toIds (ProjectPath proj branch path) = ProjectPath (proj ^. #projectId) (branch ^. #branchId) path

-- | Project a project context into a project path of just names
toNames :: ProjectPath -> ProjectPathNames
toNames (ProjectPath proj branch path) = ProjectPath (proj ^. #name) (branch ^. #name) path

asProjectAndBranch_ :: Lens' ProjectPath (ProjectAndBranch Project ProjectBranch)
asProjectAndBranch_ = lens get set
  where
    get (ProjectPath proj branch _) = ProjectAndBranch proj branch
    set p (ProjectAndBranch proj branch) = p & #project .~ proj & #branch .~ branch

instance Bifunctor ProjectPathG where
  bimap f g (ProjectPath p b path) = ProjectPath (f p) (g b) path

instance Bifoldable ProjectPathG where
  bifoldMap f g (ProjectPath p b _) = f p <> g b

instance Bitraversable ProjectPathG where
  bitraverse f g (ProjectPath p b path) = ProjectPath <$> f p <*> g b <*> pure path

toText :: ProjectPathG Project ProjectBranch -> Text
toText (ProjectPath proj branch path) =
  into @Text (proj ^. #name) <> "/" <> into @Text (branch ^. #name) <> ":" <> Path.absToText path

absPath_ :: Lens' (ProjectPathG p b) Path.Absolute
absPath_ = lens go set
  where
    go (ProjectPath _ _ p) = p
    set (ProjectPath n b _) p = ProjectPath n b p

path_ :: Lens' (ProjectPathG p b) Path.Path
path_ = absPath_ . Path.absPath_

projectAndBranch_ :: Lens' (ProjectPathG p b) (ProjectAndBranch p b)
projectAndBranch_ = lens go set
  where
    go (ProjectPath proj branch _) = ProjectAndBranch proj branch
    set (ProjectPath _ _ p) (ProjectAndBranch proj branch) = ProjectPath proj branch p
