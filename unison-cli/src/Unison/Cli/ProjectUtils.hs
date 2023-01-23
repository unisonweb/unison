-- | Project-related utilities.
module Unison.Cli.ProjectUtils
  ( projectPath,
    projectBranchPath,
  )
where

import qualified Data.Text as Text
import qualified Data.UUID as Uuid
import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.Codebase.Path as Path
import Unison.NameSegment (NameSegment (..))

-- | Get the very hacky path from the root branch that we store a project. Users aren't supposed to go here.
projectPath :: Queries.ProjectId -> Path.Absolute
projectPath projectId =
  Path.Absolute (Path.fromList ["__projects", projectNameSegment])
  where
    projectNameSegment :: NameSegment
    projectNameSegment =
      NameSegment (Text.cons '_' (Uuid.toText (Queries.unProjectId projectId)))

-- | Get the very hacky path from the root branch that we store a project branch. Users aren't supposed to go here.
projectBranchPath :: Queries.ProjectId -> Queries.BranchId -> Path.Absolute
projectBranchPath projectId branchId =
  Path.resolve (projectPath projectId) (Path.Relative (Path.fromList ["branches", branchNameSegment]))
  where
    branchNameSegment :: NameSegment
    branchNameSegment =
      NameSegment (Text.cons '_' (Uuid.toText (Queries.unBranchId branchId)))
