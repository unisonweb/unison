-- | @project.create@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectCreate
  ( projectCreate,
  )
where

import qualified Data.UUID.V4 as Uuid
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (stepAt)
import Unison.Cli.ProjectUtils (projectBranchPath)
import Unison.Codebase.Branch (Branch0)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectName)
import Witch

projectCreate :: ProjectName -> Cli ()
projectCreate name = do
  projectId <- liftIO (Queries.ProjectId <$> Uuid.nextRandom)
  branchId <- liftIO (Queries.BranchId <$> Uuid.nextRandom)

  Cli.runEitherTransaction do
    Queries.projectExistsByName (into @Text name) >>= \case
      False -> do
        Queries.insertProject projectId (into @Text name)
        Queries.insertBranch projectId branchId "main"
        pure (Right ())
      True -> pure (Left (error "project by that name already exists"))

  let path :: Path.Absolute
      path = projectBranchPath projectId branchId

  let initialBranchContents :: Branch0 m
      initialBranchContents =
        Branch.empty0

  Cli.stepAt "project.create" (Path.unabsolute path, const initialBranchContents)

  Cli.cd path
