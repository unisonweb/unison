-- | @project.switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Control.Lens ((^.))
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import Unison.Cli.ProjectUtils (projectBranchPath)
import Unison.Codebase.Editor.Input (ProjectSwitchInput (..))
import Unison.Prelude
import Witch (into, unsafeFrom)

-- | "Switch" to a project branch.
--
-- Temporary restriction: this command currently does not try to download a missing remote project from Share.
projectSwitch :: ProjectSwitchInput -> Cli ()
projectSwitch ProjectSwitchInput {projectName, maybeBranchName} = do
  let branchName = fromMaybe (unsafeFrom @Text "main") maybeBranchName

  (projectId, branchId) <-
    Cli.label \foundIds -> do
      maybeIds <-
        Cli.runTransaction do
          Queries.loadProjectByName (into @Text projectName) >>= \case
            Nothing -> pure Nothing
            Just project -> do
              let projectId = project ^. #projectId
              Queries.loadProjectBranchByName projectId (into @Text branchName) <&> \case
                Nothing -> Nothing
                Just branch -> Just (projectId, branch ^. #branchId)
      case maybeIds of
        Just ids -> foundIds ids
        Nothing -> error "project not found"

  Cli.cd (projectBranchPath projectId branchId)
