{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Server.Local.Endpoints.Current where

import Control.Monad.Except
import Data.Aeson
import Data.OpenApi (ToSchema (..))
import Servant ((:>))
import Servant.Docs (ToSample (..))
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Project qualified as Project
import U.Codebase.Sqlite.ProjectBranch qualified as ProjectBranch
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName (..), ProjectName (..))
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project.Util (pattern UUIDNameSegment)
import Unison.Server.Backend
import Unison.Server.Types (APIGet)

type CurrentEndpoint =
  "current"
    :> APIGet Current

data Current = Current
  { project :: Maybe ProjectName,
    branch :: Maybe ProjectBranchName,
    path :: Path.Absolute
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

instance ToSample Current where
  toSamples _ =
    [ ( "Current ucm state",
        Current
          (Just $ UnsafeProjectName "@unison/base")
          (Just $ UnsafeProjectBranchName "main")
          (Path.Absolute $ Path.fromText ".__projects._53393e4b_1f61_467c_a488_b6068c727daa.branches._f0aec0e3_249f_4004_b836_572fea3981c1")
      )
    ]

instance ToJSON Current where
  toJSON (Current {..}) =
    object
      [ "project" .= project,
        "branch" .= branch,
        "path" .= path
      ]

serveCurrent :: MonadIO m => Codebase m v a -> Backend m Current
serveCurrent = lift . getCurrentProjectBranch

getCurrentProjectBranch :: MonadIO m => Codebase m v a -> m Current
getCurrentProjectBranch codebase = do
  namespace <- Codebase.runTransaction codebase Queries.expectMostRecentNamespace
  let segments = NameSegment <$> namespace
  let absolutePath = toPath segments
  case toIds segments of
    ProjectAndBranch (Just projectId) branchId ->
      Codebase.runTransaction codebase do
        project <- Queries.expectProject projectId
        branch <- traverse (Queries.expectProjectBranch projectId) branchId
        pure $ Current (Just $ Project.name project) (ProjectBranch.name <$> branch) absolutePath
    ProjectAndBranch _ _ ->
      pure $ Current Nothing Nothing absolutePath
  where
    toIds :: [NameSegment] -> ProjectAndBranch (Maybe ProjectId) (Maybe ProjectBranchId)
    toIds segments =
      case segments of
        "__projects" : UUIDNameSegment projectId : "branches" : UUIDNameSegment branchId : _ ->
          ProjectAndBranch {project = Just $ ProjectId projectId, branch = Just $ ProjectBranchId branchId}
        "__projects" : UUIDNameSegment projectId : _ ->
          ProjectAndBranch {project = Just $ ProjectId projectId, branch = Nothing}
        _ ->
          ProjectAndBranch {project = Nothing, branch = Nothing}

    toPath :: [NameSegment] -> Path.Absolute
    toPath = Path.Absolute . Path.fromList
