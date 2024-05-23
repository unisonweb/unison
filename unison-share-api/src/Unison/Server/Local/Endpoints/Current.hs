{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Server.Local.Endpoints.Current where

import Control.Lens hiding ((.=))
import Control.Monad.Except
import Data.Aeson
import Data.OpenApi (ToSchema (..))
import Servant ((:>))
import Servant.Docs (ToSample (..))
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.Prelude
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
          (Path.Absolute $ Path.unsafeParseText ".my.namespace")
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
  pp <-
    Codebase.runTransaction codebase Codebase.loadCurrentProjectPath <&> \case
      Nothing ->
        -- TODO: Come up with a better solution for this
        error "No current project path context"
      Just pp -> pp
  let (PP.ProjectPath projName branchName path) = pp ^. PP.asNames_
  pure $ Current (Just projName) (Just branchName) path
