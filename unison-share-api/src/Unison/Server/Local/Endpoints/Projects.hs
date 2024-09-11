{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Server.Local.Endpoints.Projects
  ( projectListingEndpoint,
    projectBranchListingEndpoint,
    ListProjectsEndpoint,
    ListProjectBranchesEndpoint,
  )
where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import GHC.Generics ()
import Servant
import Servant.Docs
import U.Codebase.Sqlite.Project qualified as SqliteProject
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName), ProjectName (UnsafeProjectName))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Server.Backend (Backend)
import Unison.Symbol (Symbol)

data ProjectListing = ProjectListing
  { projectName :: ProjectName
  }
  deriving stock (Show, Generic)

instance ToSchema ProjectListing

instance ToJSON ProjectListing where
  toJSON ProjectListing {projectName} =
    Aeson.object ["projectName" Aeson..= projectName]

instance ToSample ProjectListing where
  toSamples _ =
    singleSample $ ProjectListing (UnsafeProjectName "my-project")

data ProjectBranchListing = ProjectBranchListing
  { branchName :: ProjectBranchName
  }
  deriving stock (Show, Generic)

instance ToSchema ProjectBranchListing

instance ToJSON ProjectBranchListing where
  toJSON ProjectBranchListing {branchName} =
    Aeson.object ["branchName" Aeson..= branchName]

instance ToSample ProjectBranchListing where
  toSamples _ =
    singleSample $ ProjectBranchListing (UnsafeProjectBranchName "my-branch")

type ListProjectsEndpoint =
  QueryParam "prefix" PrefixFilter
    :> Get '[JSON] [ProjectListing]

type ListProjectBranchesEndpoint =
  QueryParam "prefix" PrefixFilter
    :> Get '[JSON] [ProjectBranchListing]

newtype PrefixFilter = PrefixFilter
  { prefix :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (FromHttpApiData)

instance ToParamSchema PrefixFilter

instance ToParam (QueryParam "prefix" PrefixFilter) where
  toParam _ =
    DocQueryParam
      "prefix"
      ["my-proj"]
      "Filter by project or branch prefix"
      Normal

projectListingEndpoint ::
  Codebase IO Symbol Ann ->
  Maybe PrefixFilter ->
  Backend IO [ProjectListing]
projectListingEndpoint codebase mayPrefix = liftIO . Codebase.runTransaction codebase $ do
  projects <- Q.loadAllProjectsBeginningWith (prefix <$> mayPrefix)
  pure $ ProjectListing . SqliteProject.name <$> projects

projectBranchListingEndpoint ::
  Codebase IO Symbol Ann ->
  ProjectName ->
  Maybe PrefixFilter ->
  Backend IO [ProjectBranchListing]
projectBranchListingEndpoint codebase projectName mayPrefix = liftIO . Codebase.runTransaction codebase . fmap fold . runMaybeT $ do
  SqliteProject.Project {projectId} <- MaybeT $ Q.loadProjectByName projectName
  lift (Q.loadAllProjectBranchesBeginningWith projectId (prefix <$> mayPrefix))
    <&> fmap (ProjectBranchListing . snd)
