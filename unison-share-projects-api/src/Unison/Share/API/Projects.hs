{-# LANGUAGE DataKinds #-}

module Unison.Share.API.Projects
  ( -- * API
    ProjectsAPI,

    -- ** Get project
    GetProjectAPI,
    GetProjectResponse (..),

    -- ** Create project
    CreateProjectAPI,
    CreateProjectRequest (..),
    CreateProjectResponse (..),

    -- ** Create project branch
    CreateProjectBranchAPI,
    CreateProjectBranchRequest (..),
    CreateProjectBranchResponse (..),

    -- * Types
    Project (..),
    ProjectBranch (..),
    ProjectBranchIds (..),
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Servant.API
import Unison.Hash32 (Hash32)

type ProjectsAPI =
  GetProjectAPI
    :<|> CreateProjectAPI
    :<|> CreateProjectBranchAPI

------------------------------------------------------------------------------------------------------------------------
-- Get project

-- | [@GET /project?id=XXX@]: Get a project by id.
--
-- [@GET /project?name=XXX@]: Get a project by name.
type GetProjectAPI =
  "project"
    :> QueryParam "id" Text
    :> QueryParam "name" Text
    :> Verb Get 200 '[JSON] GetProjectResponse

-- | @GET /project@ response.
data GetProjectResponse
  = GetProjectResponseNotFound
  | GetProjectResponseSuccess !Project
  deriving stock (Eq, Show)

instance FromJSON GetProjectResponse where
  parseJSON =
    withSumType "GetProjectResponse" \typ val ->
      case typ of
        "not-found" -> pure GetProjectResponseNotFound
        "success" -> GetProjectResponseSuccess <$> parseJSON val
        _ -> fail (Text.unpack ("unknown GetProjectResponse type: " <> typ))

instance ToJSON GetProjectResponse where
  toJSON = \case
    GetProjectResponseNotFound -> toSumType "not-found" (object [])
    GetProjectResponseSuccess project -> toSumType "success" (toJSON project)

------------------------------------------------------------------------------------------------------------------------
-- Create project

-- | [@POST /create-project@]: Create a project
type CreateProjectAPI =
  "create-project"
    :> ReqBody '[JSON] CreateProjectRequest
    :> Verb Post 200 '[JSON] CreateProjectResponse

-- | @POST /create-project@ request.
data CreateProjectRequest = CreateProjectRequest
  { projectName :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON CreateProjectRequest where
  parseJSON =
    withObject "CreateProjectRequest" \o -> do
      projectName <- parseField o "projectName"
      pure CreateProjectRequest {projectName}

instance ToJSON CreateProjectRequest where
  toJSON CreateProjectRequest {projectName} =
    object
      [ "projectName" .= projectName
      ]

-- | @POST /create-project@ response.
data CreateProjectResponse
  = -- | Request payload invalid.
    CreateProjectResponseBadRequest
  | CreateProjectResponseUnauthorized
  | CreateProjectResponseSuccess !Project
  deriving stock (Eq, Show)

instance FromJSON CreateProjectResponse where
  parseJSON =
    withSumType "CreateProjectResponse" \typ val ->
      case typ of
        "bad-request" -> pure CreateProjectResponseBadRequest
        "unauthorized" -> pure CreateProjectResponseUnauthorized
        "success" -> CreateProjectResponseSuccess <$> parseJSON val
        _ -> fail (Text.unpack ("unknown CreateProjectResponse type: " <> typ))

instance ToJSON CreateProjectResponse where
  toJSON = \case
    CreateProjectResponseBadRequest -> toSumType "bad-request" (object [])
    CreateProjectResponseUnauthorized -> toSumType "unauthorized" (object [])
    CreateProjectResponseSuccess project -> toSumType "success" (toJSON project)

------------------------------------------------------------------------------------------------------------------------
-- Create project branch

-- | [@POST /create-project-branch@]: Create a project branch
type CreateProjectBranchAPI =
  "create-project-branch"
    :> ReqBody '[JSON] CreateProjectBranchRequest
    :> Verb Post 200 '[JSON] CreateProjectBranchResponse

-- | @POST /create-project-branch@ request.
data CreateProjectBranchRequest = CreateProjectBranchRequest
  { projectId :: Text,
    branchName :: Text,
    branchCausalHash :: Hash32,
    branchMergeTarget :: Maybe ProjectBranchIds
  }
  deriving stock (Eq, Show)

-- | @POST /create-project-branch@ response.
data CreateProjectBranchResponse
  = -- | Request payload invalid.
    CreateProjectBranchResponseBadRequest
  | CreateProjectBranchResponseUnauthorized
  | CreateProjectBranchResponseSuccess !ProjectBranch
  deriving stock (Eq, Show)

------------------------------------------------------------------------------------------------------------------------
-- Types

-- | A project.
data Project = Project
  { projectId :: Text,
    projectName :: Text
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON Project where
  parseJSON =
    withObject "Project" \o -> do
      projectId <- parseField o "projectId"
      projectName <- parseField o "projectName"
      pure Project {projectId, projectName}

instance ToJSON Project where
  toJSON Project {projectId, projectName} =
    object
      [ "projectId" .= projectId,
        "projectName" .= projectName
      ]

-- | A project branch.
data ProjectBranch = ProjectBranch
  { projectId :: Text,
    projectName :: Text,
    branchId :: Text,
    branchName :: Text
  }
  deriving stock (Eq, Generic, Show)

-- | A project id and branch id.
data ProjectBranchIds = ProjectBranchIds
  { projectId :: Text,
    branchId :: Text
  }
  deriving stock (Eq, Generic, Show)

------------------------------------------------------------------------------------------------------------------------
-- Aeson helpers. These could be extracted to a different module or package.

toSumType :: Text -> Value -> Value
toSumType typ payload =
  object ["type" .= typ, "payload" .= payload]

withSumType :: String -> (Text -> Value -> Parser a) -> Value -> Parser a
withSumType name k =
  withObject name \o -> do
    typ <- parseField o "type"
    val <- parseField o "payload"
    k typ val
