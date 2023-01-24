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

import Data.Text (Text)
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

------------------------------------------------------------------------------------------------------------------------
-- Create project

-- | [@POST /create-project@]: Create a project
type CreateProjectAPI =
  "create-project"
    :> ReqBody '[JSON] CreateProjectRequest
    :> Verb Post 200 '[JSON] CreateProjectResponse

-- | @POST /create-project@ request.
data CreateProjectRequest = CreateProjectRequest
  { name :: Text
  }
  deriving stock (Eq, Show)

-- | @POST /create-project@ response.
data CreateProjectResponse
  = -- | Request payload invalid.
    CreateProjectResponseBadRequest
  | CreateProjectResponseUnauthorized
  | CreateProjectResponseSuccess !Project
  deriving stock (Eq, Show)

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
  { id :: Text,
    name :: Text
  }
  deriving stock (Eq, Generic, Show)

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
