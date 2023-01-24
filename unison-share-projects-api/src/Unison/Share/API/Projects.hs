{-# LANGUAGE DataKinds #-}

module Unison.Share.API.Projects
  ( -- * API
    ProjectsAPI,

    -- ** Get project
    GetProjectAPI,
    GetProjectResponse (..),

    -- ** Create project
    CreateProjectAPI,
    CreateProjectResponse (..),

    -- * Types
    Project (..),
    ServerProjectId (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API

type ProjectsAPI =
  GetProjectAPI
    :<|> CreateProjectAPI

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
-- Types

-- | A project.
data Project = Project
  { id :: ServerProjectId,
    name :: Text
  }
  deriving stock (Eq, Generic, Show)

-- | A project id that was generated on the server.
newtype ServerProjectId = ServerProjectId
  { unServerProjectId :: Text
  }
  deriving newtype (Eq, Show)
