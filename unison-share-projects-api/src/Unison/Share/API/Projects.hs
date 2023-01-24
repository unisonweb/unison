{-# LANGUAGE DataKinds #-}

module Unison.Share.API.Projects
  ( ProjectsAPI,

    -- * Types
    GetProjectResponse (..),
    Project (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API

-- |
--
-- [@GET /project?id=XXX@]: Get a project by id.
--
-- [@GET /project?name=XXX@]: Get a project by name.
type ProjectsAPI =
  "project"
    :> QueryParam "id" Text
    :> QueryParam "name" Text
    :> Verb Get 200 '[JSON] GetProjectResponse

-- | @GET /project@ response.
data GetProjectResponse
  = -- | 404 not found.
    GetProjectResponseNotFound
  | GetProjectResponseSuccess !Project

data Project = Project
  { id :: Text,
    name :: Text
  }
  deriving stock (Eq, Generic, Show)
