{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

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

    -- ** Get project branch
    GetProjectBranchAPI,
    GetProjectBranchResponse (..),

    -- ** Create project branch
    CreateProjectBranchAPI,
    CreateProjectBranchRequest (..),
    CreateProjectBranchResponse (..),

    -- ** Set project branch head
    SetProjectBranchHeadAPI,
    SetProjectBranchHeadRequest (..),
    SetProjectBranchHeadResponse (..),

    -- * Types
    Project (..),
    ProjectBranch (..),
    ProjectBranchIds (..),
  )
where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid (Endo (..))
import qualified Data.Text as Text
import Servant.API
import Unison.Hash32 (Hash32)
import Unison.Hash32.Orphans.Aeson ()
import Unison.Prelude

type ProjectsAPI =
  GetProjectAPI
    :<|> CreateProjectAPI
    :<|> GetProjectBranchAPI
    :<|> CreateProjectBranchAPI
    :<|> SetProjectBranchHeadAPI

------------------------------------------------------------------------------------------------------------------------
-- Get project

-- | [@GET /project?id=XXX@]: Get a project by id.
--
-- [@GET /project?name=XXX@]: Get a project by name.
type GetProjectAPI =
  "project"
    :> QueryParam "id" Text
    :> QueryParam "name" Text
    :> Verb 'GET 200 '[JSON] GetProjectResponse

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
    :> Verb 'POST 200 '[JSON] CreateProjectResponse

-- | @POST /create-project@ request.
data CreateProjectRequest = CreateProjectRequest
  { projectName :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON CreateProjectRequest where
  parseJSON =
    withObject "CreateProjectRequest" \o -> do
      projectName <- parseField o "projectName"
      pure CreateProjectRequest {..}

instance ToJSON CreateProjectRequest where
  toJSON (CreateProjectRequest projectName) =
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
-- Get project branch

-- | [@GET /project-branch?projectId=XXX&branchId=YYY@]: Get a project branch by id.
--
-- [@GET /project-branch?projectId=XXX&branchName=YYY@]: Get a project branch by name.
type GetProjectBranchAPI =
  "project-branch"
    :> QueryParam' '[Required, Strict] "projectId" Text
    :> QueryParam "branchId" Text
    :> QueryParam "branchName" Text
    :> Verb 'GET 200 '[JSON] GetProjectBranchResponse

-- | @GET /project-branch@ response.
data GetProjectBranchResponse
  = GetProjectBranchResponseNotFound
  | GetProjectBranchResponseSuccess !ProjectBranch
  deriving stock (Eq, Show)

instance FromJSON GetProjectBranchResponse where
  parseJSON =
    withSumType "GetProjectBranchResponse" \typ val ->
      case typ of
        "not-found" -> pure GetProjectBranchResponseNotFound
        "success" -> GetProjectBranchResponseSuccess <$> parseJSON val
        _ -> fail (Text.unpack ("unknown GetProjectBranchResponse type: " <> typ))

instance ToJSON GetProjectBranchResponse where
  toJSON = \case
    GetProjectBranchResponseNotFound -> toSumType "not-found" (object [])
    GetProjectBranchResponseSuccess branch -> toSumType "success" (toJSON branch)

------------------------------------------------------------------------------------------------------------------------
-- Create project branch

-- | [@POST /create-project-branch@]: Create a project branch
type CreateProjectBranchAPI =
  "create-project-branch"
    :> ReqBody '[JSON] CreateProjectBranchRequest
    :> Verb 'POST 200 '[JSON] CreateProjectBranchResponse

-- | @POST /create-project-branch@ request.
data CreateProjectBranchRequest = CreateProjectBranchRequest
  { projectId :: Text,
    branchName :: Text,
    branchCausalHash :: Hash32,
    branchMergeTarget :: Maybe ProjectBranchIds
  }
  deriving stock (Eq, Show)

instance FromJSON CreateProjectBranchRequest where
  parseJSON =
    withObject "CreateProjectBranchRequest" \o -> do
      projectId <- parseField o "projectId"
      branchName <- parseField o "branchName"
      branchCausalHash <- parseField o "branchCausalHash"
      branchMergeTarget <- parseFieldMaybe' o "branchMergeTarget"
      pure CreateProjectBranchRequest {..}

instance ToJSON CreateProjectBranchRequest where
  toJSON (CreateProjectBranchRequest projectId branchName branchCausalHash branchMergeTarget) =
    objectWithMaybes
      [ "projectId" .= projectId,
        "branchName" .= branchName,
        "branchCausalHash" .= branchCausalHash
      ]
      [ "branchMergeTarget" .=? branchMergeTarget
      ]

-- | @POST /create-project-branch@ response.
data CreateProjectBranchResponse
  = -- | Request payload invalid.
    CreateProjectBranchResponseBadRequest
  | CreateProjectBranchResponseUnauthorized
  | CreateProjectBranchResponseSuccess !ProjectBranch
  deriving stock (Eq, Show)

instance FromJSON CreateProjectBranchResponse where
  parseJSON =
    withSumType "CreateProjectBranchResponse" \typ val ->
      case typ of
        "bad-request" -> pure CreateProjectBranchResponseBadRequest
        "unauthorized" -> pure CreateProjectBranchResponseUnauthorized
        "success" -> CreateProjectBranchResponseSuccess <$> parseJSON val
        _ -> fail (Text.unpack ("unknown CreateProjectBranchResponse type: " <> typ))

instance ToJSON CreateProjectBranchResponse where
  toJSON = \case
    CreateProjectBranchResponseBadRequest -> toSumType "bad-request" (object [])
    CreateProjectBranchResponseUnauthorized -> toSumType "unauthorized" (object [])
    CreateProjectBranchResponseSuccess branch -> toSumType "success" (toJSON branch)

------------------------------------------------------------------------------------------------------------------------
-- Set project branch head

-- | [@POST /set-project-branch-head@]: Make a project branch point at an already-uploaded causal
type SetProjectBranchHeadAPI =
  "set-project-branch-head"
    :> ReqBody '[JSON] SetProjectBranchHeadRequest
    :> Verb 'POST 200 '[JSON] SetProjectBranchHeadResponse

-- | @POST /set-project-branch-head@ request.
data SetProjectBranchHeadRequest = SetProjectBranchHeadRequest
  { projectId :: Text,
    branchId :: Text,
    -- | If @Nothing@, just set (force-push semantics). If @Just@, check-and-set (push-with-lease).
    branchOldCausalHash :: Maybe Hash32,
    branchNewCausalHash :: Hash32
  }
  deriving stock (Eq, Show)

instance FromJSON SetProjectBranchHeadRequest where
  parseJSON =
    withObject "SetProjectBranchHeadRequest" \o -> do
      projectId <- parseField o "projectId"
      branchId <- parseField o "branchId"
      branchOldCausalHash <- parseFieldMaybe' o "branchOldCausalHash"
      branchNewCausalHash <- parseField o "branchNewCausalHash"
      pure SetProjectBranchHeadRequest {..}

instance ToJSON SetProjectBranchHeadRequest where
  toJSON (SetProjectBranchHeadRequest projectId branchId branchOldCausalHash branchNewCausalHash) =
    objectWithMaybes
      [ "projectId" .= projectId,
        "branchId" .= branchId,
        "branchNewCausalHash" .= branchNewCausalHash
      ]
      ["branchOldCausalHash" .=? branchOldCausalHash]

-- | @POST /set-project-branch-hash@ response.
data SetProjectBranchHeadResponse
  = -- | Request payload invalid.
    SetProjectBranchHeadResponseBadRequest
  | SetProjectBranchHeadResponseUnauthorized
  | SetProjectBranchHeadResponseSuccess
  deriving stock (Eq, Show)

instance FromJSON SetProjectBranchHeadResponse where
  parseJSON =
    withSumType "SetProjectBranchHeadResponse" \typ _val ->
      case typ of
        "bad-request" -> pure SetProjectBranchHeadResponseBadRequest
        "unauthorized" -> pure SetProjectBranchHeadResponseUnauthorized
        "success" -> pure SetProjectBranchHeadResponseSuccess
        _ -> fail (Text.unpack ("unknown SetProjectBranchHeadResponse type: " <> typ))

instance ToJSON SetProjectBranchHeadResponse where
  toJSON = \case
    SetProjectBranchHeadResponseBadRequest -> toSumType "bad-request" (object [])
    SetProjectBranchHeadResponseUnauthorized -> toSumType "unauthorized" (object [])
    SetProjectBranchHeadResponseSuccess -> toSumType "success" (object [])

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
  toJSON (Project projectId projectName) =
    object
      [ "projectId" .= projectId,
        "projectName" .= projectName
      ]

-- | A project branch.
data ProjectBranch = ProjectBranch
  { projectId :: Text,
    projectName :: Text,
    branchId :: Text,
    branchName :: Text,
    branchHead :: Hash32
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON ProjectBranch where
  parseJSON =
    withObject "ProjectBranch" \o -> do
      projectId <- parseField o "projectId"
      projectName <- parseField o "projectName"
      branchId <- parseField o "branchId"
      branchName <- parseField o "branchName"
      branchHead <- parseField o "branchHead"
      pure ProjectBranch {..}

instance ToJSON ProjectBranch where
  toJSON (ProjectBranch projectId projectName branchId branchName branchHead) =
    object
      [ "projectId" .= projectId,
        "projectName" .= projectName,
        "branchId" .= branchId,
        "branchName" .= branchName,
        "branchHead" .= branchHead
      ]

-- | A project id and branch id.
data ProjectBranchIds = ProjectBranchIds
  { projectId :: Text,
    branchId :: Text
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON ProjectBranchIds where
  parseJSON =
    withObject "ProjectBranchIds" \o -> do
      projectId <- parseField o "projectId"
      branchId <- parseField o "branchId"
      pure ProjectBranchIds {..}

instance ToJSON ProjectBranchIds where
  toJSON (ProjectBranchIds projectId branchId) =
    object
      [ "projectId" .= projectId,
        "branchId" .= branchId
      ]

------------------------------------------------------------------------------------------------------------------------
-- Aeson helpers. These could be extracted to a different module or package.

-- | Like 'object', but takes a second list of pairs whose values are Maybes; Nothing values' keys are not put into the
-- object at all.
--
-- For example, the Haskell value
--
-- @
-- Foo
--   { bar = 5
--   , qux = Nothing
--   }
-- @
--
-- would be serialized as the JSON
--
-- @
-- { "bar" = 5 }
-- @
--
-- using this combinator.
objectWithMaybes :: [Pair] -> [Endo Object] -> Value
objectWithMaybes nonMaybeFields maybeFields =
  Object (appEndo (fold maybeFields) (HashMap.fromList nonMaybeFields))

-- | Like ('.='), but omits the key/value pair if the value is Nothing.
(.=?) :: ToJSON a => Text -> Maybe a -> Endo Object
k .=? mv =
  case mv of
    Nothing -> mempty
    Just v -> Endo (HashMap.insert k (toJSON v))

toSumType :: Text -> Value -> Value
toSumType typ payload =
  object ["type" .= typ, "payload" .= payload]

withSumType :: String -> (Text -> Value -> Parser a) -> Value -> Parser a
withSumType name k =
  withObject name \o -> do
    typ <- parseField o "type"
    val <- parseField o "payload"
    k typ val
