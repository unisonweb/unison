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
    NotFound (..),
    Unauthorized (..),
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
import Unison.Share.API.Hash (HashJWT)

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
  = GetProjectResponseNotFound NotFound
  | GetProjectResponseUnauthorized Unauthorized
  | GetProjectResponseSuccess !Project
  deriving stock (Eq, Show, Generic)

instance FromJSON GetProjectResponse where
  parseJSON =
    withSumType "GetProjectResponse" \typ val ->
      case typ of
        "not-found" -> GetProjectResponseNotFound <$> parseJSON val
        "unauthorized" -> GetProjectResponseUnauthorized <$> parseJSON val
        "success" -> GetProjectResponseSuccess <$> parseJSON val
        _ -> fail (Text.unpack ("unknown GetProjectResponse type: " <> typ))

instance ToJSON GetProjectResponse where
  toJSON = \case
    GetProjectResponseNotFound notFound -> toSumType "not-found" (toJSON notFound)
    GetProjectResponseUnauthorized unauthorized -> toSumType "unauthorized" (toJSON unauthorized)
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
  deriving stock (Eq, Show, Generic)

instance FromJSON CreateProjectRequest where
  parseJSON =
    withObject "CreateProjectRequest" \o -> do
      projectName <- parseField o "project-name"
      pure CreateProjectRequest {..}

instance ToJSON CreateProjectRequest where
  toJSON (CreateProjectRequest projectName) =
    object
      [ "project-name" .= projectName
      ]

-- | @POST /create-project@ response.
data CreateProjectResponse
  = CreateProjectResponseUnauthorized Unauthorized
  | CreateProjectResponseNotFound !NotFound
  | CreateProjectResponseSuccess !Project
  deriving stock (Eq, Show, Generic)

instance FromJSON CreateProjectResponse where
  parseJSON =
    withSumType "CreateProjectResponse" \typ val ->
      case typ of
        "not-found" -> CreateProjectResponseNotFound <$> parseJSON val
        "unauthorized" -> CreateProjectResponseUnauthorized <$> parseJSON val
        "success" -> CreateProjectResponseSuccess <$> parseJSON val
        _ -> fail (Text.unpack ("unknown CreateProjectResponse type: " <> typ))

instance ToJSON CreateProjectResponse where
  toJSON = \case
    CreateProjectResponseNotFound notFound -> toSumType "not-found" (toJSON notFound)
    CreateProjectResponseUnauthorized unauthorized -> toSumType "unauthorized" (toJSON unauthorized)
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
  = GetProjectBranchResponseProjectNotFound NotFound
  | GetProjectBranchResponseBranchNotFound NotFound
  | GetProjectBranchResponseUnauthorized Unauthorized
  | GetProjectBranchResponseSuccess !ProjectBranch
  deriving stock (Eq, Show, Generic)

instance FromJSON GetProjectBranchResponse where
  parseJSON =
    withSumType "GetProjectBranchResponse" \typ val ->
      case typ of
        "project-not-found" -> GetProjectBranchResponseProjectNotFound <$> parseJSON val
        "branch-not-found" -> GetProjectBranchResponseBranchNotFound <$> parseJSON val
        "unauthorized" -> GetProjectBranchResponseUnauthorized <$> parseJSON val
        "success" -> GetProjectBranchResponseSuccess <$> parseJSON val
        _ -> fail (Text.unpack ("unknown GetProjectBranchResponse type: " <> typ))

instance ToJSON GetProjectBranchResponse where
  toJSON = \case
    GetProjectBranchResponseProjectNotFound notFound -> toSumType "project-not-found" (toJSON notFound)
    GetProjectBranchResponseBranchNotFound notFound -> toSumType "branch-not-found" (toJSON notFound)
    GetProjectBranchResponseUnauthorized unauthorized -> toSumType "unauthorized" (toJSON unauthorized)
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
  deriving stock (Eq, Show, Generic)

instance FromJSON CreateProjectBranchRequest where
  parseJSON =
    withObject "CreateProjectBranchRequest" \o -> do
      projectId <- parseField o "project-id"
      branchName <- parseField o "branch-name"
      branchCausalHash <- parseField o "branch-head"
      branchMergeTarget <- parseFieldMaybe' o "branch-merge-target"
      pure CreateProjectBranchRequest {..}

instance ToJSON CreateProjectBranchRequest where
  toJSON (CreateProjectBranchRequest projectId branchName branchCausalHash branchMergeTarget) =
    objectWithMaybes
      [ "project-id" .= projectId,
        "branch-name" .= branchName,
        "branch-head" .= branchCausalHash
      ]
      [ "branch-merge-target" .=? branchMergeTarget
      ]

-- | @POST /create-project-branch@ response.
data CreateProjectBranchResponse
  = CreateProjectBranchResponseUnauthorized Unauthorized
  | CreateProjectBranchResponseNotFound NotFound
  | CreateProjectBranchResponseMissingCausalHash !Hash32
  | CreateProjectBranchResponseSuccess !ProjectBranch
  deriving stock (Eq, Show, Generic)

instance FromJSON CreateProjectBranchResponse where
  parseJSON =
    withSumType "CreateProjectBranchResponse" \typ val ->
      case typ of
        "unauthorized" -> CreateProjectBranchResponseUnauthorized <$> parseJSON val
        "missing-causal-hash" ->
          val & withObject "CreateProjectBranchResponseMissingCausalHash" \obj ->
            CreateProjectBranchResponseMissingCausalHash <$> obj .: "causalHash"
        "not-found" -> CreateProjectBranchResponseNotFound <$> parseJSON val
        "success" -> CreateProjectBranchResponseSuccess <$> parseJSON val
        _ -> fail (Text.unpack ("unknown CreateProjectBranchResponse type: " <> typ))

instance ToJSON CreateProjectBranchResponse where
  toJSON = \case
    CreateProjectBranchResponseUnauthorized unauthorized -> toSumType "unauthorized" (toJSON unauthorized)
    CreateProjectBranchResponseNotFound notFound -> toSumType "not-found" (toJSON notFound)
    CreateProjectBranchResponseMissingCausalHash hash -> toSumType "missing-causal-hash" (object ["causalHash" .= hash])
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
  deriving stock (Eq, Show, Generic)

instance FromJSON SetProjectBranchHeadRequest where
  parseJSON =
    withObject "SetProjectBranchHeadRequest" \o -> do
      projectId <- parseField o "project-id"
      branchId <- parseField o "branch-id"
      branchOldCausalHash <- parseFieldMaybe' o "branch-old-head"
      branchNewCausalHash <- parseField o "branch-new-head"
      pure SetProjectBranchHeadRequest {..}

instance ToJSON SetProjectBranchHeadRequest where
  toJSON (SetProjectBranchHeadRequest projectId branchId branchOldCausalHash branchNewCausalHash) =
    objectWithMaybes
      [ "project-id" .= projectId,
        "branch-id" .= branchId,
        "branch-new-head" .= branchNewCausalHash
      ]
      ["branch-old-head" .=? branchOldCausalHash]

-- | @POST /set-project-branch-hash@ response.
data SetProjectBranchHeadResponse
  = SetProjectBranchHeadResponseUnauthorized Unauthorized
  | SetProjectBranchHeadResponseNotFound NotFound
  | SetProjectBranchHeadResponseSuccess
  | SetProjectBranchHeadResponseMissingCausalHash !Hash32
  | -- | (expected, actual)
    SetProjectBranchHeadResponseExpectedCausalHashMismatch !Hash32 !Hash32
  deriving stock (Eq, Show, Generic)

instance FromJSON SetProjectBranchHeadResponse where
  parseJSON =
    withSumType "SetProjectBranchHeadResponse" \typ val ->
      case typ of
        "unauthorized" -> SetProjectBranchHeadResponseUnauthorized <$> parseJSON val
        "not-found" -> SetProjectBranchHeadResponseNotFound <$> parseJSON val
        "missing-causal-hash" ->
          val & withObject "SetProjectBranchHeadResponseMissingCausalHash" \obj -> do
            SetProjectBranchHeadResponseMissingCausalHash <$> (obj .: "causalHash")
        "expected-causal-hash-mismatch" ->
          val & withObject "SetProjectBranchHeadResponseExpectedCausalHashMismatch" \obj -> do
            expected <- obj .: "expected"
            actual <- obj .: "actual"
            pure (SetProjectBranchHeadResponseExpectedCausalHashMismatch expected actual)
        "success" -> pure SetProjectBranchHeadResponseSuccess
        _ -> fail (Text.unpack ("unknown SetProjectBranchHeadResponse type: " <> typ))

instance ToJSON SetProjectBranchHeadResponse where
  toJSON = \case
    SetProjectBranchHeadResponseUnauthorized unauthorized -> toSumType "unauthorized" (toJSON unauthorized)
    SetProjectBranchHeadResponseMissingCausalHash ch -> toSumType "missing-causal-hash" (object ["causalHash" .= ch])
    SetProjectBranchHeadResponseExpectedCausalHashMismatch expected actual ->
      toSumType "expected-causal-hash-mismatch" (object ["expected" .= expected, "actual" .= actual])
    SetProjectBranchHeadResponseNotFound notFound -> toSumType "not-found" (toJSON notFound)
    SetProjectBranchHeadResponseSuccess -> toSumType "success" (object [])

------------------------------------------------------------------------------------------------------------------------
-- Types

-- | A project.
data Project = Project
  { projectId :: Text,
    projectName :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Project where
  parseJSON =
    withObject "Project" \o -> do
      projectId <- parseField o "project-id"
      projectName <- parseField o "project-name"
      pure Project {projectId, projectName}

instance ToJSON Project where
  toJSON (Project projectId projectName) =
    object
      [ "project-id" .= projectId,
        "project-name" .= projectName
      ]

-- | A project branch.
data ProjectBranch = ProjectBranch
  { projectId :: Text,
    projectName :: Text,
    branchId :: Text,
    branchName :: Text,
    branchHead :: HashJWT
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ProjectBranch where
  parseJSON =
    withObject "ProjectBranch" \o -> do
      projectId <- parseField o "project-id"
      projectName <- parseField o "project-name"
      branchId <- parseField o "branch-id"
      branchName <- parseField o "branch-name"
      branchHead <- parseField o "branch-head"
      pure ProjectBranch {..}

instance ToJSON ProjectBranch where
  toJSON (ProjectBranch projectId projectName branchId branchName branchHead) =
    object
      [ "project-id" .= projectId,
        "project-name" .= projectName,
        "branch-id" .= branchId,
        "branch-name" .= branchName,
        "branch-head" .= branchHead
      ]

-- | A project id and branch id.
data ProjectBranchIds = ProjectBranchIds
  { projectId :: Text,
    branchId :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ProjectBranchIds where
  parseJSON =
    withObject "ProjectBranchIds" \o -> do
      projectId <- parseField o "project-id"
      branchId <- parseField o "branch-id"
      pure ProjectBranchIds {..}

instance ToJSON ProjectBranchIds where
  toJSON (ProjectBranchIds projectId branchId) =
    object
      [ "project-id" .= projectId,
        "branch-id" .= branchId
      ]

data NotFound = NotFound {message :: Text}
  deriving stock (Eq, Show, Generic)

instance ToJSON NotFound where
  toJSON (NotFound message) = object ["message" .= message]

instance FromJSON NotFound where
  parseJSON =
    withObject "NotFound" \o -> do
      message <- parseField o "message"
      pure NotFound {..}

data Unauthorized = Unauthorized {message :: Text}
  deriving stock (Eq, Show, Generic)

instance ToJSON Unauthorized where
  toJSON (Unauthorized message) = object ["message" .= message]

instance FromJSON Unauthorized where
  parseJSON =
    withObject "Unauthorized" \o -> do
      message <- parseField o "message"
      pure Unauthorized {..}

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
