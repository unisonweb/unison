{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Server.Endpoints.Projects where

import Control.Error.Util ((??))
import Control.Monad.Except
import Data.Aeson
import Data.Char
import Data.OpenApi
  ( ToParamSchema (..),
    ToSchema (..),
  )
import qualified Data.Text as Text
import Servant (QueryParam, (:>))
import Servant.API (FromHttpApiData (..))
import Servant.Docs
  ( DocQueryParam (..),
    ParamKind (Normal),
    ToParam (..),
    ToSample (..),
  )
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.NameSegment as NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Server.Backend
import qualified Unison.Server.Backend as Backend
import Unison.Server.Types (APIGet, UnisonHash)
import Unison.Symbol (Symbol)
import Unison.Util.Monoid (foldMapM)

type ProjectsAPI =
  "projects" :> QueryParam "rootBranch" ShortBranchHash
    :> QueryParam "owner" ProjectOwner
    :> APIGet [ProjectListing]

instance ToSample ProjectListing where
  toSamples _ =
    [ ( "Projects in the root branch",
        ProjectListing
          (ProjectOwner "unison")
          "base"
          "#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"
      )
    ]

newtype ProjectOwner = ProjectOwner Text
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

instance ToParam (QueryParam "owner" ProjectOwner) where
  toParam _ =
    DocQueryParam
      "owner"
      ["unison", "alice", "bob"]
      "The name of a project owner"
      Normal

instance ToJSON ProjectOwner where
  toEncoding = genericToEncoding defaultOptions

deriving anyclass instance ToParamSchema ProjectOwner

instance FromHttpApiData ProjectOwner where
  parseUrlPiece = Right . ProjectOwner

-- ProjectOwner is slightly more restrictive than a regular FQN in that we only
-- want alphanumeric characters
projectOwnerFromText :: Text -> Either Text ProjectOwner
projectOwnerFromText raw =
  if isAllAlphaNum raw
    then Right (ProjectOwner raw)
    else Left "Invalid owner name"
  where
    isAllAlphaNum t =
      t & Text.unpack & all isAlphaNum

data ProjectListing = ProjectListing
  { owner :: ProjectOwner,
    name :: Text,
    hash :: UnisonHash
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

instance ToJSON ProjectListing where
  toEncoding = genericToEncoding defaultOptions

backendListEntryToProjectListing ::
  ProjectOwner ->
  Backend.ShallowListEntry Symbol a ->
  Maybe ProjectListing
backendListEntryToProjectListing owner = \case
  Backend.ShallowBranchEntry name hash ->
    Just $
      ProjectListing
        { owner = owner,
          name = NameSegment.toText name,
          hash = "#" <> SBH.toText hash
        }
  _ -> Nothing

entryToOwner ::
  Backend.ShallowListEntry Symbol a ->
  Maybe ProjectOwner
entryToOwner = \case
  Backend.ShallowBranchEntry name _ ->
    Just $ ProjectOwner $ NameSegment.toText name
  _ -> Nothing

serve ::
  forall m.
  MonadIO m =>
  Codebase m Symbol Ann ->
  Maybe ShortBranchHash ->
  Maybe ProjectOwner ->
  Backend m [ProjectListing]
serve codebase mayRoot mayOwner = projects
  where
    projects :: Backend m [ProjectListing]
    projects = do
      root <- case mayRoot of
        Nothing -> do
          gotRoot <- lift $ Codebase.getRootBranch codebase
          errFromEither BadRootBranch gotRoot
        Just sbh -> do
          ea <- lift . runExceptT $ do
            h <- Backend.expandShortBranchHash codebase sbh
            mayBranch <- lift $ Codebase.getBranchForHash codebase h
            mayBranch ?? Backend.CouldntLoadBranch h
          liftEither ea

      ownerEntries <- findShallow root
      -- If an owner is provided, we only want projects belonging to them
      let owners =
            case mayOwner of
              Just o -> [o]
              Nothing -> mapMaybe entryToOwner ownerEntries
      foldMapM (ownerToProjectListings root) owners

    ownerToProjectListings :: Branch.Branch m -> ProjectOwner -> Backend m [ProjectListing]
    ownerToProjectListings root owner = do
      let (ProjectOwner ownerName) = owner
      ownerPath' <- (parsePath . Text.unpack) ownerName
      let path = Path.fromPath' ownerPath'
      let ownerBranch = Branch.getAt' path root
      entries <- findShallow ownerBranch
      pure $ mapMaybe (backendListEntryToProjectListing owner) entries

    -- Minor helpers

    findShallow :: Branch.Branch m -> Backend m [Backend.ShallowListEntry Symbol Ann]
    findShallow branch =
      Backend.findShallowInBranch codebase branch

    parsePath :: String -> Backend m Path.Path'
    parsePath p =
      errFromEither (`Backend.BadNamespace` p) $ Path.parsePath' p

    errFromEither :: (e -> BackendError) -> Either e a -> Backend m a
    errFromEither f =
      either (throwError . f) pure
