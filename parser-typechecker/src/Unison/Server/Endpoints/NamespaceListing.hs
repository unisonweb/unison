{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.NamespaceListing where

import Control.Error (runExceptT)
import Control.Error.Util ((??))
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text as Text
import Servant
  ( QueryParam,
    throwError,
    (:>),
  )
import Servant.Docs
  ( DocQueryParam (..),
    ParamKind (Normal),
    ToParam (..),
    ToSample (..),
  )
import Servant.OpenApi ()
import Servant.Server (Handler)
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
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Server.Backend as Backend
import Unison.Server.Errors
  ( backendError,
    badNamespace,
    rootBranchError,
  )
import Unison.Server.Types
  ( APIGet,
    APIHeaders,
    HashQualifiedName,
    NamedTerm (..),
    NamedType (..),
    NamespaceFQN,
    Size,
    UnisonHash,
    UnisonName,
    addHeaders,
    branchToUnisonHash,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)
import Unison.Var (Var)

type NamespaceListingAPI =
  "list" :> QueryParam "rootBranch" ShortBranchHash
    :> QueryParam "relativeTo" NamespaceFQN
    :> QueryParam "namespace" NamespaceFQN
    :> APIGet NamespaceListing

instance ToParam (QueryParam "namespace" Text) where
  toParam _ =
    DocQueryParam
      "namespace"
      [".", ".base.List", "foo.bar"]
      "The fully qualified name of a namespace. The leading `.` is optional."
      Normal

instance ToSample NamespaceListing where
  toSamples _ =
    [ ( "When no value is provided for `namespace`, the root namespace `.` is "
          <> "listed by default",
        NamespaceListing
          "."
          "#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"
          [Subnamespace $ NamedNamespace "base" "#19d1o9hi5n642t8jttg" 1244]
      )
    ]

data NamespaceListing = NamespaceListing
  { namespaceListingFQN :: UnisonName,
    namespaceListingHash :: UnisonHash,
    namespaceListingChildren :: [NamespaceObject]
  }
  deriving (Generic, Show)

instance ToJSON NamespaceListing where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamespaceListing

data NamespaceObject
  = Subnamespace NamedNamespace
  | TermObject NamedTerm
  | TypeObject NamedType
  | PatchObject NamedPatch
  deriving (Generic, Show)

instance ToJSON NamespaceObject where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamespaceObject

data NamedNamespace = NamedNamespace
  { namespaceName :: UnisonName,
    namespaceHash :: UnisonHash,
    namespaceSize :: Size
  }
  deriving (Generic, Show)

instance ToJSON NamedNamespace where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamedNamespace

newtype NamedPatch = NamedPatch {patchName :: HashQualifiedName}
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

instance ToJSON NamedPatch where
  toEncoding = genericToEncoding defaultOptions

newtype KindExpression = KindExpression {kindExpressionText :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

instance ToJSON KindExpression where
  toEncoding = genericToEncoding defaultOptions

backendListEntryToNamespaceObject ::
  Var v =>
  PPE.PrettyPrintEnv ->
  Maybe Width ->
  Backend.ShallowListEntry v a ->
  NamespaceObject
backendListEntryToNamespaceObject ppe typeWidth = \case
  Backend.ShallowTermEntry te ->
    TermObject $ Backend.termEntryToNamedTerm ppe typeWidth te
  Backend.ShallowTypeEntry te -> TypeObject $ Backend.typeEntryToNamedType te
  Backend.ShallowBranchEntry name hash size ->
    Subnamespace $
      NamedNamespace
        { namespaceName = NameSegment.toText name,
          namespaceHash = "#" <> SBH.toText hash,
          namespaceSize = size
        }
  Backend.ShallowPatchEntry name ->
    PatchObject . NamedPatch $ NameSegment.toText name

serve ::
  Handler () ->
  Codebase IO Symbol Ann ->
  Maybe ShortBranchHash ->
  Maybe NamespaceFQN ->
  Maybe NamespaceFQN ->
  Handler (APIHeaders NamespaceListing)
serve tryAuth codebase mayRoot mayRelativeTo mayNamespaceName =
  let -- Various helpers
      errFromEither f = either (throwError . f) pure

      parsePath p = errFromEither (`badNamespace` p) $ Path.parsePath' p

      doBackend a = do
        ea <- liftIO $ runExceptT a
        errFromEither backendError ea

      findShallow branch = doBackend $ Backend.findShallowInBranch codebase branch

      makeNamespaceListing ppe fqn hash entries =
        pure . NamespaceListing fqn hash $
          fmap
            (backendListEntryToNamespaceObject ppe Nothing)
            entries

      -- Lookup paths, root and listing and construct response
      namespaceListing = do
        root <- case mayRoot of
          Nothing -> do
            gotRoot <- liftIO $ Codebase.getRootBranch codebase
            errFromEither rootBranchError gotRoot
          Just sbh -> do
            ea <- liftIO . runExceptT $ do
              h <- Backend.expandShortBranchHash codebase sbh
              mayBranch <- lift $ Codebase.getBranchForHash codebase h
              mayBranch ?? Backend.CouldntLoadBranch h
            errFromEither backendError ea

        -- Relative and Listing Path resolution
        --
        -- The full listing path is a combination of the relativeToPath (prefix) and the namespace path
        --
        -- For example:
        --            "base.List"    <>    "Nonempty"
        --                ↑                    ↑
        --         relativeToPath        namespacePath
        --
        -- resulting in "base.List.map" which we can use via the root branch (usually the codebase hash)
        -- to look up the namespace listing and present shallow name, so that the
        -- definition "base.List.Nonempty.map", simple has the name "map"
        --
        relativeToPath' <- (parsePath . Text.unpack) $ fromMaybe "." mayRelativeTo
        namespacePath' <- (parsePath . Text.unpack) $ fromMaybe "." mayNamespaceName

        let path = Path.fromPath' relativeToPath' <> Path.fromPath' namespacePath'
        let path' = Path.toPath' path

        -- Actually construct the NamespaceListing

        let listingBranch = Branch.getAt' path root
        hashLength <- liftIO $ Codebase.hashLength codebase

        let shallowPPE = Backend.basicSuffixifiedNames hashLength root $ (Backend.Within $ Path.fromPath' path')
        let listingFQN = Path.toText . Path.unabsolute . either id (Path.Absolute . Path.unrelative) $ Path.unPath' path'
        let listingHash = branchToUnisonHash listingBranch
        listingEntries <- findShallow listingBranch

        makeNamespaceListing shallowPPE listingFQN listingHash listingEntries
   in addHeaders <$> (tryAuth *> namespaceListing)
