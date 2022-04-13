{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.NamespaceListing where

import Control.Error.Util ((??))
import Control.Monad.Except
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text as Text
import Servant
  ( QueryParam,
    (:>),
  )
import Servant.Docs
  ( DocQueryParam (..),
    ParamKind (Normal),
    ToParam (..),
    ToSample (..),
  )
import Servant.OpenApi ()
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch, ShallowBranch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.NameSegment as NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Server.Backend (Backend)
import qualified Unison.Server.Backend as Backend
import Unison.Server.Types
  ( APIGet,
    HashQualifiedName,
    NamedTerm (..),
    NamedType (..),
    NamespaceFQN,
    Size,
    UnisonHash,
    UnisonName,
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
          [Subnamespace $ NamedNamespace "base" "#19d1o9hi5n642t8jttg"]
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
    namespaceHash :: UnisonHash
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
  Backend.ShallowBranchEntry name hash ->
    Subnamespace $
      NamedNamespace
        { namespaceName = NameSegment.toText name,
          namespaceHash = "#" <> SBH.toText hash
        }
  Backend.ShallowPatchEntry name ->
    PatchObject . NamedPatch $ NameSegment.toText name

serve ::
  Codebase IO Symbol Ann ->
  Maybe ShortBranchHash ->
  Maybe NamespaceFQN ->
  Maybe NamespaceFQN ->
  Backend.Backend IO NamespaceListing
serve codebase mayRoot mayRelativeTo mayNamespaceName =
  let -- Various helpers
      errFromEither f = either (throwError . f) pure

      parsePath :: String -> Backend IO Path.Path'
      parsePath p = errFromEither (`Backend.BadNamespace` p) $ Path.parsePath' p

      findShallow ::
        ( ShallowBranch ->
          Backend IO [Backend.ShallowListEntry Symbol Ann]
        )
      findShallow branch = Backend.findInShallowBranch codebase branch

      makeNamespaceListing ::
        ( PPE.PrettyPrintEnv ->
          UnisonName ->
          UnisonHash ->
          [Backend.ShallowListEntry Symbol a] ->
          ExceptT Backend.BackendError IO NamespaceListing
        )
      makeNamespaceListing ppe fqn hash entries =
        pure . NamespaceListing fqn hash $
          fmap
            (backendListEntryToNamespaceObject ppe Nothing)
            entries

      -- Lookup paths, root and listing and construct response
      namespaceListing :: Backend IO NamespaceListing
      namespaceListing = do
        shallowRoot <- case mayRoot of
          Nothing -> do
            gotRoot <- liftIO $ Codebase.getShallowRootBranch codebase
            errFromEither Backend.BadRootBranch gotRoot
          Just sbh -> do
            ea <- liftIO . runExceptT $ do
              h <- Backend.expandShortBranchHash codebase sbh
              mayBranch <- lift $ Codebase.getShallowBranchForHash codebase h
              mayBranch ?? Backend.CouldntLoadBranch h
            liftEither ea

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
   in namespaceListing
