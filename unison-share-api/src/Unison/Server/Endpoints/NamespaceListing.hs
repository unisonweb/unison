{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.NamespaceListing (serve, NamespaceListingAPI, NamespaceListing (..), NamespaceObject (..), NamedNamespace (..), NamedPatch (..), KindExpression (..)) where

import Control.Monad.Except
import Control.Monad.Reader
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
import qualified U.Codebase.Causal as V2Causal
import qualified U.Util.Hash as Hash
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Codebase.SqliteCodebase.Conversions (causalHash2to1)
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
    UnisonHash,
    UnisonName,
    branchToUnisonHash,
    v2CausalBranchToUnisonHash,
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
          [Subnamespace $ NamedNamespace "base" "#19d1o9hi5n642t8jttg" (Just 237)]
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
    -- May not be provided on all server implementations.
    namespaceSize :: Maybe Int
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
  Backend.ShallowBranchEntry name hash _size ->
    Subnamespace $
      NamedNamespace
        { namespaceName = NameSegment.toText name,
          namespaceHash = "#" <> Hash.toBase32HexText (Causal.unCausalHash hash),
          namespaceSize = Nothing
        }
  Backend.ShallowPatchEntry name ->
    PatchObject . NamedPatch $ NameSegment.toText name

serve ::
  Codebase IO Symbol Ann ->
  Maybe ShortBranchHash ->
  Maybe NamespaceFQN ->
  Maybe NamespaceFQN ->
  Backend.Backend IO NamespaceListing
serve codebase maySBH mayRelativeTo mayNamespaceName = do
  useIndex <- asks Backend.useNamesIndex
  mayRootHash <- traverse (Backend.expandShortBranchHash codebase) maySBH
  codebaseRootHash <- liftIO $ Codebase.getRootBranchHash codebase

  --Relative and Listing Path resolution
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

  case (useIndex, mayRootHash) of
    (True, Nothing) ->
      serveFromIndex codebase mayRootHash path'
    (True, Just rh)
      | rh == causalHash2to1 codebaseRootHash ->
        serveFromIndex codebase mayRootHash path'
    (_, Just rh) -> do
      mayBranch <- liftIO $ Codebase.getBranchForHash codebase rh
      branch <- maybe (throwError $ Backend.NoBranchForHash rh) pure mayBranch
      serveFromBranch codebase path' branch
    (False, Nothing) -> do
      branch <- liftIO $ Codebase.getRootBranch codebase
      serveFromBranch codebase path' branch
  where
    parsePath :: String -> Backend IO Path.Path'
    parsePath p = errFromEither (`Backend.BadNamespace` p) $ Path.parsePath' p
    errFromEither f = either (throwError . f) pure

serveFromBranch ::
  Codebase IO Symbol Ann ->
  Path.Path' ->
  Branch IO ->
  Backend.Backend IO NamespaceListing
serveFromBranch codebase path' branch = do
  -- TODO: Currently the ppe is just used for rendering types which don't appear in the UI,
  -- If we ever show types on hover we need to build and use a proper PPE here, but it's not
  -- worth slowing down the request for this right now.
  let ppe = mempty
  let listingFQN = Path.toText . Path.unabsolute . either id (Path.Absolute . Path.unrelative) $ Path.unPath' path'
  let listingHash = branchToUnisonHash branch
  listingEntries <- liftIO $ Backend.lsBranch codebase branch
  makeNamespaceListing ppe listingFQN listingHash listingEntries

serveFromIndex ::
  Codebase IO Symbol Ann ->
  Maybe Branch.CausalHash ->
  Path.Path' ->
  Backend.Backend IO NamespaceListing
serveFromIndex codebase mayRootHash path' = do
  listingCausal <- Backend.getShallowCausalAtPathFromRootHash codebase mayRootHash (Path.fromPath' path')
  listingBranch <- liftIO $ V2Causal.value listingCausal

  -- TODO: Currently the ppe is just used for rendering types which don't appear in the UI,
  -- If we ever show types on hover we need to build and use a proper PPE here, but it's not
  -- worth slowing down the request for this right now.
  -- shallowPPE <- liftIO $ Backend.shallowPPE codebase listingBranch
  let shallowPPE = mempty
  let listingFQN = Path.toText . Path.unabsolute . either id (Path.Absolute . Path.unrelative) $ Path.unPath' path'
  let listingHash = v2CausalBranchToUnisonHash listingCausal
  listingEntries <- lift (Backend.lsShallowBranch codebase listingBranch)
  makeNamespaceListing shallowPPE listingFQN listingHash listingEntries

makeNamespaceListing ::
  ( PPE.PrettyPrintEnv ->
    UnisonName ->
    UnisonHash ->
    [Backend.ShallowListEntry Symbol a] ->
    Backend IO NamespaceListing
  )
makeNamespaceListing ppe fqn hash entries =
  pure . NamespaceListing fqn hash $
    fmap
      (backendListEntryToNamespaceObject ppe Nothing)
      entries
