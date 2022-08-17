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
import qualified U.Codebase.Branch as V2Causal
import qualified U.Codebase.Causal as V2Causal
import qualified U.Util.Hash as Hash
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Codebase.SqliteCodebase.Conversions (causalHash2to1)
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
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
  Backend.ShallowBranchEntry name hash size ->
    Subnamespace $
      NamedNamespace
        { namespaceName = NameSegment.toText name,
          namespaceHash = "#" <> Hash.toBase32HexText (Causal.unCausalHash hash),
          namespaceSize = size
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

  case (useIndex, mayRootHash) of
    (True, Nothing) ->
      serveFromIndex codebase mayRootHash path'
    (True, Just rh)
      | rh == causalHash2to1 codebaseRootHash ->
          serveFromIndex codebase mayRootHash path'
      | otherwise -> do
          serveFromBranch codebase path' (Cv.causalHash1to2 rh)
    (False, Just rh) -> do
      serveFromBranch codebase path' (Cv.causalHash1to2 rh)
    (False, Nothing) -> do
      rh <- liftIO $ Codebase.getRootBranchHash codebase
      serveFromBranch codebase path' rh
  where
    parsePath :: String -> Backend IO Path.Path'
    parsePath p = errFromEither (`Backend.BadNamespace` p) $ Path.parsePath' p
    errFromEither f = either (throwError . f) pure

serveFromBranch ::
  Codebase IO Symbol Ann ->
  Path.Path' ->
  V2Causal.CausalHash ->
  Backend.Backend IO NamespaceListing
serveFromBranch codebase path' rootHash = do
  let absPath = Path.Absolute . Path.fromPath' $ path'
  -- TODO: Currently the ppe is just used to render the types returned from the namespace
  -- listing, which are currently unused because we don't show types in the side-bar.
  -- If we ever show types on hover we need to build and use a proper PPE here, but it's not
  -- worth slowing down the request for this right now.
  let ppe = PPE.empty
  let listingFQN = Path.toText . Path.unabsolute . either id (Path.Absolute . Path.unrelative) $ Path.unPath' path'
  mayCausalAtPath <- liftIO $ Codebase.getShallowCausalFromRoot codebase (Just rootHash) absPath
  mayBranchAtPath <- liftIO $ traverse V2Causal.value mayCausalAtPath
  let branchAtPath = fromMaybe V2Causal.Empty mayBranchAtPath
  let listingHash = maybe (branchToUnisonHash Branch.empty) v2CausalBranchToUnisonHash mayCausalAtPath
  listingEntries <- liftIO $ Backend.lsBranch codebase branchAtPath
  makeNamespaceListing ppe listingFQN listingHash listingEntries

serveFromIndex ::
  Codebase IO Symbol Ann ->
  Maybe Branch.CausalHash ->
  Path.Path' ->
  Backend.Backend IO NamespaceListing
serveFromIndex codebase mayRootHash path' = do
  listingCausal <- Backend.getShallowCausalAtPathFromRootHash codebase mayRootHash (Path.fromPath' path')
  listingBranch <- liftIO $ V2Causal.value listingCausal
  -- TODO: Currently the ppe is just used to render the types returned from the namespace
  -- listing, which are currently unused because we don't show types in the side-bar.
  -- If we ever show types on hover we need to build and use a proper PPE here, but it's not
  -- shallowPPE <- liftIO $ Backend.shallowPPE codebase listingBranch
  let shallowPPE = PPE.empty
  let listingFQN = Path.toText . Path.unabsolute . either id (Path.Absolute . Path.unrelative) $ Path.unPath' path'
  let listingHash = v2CausalBranchToUnisonHash listingCausal
  listingEntries <- lift (Backend.lsBranch codebase listingBranch)
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
