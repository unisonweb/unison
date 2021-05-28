{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.ListNamespace where

import Control.Lens (view)
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text as Text
import Servant
  ( Get,
    JSON,
    QueryParam,
    ServerError (errBody),
    err400,
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
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.Hash as Hash
import qualified Unison.HashQualified as HQ
import qualified Unison.Name as Name
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Server.AppState (AppM, codebase, doBackend, tryAuth)
import qualified Unison.Server.Backend as Backend
import Unison.Server.Errors
  ( badHQN,
    badNamespace,
    errFromEither,
    errFromMaybe,
    rootBranchError,
  )
import Unison.Server.Types
  ( HashQualifiedName,
    NamedTerm (..),
    NamedType (..),
    Size,
    UnisonHash,
    UnisonName,
  )
import qualified Unison.ShortHash as ShortHash
import Unison.Util.Pretty (Width)
import Unison.Var (Var)

type NamespaceAPI =
  "list" :> QueryParam "namespace" HashQualifiedName
    :> Get '[JSON] NamespaceListing

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
        <> "listed by default"
      , NamespaceListing
        (Just ".")
        "#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"
        [Subnamespace $ NamedNamespace "base" "#19d1o9hi5n642t8jttg" 1244]
      )
    ]

data NamespaceListing = NamespaceListing
  { namespaceListingName :: Maybe UnisonName,
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
  { namespaceName :: UnisonName
  , namespaceHash :: UnisonHash
  , namespaceSize :: Size
  }
  deriving (Generic, Show)

instance ToJSON NamedNamespace where
   toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamedNamespace

newtype NamedPatch = NamedPatch { patchName :: HashQualifiedName }
  deriving (Generic, Show)

instance ToJSON NamedPatch where
   toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamedPatch

newtype KindExpression = KindExpression {kindExpressionText :: Text}
  deriving (Generic, Show)

instance ToJSON KindExpression where
   toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema KindExpression

backendListEntryToNamespaceObject
  :: Var v
  => PPE.PrettyPrintEnv
  -> Maybe Width
  -> Backend.ShallowListEntry v a
  -> NamespaceObject
backendListEntryToNamespaceObject ppe typeWidth = \case
  Backend.ShallowTermEntry te ->
    TermObject $ Backend.termEntryToNamedTerm ppe typeWidth te
  Backend.ShallowTypeEntry te -> TypeObject $ Backend.typeEntryToNamedType te
  Backend.ShallowBranchEntry name hash size -> Subnamespace $ NamedNamespace
    { namespaceName = NameSegment.toText name
    , namespaceHash = "#" <> SBH.toText hash
    , namespaceSize = size
    }
  Backend.ShallowPatchEntry name ->
    PatchObject . NamedPatch $ NameSegment.toText name

serveNamespace :: Var v => Maybe HashQualifiedName -> AppM v NamespaceListing
serveNamespace mayHQN = tryAuth *> case mayHQN of
  Nothing  -> serveNamespace $ Just "."
  Just hqn -> do
    parsedName <- parseHQN hqn
    cb         <- view codebase
    hashLength <- liftIO $ Codebase.hashLength cb
    case parsedName of
      HQ.NameOnly n -> do
        path' <- parsePath $ Name.toString n
        er <- liftIO $ Codebase.getRootBranch cb
        root <- either (throwError . rootBranchError) pure er
        ppe <- doBackend . Backend.suffixifiedNames hashLength root $ Path.fromPath' path'
        let p = either id (Path.Absolute . Path.unrelative) $ Path.unPath' path'
        entries <- findShallow p root
        processEntries
          ppe
          (Just $ Name.toText n)
          (("#" <>) . Hash.base32Hex . Causal.unRawHash $ Branch.headHash root)
          entries
      HQ.HashOnly sh -> case SBH.fromText $ ShortHash.toText sh of
        Nothing ->
          throwError
            . badNamespace "Malformed branch hash."
            $ ShortHash.toString sh
        Just h -> doBackend $ do
          hash    <- Backend.expandShortBranchHash h
          branch  <- Backend.resolveBranchHash hash
          entries <- Backend.findShallowInBranch branch
          ppe <- Backend.suffixifiedNames hashLength branch mempty
          let sbh = Text.pack . show $ SBH.fullFromHash hash
          processEntries ppe Nothing sbh entries
      HQ.HashQualified _ _ -> hashQualifiedNotSupported
 where
  parseHQN hqn = errFromMaybe (badHQN hqn) $ HQ.fromText hqn
  parsePath p = errFromEither (`badNamespace` p) $ Path.parsePath' p
  findShallow p r = doBackend $ Backend.findShallow p r
  processEntries ppe name hash entries =
    pure . NamespaceListing name hash $ fmap
      (backendListEntryToNamespaceObject ppe Nothing)
      entries
  hashQualifiedNotSupported = throwError $ err400
    { errBody = "This server does not yet support searching namespaces by "
                  <> "hash-qualified name."
    }

