{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Unison.Server.CodebaseServer where

import Data.Aeson
import GHC.Generics
--import GHC.TypeLits
--import Network.Wai.Handler.Warp
import Servant.API
import Data.Map
import Data.Text (Text)

type HashQualifiedName = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

type NamespaceAPI = "list" :> QueryParam "namespace" HashQualifiedName :> Get '[JSON] NamespaceListing

data NamespaceListing = NamespaceListing
  { namespaceListingName :: UnisonName
  , namespaceListingHash :: UnisonHash
  , namespaceListingChildren :: [NamespaceEntry]
  } deriving Generic

instance ToJSON NamespaceListing

data NamespaceEntry = NamespaceEntry
  { namespaceEntryObject :: NamespaceObject
  , namespaceEntryMetadata :: [NamedTerm]
  } deriving Generic

instance ToJSON NamespaceEntry

data NamespaceObject = Namespace NamedNamespace| Named | Type NamedType
  deriving Generic

instance ToJSON NamespaceObject

data NamedNamespace = NamedNamespace
  { namespaceName :: UnisonName
  , namespaceHash :: UnisonHash
  , namespaceSize :: Size
  } deriving Generic

instance ToJSON NamedNamespace

data NamedTerm = NamedTerm
  { termName :: HashQualifiedName
  , termHash :: UnisonHash
  , termType :: TypeExpression
  } deriving Generic

instance ToJSON NamedTerm

data NamedType = NamedType
  { typeName :: HashQualifiedName
  , typeHash :: UnisonHash
  , typeKind :: KindExpression
  } deriving Generic

instance ToJSON NamedType

data NamedPatch = NamedPatch
  { patchName :: HashQualifiedName
  , patchHash :: UnisonHash
  } deriving Generic

instance ToJSON NamedPatch

data TypeExpression = TypeExpression
  { expression :: Text
  , dependencies :: Map HashQualifiedName UnisonHash
  } deriving Generic

instance ToJSON TypeExpression

newtype KindExpression = KindExpression { kindExpressionText :: Text }
  deriving Generic

instance ToJSON KindExpression

