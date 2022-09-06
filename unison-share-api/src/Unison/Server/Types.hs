{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unison.Server.Types where

-- Types common to endpoints --

import Data.Aeson
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Map as Map
import Data.OpenApi
  ( ToParamSchema (..),
    ToSchema (..),
  )
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Servant.API
  ( FromHttpApiData (..),
    Get,
    Header,
    Headers,
    JSON,
    addHeader,
  )
import qualified U.Codebase.Branch as V2Branch
import qualified U.Codebase.Causal as V2Causal
import qualified U.Codebase.HashTags as V2
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Editor.DisplayObject
  ( DisplayObject,
  )
import qualified Unison.Hash as Hash
import Unison.Prelude
import Unison.Server.Doc (Doc)
import Unison.Server.Orphans ()
import Unison.Server.Syntax (SyntaxText)
import Unison.Util.Pretty (Width (..))

type APIHeaders x =
  Headers
    '[ Header "Cache-Control" String
     ]
    x

type APIGet c = Get '[JSON] (APIHeaders c)

type HashQualifiedName = Text

type NamespaceFQN = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

deriving via Bool instance FromHttpApiData Suffixify

deriving anyclass instance ToParamSchema Suffixify

instance ToJSON TypeDefinition where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TypeDefinition

instance ToJSON TermDefinition where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TermDefinition

instance ToJSON DefinitionDisplayResults where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema DefinitionDisplayResults

newtype Suffixify = Suffixify {suffixified :: Bool}
  deriving (Eq, Ord, Show, Generic)

data TermDefinition = TermDefinition
  { termNames :: [HashQualifiedName],
    bestTermName :: HashQualifiedName,
    defnTermTag :: TermTag,
    termDefinition :: DisplayObject SyntaxText SyntaxText,
    signature :: SyntaxText,
    termDocs :: [(HashQualifiedName, UnisonHash, Doc)]
  }
  deriving (Eq, Show, Generic)

data TypeDefinition = TypeDefinition
  { typeNames :: [HashQualifiedName],
    bestTypeName :: HashQualifiedName,
    defnTypeTag :: TypeTag,
    typeDefinition :: DisplayObject SyntaxText SyntaxText,
    typeDocs :: [(HashQualifiedName, UnisonHash, Doc)]
  }
  deriving (Eq, Show, Generic)

data DefinitionDisplayResults = DefinitionDisplayResults
  { termDefinitions :: Map UnisonHash TermDefinition,
    typeDefinitions :: Map UnisonHash TypeDefinition,
    missingDefinitions :: [HashQualifiedName]
  }
  deriving (Eq, Show, Generic)

instance Semigroup DefinitionDisplayResults where
  DefinitionDisplayResults terms1 types1 missing1 <> DefinitionDisplayResults terms2 types2 missing2 =
    DefinitionDisplayResults (terms1 `Map.union` terms2) (types1 `Map.union` types2) (missing1 ++ missing2)

instance Monoid DefinitionDisplayResults where
  mempty = DefinitionDisplayResults mempty mempty mempty

data TermTag = Doc | Test | Plain
  deriving (Eq, Ord, Show, Generic)

data TypeTag = Ability | Data
  deriving (Eq, Ord, Show, Generic)

data UnisonRef
  = TypeRef UnisonHash
  | TermRef UnisonHash
  deriving (Eq, Ord, Show, Generic)

data FoundEntry
  = FoundTerm NamedTerm
  | FoundType NamedType
  deriving (Eq, Show, Generic)

instance ToJSON FoundEntry where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema FoundEntry

unisonRefToText :: UnisonRef -> Text
unisonRefToText = \case
  TypeRef r -> r
  TermRef r -> r

data NamedTerm = NamedTerm
  { termName :: HashQualifiedName,
    termHash :: UnisonHash,
    termType :: Maybe SyntaxText,
    termTag :: TermTag
  }
  deriving (Eq, Generic, Show)

instance ToJSON NamedTerm where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamedTerm

data NamedType = NamedType
  { typeName :: HashQualifiedName,
    typeHash :: UnisonHash,
    typeTag :: TypeTag
  }
  deriving (Eq, Generic, Show)

instance ToJSON NamedType where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamedType

instance ToJSON TermTag where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TermTag

instance ToJSON TypeTag where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TypeTag

-- Helpers

munge :: Text -> LZ.ByteString
munge = Text.encodeUtf8 . Text.fromStrict

mungeShow :: Show s => s -> LZ.ByteString
mungeShow = mungeString . show

mungeString :: String -> LZ.ByteString
mungeString = Text.encodeUtf8 . Text.pack

defaultWidth :: Width
defaultWidth = 80

discard :: Applicative m => a -> m ()
discard = const $ pure ()

mayDefaultWidth :: Maybe Width -> Width
mayDefaultWidth = fromMaybe defaultWidth

setCacheControl :: v -> APIHeaders v
setCacheControl = addHeader @"Cache-Control" "public"

branchToUnisonHash :: Branch.Branch m -> UnisonHash
branchToUnisonHash b =
  ("#" <>) . Hash.base32Hex . Causal.unCausalHash $ Branch.headHash b

v2CausalBranchToUnisonHash :: V2Branch.CausalBranch m -> UnisonHash
v2CausalBranchToUnisonHash b =
  ("#" <>) . Hash.base32Hex . V2.unCausalHash $ V2Causal.causalHash b
