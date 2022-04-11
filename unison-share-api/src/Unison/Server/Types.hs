{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Types where

-- Types common to endpoints --

import Data.Aeson
import qualified Data.ByteString.Lazy as LZ
import Data.OpenApi
  ( ToParamSchema (..),
    ToSchema (..),
  )
import Data.Proxy (Proxy (..))
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
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Editor.DisplayObject
  ( DisplayObject,
  )
import Unison.Codebase.ShortBranchHash
  ( ShortBranchHash (..),
  )
import qualified Unison.Codebase.ShortBranchHash as SBH
import Unison.ConstructorType (ConstructorType)
import qualified Unison.Hash as Hash
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Prelude
import Unison.Server.Doc (Doc)
import qualified Unison.Server.Doc as Doc
import Unison.Server.Syntax (SyntaxText)
import Unison.ShortHash (ShortHash)
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

-- [21/10/07] Hello, this is Mitchell. Name refactor in progress. Changing internal representation from a flat text to a
-- list of segments (in reverse order) plus an "is absolute?" bit.
--
-- To preserve backwards compatibility (for now, anyway -- is this even important long term?), the ToJSON and ToSchema
-- instances below treat Name as before.

instance ToJSON Name where
  toEncoding = toEncoding . Name.toText
  toJSON = toJSON . Name.toText

instance ToSchema Name where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

deriving via Bool instance FromHttpApiData Suffixify

deriving anyclass instance ToParamSchema Suffixify

instance FromHttpApiData ShortBranchHash where
  parseUrlPiece = maybe (Left "Invalid ShortBranchHash") Right . SBH.fromText

deriving anyclass instance ToParamSchema ShortBranchHash

deriving via Int instance FromHttpApiData Width

deriving anyclass instance ToParamSchema Width

instance (ToJSON b, ToJSON a) => ToJSON (DisplayObject b a) where
  toEncoding = genericToEncoding defaultOptions

deriving instance (ToSchema b, ToSchema a) => ToSchema (DisplayObject b a)

instance ToJSON ShortHash where
  toEncoding = genericToEncoding defaultOptions

instance ToJSONKey ShortHash

deriving instance ToSchema ShortHash

instance ToJSON n => ToJSON (HQ.HashQualified n) where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema n => ToSchema (HQ.HashQualified n)

instance ToJSON ConstructorType where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema ConstructorType

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
    defnTermTag :: Maybe TermTag,
    termDefinition :: DisplayObject SyntaxText SyntaxText,
    signature :: SyntaxText,
    termDocs :: [(HashQualifiedName, UnisonHash, Doc)]
  }
  deriving (Eq, Show, Generic)

data TypeDefinition = TypeDefinition
  { typeNames :: [HashQualifiedName],
    bestTypeName :: HashQualifiedName,
    defnTypeTag :: Maybe TypeTag,
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

data TermTag = Doc | Test
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
    termTag :: Maybe TermTag
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

instance ToJSON Doc

instance ToJSON Doc.MediaSource

instance ToJSON Doc.SpecialForm

instance ToJSON Doc.Src

instance ToJSON a => ToJSON (Doc.Ref a)

instance ToSchema Doc

instance ToSchema Doc.MediaSource

instance ToSchema Doc.SpecialForm

instance ToSchema Doc.Src

instance ToSchema a => ToSchema (Doc.Ref a)

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
  ("#" <>) . Hash.base32Hex . Causal.unRawHash $ Branch.headHash b
