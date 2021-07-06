{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Servant.API
  ( FromHttpApiData,
    Get,
    Header,
    Headers,
    JSON,
    addHeader,
  )
import Unison.Codebase.Editor.DisplayObject
  ( DisplayObject,
  )
import Unison.Codebase.ShortBranchHash
  ( ShortBranchHash (..),
  )
import Unison.ConstructorType (ConstructorType)
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import Unison.Prelude
import Unison.Server.Doc (Doc)
import qualified Unison.Server.Doc as Doc
import Unison.Server.Syntax (SyntaxText)
import Unison.ShortHash (ShortHash)
import Unison.Util.Pretty ( Width (..) )

type APIHeaders x =
  Headers
    '[ Header "Access-Control-Allow-Origin" String,
       Header "Cache-Control" String
     ]
    x

type APIGet c = Get '[JSON] (APIHeaders c)

type HashQualifiedName = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

instance ToJSON Name where
    toEncoding = genericToEncoding defaultOptions
deriving instance ToSchema Name

deriving via Bool instance FromHttpApiData Suffixify
deriving instance ToParamSchema Suffixify

deriving via Text instance FromHttpApiData ShortBranchHash
deriving instance ToParamSchema ShortBranchHash

deriving via Int instance FromHttpApiData Width
deriving instance ToParamSchema Width

instance ToJSON a => ToJSON (DisplayObject a) where
   toEncoding = genericToEncoding defaultOptions
deriving instance ToSchema a => ToSchema (DisplayObject a)

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

newtype Suffixify = Suffixify { suffixified :: Bool }
  deriving (Eq, Ord, Show, Generic)

data TermDefinition = TermDefinition
  { termNames :: [HashQualifiedName]
  , bestTermName :: HashQualifiedName
  , defnTermTag :: Maybe TermTag
  , termDefinition :: DisplayObject SyntaxText
  , signature :: SyntaxText
  , termDocs :: [(HashQualifiedName, UnisonHash, Doc)]
  } deriving (Eq, Show, Generic)

data TypeDefinition = TypeDefinition
  { typeNames :: [HashQualifiedName]
  , bestTypeName :: HashQualifiedName
  , defnTypeTag :: Maybe TypeTag
  , typeDefinition :: DisplayObject SyntaxText
  , typeDocs :: [(HashQualifiedName, UnisonHash, Doc)]
  } deriving (Eq, Show, Generic)

data DefinitionDisplayResults =
  DefinitionDisplayResults
    { termDefinitions :: Map UnisonHash TermDefinition
    , typeDefinitions :: Map UnisonHash TypeDefinition
    , missingDefinitions :: [HashQualifiedName]
    } deriving (Eq, Show, Generic)

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
  { termName :: HashQualifiedName
  , termHash :: UnisonHash
  , termType :: Maybe SyntaxText
  , termTag :: Maybe TermTag
  }
  deriving (Eq, Generic, Show)

instance ToJSON NamedTerm where
   toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamedTerm

data NamedType = NamedType
  { typeName :: HashQualifiedName
  , typeHash :: UnisonHash
  , typeTag :: TypeTag
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

instance ToJSON Doc where
instance ToJSON Doc.SpecialForm where
instance ToJSON Doc.Src where
instance ToJSON a => ToJSON (Doc.Ref a) where
instance ToSchema Doc where
instance ToSchema Doc.SpecialForm where
instance ToSchema Doc.Src where
instance ToSchema a => ToSchema (Doc.Ref a) where

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

mayDefault :: Maybe Width -> Width
mayDefault = fromMaybe defaultWidth

addHeaders :: v -> APIHeaders v
addHeaders = addHeader "*" . addHeader "public"
