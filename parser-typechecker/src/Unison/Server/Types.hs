{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Types where

-- Types common to endpoints --

import           Unison.Prelude
import           Data.Aeson                     ( ToJSON
                                                , ToJSONKey
                                                )
import qualified Data.ByteString.Lazy          as LZ
import qualified Data.Text.Lazy                as Text
import qualified Data.Text.Lazy.Encoding       as Text
import           Data.OpenApi                   ( ToSchema(..)
                                                , ToParamSchema(..)
                                                )
import           Servant.API                    ( FromHttpApiData )
import qualified Unison.HashQualified          as HQ
import           Unison.ConstructorType         ( ConstructorType )
import           Unison.Name                    ( Name )
import           Unison.ShortHash               ( ShortHash )
import           Unison.Codebase.ShortBranchHash
                                                ( ShortBranchHash(..) )
import           Unison.Util.Pretty             ( Width
                                                , render
                                                )
import           Unison.Var                     ( Var )
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Type                    ( Type )
import qualified Unison.TypePrinter            as TypePrinter
import           Unison.Codebase.Editor.DisplayObject
                                                ( DisplayObject )
import           Unison.Server.Syntax           ( SyntaxText )
import qualified Unison.Server.Syntax          as Syntax

type HashQualifiedName = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

instance ToJSON Name
deriving instance ToSchema Name

deriving via Text instance FromHttpApiData ShortBranchHash
deriving instance ToParamSchema ShortBranchHash

instance ToJSON a => ToJSON (DisplayObject a)
deriving instance ToSchema a => ToSchema (DisplayObject a)

instance ToJSON ShortHash
instance ToJSONKey ShortHash
deriving instance ToSchema ShortHash

instance ToJSON n => ToJSON (HQ.HashQualified n)
deriving instance ToSchema n => ToSchema (HQ.HashQualified n)

instance ToJSON ConstructorType

deriving instance ToSchema ConstructorType

instance ToJSON TypeDefinition

deriving instance ToSchema TypeDefinition

instance ToJSON TermDefinition

deriving instance ToSchema TermDefinition

instance ToJSON DefinitionDisplayResults

deriving instance ToSchema DefinitionDisplayResults

data TermDefinition = TermDefinition
  { termNames :: [HashQualifiedName]
  , bestTermName :: HashQualifiedName
  , termDefinition :: DisplayObject SyntaxText
  , signature :: SyntaxText
  } deriving (Eq, Show, Generic)

data TypeDefinition = TypeDefinition
  { typeNames :: [HashQualifiedName]
  , bestTypeName :: HashQualifiedName
  , typeDefinition :: DisplayObject SyntaxText
  } deriving (Eq, Show, Generic)

data DefinitionDisplayResults =
  DefinitionDisplayResults
    { termDefinitions :: Map UnisonHash TermDefinition
    , typeDefinitions :: Map UnisonHash TypeDefinition
    , missingDefinitions :: [HashQualifiedName]
    } deriving (Eq, Show, Generic)

formatType :: Var v => PPE.PrettyPrintEnv -> Width -> Type v a -> SyntaxText
formatType ppe w =
  fmap Syntax.convertElement . render w . TypePrinter.pretty0 ppe mempty (-1)

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

