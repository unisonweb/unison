{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Orphans where

import U.Codebase.HashTags
import Data.Aeson
import Data.Binary
import Data.ByteString.Short (ShortByteString)
import Data.OpenApi
import Data.Proxy
import Servant
import U.Util.Hash (Hash (..))
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.ShortBranchHash
  ( ShortBranchHash (..),
  )
import qualified Unison.Codebase.ShortBranchHash as SBH
import Unison.ConstructorType (ConstructorType)
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.Util.Pretty (Width (..))

instance ToJSON ShortHash where
  toEncoding = genericToEncoding defaultOptions

instance ToJSONKey ShortHash

deriving instance ToSchema ShortHash

instance FromHttpApiData ShortBranchHash where
  parseUrlPiece = maybe (Left "Invalid ShortBranchHash") Right . SBH.fromText

deriving via ShortByteString instance Binary Hash

deriving via Hash instance Binary CausalHash

deriving via Text instance ToHttpApiData ShortBranchHash

instance (ToJSON b, ToJSON a) => ToJSON (DisplayObject b a) where
  toEncoding = genericToEncoding defaultOptions

deriving instance (ToSchema b, ToSchema a) => ToSchema (DisplayObject b a)

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

deriving anyclass instance ToParamSchema ShortBranchHash

deriving via Int instance FromHttpApiData Width

deriving via Int instance ToHttpApiData Width

deriving anyclass instance ToParamSchema Width

instance ToJSON n => ToJSON (HQ.HashQualified n) where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema n => ToSchema (HQ.HashQualified n)

instance ToJSON ConstructorType where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema ConstructorType
