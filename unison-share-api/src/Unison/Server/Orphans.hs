{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Orphans where

import Control.Lens
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Binary
import Data.ByteString.Short (ShortByteString)
import Data.OpenApi
import Data.Proxy
import qualified Data.Text as Text
import Servant
import Servant.Docs (DocCapture (DocCapture), ToCapture (..))
import U.Codebase.HashTags
import U.Util.Hash (Hash (..))
import qualified U.Util.Hash as Hash
import Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
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

instance ToJSON Hash where
  toJSON h = String $ Hash.toBase32HexText h

instance FromJSON Hash where
  parseJSON = Aeson.withText "Hash" $ pure . Hash.unsafeFromBase32HexText

deriving via Hash instance ToJSON CausalHash

deriving via Hash instance FromJSON CausalHash

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

instance ToParamSchema Name where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "base.List.map"

instance ToParamSchema Path.Path where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "base.List"

instance ToParamSchema Path.Relative where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "base.List"

deriving via Int instance FromHttpApiData Width

deriving via Int instance ToHttpApiData Width

deriving anyclass instance ToParamSchema Width

instance ToJSON n => ToJSON (HQ.HashQualified n) where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema n => ToSchema (HQ.HashQualified n)

instance ToJSON ConstructorType where
  toEncoding = genericToEncoding defaultOptions

instance FromHttpApiData Path.Relative where
  parseUrlPiece txt = case Path.parsePath' (Text.unpack txt) of
    Left s -> Left (Text.pack s)
    Right (Path.RelativePath' p) -> Right p
    Right (Path.AbsolutePath' _) -> Left $ "Expected relative path, but " <> txt <> " was absolute."

instance ToHttpApiData Path.Relative where
  toUrlPiece = tShow

instance FromHttpApiData Path.Absolute where
  parseUrlPiece txt = case Path.parsePath' (Text.unpack txt) of
    Left s -> Left (Text.pack s)
    Right (Path.RelativePath' _) -> Left $ "Expected absolute path, but " <> txt <> " was relative."
    Right (Path.AbsolutePath' p) -> Right p

instance ToHttpApiData Path.Absolute where
  toUrlPiece = tShow

instance FromHttpApiData Path.Path' where
  parseUrlPiece txt = mapLeft Text.pack $ Path.parsePath' (Text.unpack txt)

instance ToHttpApiData Path.Path' where
  toUrlPiece = tShow

instance FromHttpApiData Path.Path where
  parseUrlPiece txt = case Path.parsePath' (Text.unpack txt) of
    Left s -> Left (Text.pack s)
    Right (Path.RelativePath' p) -> Right (Path.unrelative p)
    Right (Path.AbsolutePath' _) -> Left $ "Expected relative path, but " <> txt <> " was absolute."

instance ToCapture (Capture "fqn" Name) where
  toCapture _ =
    DocCapture
      "fqn"
      "The fully qualified name of a definition."

instance ToCapture (Capture "namespace" Path.Path) where
  toCapture _ =
    DocCapture
      "namespace"
      "E.g. base.List"

instance ToJSON Path.Path where
  toJSON p = Aeson.String (tShow p)

instance ToSchema Path.Path where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)
