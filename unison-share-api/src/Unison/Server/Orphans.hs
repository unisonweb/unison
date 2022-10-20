{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Orphans where

import Control.Lens
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Binary
import Data.ByteString.Short (ShortByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.OpenApi
import Data.Proxy
import qualified Data.Text as Text
import Servant
import Servant.Docs (DocCapture (DocCapture), DocQueryParam (..), ParamKind (..), ToCapture (..), ToParam (..))
import U.Codebase.HashTags
import U.Util.Hash (Hash (..))
import qualified U.Util.Hash as Hash
import Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash (..))
import qualified Unison.Codebase.ShortBranchHash as SBH
import Unison.ConstructorType (ConstructorType)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import qualified Unison.Syntax.HashQualified as HQ (fromText)
import qualified Unison.Syntax.HashQualified' as HQ' (fromText)
import qualified Unison.Syntax.Name as Name (fromTextEither, toText)
import Unison.Util.Pretty (Width (..))

instance ToJSON Hash where
  toJSON h = String $ Hash.toBase32HexText h

instance FromJSON Hash where
  parseJSON = Aeson.withText "Hash" $ pure . Hash.unsafeFromBase32HexText

deriving via Hash instance ToJSON CausalHash

deriving via Hash instance FromJSON CausalHash

instance ToJSON ShortHash where
  toJSON = Aeson.String . SH.toText

instance ToJSONKey ShortHash where
  toJSONKey = contramap SH.toText (toJSONKey @Text)

instance FromJSON ShortHash where
  parseJSON = Aeson.withText "ShortHash" \txt ->
    case SH.fromText txt of
      Nothing -> fail $ "Invalid Shorthash" <> Text.unpack txt
      Just sh -> pure sh

instance FromJSONKey ShortHash where
  fromJSONKey =
    Aeson.FromJSONKeyTextParser \txt ->
      case SH.fromText txt of
        Nothing -> fail $ "Invalid Shorthash" <> Text.unpack txt
        Just sh -> pure sh

instance FromHttpApiData ShortBranchHash where
  parseUrlPiece = maybe (Left "Invalid ShortBranchHash") Right . SBH.fromText

-- | Always renders to the form: #abcdef
instance ToHttpApiData ShortHash where
  toQueryParam = SH.toText

-- | Accepts shorthashes of any of the following forms:
-- @abcdef
-- @@builtin
-- #abcdef
-- ##builtin
-- abcdef
instance FromHttpApiData ShortHash where
  parseUrlPiece txt =
    Text.replace "@" "#" txt
      & \t ->
        ( if Text.isPrefixOf "#" t
            then t
            else ("#" <> t)
        )
          & SH.fromText
          & maybe (Left "Invalid ShortBranchHash") Right

instance ToSchema ShortHash where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

-- | Always renders to the form: #abcdef
instance ToHttpApiData Reference.Reference where
  toQueryParam = Reference.toText

-- | Always renders to the form: #abcdef
instance ToHttpApiData Referent.Referent where
  toQueryParam = Referent.toText

-- | Accepts shorthashes of any of the following forms:
-- @abcdef
-- @@builtin
-- #abcdef
-- ##builtin
-- abcdef
instance FromHttpApiData Reference.Reference where
  parseUrlPiece txt =
    Text.replace "@" "#" txt
      & \t ->
        ( if Text.isPrefixOf "#" t
            then t
            else ("#" <> t)
        )
          & Reference.fromText
          & mapLeft Text.pack

-- | Accepts shorthashes of any of the following forms:
-- @abcdef
-- @@builtin
-- #abcdef
-- ##builtin
-- abcdef
instance FromHttpApiData Referent.Referent where
  parseUrlPiece txt =
    Text.replace "@" "#" txt
      & \t ->
        ( if Text.isPrefixOf "#" t
            then t
            else ("#" <> t)
        )
          & Referent.fromText
          & maybe (Left "Invalid Referent") Right

instance ToSchema Reference where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

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

instance ToParamSchema ShortHash where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "@abcdef"

instance ToParamSchema Reference.Reference where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "@abcdef"

instance ToParamSchema Referent.Referent where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "@abcdef"

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

instance ToParam (QueryParam "name" Name) where
  toParam _ =
    DocQueryParam
      "name"
      []
      "A definition name. See API documentation to determine how it should be qualified."
      Normal

instance FromHttpApiData Name where
  parseQueryParam = Name.fromTextEither

deriving via Int instance FromHttpApiData Width

deriving via Int instance ToHttpApiData Width

deriving anyclass instance ToParamSchema Width

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

instance ToCapture (Capture "hash" ShortHash) where
  toCapture _ =
    DocCapture
      "hash"
      "A shorthash for a term or type. E.g. @abcdef, #abcdef, @@builtin, ##builtin, abcdef"

instance ToCapture (Capture "hash" Reference.Reference) where
  toCapture _ =
    DocCapture
      "hash"
      "A hash reference for a type. E.g. @abcdef, #abcdef, @@builtin, ##builtin, abcdef"

instance ToCapture (Capture "hash" Referent.Referent) where
  toCapture _ =
    DocCapture
      "hash"
      "A hash reference for a term. E.g. @abcdef, #abcdef, @@builtin, ##builtin, abcdef"

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

instance Show n => ToJSON (HQ.HashQualified n) where
  toJSON = Aeson.String . HQ.toText

instance Show n => ToJSON (HQ'.HashQualified n) where
  toJSON = Aeson.String . HQ'.toText

instance FromJSON (HQ'.HashQualified Name) where
  parseJSON = Aeson.withText "HashQualified'" \txt ->
    maybe (fail "Invalid HashQualified' Name") pure $ HQ'.fromText txt

instance FromJSON (HQ.HashQualified Name) where
  parseJSON = Aeson.withText "HashQualified" \txt ->
    maybe (fail "Invalid HashQualified Name") pure $ HQ.fromText txt

instance FromJSON (HQ'.HashQualified NameSegment) where
  parseJSON = Aeson.withText "HashQualified'" \txt -> do
    hqName <- maybe (fail "Invalid HashQualified' NameSegment") pure $ HQ'.fromText txt
    for hqName \name -> case Name.segments name of
      (ns :| []) -> pure ns
      _ -> fail $ "Expected a single name segment but received several: " <> Text.unpack txt

instance FromJSON (HQ.HashQualified NameSegment) where
  parseJSON = Aeson.withText "HashQualified" \txt -> do
    hqName <- maybe (fail "Invalid HashQualified' NameSegment") pure $ HQ.fromText txt
    for hqName \name -> case Name.segments name of
      (ns :| []) -> pure ns
      _ -> fail $ "Expected a single name segment but received several: " <> Text.unpack txt

instance FromHttpApiData (HQ.HashQualified Name) where
  parseQueryParam txt =
    Text.replace "@" "#" txt
      & HQ.fromText
      & maybe (Left "Invalid Hash Qualified Name. Expected one of the following forms: name@hash, name, @hash") Right

instance FromHttpApiData (HQ'.HashQualified Name) where
  parseQueryParam txt =
    Text.replace "@" "#" txt
      & HQ'.fromText
      & maybe (Left "Invalid Hash Qualified Name. Expected one of the following forms: name@hash, name") Right

instance ToParamSchema (HQ.HashQualified n) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "name@hash"

instance ToParamSchema (HQ'.HashQualified n) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "name@hash"

instance ToHttpApiData Name where
  toQueryParam = Name.toText

deriving newtype instance ToSchema NameSegment

deriving anyclass instance ToSchema n => ToSchema (HQ.HashQualified n)

deriving anyclass instance ToSchema n => ToSchema (HQ'.HashQualified n)
