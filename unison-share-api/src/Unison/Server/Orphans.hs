{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Orphans where

import Control.Lens
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.OpenApi
import Data.Proxy
import Data.Text qualified as Text
import Servant
import Servant.Docs (DocCapture (DocCapture), DocQueryParam (..), ParamKind (..), ToCapture (..), ToParam (..))
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Prelude
import Unison.Project
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Unison.Syntax.HashQualified qualified as HQ (parseText)
import Unison.Syntax.HashQualifiedPrime qualified as HQ' (parseText)
import Unison.Syntax.Name qualified as Name (parseTextEither, toText)
import Unison.Util.Pretty (Width (..))

instance ToJSON ShortHash where
  toJSON = Aeson.String . SH.toText

instance FromJSON ShortHash where
  parseJSON = Aeson.withText "ShortHash" \txt ->
    case SH.fromText txt of
      Nothing -> fail $ "Invalid Shorthash" <> Text.unpack txt
      Just sh -> pure sh

instance ToSchema ShortHash where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

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

instance (ToJSON b, ToJSON a) => ToJSON (DisplayObject b a) where
  toJSON = \case
    BuiltinObject b -> object ["tag" Aeson..= String "BuiltinObject", "contents" Aeson..= b]
    MissingObject sh -> object ["tag" Aeson..= String "MissingObject", "contents" Aeson..= sh]
    UserObject a -> object ["tag" Aeson..= String "UserObject", "contents" Aeson..= a]

deriving instance (ToSchema b, ToSchema a) => ToSchema (DisplayObject b a)

-- [21/10/07] Hello, this is Mitchell. Name refactor in progress. Changing internal representation from a flat text to a
-- list of segments (in reverse order) plus an "is absolute?" bit.
--
-- To preserve backwards compatibility (for now, anyway -- is this even important long term?), the ToJSON and ToSchema
-- instances below treat Name as before.

instance ToSchema Name where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

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

instance ToParam (QueryParam "name" Name) where
  toParam _ =
    DocQueryParam
      "name"
      []
      "A definition name. See API documentation to determine how it should be qualified."
      Normal

instance FromHttpApiData Name where
  parseQueryParam = Name.parseTextEither

deriving via Int instance FromHttpApiData Width

deriving anyclass instance ToParamSchema Width

instance FromHttpApiData Path.Path where
  parseUrlPiece txt = case Path.parsePath' (Text.unpack txt) of
    Left s -> Left s
    Right (Path.RelativePath' p) -> Right (Path.unrelative p)
    Right (Path.AbsolutePath' _) -> Left $ "Expected relative path, but " <> txt <> " was absolute."

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

instance ToCapture (Capture "namespace" Path.Path) where
  toCapture _ =
    DocCapture
      "namespace"
      "E.g. base.List"

instance ToJSON Path.Path where
  toJSON p = Aeson.String (tShow p)

instance ToJSON Path.Absolute where
  toJSON p = Aeson.String (tShow p)

instance ToSchema Path.Path where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToSchema Path.Absolute where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToJSON (HQ.HashQualified Name) where
  toJSON = Aeson.String . HQ.toTextWith Name.toText

instance FromJSON (HQ'.HashQualified Name) where
  parseJSON = Aeson.withText "HashQualified'" \txt ->
    maybe (fail "Invalid HashQualified' Name") pure $ HQ'.parseText txt

instance FromJSON (HQ.HashQualified Name) where
  parseJSON = Aeson.withText "HashQualified" \txt ->
    maybe (fail "Invalid HashQualified Name") pure $ HQ.parseText txt

instance FromHttpApiData (HQ.HashQualified Name) where
  parseQueryParam txt =
    Text.replace "@" "#" txt
      & HQ.parseText
      & maybe (Left "Invalid Hash Qualified Name. Expected one of the following forms: name@hash, name, @hash") Right

instance ToParamSchema (HQ.HashQualified n) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "name@hash"

deriving anyclass instance (ToSchema n) => ToSchema (HQ.HashQualified n)

deriving anyclass instance (ToSchema n) => ToSchema (HQ'.HashQualified n)

instance FromHttpApiData ProjectName where
  parseQueryParam = mapLeft tShow . tryInto @ProjectName

instance ToParamSchema ProjectName where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "[@handle/]name"

instance ToCapture (Capture "project-name" ProjectName) where
  toCapture _ =
    DocCapture
      "project-name"
      "The name of a project. E.g. @handle/slug"

instance ToSchema ProjectName

deriving via Text instance ToJSON ProjectName

instance FromHttpApiData ProjectBranchName where
  parseQueryParam = mapLeft tShow . tryInto @ProjectBranchName

instance ToSchema ProjectBranchName

instance ToParamSchema ProjectBranchName where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "[@handle/]name"

instance ToCapture (Capture "branch-name" ProjectBranchName) where
  toCapture _ =
    DocCapture
      "branch-name"
      "The name of a branch in a project. E.g. @handle/name"

deriving via Text instance ToJSON ProjectBranchName
