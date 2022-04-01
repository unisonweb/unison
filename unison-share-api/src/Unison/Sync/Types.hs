{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Sync.Types where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteArray.Encoding (Base (Base64), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import Data.Map.NonEmpty (NEMap)
import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- | A newtype for JSON encoding binary data.
newtype Base64Bytes = Base64Bytes ByteString

instance ToJSON Base64Bytes where
  toJSON (Base64Bytes bytes) = String . Text.decodeUtf8 $ convertToBase Base64 bytes

instance FromJSON Base64Bytes where
  parseJSON = Aeson.withText "Base64" $ \txt -> do
    either fail (pure . Base64Bytes) $ convertFromBase Base64 (Text.encodeUtf8 txt)

newtype RepoName = RepoName Text
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

newtype HashJWT = HashJWT Text
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data HashJWTClaims = HashJWTClaims
  { hash :: Hash,
    entityType :: EntityType
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON HashJWTClaims where
  toJSON (HashJWTClaims hash entityType) =
    object
      [ "h" .= hash,
        "t" .= entityType
      ]

instance FromJSON HashJWTClaims where
  parseJSON = Aeson.withObject "HashJWTClaims" $ \obj -> do
    hash <- obj .: "h"
    entityType <- obj .: "t"
    pure HashJWTClaims {..}

newtype Hash = Hash {toBase32Hex :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data TypedHash = TypedHash
  { hash :: Hash,
    entityType :: EntityType
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON TypedHash where
  toJSON (TypedHash hash entityType) =
    object
      [ "hash" .= hash,
        "type" .= entityType
      ]

instance FromJSON TypedHash where
  parseJSON = Aeson.withObject "TypedHash" $ \obj -> do
    hash <- obj .: "hash"
    entityType <- obj .: "type"
    pure $ TypedHash {..}

data RepoPath = RepoPath
  { repoName :: RepoName,
    pathSegments :: [Text]
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON RepoPath where
  toJSON (RepoPath name segments) =
    object
      [ "repo_name" .= name,
        "path" .= segments
      ]

instance FromJSON RepoPath where
  parseJSON = Aeson.withObject "RepoPath" $ \obj -> do
    repoName <- obj .: "repo_name"
    pathSegments <- obj .: "path"
    pure RepoPath {..}

newtype GetCausalHashByPathRequest = GetCausalHashByPathRequest
  { repoPath :: RepoPath
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON GetCausalHashByPathRequest where
  toJSON (GetCausalHashByPathRequest repoPath) =
    object
      [ "repo_path" .= repoPath
      ]

instance FromJSON GetCausalHashByPathRequest where
  parseJSON = Aeson.withObject "GetCausalHashByPathRequest" $ \obj -> do
    repoPath <- obj .: "repo_path"
    pure GetCausalHashByPathRequest {..}

newtype GetCausalHashByPathResponse = GetCausalHashByPathResponse
  { causalHash :: Maybe HashJWT
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON GetCausalHashByPathResponse where
  toJSON (GetCausalHashByPathResponse hashJWT) =
    object
      [ "causal_hash" .= hashJWT
      ]

instance FromJSON GetCausalHashByPathResponse where
  parseJSON = Aeson.withObject "GetCausalHashByPathResponse" $ \obj -> do
    causalHash <- obj .: "causal_hash"
    pure GetCausalHashByPathResponse {..}

data DownloadEntitiesRequest = DownloadEntitiesRequest
  { repoName :: RepoName,
    hashes :: NESet HashJWT
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON DownloadEntitiesRequest where
  toJSON (DownloadEntitiesRequest repoName hashes) =
    object
      [ "repo_name" .= repoName,
        "hashes" .= hashes
      ]

instance FromJSON DownloadEntitiesRequest where
  parseJSON = Aeson.withObject "DownloadEntitiesRequest" $ \obj -> do
    repoName <- obj .: "repo_name"
    hashes <- obj .: "hashes"
    pure DownloadEntitiesRequest {..}

data DownloadEntitiesResponse = DownloadEntitiesResponse
  { entities :: NEMap Hash (Entity HashJWT Hash Text)
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON DownloadEntitiesResponse where
  toJSON (DownloadEntitiesResponse entities) =
    object
      [ "entities" .= entities
      ]

instance FromJSON DownloadEntitiesResponse where
  parseJSON = Aeson.withObject "DownloadEntitiesResponse" $ \obj -> do
    DownloadEntitiesResponse <$> obj .: "entities"

data UpdatePathRequest = UpdatePathRequest
  { path :: RepoPath,
    expectedHash :: Maybe TypedHash, -- Nothing requires empty history at destination
    newHash :: TypedHash
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON UpdatePathRequest where
  toJSON (UpdatePathRequest path expectedHash newHash) =
    object
      [ "path" .= path,
        "expected_hash" .= expectedHash,
        "new_hash" .= newHash
      ]

instance FromJSON UpdatePathRequest where
  parseJSON = Aeson.withObject "UpdatePathRequest" $ \obj -> do
    path <- obj .: "path"
    expectedHash <- obj .: "expected_hash"
    newHash <- obj .: "new_hash"
    pure UpdatePathRequest {..}

data UpdatePathResponse
  = UpdatePathSuccess
  | UpdatePathHashMismatch HashMismatch
  | UpdatePathMissingDependencies (NeedDependencies Hash)
  deriving stock (Show, Eq)

jsonUnion :: ToJSON a => Text -> a -> Value
jsonUnion typeName val =
  Aeson.object
    [ "type" .= String typeName,
      "payload" .= val
    ]

instance ToJSON UpdatePathResponse where
  toJSON = \case
    UpdatePathSuccess -> jsonUnion "success" ()
    UpdatePathHashMismatch hm -> jsonUnion "hash_mismatch" hm
    UpdatePathMissingDependencies md -> jsonUnion "missing_dependencies" md

instance FromJSON UpdatePathResponse where
  parseJSON = Aeson.withObject "UploadEntitiesResponse" $ \obj ->
    obj .: "type" >>= Aeson.withText "type" \case
      "success" -> pure UpdatePathSuccess
      "hash_mismatch" -> UpdatePathHashMismatch <$> obj .: "payload"
      "missing_dependencies" -> UpdatePathMissingDependencies <$> obj .: "payload"
      _ -> fail "Unknown UpdatePathResponse type"

data NeedDependencies hash = NeedDependencies
  { missingDependencies :: NESet hash
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON hash => ToJSON (NeedDependencies hash) where
  toJSON (NeedDependencies missingDependencies) =
    object ["missing_dependencies" .= missingDependencies]

instance (FromJSON hash, Ord hash) => FromJSON (NeedDependencies hash) where
  parseJSON = Aeson.withObject "NeedDependencies" $ \obj -> do
    missingDependencies <- obj .: "missing_dependencies"
    pure NeedDependencies {..}

data HashMismatch = HashMismatch
  { repoPath :: RepoPath,
    expectedHash :: Maybe TypedHash,
    actualHash :: Maybe TypedHash
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON HashMismatch where
  toJSON (HashMismatch repoPath expectedHash actualHash) =
    object
      [ "repo_path" .= repoPath,
        "expected_hash" .= expectedHash,
        "actual_hash" .= actualHash
      ]

instance FromJSON HashMismatch where
  parseJSON = Aeson.withObject "HashMismatch" $ \obj -> do
    repoPath <- obj .: "repo_path"
    expectedHash <- obj .: "expected_hash"
    actualHash <- obj .: "actual_hash"
    pure HashMismatch {..}

data UploadEntitiesRequest = UploadEntitiesRequest
  { repoName :: RepoName,
    entities :: NEMap Hash (Entity TypedHash TypedHash Text)
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON UploadEntitiesRequest where
  toJSON (UploadEntitiesRequest repoName entities) =
    object
      [ "repo_name" .= repoName,
        "entities" .= entities
      ]

instance FromJSON UploadEntitiesRequest where
  parseJSON = Aeson.withObject "UploadEntitiesRequest" $ \obj -> do
    repoName <- obj .: "repo_name"
    entities <- obj .: "entities"
    pure UploadEntitiesRequest {..}

data UploadEntitiesResponse
  = UploadEntitiesSuccess
  | UploadEntitiesNeedDependencies (NeedDependencies Hash)
  deriving stock (Show, Eq, Ord)

instance ToJSON UploadEntitiesResponse where
  toJSON = \case
    UploadEntitiesSuccess ->
      object
        [ "type" .= String "success",
          "payload" .= Null
        ]
    UploadEntitiesNeedDependencies nd ->
      object
        [ "type" .= String "need_dependencies",
          "payload" .= toJSON nd
        ]

instance FromJSON UploadEntitiesResponse where
  parseJSON = Aeson.withObject "UploadEntitiesResponse" $ \obj ->
    obj .: "type" >>= Aeson.withText "type" \case
      "success" -> pure UploadEntitiesSuccess
      "need_dependencies" -> UploadEntitiesNeedDependencies <$> obj .: "payload"
      _ -> fail "Unknown UploadEntitiesResponse type"

data Entity hash replacementHash text
  = TC (TermComponent hash text)
  | DC (DeclComponent hash text)
  | P (Patch hash replacementHash text)
  | N (Namespace hash text)
  | C (Causal hash)
  deriving stock (Show, Eq, Ord)

instance (ToJSON hash, ToJSON replacementHash, ToJSON text) => ToJSON (Entity hash replacementHash text) where
  toJSON = \case
    TC tc ->
      object
        [ "type" .= TermComponentType,
          "object" .= tc
        ]
    DC dc ->
      object
        [ "type" .= DeclComponentType,
          "object" .= dc
        ]
    P patch ->
      object
        [ "type" .= PatchType,
          "object" .= patch
        ]
    N ns ->
      object
        [ "type" .= NamespaceType,
          "object" .= ns
        ]
    C causal ->
      object
        [ "type" .= CausalType,
          "object" .= causal
        ]

instance (FromJSON hash, FromJSON replacementHash, FromJSON text, Ord hash) => FromJSON (Entity hash replacementHash text) where
  parseJSON = Aeson.withObject "Entity" $ \obj -> do
    entityType <- obj .: "type"
    case entityType of
      TermComponentType -> TC <$> obj .: "object"
      DeclComponentType -> DC <$> obj .: "object"
      PatchType -> P <$> obj .: "object"
      NamespaceType -> N <$> obj .: "object"
      CausalType -> C <$> obj .: "object"

data TermComponent hash text = TermComponent [(LocalIds hash text, ByteString)]
  deriving stock (Show, Eq, Ord)

instance Bifoldable TermComponent where
  bifoldMap = bifoldMapDefault

instance Bifunctor TermComponent where
  bimap = bimapDefault

instance Bitraversable TermComponent where
  bitraverse f g (TermComponent xs) =
    TermComponent <$> bitraverseComponents f g xs

instance (ToJSON hash, ToJSON text) => ToJSON (TermComponent hash text) where
  toJSON (TermComponent components) =
    object
      [ "terms" .= (encodeComponentPiece <$> components)
      ]

bitraverseComponents ::
  Applicative f =>
  (a -> f a') ->
  (b -> f b') ->
  [(LocalIds a b, ByteString)] ->
  f [(LocalIds a' b', ByteString)]
bitraverseComponents f g =
  traverse . _1 $ bitraverse f g
  where
    _1 f (l, r) = (,r) <$> f l

encodeComponentPiece :: (ToJSON hash, ToJSON text) => (LocalIds hash text, ByteString) -> Value
encodeComponentPiece (localIDs, bytes) =
  object
    [ "local_ids" .= localIDs,
      "bytes" .= Base64Bytes bytes
    ]

decodeComponentPiece :: (FromJSON hash, FromJSON text) => Value -> Aeson.Parser (LocalIds hash text, ByteString)
decodeComponentPiece = Aeson.withObject "Component Piece" $ \obj -> do
  localIDs <- obj .: "local_ids"
  Base64Bytes bytes <- obj .: "local_ids"
  pure (localIDs, bytes)

instance (FromJSON hash, FromJSON text) => FromJSON (TermComponent hash text) where
  parseJSON = Aeson.withObject "TermComponent" $ \obj -> do
    pieces <- obj .: "terms"
    terms <- traverse decodeComponentPiece pieces
    pure (TermComponent terms)

data DeclComponent hash text = DeclComponent [(LocalIds hash text, ByteString)]
  deriving stock (Show, Eq, Ord)

instance Bifoldable DeclComponent where
  bifoldMap = bifoldMapDefault

instance Bifunctor DeclComponent where
  bimap = bimapDefault

instance Bitraversable DeclComponent where
  bitraverse f g (DeclComponent xs) =
    DeclComponent <$> bitraverseComponents f g xs

instance (ToJSON hash, ToJSON text) => ToJSON (DeclComponent hash text) where
  toJSON (DeclComponent components) =
    object
      [ "decls" .= (encodeComponentPiece <$> components)
      ]

instance (FromJSON hash, FromJSON text) => FromJSON (DeclComponent hash text) where
  parseJSON = Aeson.withObject "DeclComponent" $ \obj -> do
    pieces <- obj .: "decls"
    terms <- traverse decodeComponentPiece pieces
    pure (DeclComponent terms)

data LocalIds hash text = LocalIds
  { hashes :: [hash],
    texts :: [text]
  }
  deriving stock (Show, Eq, Ord)

instance Bifoldable LocalIds where
  bifoldMap = bifoldMapDefault

instance Bifunctor LocalIds where
  bimap = bimapDefault

instance Bitraversable LocalIds where
  bitraverse f g (LocalIds hashes texts) =
    LocalIds <$> traverse f hashes <*> traverse g texts

instance (ToJSON hash, ToJSON text) => ToJSON (LocalIds hash text) where
  toJSON (LocalIds hashes texts) =
    object
      [ "hashes" .= hashes,
        "texts" .= texts
      ]

instance (FromJSON hash, FromJSON text) => FromJSON (LocalIds hash text) where
  parseJSON = Aeson.withObject "LocalIds" $ \obj -> do
    hashes <- obj .: "hashes"
    texts <- obj .: "texts"
    pure LocalIds {..}

data Patch hash replacementHash text = Patch
  { textLookup :: [text],
    oldHashLookup :: [hash],
    replacementHashLookup :: [replacementHash],
    bytes :: ByteString
  }
  deriving stock (Show, Eq, Ord)

instance (ToJSON hash, ToJSON replacementHash, ToJSON text) => ToJSON (Patch hash replacementHash text) where
  toJSON (Patch textLookup hashLookup optionalHashLookup bytes) =
    object
      [ "text_lookup" .= textLookup,
        "hash_lookup" .= hashLookup,
        "optional_hash_lookup" .= optionalHashLookup,
        "bytes" .= Base64Bytes bytes
      ]

instance (FromJSON hash, FromJSON replacementHash, FromJSON text) => FromJSON (Patch hash replacementHash text) where
  parseJSON = Aeson.withObject "Patch" $ \obj -> do
    textLookup <- obj .: "text_lookup"
    oldHashLookup <- obj .: "hash_lookup"
    replacementHashLookup <- obj .: "optional_hash_lookup"
    Base64Bytes bytes <- obj .: "bytes"
    pure (Patch {..})

data Namespace hash text = Namespace
  { textLookup :: [text],
    defnLookup :: [hash],
    patchLookup :: [hash],
    childLookup :: [hash],
    bytes :: ByteString
  }
  deriving stock (Eq, Ord, Show)

instance Bifoldable Namespace where
  bifoldMap = bifoldMapDefault

instance Bifunctor Namespace where
  bimap = bimapDefault

instance Bitraversable Namespace where
  bitraverse f g (Namespace tl dl pl cl b) =
    Namespace
      <$> traverse g tl
      <*> traverse f dl
      <*> traverse f pl
      <*> traverse f cl
      <*> pure b

instance (ToJSON hash, ToJSON text) => ToJSON (Namespace hash text) where
  toJSON (Namespace textLookup defnLookup patchLookup childLookup bytes) =
    object
      [ "text_lookup" .= textLookup,
        "defn_lookup" .= defnLookup,
        "patch_lookup" .= patchLookup,
        "child_lookup" .= childLookup,
        "bytes" .= Base64Bytes bytes
      ]

instance (FromJSON hash, FromJSON text) => FromJSON (Namespace hash text) where
  parseJSON = Aeson.withObject "Namespace" $ \obj -> do
    textLookup <- obj .: "text_lookup"
    defnLookup <- obj .: "defn_lookup"
    patchLookup <- obj .: "patch_lookup"
    childLookup <- obj .: "child_lookup"
    Base64Bytes bytes <- obj .: "bytes"
    pure Namespace {..}

-- Client _may_ choose not to download the namespace entity in the future, but
-- we still send them the hash/hashjwt.
data Causal hash = Causal
  { namespaceHash :: hash,
    parents :: Set hash
  }
  deriving stock (Eq, Ord, Show)

instance (ToJSON hash) => ToJSON (Causal hash) where
  toJSON (Causal namespaceHash parents) =
    object
      [ "namespace_hash" .= namespaceHash,
        "parents" .= parents
      ]

instance (FromJSON hash, Ord hash) => FromJSON (Causal hash) where
  parseJSON = Aeson.withObject "Causal" $ \obj -> do
    namespaceHash <- obj .: "namespace_hash"
    parents <- obj .: "parents"
    pure Causal {..}

data EntityType
  = TermComponentType
  | DeclComponentType
  | PatchType
  | NamespaceType
  | CausalType
  deriving stock (Eq, Ord, Show)

instance ToJSON EntityType where
  toJSON et = String $ case et of
    TermComponentType -> "term_component"
    DeclComponentType -> "decl_component"
    PatchType -> "patch"
    NamespaceType -> "namespace"
    CausalType -> "causal"

instance FromJSON EntityType where
  parseJSON = Aeson.withText "EntityType" \case
    "term_component" -> pure TermComponentType
    "decl_component" -> pure DeclComponentType
    "patch" -> pure PatchType
    "namespace" -> pure NamespaceType
    "causal" -> pure CausalType
    _ -> fail "Unexpected entity type"
