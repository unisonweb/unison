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
import Data.Map (Map)
import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Unison.Util.Tritraversable

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
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

newtype Base32 = Base32 Text
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

newtype Hash = Hash Text
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data TypedHash = TypedHash
  { hash :: Hash,
    entityType :: EntityType
  }
  deriving (Eq, Ord)

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

instance ToJSON RepoPath where
  toJSON (RepoPath name segments) =
    object
      [ "repo_name" .= name,
        "path" .= Text.intercalate "." segments
      ]

instance FromJSON RepoPath where
  parseJSON = Aeson.withObject "RepoPath" $ \obj -> do
    repoName <- obj .: "repo_name"
    pathSegments <- Text.splitOn "." <$> (obj .: "path")
    pure RepoPath {..}

newtype GetCausalHashByPathRequest = GetCausalHashByPathRequest
  { repoPath :: RepoPath
  }

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
  { causalHash :: HashJWT
  }

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
  { entities :: Map Hash (Entity HashJWT Hash Text)
  }

instance ToJSON DownloadEntitiesResponse where
  toJSON (DownloadEntitiesResponse entities) =
    object
      [ "entities" .= entities
      ]

data PushRequest = PushRequest
  { path :: RepoPath,
    expectedHash :: Maybe TypedHash, -- Nothing requires empty history at destination
    newHash :: TypedHash
  }

instance ToJSON PushRequest where
  toJSON (PushRequest path expectedHash newHash) =
    object
      [ "path" .= path,
        "expected_hash" .= expectedHash,
        "new_hash" .= newHash
      ]

instance FromJSON PushRequest where
  parseJSON = Aeson.withObject "PushRequest" $ \obj -> do
    path <- obj .: "path"
    expectedHash <- obj .: "expected_hash"
    newHash <- obj .: "new_hash"
    pure PushRequest {..}

data PushResponse
  = OutOfDate OutOfDateHash
  | MissingDependencies (NeedDependencies Hash)

data NeedDependencies hash = NeedDependencies
  { missingDependencies :: NESet hash
  }

instance ToJSON hash => ToJSON (NeedDependencies hash) where
  toJSON (NeedDependencies missingDependencies) =
    object ["missing_dependencies" .= missingDependencies]

instance (FromJSON hash, Ord hash) => FromJSON (NeedDependencies hash) where
  parseJSON = Aeson.withObject "NeedDependencies" $ \obj -> do
    missingDependencies <- obj .: "missing_dependencies"
    pure NeedDependencies {..}

data OutOfDateHash = OutOfDateHash
  { repoPath :: RepoPath,
    expectedHash :: Maybe TypedHash,
    actualHash :: Maybe TypedHash
  }

instance ToJSON OutOfDateHash where
  toJSON (OutOfDateHash repoPath expectedHash actualHash) =
    object
      [ "repo_path" .= repoPath,
        "expected_hash" .= expectedHash,
        "actual_hash" .= actualHash
      ]

instance FromJSON OutOfDateHash where
  parseJSON = Aeson.withObject "OutOfDateHash" $ \obj -> do
    repoPath <- obj .: "repo_path"
    expectedHash <- obj .: "expected_hash"
    actualHash <- obj .: "actual_hash"
    pure OutOfDateHash {..}

data UploadEntitiesRequest = UploadEntitiesRequest
  { repoName :: RepoName,
    entities :: Map Hash (Entity TypedHash TypedHash Text)
  }

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

data Entity hash optionalHash text
  = TC (TermComponent hash text)
  | DC (DeclComponent hash text)
  | P (Patch hash optionalHash text)
  | N (Namespace hash text)
  | C (Causal hash)
  deriving anyclass (Trifunctor, Trifoldable)

instance Tritraversable Entity where
  tritraverse f g h = \case
    TC tc -> TC <$> bitraverse f h tc
    DC dc -> DC <$> bitraverse f h dc
    P pa -> P <$> tritraverse f g h pa
    N name -> N <$> bitraverse f h name
    C ca -> C <$> traverse f ca

instance (ToJSON hash, ToJSON optionalHash, ToJSON text) => ToJSON (Entity hash optionalHash text) where
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
        [ "type" .= NamespaceType,
          "object" .= causal
        ]

instance (FromJSON hash, FromJSON optionalHash, FromJSON text, Ord hash) => FromJSON (Entity hash optionalHash text) where
  parseJSON = Aeson.withObject "Entity" $ \obj -> do
    entityType <- obj .: "type"
    case entityType of
      TermComponentType -> TC <$> obj .: "object"
      DeclComponentType -> DC <$> obj .: "object"
      PatchType -> P <$> obj .: "object"
      NamespaceType -> N <$> obj .: "object"
      CausalType -> C <$> obj .: "object"

data TermComponent hash text = TermComponent [(LocalIds hash text, ByteString)]

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

data Patch hash optionalHash text = Patch
  { textLookup :: [text],
    hashLookup :: [hash],
    optionalHashLookup :: [optionalHash],
    bytes :: ByteString
  }
  deriving anyclass (Trifunctor, Trifoldable)

instance Tritraversable Patch where
  tritraverse _f _g _h = undefined

instance (ToJSON hash, ToJSON optionalHash, ToJSON text) => ToJSON (Patch hash optionalHash text) where
  toJSON (Patch textLookup hashLookup optionalHashLookup bytes) =
    object
      [ "text_lookup" .= textLookup,
        "hash_lookup" .= hashLookup,
        "optional_hash_lookup" .= optionalHashLookup,
        "bytes" .= Base64Bytes bytes
      ]

instance (FromJSON hash, FromJSON optionalHash, FromJSON text) => FromJSON (Patch hash optionalHash text) where
  parseJSON = Aeson.withObject "Patch" $ \obj -> do
    textLookup <- obj .: "text_lookup"
    hashLookup <- obj .: "hash_lookup"
    optionalHashLookup <- obj .: "optional_hash_lookup"
    Base64Bytes bytes <- obj .: "bytes"
    pure (Patch {..})

data Namespace hash text = Namespace
  { textLookup :: [text],
    defnLookup :: [hash],
    patchLookup :: [hash],
    childLookup :: [hash],
    bytes :: ByteString
  }

instance Bifoldable Namespace where
  bifoldMap = bifoldMapDefault

instance Bifunctor Namespace where
  bimap = bimapDefault

instance Bitraversable Namespace where
  bitraverse = undefined

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

instance Functor Causal where
  fmap = undefined

instance Foldable Causal where
  foldMap = undefined

instance Traversable Causal where
  traverse = undefined

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
  deriving (Eq, Ord, Show)

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
