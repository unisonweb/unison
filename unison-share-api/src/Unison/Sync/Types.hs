{-# LANGUAGE DataKinds #-}
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
import Data.Function ((&))
import Data.Map.NonEmpty (NEMap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import U.Util.Base32Hex (Base32Hex (..))

-- | A newtype for JSON encoding binary data.
newtype Base64Bytes = Base64Bytes ByteString

instance ToJSON Base64Bytes where
  toJSON (Base64Bytes bytes) = String . Text.decodeUtf8 $ convertToBase Base64 bytes

instance FromJSON Base64Bytes where
  parseJSON = Aeson.withText "Base64" $ \txt -> do
    either fail (pure . Base64Bytes) $ convertFromBase Base64 (Text.encodeUtf8 txt)

newtype RepoName = RepoName Text
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

newtype HashJWT = HashJWT {unHashJWT :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

-- | Grab the hash out of a hash JWT.
--
-- This decodes the whole JWT, then throws away the claims; use it if you really only need the hash!
hashJWTHash :: HashJWT -> Hash
hashJWTHash =
  decodedHashJWTHash . decodeHashJWT

-- | A decoded hash JWT that retains the original encoded JWT.
data DecodedHashJWT = DecodedHashJWT
  { claims :: HashJWTClaims,
    hashJWT :: HashJWT
  }
  deriving (Eq, Ord, Show)

-- | Decode a hash JWT.
decodeHashJWT :: HashJWT -> DecodedHashJWT
decodeHashJWT = undefined

-- | Grab the hash out of a decoded hash JWT.
decodedHashJWTHash :: DecodedHashJWT -> Hash
decodedHashJWTHash = undefined

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

newtype Hash = Hash {toBase32Hex :: Base32Hex}
  deriving (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey) via (Text)

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

data GetCausalHashByPathResponse
  = GetCausalHashByPathSuccess (Maybe HashJWT)
  | GetCausalHashByPathNoReadPermission RepoPath
  deriving stock (Show, Eq, Ord)

instance ToJSON GetCausalHashByPathResponse where
  toJSON = \case
    GetCausalHashByPathSuccess hashJWT -> jsonUnion "success" hashJWT
    GetCausalHashByPathNoReadPermission repoPath -> jsonUnion "no_read_permission" repoPath

instance FromJSON GetCausalHashByPathResponse where
  parseJSON = Aeson.withObject "GetCausalHashByPathResponse" \obj -> do
    obj .: "type" >>= Aeson.withText "type" \case
      "success" -> GetCausalHashByPathSuccess <$> obj .: "payload"
      "no_read_permission" -> GetCausalHashByPathNoReadPermission <$> obj .: "payload"
      t -> failText $ "Unexpected GetCausalHashByPathResponse type: " <> t

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
  { entities :: NEMap Hash (Entity Text Hash HashJWT)
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
  | UpdatePathNoWritePermission RepoPath
  deriving stock (Show, Eq, Ord)

jsonUnion :: ToJSON a => Text -> a -> Value
jsonUnion typeName val =
  Aeson.object
    [ "type" .= String typeName,
      "payload" .= val
    ]

instance ToJSON UpdatePathResponse where
  toJSON = \case
    UpdatePathSuccess -> jsonUnion "success" (Object mempty)
    UpdatePathHashMismatch hm -> jsonUnion "hash_mismatch" hm
    UpdatePathMissingDependencies md -> jsonUnion "missing_dependencies" md
    UpdatePathNoWritePermission repoPath -> jsonUnion "no_write_permission" repoPath

instance FromJSON UpdatePathResponse where
  parseJSON v =
    v & Aeson.withObject "UploadEntitiesResponse" \obj ->
      obj .: "type" >>= Aeson.withText "type" \case
        "success" -> pure UpdatePathSuccess
        "hash_mismatch" -> UpdatePathHashMismatch <$> obj .: "payload"
        "missing_dependencies" -> UpdatePathMissingDependencies <$> obj .: "payload"
        "no_write_permission" -> UpdatePathNoWritePermission <$> obj .: "payload"
        t -> failText $ "Unexpected UpdatePathResponse type: " <> t

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
    entities :: NEMap Hash (Entity Text Hash Hash)
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
  | UploadEntitiesNoWritePermission RepoName
  deriving stock (Show, Eq, Ord)

instance ToJSON UploadEntitiesResponse where
  toJSON = \case
    UploadEntitiesSuccess -> jsonUnion "success" (Object mempty)
    UploadEntitiesNeedDependencies nd -> jsonUnion "need_dependencies" nd
    UploadEntitiesNoWritePermission repoName -> jsonUnion "no_write_permission" repoName

instance FromJSON UploadEntitiesResponse where
  parseJSON v =
    v & Aeson.withObject "UploadEntitiesResponse" \obj ->
      obj .: "type" >>= Aeson.withText "type" \case
        "success" -> pure UploadEntitiesSuccess
        "need_dependencies" -> UploadEntitiesNeedDependencies <$> obj .: "payload"
        "no_write_permission" -> UploadEntitiesNoWritePermission <$> obj .: "payload"
        t -> failText $ "Unexpected UploadEntitiesResponse type: " <> t

data Entity text noSyncHash hash
  = TC (TermComponent text hash)
  | DC (DeclComponent text hash)
  | P (Patch text noSyncHash hash)
  | N (Namespace text hash)
  | C (Causal hash)
  deriving stock (Show, Eq, Ord)

instance (ToJSON text, ToJSON noSyncHash, ToJSON hash) => ToJSON (Entity text noSyncHash hash) where
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

instance (FromJSON text, FromJSON noSyncHash, FromJSON hash, Ord hash) => FromJSON (Entity text noSyncHash hash) where
  parseJSON = Aeson.withObject "Entity" $ \obj -> do
    entityType <- obj .: "type"
    case entityType of
      TermComponentType -> TC <$> obj .: "object"
      DeclComponentType -> DC <$> obj .: "object"
      PatchType -> P <$> obj .: "object"
      NamespaceType -> N <$> obj .: "object"
      CausalType -> C <$> obj .: "object"

-- | Get the direct dependencies of an entity (which are actually sync'd).
--
-- FIXME use generic-lens here? (typed @hash)
entityDependencies :: Ord hash => Entity text noSyncHash hash -> Set hash
entityDependencies = \case
  TC (TermComponent terms) -> flip foldMap terms \(LocalIds {hashes}, _term) -> Set.fromList hashes
  DC (DeclComponent decls) -> flip foldMap decls \(LocalIds {hashes}, _decl) -> Set.fromList hashes
  P Patch {newHashLookup} -> Set.fromList newHashLookup
  N Namespace {defnLookup, patchLookup, childLookup} ->
    Set.fromList defnLookup <> Set.fromList patchLookup
      <> foldMap (\(namespaceHash, causalHash) -> Set.fromList [namespaceHash, causalHash]) childLookup
  C Causal {parents} -> parents

data TermComponent text hash = TermComponent [(LocalIds text hash, ByteString)]
  deriving stock (Show, Eq, Ord)

instance Bifoldable TermComponent where
  bifoldMap = bifoldMapDefault

instance Bifunctor TermComponent where
  bimap = bimapDefault

instance Bitraversable TermComponent where
  bitraverse f g (TermComponent xs) =
    TermComponent <$> bitraverseComponents f g xs

instance (ToJSON text, ToJSON hash) => ToJSON (TermComponent text hash) where
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

encodeComponentPiece :: (ToJSON text, ToJSON hash) => (LocalIds text hash, ByteString) -> Value
encodeComponentPiece (localIDs, bytes) =
  object
    [ "local_ids" .= localIDs,
      "bytes" .= Base64Bytes bytes
    ]

decodeComponentPiece :: (FromJSON text, FromJSON hash) => Value -> Aeson.Parser (LocalIds text hash, ByteString)
decodeComponentPiece = Aeson.withObject "Component Piece" $ \obj -> do
  localIDs <- obj .: "local_ids"
  Base64Bytes bytes <- obj .: "local_ids"
  pure (localIDs, bytes)

failText :: MonadFail m => Text -> m a
failText = fail . Text.unpack

instance (FromJSON text, FromJSON hash) => FromJSON (TermComponent text hash) where
  parseJSON = Aeson.withObject "TermComponent" $ \obj -> do
    pieces <- obj .: "terms"
    terms <- traverse decodeComponentPiece pieces
    pure (TermComponent terms)

data DeclComponent text hash = DeclComponent [(LocalIds text hash, ByteString)]
  deriving stock (Show, Eq, Ord)

instance Bifoldable DeclComponent where
  bifoldMap = bifoldMapDefault

instance Bifunctor DeclComponent where
  bimap = bimapDefault

instance Bitraversable DeclComponent where
  bitraverse f g (DeclComponent xs) =
    DeclComponent <$> bitraverseComponents f g xs

instance (ToJSON text, ToJSON hash) => ToJSON (DeclComponent text hash) where
  toJSON (DeclComponent components) =
    object
      [ "decls" .= (encodeComponentPiece <$> components)
      ]

instance (FromJSON text, FromJSON hash) => FromJSON (DeclComponent text hash) where
  parseJSON = Aeson.withObject "DeclComponent" $ \obj -> do
    pieces <- obj .: "decls"
    terms <- traverse decodeComponentPiece pieces
    pure (DeclComponent terms)

data LocalIds text hash = LocalIds
  { texts :: [text],
    hashes :: [hash]
  }
  deriving stock (Show, Eq, Ord)

instance Bifoldable LocalIds where
  bifoldMap = bifoldMapDefault

instance Bifunctor LocalIds where
  bimap = bimapDefault

instance Bitraversable LocalIds where
  bitraverse f g (LocalIds texts hashes) =
    LocalIds <$> traverse f texts <*> traverse g hashes

instance (ToJSON text, ToJSON hash) => ToJSON (LocalIds text hash) where
  toJSON (LocalIds texts hashes) =
    object
      [ "texts" .= texts,
        "hashes" .= hashes
      ]

instance (FromJSON text, FromJSON hash) => FromJSON (LocalIds text hash) where
  parseJSON = Aeson.withObject "LocalIds" $ \obj -> do
    texts <- obj .: "texts"
    hashes <- obj .: "hashes"
    pure LocalIds {..}

data Patch text oldHash newHash = Patch
  { textLookup :: [text],
    oldHashLookup :: [oldHash],
    newHashLookup :: [newHash],
    bytes :: ByteString
  }
  deriving stock (Show, Eq, Ord)

instance (ToJSON text, ToJSON oldHash, ToJSON newHash) => ToJSON (Patch text oldHash newHash) where
  toJSON (Patch textLookup oldHashLookup newHashLookup bytes) =
    object
      [ "text_lookup" .= textLookup,
        "optional_hash_lookup" .= oldHashLookup,
        "hash_lookup" .= newHashLookup,
        "bytes" .= Base64Bytes bytes
      ]

instance (FromJSON text, FromJSON oldHash, FromJSON newHash) => FromJSON (Patch text oldHash newHash) where
  parseJSON = Aeson.withObject "Patch" $ \obj -> do
    textLookup <- obj .: "text_lookup"
    oldHashLookup <- obj .: "optional_hash_lookup"
    newHashLookup <- obj .: "hash_lookup"
    Base64Bytes bytes <- obj .: "bytes"
    pure Patch {..}

data Namespace text hash = Namespace
  { textLookup :: [text],
    defnLookup :: [hash],
    patchLookup :: [hash],
    childLookup :: [(hash, hash)], -- (namespace hash, causal hash)
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
      <$> traverse f tl
      <*> traverse g dl
      <*> traverse g pl
      <*> traverse (bitraverse g g) cl
      <*> pure b

instance (ToJSON text, ToJSON hash) => ToJSON (Namespace text hash) where
  toJSON (Namespace textLookup defnLookup patchLookup childLookup bytes) =
    object
      [ "text_lookup" .= textLookup,
        "defn_lookup" .= defnLookup,
        "patch_lookup" .= patchLookup,
        "child_lookup" .= childLookup,
        "bytes" .= Base64Bytes bytes
      ]

instance (FromJSON text, FromJSON hash) => FromJSON (Namespace text hash) where
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
    t -> failText $ "Unexpected entity type: " <> t
