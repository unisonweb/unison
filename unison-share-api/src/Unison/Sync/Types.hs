{-# LANGUAGE RecordWildCards #-}

module Unison.Sync.Types
  ( -- * Misc. types
    Base64Bytes (..),
    RepoName (..),
    Path (..),
    pathRepoName,
    pathCodebasePath,

    -- ** Hash types
    Hash (..),
    HashJWT (..),
    hashJWTHash,
    HashJWTClaims (..),
    DecodedHashJWT (..),
    decodeHashJWT,
    decodeHashJWTClaims,
    decodedHashJWTHash,

    -- ** Entity types
    Entity (..),
    TermComponent (..),
    DeclComponent (..),
    Patch (..),
    PatchDiff (..),
    Namespace (..),
    NamespaceDiff (..),
    Causal (..),
    LocalIds (..),
    entityDependencies,
    EntityType (..),

    -- * Request/response types

    -- ** Get causal hash by path
    GetCausalHashByPathRequest (..),
    GetCausalHashByPathResponse (..),

    -- ** Download entities
    DownloadEntitiesRequest (..),
    DownloadEntitiesResponse (..),

    -- ** Upload entities
    UploadEntitiesRequest (..),
    UploadEntitiesResponse (..),

    -- ** Fast-forward path
    FastForwardPathRequest (..),
    FastForwardPathResponse (..),

    -- ** Update path
    UpdatePathRequest (..),
    UpdatePathResponse (..),
    HashMismatch (..),

    -- * Common/shared error types
    HashMismatchForEntity (..),
    InvalidParentage (..),
    NeedDependencies (..),
  )
where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteArray.Encoding (Base (Base64), convertFromBase, convertToBase)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import U.Util.Base32Hex (Base32Hex (..))
import Unison.Prelude
import qualified Web.JWT as JWT

------------------------------------------------------------------------------------------------------------------------
-- Misc. types

-- | A newtype for JSON encoding binary data.
newtype Base64Bytes = Base64Bytes ByteString

instance ToJSON Base64Bytes where
  toJSON (Base64Bytes bytes) = String . Text.decodeUtf8 $ convertToBase Base64 bytes

instance FromJSON Base64Bytes where
  parseJSON = Aeson.withText "Base64" \txt -> do
    either fail (pure . Base64Bytes) $ convertFromBase Base64 (Text.encodeUtf8 txt)

newtype RepoName = RepoName {unRepoName :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data Path = Path
  { -- This is a nonempty list, where we require the first segment to be the repo name / user name / whatever,
    -- which we need on the server side as an implementation detail of how we're representing different users' codebases.

    -- This could be relaxed in some other share implementation that allows access to the "root" of the shared codebase.
    -- Our share implementation doesn't have a root, just a collection of sub-roots, one per user or (eventually) organization.
    pathSegments :: NonEmpty Text
  }
  deriving stock (Show, Eq, Ord)

pathRepoName :: Path -> RepoName
pathRepoName (Path (p :| _)) = RepoName p

pathCodebasePath :: Path -> [Text]
pathCodebasePath (Path (_ :| ps)) = ps

instance ToJSON Path where
  toJSON (Path segments) =
    object
      [ "path" .= segments
      ]

instance FromJSON Path where
  parseJSON = Aeson.withObject "Path" \obj -> do
    pathSegments <- obj .: "path"
    pure Path {..}

------------------------------------------------------------------------------------------------------------------------
-- Hash types

newtype Hash = Hash {toBase32Hex :: Base32Hex}
  deriving (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey) via (Text)

newtype HashJWT = HashJWT {unHashJWT :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

-- | Grab the hash out of a hash JWT.
--
-- This decodes the whole JWT, then throws away the claims; use it if you really only need the hash!
hashJWTHash :: HashJWT -> Hash
hashJWTHash =
  decodedHashJWTHash . decodeHashJWT

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
  parseJSON = Aeson.withObject "HashJWTClaims" \obj -> do
    hash <- obj .: "h"
    entityType <- obj .: "t"
    pure HashJWTClaims {..}

-- | A decoded hash JWT that retains the original encoded JWT.
data DecodedHashJWT = DecodedHashJWT
  { claims :: HashJWTClaims,
    hashJWT :: HashJWT
  }
  deriving (Eq, Ord, Show)

-- | Decode a hash JWT.
decodeHashJWT :: HashJWT -> DecodedHashJWT
decodeHashJWT hashJWT =
  DecodedHashJWT
    { claims = decodeHashJWTClaims hashJWT,
      hashJWT
    }

-- | Decode the claims out of a hash JWT.
decodeHashJWTClaims :: HashJWT -> HashJWTClaims
decodeHashJWTClaims (HashJWT text) =
  case JWT.decode text of
    Nothing -> error "bad JWT"
    Just jwt ->
      let object =
            jwt
              & JWT.claims
              & JWT.unregisteredClaims
              & JWT.unClaimsMap
              & Map.toList
              & HashMap.fromList
              & Aeson.Object
       in case Aeson.fromJSON object of
            Aeson.Error err -> error ("bad JWT: " ++ err)
            Aeson.Success claims -> claims

-- | Grab the hash out of a decoded hash JWT.
decodedHashJWTHash :: DecodedHashJWT -> Hash
decodedHashJWTHash DecodedHashJWT {claims = HashJWTClaims {hash}} =
  hash

------------------------------------------------------------------------------------------------------------------------
-- Entity types

data Entity text noSyncHash hash
  = TC (TermComponent text hash)
  | DC (DeclComponent text hash)
  | P (Patch text noSyncHash hash)
  | PD (PatchDiff text noSyncHash hash)
  | N (Namespace text hash)
  | ND (NamespaceDiff text hash)
  | C (Causal hash)
  deriving stock (Show, Eq, Ord)

instance (ToJSON text, ToJSON noSyncHash, ToJSON hash) => ToJSON (Entity text noSyncHash hash) where
  toJSON = \case
    TC tc -> go TermComponentType tc
    DC dc -> go DeclComponentType dc
    P patch -> go PatchType patch
    PD patch -> go PatchDiffType patch
    N ns -> go NamespaceType ns
    ND ns -> go NamespaceDiffType ns
    C causal -> go CausalType causal
    where
      go :: ToJSON a => EntityType -> a -> Aeson.Value
      go typ obj = object ["type" .= typ, "object" .= obj]

instance (FromJSON text, FromJSON noSyncHash, FromJSON hash, Ord hash) => FromJSON (Entity text noSyncHash hash) where
  parseJSON = Aeson.withObject "Entity" \obj ->
    obj .: "type" >>= \case
      TermComponentType -> TC <$> obj .: "object"
      DeclComponentType -> DC <$> obj .: "object"
      PatchType -> P <$> obj .: "object"
      PatchDiffType -> PD <$> obj .: "object"
      NamespaceType -> N <$> obj .: "object"
      NamespaceDiffType -> ND <$> obj .: "object"
      CausalType -> C <$> obj .: "object"

-- | Get the direct dependencies of an entity (which are actually sync'd).
--
-- FIXME use generic-lens here? (typed @hash)
entityDependencies :: Ord hash => Entity text noSyncHash hash -> Set hash
entityDependencies = \case
  TC (TermComponent terms) -> flip foldMap terms \(LocalIds {hashes}, _term) -> Set.fromList hashes
  DC (DeclComponent decls) -> flip foldMap decls \(LocalIds {hashes}, _decl) -> Set.fromList hashes
  P Patch {newHashLookup} -> Set.fromList newHashLookup
  PD PatchDiff {parent, newHashLookup} -> Set.insert parent (Set.fromList newHashLookup)
  N Namespace {defnLookup, patchLookup, childLookup} ->
    Set.unions
      [ Set.fromList defnLookup,
        Set.fromList patchLookup,
        foldMap (\(namespaceHash, causalHash) -> Set.fromList [namespaceHash, causalHash]) childLookup
      ]
  ND NamespaceDiff {parent, defnLookup, patchLookup, childLookup} ->
    Set.unions
      [ Set.singleton parent,
        Set.fromList defnLookup,
        Set.fromList patchLookup,
        foldMap (\(namespaceHash, causalHash) -> Set.fromList [namespaceHash, causalHash]) childLookup
      ]
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
decodeComponentPiece = Aeson.withObject "Component Piece" \obj -> do
  localIDs <- obj .: "local_ids"
  Base64Bytes bytes <- obj .: "local_ids"
  pure (localIDs, bytes)

instance (FromJSON text, FromJSON hash) => FromJSON (TermComponent text hash) where
  parseJSON = Aeson.withObject "TermComponent" \obj -> do
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
  parseJSON = Aeson.withObject "DeclComponent" \obj -> do
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
  parseJSON = Aeson.withObject "LocalIds" \obj -> do
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
  parseJSON = Aeson.withObject "Patch" \obj -> do
    textLookup <- obj .: "text_lookup"
    oldHashLookup <- obj .: "optional_hash_lookup"
    newHashLookup <- obj .: "hash_lookup"
    Base64Bytes bytes <- obj .: "bytes"
    pure Patch {..}

data PatchDiff text oldHash hash = PatchDiff
  { parent :: hash,
    textLookup :: [text],
    oldHashLookup :: [oldHash],
    newHashLookup :: [hash],
    bytes :: ByteString
  }
  deriving stock (Eq, Ord, Show)

instance (ToJSON text, ToJSON oldHash, ToJSON hash) => ToJSON (PatchDiff text oldHash hash) where
  toJSON (PatchDiff parent textLookup oldHashLookup newHashLookup bytes) =
    object
      [ "parent" .= parent,
        "text_lookup" .= textLookup,
        "optional_hash_lookup" .= oldHashLookup,
        "hash_lookup" .= newHashLookup,
        "bytes" .= Base64Bytes bytes
      ]

instance (FromJSON text, FromJSON oldHash, FromJSON hash) => FromJSON (PatchDiff text oldHash hash) where
  parseJSON = Aeson.withObject "PatchDiff" \obj -> do
    parent <- obj .: "parent"
    textLookup <- obj .: "text_lookup"
    oldHashLookup <- obj .: "optional_hash_lookup"
    newHashLookup <- obj .: "hash_lookup"
    Base64Bytes bytes <- obj .: "bytes"
    pure PatchDiff {..}

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
  parseJSON = Aeson.withObject "Namespace" \obj -> do
    textLookup <- obj .: "text_lookup"
    defnLookup <- obj .: "defn_lookup"
    patchLookup <- obj .: "patch_lookup"
    childLookup <- obj .: "child_lookup"
    Base64Bytes bytes <- obj .: "bytes"
    pure Namespace {..}

data NamespaceDiff text hash = NamespaceDiff
  { parent :: hash,
    textLookup :: [text],
    defnLookup :: [hash],
    patchLookup :: [hash],
    childLookup :: [(hash, hash)], -- (namespace hash, causal hash)
    bytes :: ByteString
  }
  deriving stock (Eq, Ord, Show)

instance (ToJSON text, ToJSON hash) => ToJSON (NamespaceDiff text hash) where
  toJSON (NamespaceDiff parent textLookup defnLookup patchLookup childLookup bytes) =
    object
      [ "parent" .= parent,
        "text_lookup" .= textLookup,
        "defn_lookup" .= defnLookup,
        "patch_lookup" .= patchLookup,
        "child_lookup" .= childLookup,
        "bytes" .= Base64Bytes bytes
      ]

instance (FromJSON text, FromJSON hash) => FromJSON (NamespaceDiff text hash) where
  parseJSON = Aeson.withObject "NamespaceDiff" \obj -> do
    parent <- obj .: "parent"
    textLookup <- obj .: "text_lookup"
    defnLookup <- obj .: "defn_lookup"
    patchLookup <- obj .: "patch_lookup"
    childLookup <- obj .: "child_lookup"
    Base64Bytes bytes <- obj .: "bytes"
    pure NamespaceDiff {..}

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
  parseJSON = Aeson.withObject "Causal" \obj -> do
    namespaceHash <- obj .: "namespace_hash"
    parents <- obj .: "parents"
    pure Causal {..}

data EntityType
  = TermComponentType
  | DeclComponentType
  | PatchType
  | PatchDiffType
  | NamespaceType
  | NamespaceDiffType
  | CausalType
  deriving stock (Eq, Ord, Show)

instance ToJSON EntityType where
  toJSON =
    String . \case
      TermComponentType -> "term_component"
      DeclComponentType -> "decl_component"
      PatchType -> "patch"
      PatchDiffType -> "patch_diff"
      NamespaceType -> "namespace"
      NamespaceDiffType -> "namespace_diff"
      CausalType -> "causal"

instance FromJSON EntityType where
  parseJSON = Aeson.withText "EntityType" \case
    "term_component" -> pure TermComponentType
    "decl_component" -> pure DeclComponentType
    "patch" -> pure PatchType
    "patch_diff" -> pure PatchDiffType
    "namespace" -> pure NamespaceType
    "namespace_diff" -> pure NamespaceDiffType
    "causal" -> pure CausalType
    t -> failText $ "Unexpected entity type: " <> t

------------------------------------------------------------------------------------------------------------------------
-- Request/response types

------------------------------------------------------------------------------------------------------------------------
-- Get causal hash by path

newtype GetCausalHashByPathRequest = GetCausalHashByPathRequest
  { path :: Path
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON GetCausalHashByPathRequest where
  toJSON (GetCausalHashByPathRequest path) =
    object
      [ "path" .= path
      ]

instance FromJSON GetCausalHashByPathRequest where
  parseJSON = Aeson.withObject "GetCausalHashByPathRequest" \obj -> do
    path <- obj .: "path"
    pure GetCausalHashByPathRequest {..}

data GetCausalHashByPathResponse
  = GetCausalHashByPathSuccess (Maybe HashJWT)
  | GetCausalHashByPathNoReadPermission Path
  deriving stock (Show, Eq, Ord)

instance ToJSON GetCausalHashByPathResponse where
  toJSON = \case
    GetCausalHashByPathSuccess hashJWT -> jsonUnion "success" hashJWT
    GetCausalHashByPathNoReadPermission path -> jsonUnion "no_read_permission" path

instance FromJSON GetCausalHashByPathResponse where
  parseJSON = Aeson.withObject "GetCausalHashByPathResponse" \obj -> do
    obj .: "type" >>= Aeson.withText "type" \case
      "success" -> GetCausalHashByPathSuccess <$> obj .: "payload"
      "no_read_permission" -> GetCausalHashByPathNoReadPermission <$> obj .: "payload"
      t -> failText $ "Unexpected GetCausalHashByPathResponse type: " <> t

------------------------------------------------------------------------------------------------------------------------
-- Download entities

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
  parseJSON = Aeson.withObject "DownloadEntitiesRequest" \obj -> do
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
  parseJSON = Aeson.withObject "DownloadEntitiesResponse" \obj -> do
    DownloadEntitiesResponse <$> obj .: "entities"

------------------------------------------------------------------------------------------------------------------------
-- Upload entities

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
  parseJSON = Aeson.withObject "UploadEntitiesRequest" \obj -> do
    repoName <- obj .: "repo_name"
    entities <- obj .: "entities"
    pure UploadEntitiesRequest {..}

data UploadEntitiesResponse
  = UploadEntitiesSuccess
  | UploadEntitiesNeedDependencies (NeedDependencies Hash)
  | UploadEntitiesNoWritePermission RepoName
  | UploadEntitiesHashMismatchForEntity HashMismatchForEntity
  deriving stock (Show, Eq, Ord)

data HashMismatchForEntity = HashMismatchForEntity {supplied :: Hash, computed :: Hash}
  deriving stock (Show, Eq, Ord)

instance ToJSON UploadEntitiesResponse where
  toJSON = \case
    UploadEntitiesSuccess -> jsonUnion "success" (Object mempty)
    UploadEntitiesNeedDependencies nd -> jsonUnion "need_dependencies" nd
    UploadEntitiesNoWritePermission repoName -> jsonUnion "no_write_permission" repoName
    UploadEntitiesHashMismatchForEntity mismatch -> jsonUnion "hash_mismatch_for_entity" mismatch

instance FromJSON UploadEntitiesResponse where
  parseJSON = Aeson.withObject "UploadEntitiesResponse" \obj ->
    obj .: "type" >>= Aeson.withText "type" \case
      "success" -> pure UploadEntitiesSuccess
      "need_dependencies" -> UploadEntitiesNeedDependencies <$> obj .: "payload"
      "no_write_permission" -> UploadEntitiesNoWritePermission <$> obj .: "payload"
      "hash_mismatch_for_entity" -> UploadEntitiesHashMismatchForEntity <$> obj .: "payload"
      t -> failText $ "Unexpected UploadEntitiesResponse type: " <> t

instance ToJSON HashMismatchForEntity where
  toJSON (HashMismatchForEntity supplied computed) = object ["supplied" .= supplied, "computed" .= computed]

instance FromJSON HashMismatchForEntity where
  parseJSON = Aeson.withObject "HashMismatchForEntity" \obj -> HashMismatchForEntity <$> obj .: "supplied" <*> obj .: "computed"

------------------------------------------------------------------------------------------------------------------------
-- Fast-forward path

-- | A non-empty list of causal hashes, latest first, that show the lineage from wherever the client wants to
-- fast-forward to back to wherever the (client believes the) server is (including the server head, in a separate
-- field).
--
-- For example, if the client wants to update
--
-- @
-- A -> B -> C
-- @
--
-- to
--
-- @
-- A -> B -> C -> D -> E -> F
-- @
--
-- then it would send hashes
--
-- @
-- expectedHash = C
-- hashes = [D, E, F]
-- @
--
-- Note that if the client wants to begin a history at a new path on the server, it would use the "update path" endpoint
-- instead.
data FastForwardPathRequest = FastForwardPathRequest
  { -- | The causal that the client believes exists at `path`
    expectedHash :: Hash,
    -- | The sequence of causals to fast-forward with, starting from the oldest new causal to the newest new causal
    hashes :: NonEmpty Hash,
    -- | The path to fast-forward
    path :: Path
  }
  deriving stock (Show)

instance ToJSON FastForwardPathRequest where
  toJSON FastForwardPathRequest {expectedHash, hashes, path} =
    object
      [ "expected_hash" .= expectedHash,
        "hashes" .= hashes,
        "path" .= path
      ]

instance FromJSON FastForwardPathRequest where
  parseJSON =
    Aeson.withObject "FastForwardPathRequest" \o -> do
      expectedHash <- o .: "expected_hash"
      hashes <- o .: "hashes"
      path <- o .: "path"
      pure FastForwardPathRequest {expectedHash, hashes, path}

data FastForwardPathResponse
  = FastForwardPathSuccess
  | FastForwardPathMissingDependencies (NeedDependencies Hash)
  | FastForwardPathNoWritePermission Path
  | -- | This wasn't a fast-forward. Here's a JWT to download the causal head, if you want it.
    FastForwardPathNotFastForward HashJWT
  | -- | There was no history at this path; the client should use the "update path" endpoint instead.
    FastForwardPathNoHistory
  | -- | This wasn't a fast-forward. You said the first hash was a parent of the second hash, but I disagree.
    FastForwardPathInvalidParentage InvalidParentage
  deriving stock (Show)

data InvalidParentage = InvalidParentage {parent :: Hash, child :: Hash}
  deriving stock (Show)

instance ToJSON FastForwardPathResponse where
  toJSON = \case
    FastForwardPathSuccess -> jsonUnion "success" (Object mempty)
    FastForwardPathMissingDependencies deps -> jsonUnion "missing_dependencies" deps
    FastForwardPathNoWritePermission path -> jsonUnion "no_write_permission" path
    FastForwardPathNotFastForward hashJwt -> jsonUnion "not_fast_forward" hashJwt
    FastForwardPathNoHistory -> jsonUnion "no_history" (Object mempty)
    FastForwardPathInvalidParentage invalidParentage -> jsonUnion "invalid_parentage" invalidParentage

instance FromJSON FastForwardPathResponse where
  parseJSON =
    Aeson.withObject "FastForwardPathResponse" \o ->
      o .: "type" >>= Aeson.withText "type" \case
        "success" -> pure FastForwardPathSuccess
        "missing_dependencies" -> FastForwardPathMissingDependencies <$> o .: "payload"
        "no_write_permission" -> FastForwardPathNoWritePermission <$> o .: "payload"
        "not_fast_forward" -> FastForwardPathNotFastForward <$> o .: "payload"
        "no_history" -> pure FastForwardPathNoHistory
        "invalid_parentage" -> FastForwardPathInvalidParentage <$> o .: "payload"
        t -> failText $ "Unexpected FastForwardPathResponse type: " <> t

instance ToJSON InvalidParentage where
  toJSON (InvalidParentage parent child) = object ["parent" .= parent, "child" .= child]

instance FromJSON InvalidParentage where
  parseJSON =
    Aeson.withObject "InvalidParentage" \o -> InvalidParentage <$> o .: "parent" <*> o .: "child"

------------------------------------------------------------------------------------------------------------------------
-- Update path

data UpdatePathRequest = UpdatePathRequest
  { path :: Path,
    expectedHash :: Maybe Hash, -- Nothing requires empty history at destination
    newHash :: Hash
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
  parseJSON = Aeson.withObject "UpdatePathRequest" \obj -> do
    path <- obj .: "path"
    expectedHash <- obj .: "expected_hash"
    newHash <- obj .: "new_hash"
    pure UpdatePathRequest {..}

data UpdatePathResponse
  = UpdatePathSuccess
  | UpdatePathHashMismatch HashMismatch
  | UpdatePathMissingDependencies (NeedDependencies Hash)
  | UpdatePathNoWritePermission Path
  deriving stock (Show, Eq, Ord)

instance ToJSON UpdatePathResponse where
  toJSON = \case
    UpdatePathSuccess -> jsonUnion "success" (Object mempty)
    UpdatePathHashMismatch hm -> jsonUnion "hash_mismatch" hm
    UpdatePathMissingDependencies md -> jsonUnion "missing_dependencies" md
    UpdatePathNoWritePermission path -> jsonUnion "no_write_permission" path

instance FromJSON UpdatePathResponse where
  parseJSON v =
    v & Aeson.withObject "UpdatePathResponse" \obj ->
      obj .: "type" >>= Aeson.withText "type" \case
        "success" -> pure UpdatePathSuccess
        "hash_mismatch" -> UpdatePathHashMismatch <$> obj .: "payload"
        "missing_dependencies" -> UpdatePathMissingDependencies <$> obj .: "payload"
        "no_write_permission" -> UpdatePathNoWritePermission <$> obj .: "payload"
        t -> failText $ "Unexpected UpdatePathResponse type: " <> t

data HashMismatch = HashMismatch
  { path :: Path,
    expectedHash :: Maybe Hash,
    actualHash :: Maybe Hash
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON HashMismatch where
  toJSON (HashMismatch path expectedHash actualHash) =
    object
      [ "path" .= path,
        "expected_hash" .= expectedHash,
        "actual_hash" .= actualHash
      ]

instance FromJSON HashMismatch where
  parseJSON = Aeson.withObject "HashMismatch" \obj -> do
    path <- obj .: "path"
    expectedHash <- obj .: "expected_hash"
    actualHash <- obj .: "actual_hash"
    pure HashMismatch {..}

------------------------------------------------------------------------------------------------------------------------
-- Common/shared error types

data NeedDependencies hash = NeedDependencies
  { missingDependencies :: NESet hash
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON hash => ToJSON (NeedDependencies hash) where
  toJSON (NeedDependencies missingDependencies) =
    object ["missing_dependencies" .= missingDependencies]

instance (FromJSON hash, Ord hash) => FromJSON (NeedDependencies hash) where
  parseJSON = Aeson.withObject "NeedDependencies" \obj -> do
    missingDependencies <- obj .: "missing_dependencies"
    pure NeedDependencies {..}

------------------------------------------------------------------------------------------------------------------------
-- Misc. helpers

failText :: MonadFail m => Text -> m a
failText = fail . Text.unpack

jsonUnion :: ToJSON a => Text -> a -> Value
jsonUnion typeName val =
  Aeson.object
    [ "type" .= String typeName,
      "payload" .= val
    ]
