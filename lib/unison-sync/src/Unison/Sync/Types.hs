module Unison.Sync.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import Data.Text (Text)

newtype RepoName = RepoName Text

newtype HashJWT = HashJWT Text

newtype Base32 = Base32 Text

data Hash = Hash
  { base32 :: Base32,
    entityType :: EntityType
  }

data RepoPath = RepoPath
  { repoName :: RepoName,
    pathSegments :: [Text]
  }

newtype GetCausalHashByPathRequest = GetCausalHashByPathRequest
  { repoPath :: RepoPath
  }

newtype GetCausalHashByPathResponse = GetCausalHashByPathResponse
  { causalHash :: HashJWT
  }

data DownloadEntitiesRequest = DownloadEntitiesRequest
  { repoName :: RepoName,
    hashes :: NESet HashJWT
  }

data DownloadEntitiesResponse = DownloadEntities
  { entities :: Map Hash (Entity HashJWT Hash Text)
  }

data PushRequest = PushRequest
  { path :: RepoPath,
    expectedHash :: Maybe Hash, -- Nothing requires empty history at destination
    newHash :: Hash
  }

data NeedDependencies hash = NeedDependencies
  { missingDependencies :: NESet hash
  }

data OutOfDateHash = OutOfDateHash
  { path :: RepoPath,
    expectedHash :: Maybe Hash,
    actualHash :: Maybe Hash
  }

data UploadEntitiesRequest = UploadEntitiesRequest
  { repo :: RepoName,
    entities :: Map Hash (Entity Hash Hash Text)
  }

data Entity hash optionalHash text
  = TC (TermComponent hash text)
  | DC (DeclComponent hash text)
  | P (Patch hash optionalHash text)
  | N (Namespace hash text)
  | C (Causal hash)

data TermComponent hash text = TermComponent [(LocalIds hash text, ByteString)]

data DeclComponent hash text = DeclComponent [(LocalIds hash text, ByteString)]

data LocalIds hash text = LocalIds
  { hashes :: [hash],
    texts :: [text]
  }

data Patch hash optionalHash text = Patch
  { textLookup :: [text],
    hashLookup :: [hash],
    optionalHashLookup :: [optionalHash],
    body :: ByteString
  }

data Namespace hash text = Namespace
  { textLookup :: [text],
    defnLookup :: [hash],
    patchLookup :: [hash],
    childLookup :: [hash],
    body :: ByteString
  }

-- Client _may_ choose not to download the namespace entity in the future, but
-- we still send them the hash/hashjwt.
data Causal hash = Causal
  { namespaceHash :: hash,
    parents :: Set hash
  }

data EntityType
  = TermComponentType
  | DeclComponentType
  | PatchType
  | NamespaceType
  | CausalType
