{-# LANGUAGE DerivingVia #-}

module U.Codebase.Sqlite.TypeAlias.Format where

import U.Codebase.Reference (Reference')
import U.Codebase.Sqlite.DbId (ObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalIds', LocalTextId)
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.TypeAlias qualified as TypeAlias
import Unison.Hash32 (Hash32)
import Unison.Prelude

-- | References inside an alias body. Aliases cannot be recursive, so unlike
-- 'U.Codebase.Sqlite.Decl.Format.TypeRef' there is no @Maybe@ self-reference
-- variant.
type TypeRef = Reference' LocalTextId LocalDefnId

-- | A single type alias entry, with its references localized to the lookup
-- vectors held in its accompanying 'LocalIds''. Refs in the body always use
-- 'LocalTextId'/'LocalDefnId' regardless of what the lookup vectors contain
-- (matching how decls are stored).
type TypeAlias = TypeAlias.TypeAliasR TypeRef Symbol

-- | The on-disk format for a single type alias. Unlike decls, aliases are
-- always single-entry rather than components, since they can't be
-- recursive.
--
-- The @text@ and @defn@ parameters describe the lookup vectors in
-- 'LocalIds'', not the body's reference types (which are always local).
data TypeAliasFormat' text defn
  = TypeAlias (LocalIds' text defn) TypeAlias
  deriving (Show)

type TypeAliasFormat = TypeAliasFormat' TextId ObjectId

-- | A 'TypeAliasFormat' that uses hash references instead of database ids.
type HashTypeAliasFormat = TypeAliasFormat' Text Hash32

-- | The sync-side format: the body has been serialized to bytes by the source
-- codebase and is opaque to the sync protocol.
data SyncTypeAliasFormat' t d
  = SyncTypeAlias (LocalIds' t d) ByteString
  deriving stock (Eq, Show)

type SyncTypeAliasFormat = SyncTypeAliasFormat' TextId ObjectId
