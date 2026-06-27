{-# LANGUAGE DerivingVia #-}

module U.Codebase.Sqlite.OpaqueDeclaration.Format where

import U.Codebase.OpaqueDeclaration qualified as OpaqueDeclaration
import U.Codebase.Reference (Reference')
import U.Codebase.Sqlite.DbId (ObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalIds', LocalTextId)
import U.Codebase.Sqlite.Symbol (Symbol)
import Unison.Hash32 (Hash32)
import Unison.Prelude

-- | References inside an opaque RHS. Opaque types cannot mention themselves in
-- their RHS, so unlike 'U.Codebase.Sqlite.Decl.Format.TypeRef' there is no
-- @Maybe@ self-reference variant — mirroring the alias case.
type TypeRef = Reference' LocalTextId LocalDefnId

-- | A single opaque-declaration entry, with its references localized to the
-- lookup vectors held in its accompanying 'LocalIds''.
type OpaqueDeclaration = OpaqueDeclaration.OpaqueDeclarationR TypeRef Symbol

-- | The on-disk format for a single opaque-type declaration. Like aliases and
-- unlike decls, opaques are single-entry rather than components (they can't
-- be recursive — the LHS may not appear in the RHS, and the body's hashes
-- are stored separately as ordinary terms).
--
-- The @text@ and @defn@ parameters describe the lookup vectors in
-- 'LocalIds'', not the body's reference types (which are always local).
data OpaqueDeclarationFormat' text defn
  = OpaqueDeclaration (LocalIds' text defn) OpaqueDeclaration
  deriving (Show)

type OpaqueDeclarationFormat = OpaqueDeclarationFormat' TextId ObjectId

-- | An 'OpaqueDeclarationFormat' that uses hash references instead of database
-- ids.
type HashOpaqueDeclarationFormat = OpaqueDeclarationFormat' Text Hash32

-- | The sync-side format: the body has been serialized to bytes by the source
-- codebase and is opaque to the sync protocol.
data SyncOpaqueDeclarationFormat' t d
  = SyncOpaqueDeclaration (LocalIds' t d) ByteString
  deriving stock (Eq, Show)

type SyncOpaqueDeclarationFormat = SyncOpaqueDeclarationFormat' TextId ObjectId
