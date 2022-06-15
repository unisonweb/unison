module U.Codebase.Sqlite.V2.Decl
  ( saveDeclComponent,
  )
where

import qualified U.Codebase.Decl as V2
import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Sqlite.Operations as U.Sqlite
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Sqlite.V2.HashHandle
import U.Util.Hash (Hash)
import Unison.Prelude
import Unison.Sqlite

saveDeclComponent ::
  -- | The serialized decl component if we already have it e.g. via sync
  Maybe ByteString ->
  -- | decl component hash
  Hash ->
  -- | decl component
  [V2.Decl Symbol] ->
  Transaction ObjectId
saveDeclComponent =
  U.Sqlite.saveDeclComponent v2HashHandle
