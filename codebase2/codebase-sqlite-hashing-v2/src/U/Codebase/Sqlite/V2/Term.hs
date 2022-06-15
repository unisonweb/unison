module U.Codebase.Sqlite.V2.Term
  ( saveTermComponent,
  )
where

import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Sqlite.Operations as U.Sqlite
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Sqlite.V2.HashHandle
import qualified U.Codebase.Term as V2
import U.Util.Hash (Hash)
import Unison.Prelude
import Unison.Sqlite

saveTermComponent ::
  -- | The serialized term component if we already have it e.g. via sync
  Maybe ByteString ->
  -- | term component hash
  Hash ->
  -- | term component
  [(V2.Term Symbol, V2.Type Symbol)] ->
  Transaction ObjectId
saveTermComponent =
  U.Sqlite.saveTermComponent v2HashHandle
