module U.Codebase.Sqlite.V2.SyncEntity where

import qualified U.Codebase.Sqlite.DbId as Db
import U.Codebase.Sqlite.Entity (SyncEntity)
import qualified U.Codebase.Sqlite.Operations as Ops
import U.Codebase.Sqlite.V2.HashHandle
import U.Util.Hash32 (Hash32)
import Unison.Sqlite

saveSyncEntity :: Hash32 -> SyncEntity -> Transaction (Either Db.CausalHashId Db.ObjectId)
saveSyncEntity = Ops.saveSyncEntity v2HashHandle
