module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4) where

import qualified U.Codebase.Sqlite.Queries as Q
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.SqliteCodebase.Operations as Ops
import qualified Unison.Sqlite as Sqlite

migrateSchema3To4 :: Sqlite.Transaction ()
migrateSchema3To4 = do
  Q.expectSchemaVersion 3
  root <- Ops.uncachedGetRootBranch
  let rootNames = Branch.toNames (Branch.head root)
  Ops.saveRootNamesIndex rootNames
  Q.setSchemaVersion 4
