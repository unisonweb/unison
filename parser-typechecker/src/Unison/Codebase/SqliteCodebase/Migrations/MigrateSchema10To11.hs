{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema10To11 (migrateSchema10To11) where

import U.Codebase.Projects (inferDependencyMounts)
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.Codebase.SqliteCodebase.Operations as CodebaseOps
import qualified Unison.ConstructorType as CT
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import Prelude hiding (log)

migrateSchema10To11 ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  Sqlite.Transaction ()
migrateSchema10To11 getDeclType = do
  Queries.expectSchemaVersion 10
  Queries.addNameLookupMountTables
  backfillNameLookupMounts getDeclType
  removeLibFromNameLookups
  Queries.setSchemaVersion 11

-- | Add the correct dependency mounts to existing indexes
backfillNameLookupMounts ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  Sqlite.Transaction ()
backfillNameLookupMounts getDeclType = do
  rootsWithNameLookups <- Sqlite.queryListCol_ "SELECT root_branch_hash_id FROM name_lookups"
  for_ rootsWithNameLookups \bhId -> do
    branch <- Ops.expectBranchByBranchHashId bhId
    mounts <- inferDependencyMounts branch
    for_ mounts \(_path, mountBH) -> do
      CodebaseOps.ensureNameLookupForBranchHash getDeclType Nothing mountBH
    Ops.associateNameLookupMounts bhId (mounts & map first)

-- | As part of adding name lookup mounts for dependencies we no longer want dependencies to
-- be included in the name lookup, they just bloat the index.
removeLibFromNameLookups :: Sqlite.Transaction ()
removeLibFromNameLookups = do
  Sqlite.execute2
    [Sqlite.sql2|
    DELETE FROM scoped_term_name_lookup
      WHERE namespace GLOB 'lib.*' OR namespace GLOB '*.lib.*'
    |]
  Sqlite.execute2
    [Sqlite.sql2|
    DELETE FROM scoped_type_name_lookup
      WHERE namespace GLOB 'lib.*' OR namespace GLOB '*.lib.*'
    |]
