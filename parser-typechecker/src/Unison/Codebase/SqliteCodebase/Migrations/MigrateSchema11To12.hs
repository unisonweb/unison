{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema11To12 (migrateSchema11To12) where

import U.Codebase.HashTags (BranchHash (..))
import U.Codebase.Projects (inferDependencyMounts)
import U.Codebase.Reference qualified as C.Reference
import U.Codebase.Sqlite.NameLookups (PathSegments (PathSegments))
import U.Codebase.Sqlite.Operations qualified as Ops
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Operations qualified as CodebaseOps
import Unison.ConstructorType qualified as CT
import Unison.Hash (Hash (..))
import Unison.Hash32 qualified as Hash32
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Prelude hiding (log)

migrateSchema11To12 ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  Sqlite.Transaction ()
migrateSchema11To12 getDeclType = do
  Queries.expectSchemaVersion 11
  Queries.addNameLookupMountTables
  backfillNameLookupMounts getDeclType
  removeLibFromNameLookups
  Queries.setSchemaVersion 12

-- | Add the correct dependency mounts to existing indexes
backfillNameLookupMounts ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  Sqlite.Transaction ()
backfillNameLookupMounts getDeclType = do
  branchHashesWithNameLookups <- fmap (coerce . Hash32.toHash) <$> Sqlite.queryListCol_ "SELECT hash.base32 FROM name_lookups nl JOIN hash ON nl.root_branch_hash_id = hash.id"
  for_ branchHashesWithNameLookups \bh -> do
    branch <- Ops.expectBranchByBranchHash bh
    mounts <- inferDependencyMounts branch
    for_ mounts \(_path, mountBH) -> do
      CodebaseOps.ensureNameLookupForBranchHash getDeclType Nothing mountBH
    Ops.associateNameLookupMounts bh (mounts & map (first (coerce . Path.toList)))

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
