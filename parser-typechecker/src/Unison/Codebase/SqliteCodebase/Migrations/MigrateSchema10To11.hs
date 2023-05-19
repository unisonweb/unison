{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema10To11 (migrateSchema10To11) where

import U.Codebase.HashTags (BranchHash (..))
import U.Codebase.Projects (inferDependencyMounts)
import qualified U.Codebase.Reference as C.Reference
import U.Codebase.Sqlite.NameLookups (PathSegments (PathSegments))
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Operations as CodebaseOps
import qualified Unison.ConstructorType as CT
import Unison.Hash (Hash (..))
import qualified Unison.Hash32 as Hash32
import Unison.NameSegment (NameSegment (..))
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
