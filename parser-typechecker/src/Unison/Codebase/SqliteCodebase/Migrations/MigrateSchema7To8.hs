{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema7To8 (migrateSchema7To8) where

import Data.String.Here.Uninterpolated (here)
import qualified U.Codebase.Sqlite.Queries as Q
import qualified Unison.Sqlite as Sqlite

-- | Adds a table for tracking namespace statistics
-- Adds stats for all existing namespaces, even though missing stats are computed on-demand if missing.
migrateSchema7To8 :: Sqlite.Transaction ()
migrateSchema7To8 = do
  Q.expectSchemaVersion 7
  createScopedNameLookupTables
  Q.setSchemaVersion 8

-- | Create the scoped name lookup tables.
createScopedNameLookupTables :: Sqlite.Transaction ()
createScopedNameLookupTables = do
  -- This table allows us to look up which causal hashes have a name lookup.
  Sqlite.execute_
    [here|
      CREATE TABLE name_lookups (
        root_branch_hash_id INTEGER PRIMARY KEY REFERENCES hash(id) ON DELETE CASCADE
      )
    |]

  Sqlite.execute_
    [here|
      CREATE TABLE scoped_term_name_lookup (
        root_branch_hash_id INTEGER NOT NULL REFERENCES hash(id) ON DELETE CASCADE,

        -- The name of the term in reversed form, with a trailing '.':
        -- E.g. map.List.base.
        --
        -- The trailing '.' is helpful when performing suffix queries where we may not know
        -- whether the suffix is complete or not, e.g. we could suffix search using any of the
        -- following globs and it would still find 'map.List.base.':
        --  map.List.base.*
        --  map.List.*
        --  map.*
        reversed_name TEXT NOT NULL,

        -- The last name segment of the name. This is used when looking up names for
        -- suffixification when building PPEs.
        -- E.g. for the name 'base.List.map' this would be 'map'
        last_name_segment TEXT NOT NULL,

        -- The namespace containing this definition, not reversed, with a trailing '.'
        -- The trailing '.' simplifies GLOB queries, so that 'base.*' matches both things in
        -- 'base' and 'base.List', but not 'base1', which allows us to avoid an OR in our where
        -- clauses which in turn helps the sqlite query planner use indexes more effectively.
        --
        -- example value: 'base.List.'
        namespace TEXT NOT NULL,
        referent_builtin TEXT NULL,
        referent_component_hash TEXT NULL,
        referent_component_index INTEGER NULL,
        referent_constructor_index INTEGER NULL,
        referent_constructor_type INTEGER NULL,
        PRIMARY KEY (root_branch_hash_id, reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index)
      )
    |]

  -- This index allows finding all names we need to consider within a given namespace for
  -- suffixification of a name.
  -- It may seem strange to use last_name_segment rather than a suffix search over reversed_name name here;
  -- but SQLite will only optimize for a single prefix-glob at once, so we can't glob search
  -- over both namespace and reversed_name, but we can EXACT match on last_name_segment and
  -- then glob search on the namespace prefix, and have SQLite do the final glob search on
  -- reversed_name over rows with a matching last segment without using an index and should be plenty fast.
  Sqlite.execute_
    [here|
      CREATE INDEX scoped_term_names_by_namespace_and_last_name_segment ON scoped_term_name_lookup(root_branch_hash_id, last_name_segment, namespace)
    |]
  -- This index allows us to find all names with a given ref within a specific namespace
  Sqlite.execute_
    [here|
      CREATE INDEX scoped_term_name_by_referent_lookup ON scoped_term_name_lookup(root_branch_hash_id, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, namespace)
    |]

  -- Allows fetching ALL names within a specific namespace prefix. We currently use this to
  -- pretty-print on share, but will be replaced with a more precise set of queries soon.
  Sqlite.execute_
    [here|
      CREATE INDEX scoped_term_names_by_namespace ON scoped_term_name_lookup(root_branch_hash_id, namespace)
    |]
  Sqlite.execute_
    [here|
      CREATE TABLE scoped_type_name_lookup (
        root_branch_hash_id INTEGER NOT NULL REFERENCES hash(id),
        -- The name of the term: E.g. List.base
        reversed_name TEXT NOT NULL,
        -- The last name segment of the name. This is used when looking up names for
        -- suffixification when building PPEs.
        -- E.g. for the name 'base.List.map' this would be 'map'
        last_name_segment TEXT NOT NULL,
        -- The namespace containing this definition, not reversed, with a trailing '.'
        -- The trailing '.' simplifies GLOB queries, so that 'base.*' matches both things in
        -- 'base' and 'base.List', but not 'base1', which allows us to avoid an OR in our where
        -- clauses which in turn helps the sqlite query planner use indexes more effectively.
        --
        -- example value: 'base.List.'
        namespace TEXT NOT NULL,
        reference_builtin TEXT NULL,
        reference_component_hash INTEGER NULL,
        reference_component_index INTEGER NULL,
        PRIMARY KEY (reversed_name, reference_builtin, reference_component_hash, reference_component_index)
      );
    |]

  -- This index allows finding all names we need to consider within a given namespace for
  -- suffixification of a name.
  -- It may seem strange to use last_name_segment rather than a suffix search over reversed_name name here;
  -- but SQLite will only optimize for a single prefix-glob at once, so we can't glob search
  -- over both namespace and reversed_name, but we can EXACT match on last_name_segment and
  -- then glob search on the namespace prefix, and have SQLite do the final glob search on
  -- reversed_name over rows with a matching last segment without using an index and should be plenty fast.
  Sqlite.execute_
    [here|
      CREATE INDEX scoped_type_names_by_namespace_and_last_name_segment ON scoped_type_name_lookup(root_branch_hash_id, last_name_segment, namespace)
    |]

  -- This index allows us to find all names with a given ref within a specific namespace.
  Sqlite.execute_
    [here|
      CREATE INDEX scoped_type_name_by_reference_lookup ON scoped_type_name_lookup(root_branch_hash_id, reference_builtin, reference_component_hash, reference_component_index, namespace)
    |]

  -- Allows fetching ALL names within a specific namespace prefix. We currently use this to
  -- pretty-print on share, but will be replaced with a more precise set of queries soon.
  Sqlite.execute_
    [here|
      CREATE INDEX scoped_type_names_by_namespace ON scoped_type_name_lookup(root_branch_hash_id, namespace)
    |]
