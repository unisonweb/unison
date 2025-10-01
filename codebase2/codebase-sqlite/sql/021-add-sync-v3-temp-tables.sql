-- Add a new table for storing entities which are currently being synced

CREATE TABLE syncv3_temp_entity (
  root_causal INTEGER NOT NULL REFERENCES hash (id) ON DELETE CASCADE,
  entity_hash TEXT NOT NULL,
  entity_kind TEXT NOT NULL,
  entity_data BLOB NOT NULL,
  entity_depth INTEGER NOT NULL,
  PRIMARY KEY (root_causal, entity_hash)
) WITHOUT ROWID;

-- We _could_ add an index on (root_causal, entity_depth), since that's how we'll
-- be querying this table, but we only run the query exactly once per sync, so it's
-- probably faster to sort on query rather than maintaining the index.
