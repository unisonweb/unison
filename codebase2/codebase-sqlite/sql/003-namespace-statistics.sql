-- Note: Namespace stats are currently computed on-demand,
-- which means you should NOT perform queries or joins on this table
-- which expect a row to exist for every namespace in the codebase.
CREATE TABLE IF NOT EXISTS namespace_statistics (
  -- Ideally this would have a stronger foreign key constraint which requires it to be a namespace hash id,
  -- but we don't have a way to properly express that until we migrate namespaces to be relational.
  namespace_hash_id INTEGER PRIMARY KEY NOT NULL REFERENCES hash(id),
  -- Number of (terms|types|patches) in entire subtree
  num_contained_terms INTEGER NOT NULL,
  num_contained_types INTEGER NOT NULL,
  num_contained_patches INTEGER NOT NULL
);
