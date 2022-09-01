CREATE TABLE IF NOT EXISTS namespace_statistics (
  -- Ideally this would have a stronger foreign key constraint which requires it to be a namespace hash id, 
  -- but we don't have a way to properly express that until we migrate namespaces to be relational.
  namespace_hash_id INTEGER PRIMARY KEY NOT NULL REFERENCES hash(id),
  -- Number of (terms|types|patches) in entire subtree
  num_contained_terms INTEGER NOT NULL,
  num_contained_types INTEGER NOT NULL,
  num_contained_patches INTEGER NOT NULL
)

-- Semicolon intentionally omitted, for the same reason
-- semicolons in comments will blow up codebase initialization.
-- (oops, almost used a semicolon at the end of that last phrase!)
-- Sqlite doesn't let us submit multiple statements in the same
-- command, so we are using Haskell code to segment the statements
-- by splitting on semicolons.  It doesn't know to ignore comments,
-- though I guess that wouldn't be hard to implement.  Should have
-- done it from the start.
