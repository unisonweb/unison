CREATE TABLE namespace_statistics (
  namespace_hash_id INTEGER PRIMARY KEY NOT NULL REFERENCES causal(value_hash_id),
  -- Number of (terms|types|patches) in entire subtree
  num_contained_terms INTEGER NOT NULL,
  num_contained_types INTEGER NOT NULL,
  num_contained_patches INTEGER NOT NULL
);

