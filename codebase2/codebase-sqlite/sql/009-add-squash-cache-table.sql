-- A table for tracking the results of squashes we've performed.
-- This is used to avoid re-squashing the same branch multiple times.
CREATE TABLE "squash_results" (
  -- The branch hash of the namespace to be squashed.
  -- There should only ever be one result for each unsquashed value hash.
  branch_hash_id INTEGER PRIMARY KEY NOT NULL REFERENCES hash(id),

  -- The causal hash id which is the result of squashing the 'branch_hash_id's causal.'
  squashed_causal_hash_id INTEGER NOT NULL REFERENCES causal(self_hash_id)
) WITHOUT ROWID;
