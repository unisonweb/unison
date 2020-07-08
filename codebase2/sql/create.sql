-- actually stores the 512-byte hashes
CREATE TABLE hash (
  id INTEGER PRIMARY KEY,
  -- this would be the full hash, represented as base32 instead of bytes,
  -- to optimize for looking them up by prefix.
  base32 TEXT UNIQUE NOT NULL
);
CREATE INDEX hash_base32 ON hash(base32);

-- just came up with this, a layer of indirection to allow multiple hash_ids to
-- reference the same object.
-- so: SELECT object.id, bytes FROM object
--      INNER JOIN hash_object ON object_id = object.id
--      INNER JOIN hash ON hash_id = hash.id
--     WHERE base32 LIKE 'a1b2c3%'
CREATE TABLE hash_object (
  id INTEGER PRIMARY KEY,
  -- hashes are UNIQUE; many hashes correspond to one object
  -- (causal nodes are not considered objects atm)
  hash_id INTEGER UNIQUE NOT NULL REFERENCES hash(id),
  object_id INTEGER NOT NULL REFERENCES object(id),
  hash_version INTEGER NOT NULL
);
CREATE INDEX hash_object_hash_id ON hash_object(hash_id);

-- How should objects be linked to hashes?  (and old hashes)
-- And which id should be linked into blobs?
--   a) object.id
--   b) hash.id (no good, if many hashes equally to one object)
CREATE TABLE object (
  id INTEGER PRIMARY KEY,
  primary_hash_id INTEGER NOT NULL REFERENCES hash(id),
  type_id INTEGER NOT NULL,
  bytes blob NOT NULL
);
CREATE INDEX object_hash_id ON object(primary_hash_id);
CREATE INDEX object_type_id ON object(type_id);

CREATE TABLE causal (
  self_hash_id INTEGER PRIMARY KEY REFERENCES hash(id),
  value_hash_id INTEGER NOT NULL REFERENCES hash(id)
);

CREATE TABLE causal_parent (
  id INTEGER PRIMARY KEY,
  causal_id INTEGER NOT NULL REFERENCES causal(self_hash_id),
  parent_id INTEGER NOT NULL REFERENCES causal(self_hash_id),
  UNIQUE(causal_id, parent_id)
);
CREATE INDEX causal_parent_causal_id ON causal_parent(causal_id);
CREATE INDEX causal_parent_parent_id ON causal_parent(parent_id);
