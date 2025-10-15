-- Table for storing personal key thumbprints,
-- which we may later associate with users.
CREATE TABLE IF NOT EXISTS key_thumbprints (
  id INTEGER PRIMARY KEY,
  thumbprint TEXT UNIQUE NOT NULL
);

ALTER TABLE history_comments
  -- The hash used for comment identity.
  -- It's the hash of (causal_hash <> author <> created_at)
  ADD COLUMN comment_hash_id INTEGER UNIQUE NOT NULL REFERENCES hash(id),
  ADD COLUMN author_thumbprint_id INTEGER NOT NULL REFERENCES key_thumbprints(id);

ALTER TABLE history_comment_revisions
  -- The hash used for this revision's identity.
  -- It's the hash of (comment_hash <> subject <> contents <> hidden <> created_at)
  ADD COLUMN revision_hash_id INTEGER UNIQUE NOT NULL REFERENCES hash(id);
