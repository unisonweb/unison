-- Assert hash columns are non-nullable after we've filled in all the values.
-- SQLite annoying does not support doing this in place, so we need to make a new table.

CREATE TABLE history_comments_new (
  id INTEGER PRIMARY KEY,
  causal_hash_id INTEGER NOT NULL REFERENCES hash(id),
  author TEXT NOT NULL,

  -- Remember that SQLITE doesn't have any actual 'time' type,
  -- This column contains the number of milliseconds since epoch
  -- as an integer.
  created_at_ms INTEGER NOT NULL,

  comment_hash_id INTEGER UNIQUE NOT NULL REFERENCES hash(id),
  author_thumbprint_id INTEGER NOT NULL REFERENCES key_thumbprints(id)
);

-- Copy data from old tables to new tables.
-- We convert the created_at to created_at_ms by multiplying by 1000 and casting to INTEGER.
INSERT INTO history_comments_new (id, causal_hash_id, author, created_at_ms, comment_hash_id, author_thumbprint_id)
  SELECT id, causal_hash_id, author, CAST((created_at * 1000) AS INTEGER), comment_hash_id, author_thumbprint_id
  FROM history_comments;

-- Now do the revisions
CREATE TABLE history_comment_revisions_new (
  id INTEGER PRIMARY KEY,
  comment_id INTEGER NOT NULL REFERENCES history_comments_new(id),
  subject TEXT NOT NULL,
  contents TEXT NOT NULL,

  -- Remember that SQLITE doesn't have any actual 'time' type,
  -- This column contains the number of milliseconds since epoch
  -- as an integer.
  created_at_ms INTEGER NOT NULL,

  -- - In a distributed system you really can’t ever truly delete comments,
  -- but you can ask to hide them.
  hidden BOOL NOT NULL DEFAULT FALSE,

  revision_hash_id INTEGER UNIQUE NOT NULL REFERENCES hash(id)

  --  The signature of the author on the revision hash.
  author_signature BLOB NOT NULL
);

-- We convert the created_at to created_at_ms by multiplying by 1000 and casting to INTEGER.
INSERT INTO history_comment_revisions_new (id, comment_id, subject, contents, created_at_ms, hidden, author_signature, revision_hash_id)
  SELECT id, comment_id, subject, contents, CAST((created_at * 1000) AS INTEGER), hidden, author_signature, revision_hash_id
  FROM history_comment_revisions;

-- Drop old tables
DROP TABLE history_comment_revisions;
DROP TABLE history_comments;

-- Rename new tables to original table names
ALTER TABLE history_comments_new RENAME TO history_comments;
ALTER TABLE history_comment_revisions_new RENAME TO history_comment_revisions;

CREATE INDEX history_comments_by_causal_hash_id ON history_comments(causal_hash_id, created_at_ms DESC);
CREATE INDEX history_comment_revisions_by_comment_id_and_created_at_ms ON history_comment_revisions(comment_id, created_at_ms DESC);
