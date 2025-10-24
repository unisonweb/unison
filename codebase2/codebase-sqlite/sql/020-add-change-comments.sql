-- A simple table for storing user preferences as key/value pairs.
CREATE TABLE config (
  key TEXT NOT NULL PRIMARY KEY,
  value TEXT NOT NULL
);

-- Add tables for storing change comments
-- These tables deliberately contain less information than we'll probably need, with the
-- plan that we'll migrate them and add new features on the way.

CREATE TABLE change_comments (
  id INTEGER PRIMARY KEY,
  causal_hash_id INTEGER REFERENCES hash(id) NOT NULL,
  author TEXT NOT NULL,

  -- Remember that SQLITE doesn't have any actual 'time' type,
  -- This column contains float values constructed
  -- using strftime('%s', 'now', 'subsec')
  created_at TEXT NOT NULL
);

CREATE INDEX change_comments_by_causal_hash_id ON change_comments(causal_hash_id, created_at DESC);

CREATE TABLE change_comment_revisions (
  comment_id INTEGER REFERENCES change_comments(id),
  contents TEXT NOT NULL,

  -- Remember that SQLITE doesn't have any actual 'time' type,
  -- This column contains float values constructed
  -- using strftime('%s', 'now', 'subsec')
  created_at TEXT NOT NULL,

  -- - In a distributed system you really can’t ever truly delete comments,
  -- but you can ask to hide them.
  hidden BOOL NOT NULL DEFAULT FALSE
);

CREATE INDEX change_comment_revisions_by_comment_id_and_created_at ON change_comment_revisions(comment_id, created_at DESC);
