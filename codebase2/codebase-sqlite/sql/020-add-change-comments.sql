-- Add tables for storing change comments
-- These tables deliberately contain less information than we'll probably need, with the
-- plan that we'll migrate them and add new features on the way.

CREATE TABLE change_comments (
  id INTEGER PRIMARY KEY,
  causal_hash_id INTEGER REFERENCES hash(id) NOT NULL,
  -- Comments are scoped to a specific project.
  project_id UUID NOT NULL,
  -- Remember that SQLITE doesn't have any actual 'time' type,
  -- This column contains TEXT values formatted as ISO8601 strings
  -- ("YYYY-MM-DD HH:MM:SS.SSS")
  created_at TEXT NOT NULL,
);

CREATE INDEX change_comments_by_project_and_causal_hash_id ON change_comments(project_id, causal_hash_id);

CREATE TABLE change_comment_revisions (
  comment_id INTEGER REFERENCES change_comments(id),
  contents TEXT NOT NULL,

  -- Remember that SQLITE doesn't have any actual 'time' type,
  -- This column contains TEXT values formatted as ISO8601 strings
  -- ("YYYY-MM-DD HH:MM:SS.SSS")
  created_at TEXT NOT NULL,

  -- - In a distributed system you really can’t ever truly delete comments,
  -- but you can ask to hide them.
  hidden BOOL NOT NULL DEFAULT FALSE
);

CREATE INDEX change_comment_revisions_by_comment_id_and_created_at ON change_comment_revisions(comment_id, created_at DESC);
