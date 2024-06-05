-- The most recent namespace that a user cd'd to.
-- This table should never have more than one row.
CREATE TABLE current_project_path (
  project_id INTEGER NOT NULL REFERENCES project (id),
  branch_id INTEGER NOT NULL REFERENCES project_branch (id),
  -- A json array like ["foo", "bar"]; the root namespace is represented by the empty array
  path TEXT PRIMARY KEY NOT NULL
) WITHOUT ROWID;

DROP TABLE "most_recent_namespace";
