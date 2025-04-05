-- The most recent namespace that a user cd'd to.
-- This table should never have more than one row.
CREATE TABLE current_project_path (
  project_id INTEGER NOT NULL,
  branch_id INTEGER NOT NULL,
  -- A json array like ["foo", "bar"]; the root namespace is represented by the empty array
  path TEXT PRIMARY KEY NOT NULL,

  foreign key (project_id, branch_id)
    references project_branch (project_id, branch_id)
    -- Prevent deleting the project you're currently in.
    on delete no action
) WITHOUT ROWID;

DROP TABLE most_recent_namespace;
