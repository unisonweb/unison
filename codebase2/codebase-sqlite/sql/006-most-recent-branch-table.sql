CREATE TABLE most_recent_branch (
  project_id uuid PRIMARY KEY REFERENCES project (id) ON DELETE CASCADE,
  branch_id uuid NOT NULL,
  FOREIGN KEY (project_id, branch_id) REFERENCES project_branch (project_id, branch_id) ON DELETE CASCADE) WITHOUT rowid;
