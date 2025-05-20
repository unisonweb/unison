-- We put you on a new "update branch" whenever an `update` fails (from a non-update branch).

CREATE TABLE update_branch (
  project_id uuid NOT NULL,
  branch_id uuid NOT NULL,
  parent_causal_hash_id integer NOT NULL
    REFERENCES hash (id) ON DELETE CASCADE,
  PRIMARY KEY (project_id, branch_id),
  FOREIGN KEY (project_id, branch_id)
    REFERENCES project_branch (project_id, branch_id)
    ON DELETE CASCADE
) WITHOUT ROWID;
