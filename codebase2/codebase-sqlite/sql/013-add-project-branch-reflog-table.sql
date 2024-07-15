-- A reflog which is tied to the project/branch
CREATE TABLE project_branch_reflog (
    project_id INTEGER NOT NULL,
    project_branch_id INTEGER NOT NULL,
    -- Reminder that SQLITE doesn't have any actual 'time' type,
    -- This column contains TEXT values formatted as ISO8601 strings
    -- ("YYYY-MM-DD HH:MM:SS.SSS")
    time TEXT NOT NULL,
    -- from_root_causal_id will be null if the branch was just created
    from_root_causal_id INTEGER NULL REFERENCES causal(self_hash_id),
    to_root_causal_id INTEGER NOT NULL REFERENCES causal(self_hash_id),
    reason TEXT NOT NULL,

    foreign key (project_id, project_branch_id)
      references project_branch (project_id, branch_id)
      on delete cascade
);

CREATE INDEX project_branch_reflog_by_time ON project_branch_reflog (
  project_branch_id, time DESC
);


CREATE INDEX project_reflog_by_time ON project_branch_reflog (
  project_id, time DESC
);

CREATE INDEX global_reflog_by_time ON project_branch_reflog (
  time DESC
);


