-- When on branch `foo/main`, if you run `merge /topic` and the merge fails, you're placed on a "merge branch"
-- `foo/merge-topic-into-main`. (Say project `foo` has uuid `foo-uuid`, and similar for the branches).
--
-- If that happens, you'll have a single row in this table that looks like this:
--
--   +----------------------------------------------------+
--   | merge_branch                                       |
--   +====================================================+
--   |            project_id |                   foo-uuid |
--   +-----------------------+----------------------------+
--   |             branch_id | merge-topic-into-main-uuid |
--   +-----------------------+----------------------------+
--   |     source_project_id |                   foo-uuid |
--   +-----------------------+----------------------------+
--   |      source_branch_id |                 topic-uuid |
--   +-----------------------+----------------------------+
--   | source_causal_hash_id |                        100 |
--   +-----------------------+----------------------------+
--   |     target_project_id |                   foo-uuid |
--   +-----------------------+----------------------------+
--   |      target_branch_id |                  main-uuid |
--   +-----------------------+----------------------------+
--   | target_causal_hash_id |                        200 |
--   +-----------------------+----------------------------+
--
-- There's a check constraint that asserts all three project ids are the same, because we don't support cross-project
-- merges. So why three columns instead of just one? Because we want to allow the source or target branch to be deleted,
-- and set the corresponding branch id to null, but because the "on delete set null" clause sets all columns in the
-- foreign key reference to null, we have additional columns for the source and target projects. So say the user deletes
-- `/main`; the row will then look like this:
--
--   +----------------------------------------------------+
--   | merge_branch                                       |
--   +====================================================+
--   |            project_id |                   foo-uuid |
--   +-----------------------+----------------------------+
--   |             branch_id | merge-topic-into-main-uuid |
--   +-----------------------+----------------------------+
--   |     source_project_id |                   foo-uuid |
--   +-----------------------+----------------------------+
--   |      source_branch_id |                 topic-uuid |
--   +-----------------------+----------------------------+
--   | source_causal_hash_id |                        100 |
--   +-----------------------+----------------------------+
--   |     target_project_id |                            |
--   +-----------------------+----------------------------+
--   |      target_branch_id |                            |
--   +-----------------------+----------------------------+
--   | target_causal_hash_id |                        200 |
--   +-----------------------+----------------------------+
create table merge_branch (
  project_id uuid not null,
  branch_id uuid not null,
  local_source_project_id uuid,
  local_source_branch_id uuid,
  remote_host text,
  remote_source_project_id uuid,
  remote_source_branch_id uuid,
  source_causal_hash_id integer references hash (id) on delete set null,
  target_project_id uuid,
  target_branch_id uuid,
  target_causal_hash_id integer references hash (id) on delete set null,
  primary key (project_id, branch_id),
  foreign key (project_id, branch_id)
    references project_branch (project_id, branch_id)
    on delete cascade,
  foreign key (source_project_id, source_branch_id)
    references project_branch (project_id, branch_id)
    on delete set null,
  foreign key (target_project_id, target_branch_id)
    references project_branch (project_id, branch_id)
    on delete set null,
  check (
    (project_id = source_project_id or source_project_id is null) and
    (project_id = target_project_id or target_project_id is null)
  )
);

create table namespace_unique_type_guid (
  namespace_hash_id integer not null references hash (id) on delete cascade,
  type_name jsonb not null,
  type_guid text not null
);
