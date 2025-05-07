create table merge_branch (
  project_id uuid not null,
  branch_id uuid not null,
  local_source_project_id uuid,
  local_source_branch_id uuid,
  remote_source_project_id uuid,
  remote_source_branch_id uuid,
  remote_source_host text,
  source_causal_hash_id integer not null references hash (id) on delete cascade,
  target_project_id uuid,
  target_branch_id uuid,
  target_causal_hash_id integer not null references hash (id) on delete cascade,
  primary key (project_id, branch_id),
  foreign key (project_id, branch_id)
    references project_branch (project_id, branch_id)
    on delete cascade,
  foreign key (local_source_project_id, local_source_branch_id)
    references project_branch (project_id, branch_id)
    on delete set null,
  foreign key (remote_source_project_id, remote_source_branch_id, remote_source_host)
    references remote_project_branch (project_id, branch_id, host)
    on delete set null,
  foreign key (target_project_id, target_branch_id)
    references project_branch (project_id, branch_id)
    on delete set null,
  check (
    local_source_branch_id is null or
    remote_source_branch_id is null
  ),
  check (
    (project_id = local_source_project_id or local_source_project_id is null) and
    (project_id = target_project_id or target_project_id is null)
  )
);

create table namespace_unique_type_guid (
  namespace_hash_id integer not null references hash (id) on delete cascade,
  type_name jsonb not null,
  type_guid text not null
);
