create table project (
  id uuid primary key,
  name text unique not null
)
without rowid;

create table project_branch (
  project_id uuid not null references project (id),
  branch_id uuid not null,
  name text not null,

  primary key (project_id, branch_id),

  unique (project_id, name)
)
without rowid;

create table project_branch_parent (
  project_id uuid not null references project (id),
  branch_id uuid not null,
  parent_branch_id uuid not null,

  primary key (project_id, branch_id),

  foreign key (project_id, branch_id)
    references project_branch (project_id, branch_id)
    on delete cascade,

  foreign key (project_id, parent_branch_id)
    references project_branch (project_id, branch_id)
    on delete cascade
)
without rowid;

create table project_branch_default_pull (
  local_project_id uuid not null references project (id),
  local_branch_id uuid not null,
  named_remote_name text null,
  named_remote_project_id text null,
  named_remote_host text null,
  unnamed_remote_project_id text null,
  unnamed_remote_host text null,
  remote_branch_id text null,

  primary key (local_project_id, local_branch_id),

  foreign key (local_project_id, local_branch_id)
    references project_branch (project_id, id)
    on delete cascade,

  foreign key (local_project_id, named_remote_project_id, named_remote_host, named_remote_name)
    references project_remote_alias (local_project_id, remote_project_id, remote_host, remote_name)
    on delete cascade,

  foreign key (named_remote_project_id, remote_branch_id, named_remote_host)
    references remote_project_branch (project_id, branch_id, host)
    on delete cascade,

  foreign key (unnamed_remote_project_id, remote_branch_id, unnamed_remote_host)
    references remote_project_branch (project_id, branch_id, host)
    on delete cascade,

  constraint valid_remote_project check (
    ( named_remote_name is not null
      and named_remote_project_id is not null
      and named_remote_host is not null
      and unnamed_remote_project_id is null
      and unnamed_remote_host is null
    )
    or
    ( named_remote_name is null
      and named_remote_project_id is null
      and named_remote_host is null
      and unnamed_remote_project_id is not null
      and unnamed_remote_host is not null
    )
  )
)
without rowid;

create table project_branch_remote_mapping (
  local_project_id uuid not null references project (id),
  local_branch_id uuid not null,
  remote_project_id text not null,
  remote_branch_id text not null,
  remote_host text not null,

  primary key (local_project_id, local_branch_id, remote_host),

  foreign key (local_project_id, local_branch_id)
    references project_branch (project_id, branch_id)
    on delete cascade,

  foreign key (remote_project_id, remote_branch_id, remote_host)
    references remote_project_branch (project_id, branch_id, host)
    on delete cascade
)
without rowid;

create table project_remote_alias (
  local_project_id uuid not null references project (id),
  remote_project_id text not null,
  remote_host text not null,
  remote_name text not null,

  primary key (local_project_id, remote_name),

  unique (local_project_id, remote_project_id, remote_host, remote_name),

  foreign key (remote_project_id, remote_host)
    references remote_project (id, host)
    on delete cascade
)
without rowid;

create table remote_project (
  id text not null,
  host text not null,
  name text not null,

  primary key (id, host)
)
without rowid;

create table remote_project_branch (
  project_id text not null,
  branch_id text not null,
  host text not null,
  name text not null,

  primary key (project_id, branch_id, host),

  foreign key (project_id, host)
    references remote_project (id, host)
    on delete cascade
)
without rowid;
