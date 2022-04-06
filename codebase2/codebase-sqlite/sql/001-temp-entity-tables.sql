begin;
create table temp_entity_type_description (
  id integer primary key not null,
  description text unique not null
);
insert into temp_entity_type_description
values
  (0, 'Term Component'),
  (1, 'Decl Component'),
  (2, 'Namespace'),
  (3, 'Patch'),
  (4, 'Causal');
create table temp_entity (
  hash text primary key not null,
  blob bytes not null,
  type_id integer not null references temp_entity_type_description(id)
);
create table temp_entity_missing_dependency (
  dependent text not null references temp_entity(hash),
  dependency text not null
);
create index temp_entity_missing_dependency_ix_dependent on temp_entity_missing_dependency (dependent);
create index temp_entity_missing_dependency_ix_dependency on temp_entity_missing_dependency (dependency);
select count(*) from object;
rollback;

begin;
create table foo(a int);
create index foo on foo (a);
rollback;

.schema hash

.schema object

.schema object_type_description

select * from object_type_description;

select 1;
