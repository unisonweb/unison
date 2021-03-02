-- find type index uses hash-based references instead of component-based
-- references, because they may be arbitrary types, not just the head
-- types that are stored in the codebase.
CREATE TABLE find_type_index (
  type_reference_builtin INTEGER NULL CONSTRAINT find_type_index_fk1 REFERENCES text(id),
  type_reference_hash_id INTEGER NULL CONSTRAINT find_type_index_fk2 REFERENCES hash(id),
  type_reference_component_index INTEGER NULL,
  term_referent_object_id INTEGER NOT NULL CONSTRAINT find_type_index_fk3 REFERENCES object(id),
  term_referent_component_index INTEGER NOT NULL,
  term_referent_constructor_index INTEGER NULL,
  CONSTRAINT find_type_index_c1 UNIQUE (
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  ),
  CONSTRAINT find_type_index_c2 CHECK (
    (type_reference_builtin IS NULL) =
    (type_reference_hash_id IS NOT NULL)
  ),
  CONSTRAINT find_type_index_c3 CHECK (
    (type_reference_hash_id IS NULL) =
    (type_reference_component_index IS NULL)
  )
);
CREATE INDEX find_type_index_type ON find_type_index (
    type_reference_builtin,
    type_reference_hash_id,
    type_reference_component_index
);

CREATE TABLE find_type_mentions_index (
  type_reference_builtin INTEGER NULL CONSTRAINT find_type_mentions_index_fk1 REFERENCES text(id),
  type_reference_hash_id INTEGER NULL CONSTRAINT find_type_mentions_index_fk2 REFERENCES hash(id),
  type_reference_component_index INTEGER NULL,
  term_referent_object_id INTEGER NOT NULL CONSTRAINT find_type_mentions_index_fk3 REFERENCES object(id),
  term_referent_component_index INTEGER NOT NULL,
  term_referent_constructor_index INTEGER NULL,
  CONSTRAINT find_type_mentions_index_c1 CHECK (
    (type_reference_builtin IS NULL) =
    (type_reference_hash_id IS NOT NULL)
  ),
  CONSTRAINT find_type_mentions_index_c2 CHECK (
    (type_reference_hash_id IS NULL) =
    (type_reference_component_index IS NULL)
  )
);
CREATE INDEX find_type_mentions_index_type ON find_type_mentions_index (
  type_reference_builtin,
  type_reference_hash_id,
  type_reference_component_index
);

CREATE TABLE dependents_index (
  dependency_builtin INTEGER NULL CONSTRAINT dependents_index_fk1 REFERENCES text(id),
  dependency_object_id INTEGER NULL CONSTRAINT dependents_index_fk2 REFERENCES object(id),
  dependency_component_index INTEGER NULL,
  dependent_object_id INTEGER NOT NULL CONSTRAINT dependents_index_fk3 REFERENCES object(id),
  dependent_component_index INTEGER NOT NULL,
  CONSTRAINT dependents_index_c1 CHECK (
    (dependency_builtin IS NULL) =
    (dependency_object_id IS NOT NULL)
  ),
  CONSTRAINT dependents_index_c2 CHECK (
    (dependency_object_id IS NULL) =
    (dependency_component_index IS NULL)
  )
);
CREATE INDEX dependents_by_dependency ON dependents_index (
  dependency_builtin,
  dependency_object_id,
  dependency_component_index
);
CREATE INDEX dependencies_by_dependent ON dependents_index (
  dependent_object_id,
  dependent_component_index
)