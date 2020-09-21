-- find type index uses hash-based references instead of component-based
-- references, because they may be arbitrary types, not just the head
-- types that are stored in the codebase.
CREATE TABLE find_type_index (
  type_reference_builtin INTEGER NULL REFERENCES text(id),
  type_reference_hash_id INTEGER NULL REFERENCES hash(id),
  type_reference_component_index INTEGER NULL,
  term_referent_object_id INTEGER NOT NULL REFERENCES hash(id),
  term_referent_component_index INTEGER NOT NULL,
  term_referent_constructor_index INTEGER NULL,
  UNIQUE (
    term_referent_derived_object_id,
    term_referent_derived_component_index,
    term_referent_derived_constructor_index
  ),
  CHECK (
    type_reference_builtin IS NULL =
    type_reference_derived_hash_id IS NOT NULL
  ),
  CHECK (
    type_reference_derived_object_id IS NULL =
    type_reference_derived_component_index IS NULL
  )
);
CREATE INDEX find_type_index_type ON find_type_index (
    type_reference_builtin,
    type_reference_derived_hash_id,
    type_reference_derived_component_index
);

CREATE TABLE find_type_mentions_index (
  type_reference_builtin INTEGER NULL REFERENCES text(id),
  type_reference_hash_id INTEGER NULL REFERENCES hash(id),
  type_reference_component_index INTEGER NULL,
  term_referent_object_id INTEGER NOT NULL REFERENCES hash(id),
  term_referent_derived_component_index INTEGER NOT NULL,
  term_referent_constructor_index INTEGER NULL,
  CHECK (
    type_reference_builtin IS NULL =
    type_reference_derived_hash_id IS NOT NULL
  ),
  CHECK (
    type_reference_derived_hash_id IS NULL =
    type_reference_derived_component_index IS NULL
  )
);
CREATE INDEX find_type_mentions_index_type ON find_type_mentions_index (
  type_reference_builtin INTEGER NULL REFERENCES text(id),
  type_reference_hash_id INTEGER NULL REFERENCES hash(id),
  type_reference_component_index INTEGER NULL
);

CREATE TABLE dependents_index (
  dependency_builtin INTEGER NULL REFERENCES text(id),
  dependency_object_id INTEGER NULL REFERENCES hash(id),
  dependency_component_index INTEGER NULL
  dependent_object_id INTEGER NOT NULL REFERENCES hash(id),
  dependent_component_index INTEGER NOT NULL,
  CHECK (
    type_reference_builtin IS NULL =
    type_reference_derived_object_id IS NOT NULL
  ),
  CHECK (
    type_reference_derived_object_id IS NULL =
    type_reference_derived_component_index IS NULL
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
);
