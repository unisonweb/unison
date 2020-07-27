--CREATE TABLE reference_derived (
--  id INTEGER NOT NULL PRIMARY KEY,
--  hash_id INTEGER NOT NULL REFERENCES hash(id),
--  component_index INTEGER NOT NULL,
--  UNIQUE (hash_id, component_index)
--);
--CREATE INDEX reference_derived_hash_id ON reference_derived(hash_id);
--
--CREATE TABLE reference (
--  id INTEGER NOT NULL PRIMARY KEY,
--  builtin TEXT, -- a builtin name, or null
--  reference_derived_id INTEGER REFERENCES reference_derived(id),
--  UNIQUE(builtin, reference_derived_id),
--  -- exactly one should be null
--  CHECK (builtin IS NULL <> reference_derived_id IS NULL)
--);
--
---- `Referent' ReferenceDerivedId` but without `ConstructorType`,
---- which is linked to the object.
--CREATE TABLE referent_derived (
--  id INTEGER NOT NULL PRIMARY KEY,
--  reference_derived_id INTEGER NOT NULL REFERENCES reference_derived(id),
--  constructor_id INTEGER,
--  UNIQUE(reference_derived_id, constructor_id)
--);
--
---- just using rowid since we don't need joins
---- index terms by types
--CREATE TABLE find_type_index (
--  type_reference_id INTEGER NOT NULL REFERENCES reference(id),
--  referent_derived_id INTEGER NOT NULL REFERENCES referent_derived(id)
--);
--CREATE INDEX find_type_index_reference ON find_type_index(type_reference_id);
--CREATE INDEX find_type_index_referent ON find_type_index(referent_derived_id);
--
--CREATE TABLE find_type_mentions_index (
--  type_reference_id INTEGER NOT NULL REFERENCES reference(id),
--  referent_id INTEGER NOT NULL REFERENCES referent_derived(id)
--);
--CREATE INDEX find_type_mentions_index_reference ON find_type_mentions_index(type_reference_id);
--CREATE INDEX find_type_mentions_index_referent ON find_type_mentions_index(referent_id);
--
--CREATE TABLE dependents_index (
--  dependency_id  INTEGER NOT NULL REFERENCES reference(id),
--  dependent_id INTEGER NOT NULL REFERENCES reference_derived(id)
--);
--CREATE INDEX dependents_index_dependency ON dependents_index(dependency_id);
--CREATE INDEX dependents_index_dependent ON dependents_index(dependent_id);

CREATE TABLE find_type_index (
  type_reference_builtin TEXT NULL,
  type_reference_hash_id INTEGER NULL REFERENCES hash(id),
  type_reference_component_index INTEGER NULL,
  term_referent_hash_id INTEGER NOT NULL REFERENCES hash(id),
  term_referent_component_index INTEGER NOT NULL,
  term_referent_constructor_index INTEGER NULL,
  PRIMARY KEY(
    type_reference_builtin,
    type_reference_derived_hash_id,
    type_reference_derived_component_index
  ),
  UNIQUE (
    term_referent_derived_hash_id,
    term_referent_derived_component_index,
    term_referent_derived_constructor_index
  ),
  CHECK (
    type_reference_builtin IS NULL =
    type_reference_derived_hash_id IS NOT NULL
  ),
  CHECK (
    type_reference_derived_hash_id IS NULL =
    type_reference_derived_component_index IS NULL
  )
);

CREATE TABLE find_type_mentions_index (
  type_reference_builtin TEXT NULL,
  type_reference_hash_id INTEGER NULL REFERENCES hash(id),
  type_reference_component_index INTEGER NULL,
  term_referent_hash_id INTEGER NOT NULL REFERENCES hash(id),
  term_referent_derived_component_index INTEGER NOT NULL,
  term_referent_constructor_index INTEGER NULL,
  PRIMARY KEY(
    type_reference_builtin,
    type_reference_derived_hash_id,
    type_reference_derived_component_index
  ),
  CHECK (
    type_reference_builtin IS NULL =
    type_reference_derived_hash_id IS NOT NULL
  ),
  CHECK (
    type_reference_derived_hash_id IS NULL =
    type_reference_derived_component_index IS NULL
  )
);

CREATE TABLE dependents_index (
  dependency_builtin TEXT NULL,
  dependency_hash_id INTEGER NULL REFERENCES hash(id),
  dependency_component_index INTEGER NULL
  dependent_hash_id INTEGER NOT NULL REFERENCES hash(id),
  dependent_component_index INTEGER NOT NULL,
  CHECK (
    type_reference_builtin IS NULL =
    type_reference_derived_hash_id IS NOT NULL
  ),
  CHECK (
    type_reference_derived_hash_id IS NULL =
    type_reference_derived_component_index IS NULL
  )
);
CREATE INDEX dependents_by_dependency ON dependents_index (
  dependency_builtin,
  dependency_hash_id,
  dependency_component_index
);
CREATE INDEX dependencies_by_dependent ON dependents_index (
  dependent_hash_id,
  dependent_component_index
);
