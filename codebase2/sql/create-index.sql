CREATE TABLE reference_derived (
  id INTEGER NOT NULL PRIMARY KEY,
  hash_id INTEGER NOT NULL REFERENCES hash(id),
  component_index INTEGER NOT NULL,
  UNIQUE (hash_id, component_index)
);
CREATE INDEX reference_derived_hash_id ON reference_derived(hash_id);

CREATE TABLE reference (
  id INTEGER NOT NULL PRIMARY KEY,
  builtin TEXT, -- a builtin name, or null
  reference_derived_id INTEGER REFERENCES reference_derived(id),
  UNIQUE(builtin, reference_derived_id),
  -- exactly one should be null
  CHECK (builtin IS NULL <> reference_derived_id IS NULL)
);

CREATE TABLE referent_derived (
  id INTEGER NOT NULL PRIMARY KEY,
  reference_derived_id INTEGER NOT NULL REFERENCES reference_derived(id),
  constructor_id INTEGER,
  constructor_type INTEGER,
  UNIQUE(reference_derived_id, constructor_id, constructor_type),
  CHECK (constructor_id IS NULL = constructor_type IS NULL)
);

-- just using rowid since we don't need joins
-- index terms by type
CREATE TABLE type_index (
  type_reference_id INTEGER NOT NULL REFERENCES reference(id),
  referent_id INTEGER NOT NULL REFERENCES referent_derived(id)
);
CREATE INDEX type_index_reference ON type_index(type_reference_id);
CREATE INDEX type_index_referent ON type_index(referent_id);

CREATE TABLE type_mentions_index (
  type_reference_id INTEGER NOT NULL REFERENCES reference(id),
  referent_id INTEGER NOT NULL REFERENCES referent_derived(id)
);
CREATE INDEX type_mentions_index_reference ON type_mentions_index(type_reference_id);
CREATE INDEX type_mentions_index_referent ON type_mentions_index(referent_id);

CREATE TABLE dependents_index (
  dependency_id  INTEGER NOT NULL REFERENCES reference(id),
  dependent_id INTEGER NOT NULL REFERENCES reference_derived(id)
);
CREATE INDEX dependents_index_dependency ON dependents_index(dependency_id);
CREATE INDEX dependents_index_dependent ON dependents_index(dependent_id);

-- WITH RECURSIVE cnt(x) AS (VALUES(1) UNION ALL SELECT x+1 FROM cnt WHERE x<1000000)
-- --                        ^^^^initial SELECT
-- --                                            ^^^^^recursive SELECT
-- SELECT x FROM cnt;
