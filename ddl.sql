-- v3 codebase schema

CREATE TABLE schema_version (
  version INTEGER NOT NULL
);
INSERT INTO schema_version (version) VALUES (3);

-- actually stores the 512-byte hashes
CREATE TABLE hash (
  id INTEGER PRIMARY KEY NOT NULL,
  -- this would be the full hash, represented as base32 instead of bytes,
  -- to optimize for looking them up by prefix.
  base32 TEXT NOT NULL
);
CREATE UNIQUE INDEX hash_base32 ON hash(base32 COLLATE NOCASE);
-- Per https://sqlite.org/optoverview.html#the_like_optimization,
-- we need COLLATE NOCASE to enable prefix scanning with `LIKE`.
-- If we want LIKE to be case sensitive (defaults to no) then
-- see that link.
-- We want:
    -- sqlite> explain query plan select id from hash where base32 like 'a1b2c3%'
    -- QUERY PLAN
    -- `--SEARCH TABLE hash USING COVERING INDEX hash_base32 (base32>? AND base32<?)
-- Not:
    -- sqlite> explain query plan select id from hash where base32 like 'a1b2c3%'
    -- QUERY PLAN
    -- `--SCAN TABLE hash

CREATE TABLE text (
  id INTEGER PRIMARY KEY NOT NULL,
  text TEXT UNIQUE NOT NULL
);

-- The `hash_object` table is a layer of indirection that allows multiple
-- hashes to be associated to the same object. For example, if the hashing
-- algorithm for the object is changed.
-- I could imagine a UNIQUE (object_id, hash_version) constraint
-- or a UNIQUE (hash_id, hash_version) constraint, or both, but I'm not sure
-- if that will cause trouble later?
-- So: SELECT object.id, bytes FROM object
--      INNER JOIN hash_object ON object_id = object.id
--      INNER JOIN hash ON hash_id = hash.id
--     WHERE base32 LIKE 'a1b2c3%'
CREATE TABLE hash_object (
  -- hashes are UNIQUE, many hashes correspond to one object
  -- (causal nodes are not considered objects atm)
  hash_id INTEGER PRIMARY KEY NOT NULL CONSTRAINT hash_object_fk1 REFERENCES hash(id),
  object_id INTEGER NOT NULL CONSTRAINT hash_object_fk2 REFERENCES object(id),
  hash_version INTEGER NOT NULL
);
-- efficient lookup of hashes by objects
CREATE INDEX hash_object_object_id ON hash_object(object_id);

-- This table is just for diagnostic queries, not for normal ucm operation,
-- by joining `ON object.type_id = object_type_description.id`
CREATE TABLE object_type_description (
  id INTEGER PRIMARY KEY NOT NULL,
  description TEXT UNIQUE NOT NULL
);
INSERT INTO object_type_description (id, description) VALUES
    (0, "Term Component"), -- ping x = pong (x + 1), pong x = ping (x + 1)
    (1, "Decl Component"), -- unique type Animal = Cat | Dog | Mouse
    (2, "Namespace"), -- a one-level slice with no history
    (3, "Patch") -- replace term #abc with term #def
    ;

-- `object` stores binary blobs that are uniquely identified by hash (alone).
-- Within the database, objects (including terms, decls, namespaces) are primarily
-- referenced by their object_ids, not hash ids.  (The use of object_id is
-- meant to prove that the referenced object actually exists in the database,
-- whereas if hash_ids were used, we could only prove that the hash was in the
-- database.)
-- The `hash_object` table allows us to associate multiple hashes to objects,
-- but when we want to display a hash back to the user, we have to choose one.
-- The `primary_hash_id` tells us which one we'll use.
CREATE TABLE object (
  id INTEGER PRIMARY KEY NOT NULL,
  primary_hash_id INTEGER NOT NULL CONSTRAINT object_fk1 REFERENCES hash(id),
  type_id INTEGER NOT NULL CONSTRAINT object_fk2 REFERENCES object_type_description(id),
  bytes BLOB NOT NULL
);
-- look up objects by primary_hash_id.
CREATE UNIQUE INDEX object_hash_id ON object(primary_hash_id);
-- filter objects by
CREATE INDEX object_type_id ON object(type_id);

-- `causal` references value hash ids instead of value ids, in case you want
-- to be able to drop values and keep just the causal spine.
-- `commit_flag` and `gc_generation` are basically unused at the moment.
CREATE TABLE causal (
  self_hash_id INTEGER PRIMARY KEY NOT NULL CONSTRAINT causal_fk1 REFERENCES hash(id),
  value_hash_id INTEGER NOT NULL CONSTRAINT causal_fk2 REFERENCES hash(id)
);
-- Don't remember why this index exists —AI
CREATE INDEX causal_value_hash_id ON causal(value_hash_id);

-- We expect exactly 1 row, which we overwrite when we setRootNamespace.
CREATE TABLE namespace_root (
  causal_id INTEGER PRIMARY KEY NOT NULL CONSTRAINT namespace_root_fk1 REFERENCES causal(self_hash_id)
);

-- LCA computations only need to look at this table
-- A causal can have many parents, and a parent may be a parent to many causals.
CREATE TABLE causal_parent (
  causal_id INTEGER NOT NULL CONSTRAINT causal_parent_fk1 REFERENCES causal(self_hash_id),
  parent_id INTEGER NOT NULL CONSTRAINT causal_parent_fk2 REFERENCES causal(self_hash_id),
  PRIMARY KEY (causal_id, parent_id)
) WITHOUT ROWID;
CREATE INDEX causal_parent_causal_id ON causal_parent(causal_id); -- maybe redundant? covered by PK index? —AI
CREATE INDEX causal_parent_parent_id ON causal_parent(parent_id); -- ?  Potentially allow a "redo" command?

-- links reference.id to causals
-- Currently unused.
CREATE TABLE causal_metadata (
  causal_id INTEGER NOT NULL REFERENCES causal(self_hash_id),
  metadata_object_id INTEGER NOT NULL REFERENCES object(id),
  metadata_component_index INTEGER NOT NULL,
  PRIMARY KEY (causal_id, metadata_object_id, metadata_component_index)
) WITHOUT ROWID;
CREATE INDEX causal_metadata_causal_id ON causal_metadata(causal_id);

CREATE TABLE watch_result (
  -- See Note [Watch expression identifier]
  hash_id INTEGER NOT NULL CONSTRAINT watch_result_fk1 REFERENCES hash(id),
  component_index INTEGER NOT NULL,

  result BLOB NOT NULL, -- evaluated result of the watch expression
  PRIMARY KEY (hash_id, component_index)
) WITHOUT ROWID;


CREATE TABLE watch (
  -- See Note [Watch expression identifier]
  hash_id INTEGER NOT NULL CONSTRAINT watch_fk1 REFERENCES hash(id),
  component_index INTEGER NOT NULL,

  watch_kind_id INTEGER NOT NULL CONSTRAINT watch_fk2 REFERENCES watch_kind_description(id),
  PRIMARY KEY (hash_id, component_index, watch_kind_id)
) WITHOUT ROWID;
CREATE INDEX watch_kind ON watch(watch_kind_id);

-- Note [Watch expression identifier]
-- The hash_id + component_index is an unevaluated term reference. We use hash_id instead of object_id because the 
-- unevaluated term may not exist in the codebase: it is not added merely by watching it without a name, e.g `> 2 + 3`.


CREATE TABLE watch_kind_description (
  id INTEGER PRIMARY KEY NOT NULL,
  description TEXT UNIQUE NOT NULL
);
INSERT INTO watch_kind_description (id, description) VALUES
  (0, "Regular"), -- won't be synced
  (1, "Test") -- will be synced
  ;

-- Related to the discussion at the `object` table, `find_type_index` indexes
-- the types by hash-based references instead of object-based references, because
-- they may be arbitrary types, not just the head types that are stored in the
-- codebase.  The terms having these types are indexed by object-based referents.
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

-- dependents and dependencies are all in the codebase, so they use object-based references.
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
-- Semicolon intentionally omitted, for the same reason
-- semicolons in comments will blow up codebase initialization.
-- (oops, almost used a semicolon at the end of that last phrase!)
-- Sqlite doesn't let us submit multiple statements in the same
-- command, so we are using Haskell code to segment the statements
-- by splitting on semicolons.  It doesn't know to ignore comments,
-- though I guess that wouldn't be hard to implement.  Should have
-- done it from the start.


--- New stuff



CREATE TABLE entity (
  id INTEGER PRIMARY KEY,
  -- Make a separate table for entity type descriptions?
  -- Do we need this here or nah?
  kind_id INTEGER NOT NULL,
  builtin INTEGER NULL REFERENCES text(id),
  object_id INTEGER NULL,
  component_index INTEGER NULL,
  constructor_index INTEGER NULL,

  CONSTRAINT entity_validity CHECK (
    CASE kind_id
      WHEN 0 -- Term Component
        THEN
          (builtin IS NULL) =
          (object_id IS NOT NULL
            AND component_index IS NOT NULL
          )
      WHEN 1 -- Decl Component
        THEN
          (builtin IS NULL) =
          (object_id IS NOT NULL
            AND component_index IS NOT NULL
            AND constructor_index IS NULL
          )
      WHEN 2 -- Currently Namespace
        THEN FALSE -- This object type will be deleted as part of migrating to relational namespaces.
      WHEN 3 -- Patch
        THEN (   builtin IS NULL
             AND component_index IS NULL
             AND constructor_index IS NULL
             )
    END
  ),

  -- Index ensures kind_id's line up correctly when object_id isn't NULL.
  FOREIGN KEY (object_id, kind_id) REFERENCES object_and_type(object_id, kind_id)
);

-- Allows efficient filtering by type in joins.
CREATE INDEX entity_and_kind ON entity(id, kind_id);

CREATE INDEX object_and_kind ON object(id, type_id);

-- The basic namespace tree structure.
-- Each namespace (identified by its branch's value_hash)
-- Has connections to many children.
-- This structure requires copying all the child relationships
-- any time anything in the parent branch changes,
-- This isn't ideal for cases where a branch has many children/terms,
-- but there's really no better way since branches are immutable
CREATE TABLE namespace_child (
  parent_namespace_hash_id INTEGER NOT NULL REFERENCES hash(id),
  child_name_id INTEGER NOT NULL REFERENCES text(id),
  child_hash_id INTEGER NOT NULL REFERENCES causal(self_hash_id),
  -- This disallows multiple children at the same child name :)
  PRIMARY KEY (parent_namespace_hash_id, child_name_id)
) WITHOUT ROWID;

CREATE TABLE namespace_entity (
  parent_namespace_hash_id INTEGER NOT NULL REFERENCES hash(id),
  name_segment_id INTEGER NOT NULL REFERENCES text(id),
  entity_id INTEGER NOT NULL,
  entity_kind INTEGER NOT NULL,

  -- Ensure we can only have at most one of each kind of entity at each name.
  PRIMARY KEY (parent_namespace_hash_id, name_segment_id, entity_kind),
  -- Enforce consistency of entities and their kind.
  FOREIGN KEY (entity_id, entity_kind) REFERENCES entity_and_kind(entity_id, entity_kind)
) WITHOUT ROWID;


-- Allow finding definition hashes by name with an optional kind qualifier.
CREATE INDEX namespace_entity_by_name ON namespace_entity (
  name_segment_id, entity_kind
);

-- Allow finding definition names by entity.
-- This index can be joined with a recursive namespace query to
-- limit its scope to a given namespace tree.
CREATE INDEX names_by_entity ON namespace_entity (
  entity_id, parent_namespace_hash_id
);

CREATE TABLE namespace_metadata (
  parent_namespace_hash_id INTEGER NOT NULL REFERENCES hash(id),
  name_segment_id INTEGER NOT NULL REFERENCES text(id),
  entity_kind INTEGER NOT NULL,

  metadata_entity_id INTEGER NULL REFERENCES entity(id),

  -- Is there any primary key that actually makes sense?
  -- We might just want to use row IDs here and make indexes for what we need, but maybe there's value in asserting uniqueness on the entire row?
  PRIMARY KEY(parent_namespace_hash_id, name_segment_id, entity_kind, metadata_entity_id),
  FOREIGN KEY(parent_namespace_hash_id, name_segment_id, entity_kind) REFERENCES namespace_entity(parent_namespace_hash_id, name_segment_id, entity_kind)
) WITHOUT ROWID;


-- View representing all child namespaces reachable from the root namespace, and their paths.
CREATE VIEW root_namespace AS
  WITH RECURSIVE child_namespaces(parent_namespace_hash_id, path) AS (
    SELECT value_hash_id, ""
      FROM namespace_root
      JOIN causal ON causal_id = causal.self_hash_id
    UNION
    SELECT causal.value_hash_id, path || '.' || text.text
    FROM namespace_child
      JOIN child_namespaces
        ON child_namespaces.parent_namespace_hash_id = namespace_child.parent_namespace_hash_id
      JOIN text
        ON namespace_child.child_name_id = text.id
      JOIN causal
        ON child_hash_id = causal.self_hash_id
  )
SELECT * FROM child_namespaces;


--- Test data


INSERT INTO text(id, text)
  VALUES
    (1, "childone"),
    (2, "childtwo"),
    (3, "childthree");

INSERT INTO hash(id, base32)
  VALUES
    (1, "roothash"),
    (2, "childonehash"),
    (3, "childtwohash"),
    (4, "childthreehash"),
    (5, "rootcausalhash"),
    (6, "causaloneselfhash"),
    (7, "causaltwoselfhash"),
    (8, "causalthreeselfhash");

INSERT INTO causal(self_hash_id, value_hash_id)
  VALUES
    (5, 1),
    (6, 2),
    (7, 3),
    (8, 4);

INSERT INTO namespace_child (parent_namespace_hash_id, child_name_id, child_hash_id)
  VALUES
    (1, 1, 6),
    (2, 2, 7),
    (3, 3, 8);

INSERT INTO namespace_root(causal_id)
  VALUES
    (5);
