-- This table encompasses the Sum type of an entity reference, which is complex because we don't store rows for each entity,
-- they're stored in components. This gives us an entity ID which can refer to a definition as a single field, which makes a lot of joins
-- much easier and lets us consolidate all the checks which dictate the rules of the sum-type in a single place.
--
-- An entity is either a Term, Constructor, Type Decl, or Patch.
CREATE TABLE entity (
  id INTEGER PRIMARY KEY,
  -- Make a separate table for entity type descriptions?
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
            -- constructor_index may or may not be null
          )
      WHEN 1 -- Decl Component
        THEN
          (builtin IS NULL) =
          (object_id IS NOT NULL
            AND component_index IS NOT NULL
            AND constructor_index IS NULL
          )
      WHEN 3 -- Patch
        THEN (   builtin IS NULL
             AND component_index IS NULL
             AND constructor_index IS NULL
             object_id IS NOT NULL
             )
      -- Other object types aren't entities.
      ELSE FALSE
    END
  ),

  -- Index ensures kind_id's line up correctly when object_id isn't NULL.
  FOREIGN KEY (object_id, kind_id) REFERENCES object_and_type(object_id, kind_id)
);

-- Allows efficient filtering by kind in joins.
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

-- We don't enforce "at most one entity of a kind at a name", since definitions
-- can be 'in conflict' after a merge.
CREATE TABLE namespace_entity (
  parent_namespace_hash_id INTEGER NOT NULL REFERENCES hash(id),
  name_segment_id INTEGER NOT NULL REFERENCES text(id),
  entity_id INTEGER NOT NULL REFERENCES entity(id),

  PRIMARY KEY (parent_namespace_hash_id, name_segment_id, entity_id),
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
  entity_id INTEGER NOT NULL REFERENCES entity(id),
  metadata_entity_id INTEGER NOT NULL REFERENCES entity(id),

  PRIMARY KEY(parent_namespace_hash_id, name_segment_id, entity_id, metadata_entity_id),
) WITHOUT ROWID;


-- View representing all child namespaces reachable from the root namespace, and their paths.
-- This view can be easily adapted to select ANY subtree by parameterizing the initial causal selection, but it's convenient to keep the root namespace as a view.
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


/* CREATE VIEW root_namespace_with_history AS */
/*   WITH RECURSIVE child_namespaces(parent_namespace_hash_id, causal_something, path) AS ( */
/*     SELECT value_hash_id, "" */
/*       FROM namespace_root */
/*       JOIN causal ON causal_id = causal.self_hash_id */
/*     UNION */
/*     SELECT (causal.value_hash_id, path || '.' || text.text), */
/*            (causal, path || '.' || text.text), */
/*     FROM namespace_child */
/*       JOIN child_namespaces */
/*         ON child_namespaces.parent_namespace_hash_id = namespace_child.parent_namespace_hash_id */
/*       JOIN text */
/*         ON namespace_child.child_name_id = text.id */
/*       JOIN causal_parent */
/*         ON causal_id = causal.self_hash_id */
/*            OR causal_parent = causal.self_hash_id */
/*       JOIN causal */
/*         ON */
/*   ) */
/* SELECT * FROM child_namespaces; */
