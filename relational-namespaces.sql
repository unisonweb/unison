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

CREATE TABLE namespace_definition (
  parent_namespace_hash_id INTEGER NOT NULL REFERENCES hash(id),
  definition_kind INTEGER NOT NULL REFERENCES object_type_description(id),
  name_segment_id INTEGER NOT NULL REFERENCES text(id),


  definition_builtin INTEGER NULL REFERENCES text(id),
  definition_object_id INTEGER NULL REFERENCES object(id),
  definition_component_index INTEGER NULL,
  definition_constructor_id INTEGER NULL,

  CONSTRAINT namespace_definition_validity CHECK (
    CASE definition_kind
      WHEN 0 -- Term Component
        THEN
          (definition_builtin IS NULL) =
          (definition_object_id IS NOT NULL
            AND definition_component_index IS NOT NULL
          )
      WHEN 1 -- Decl Component
        THEN
          (definition_builtin IS NULL) =
          (definition_object_id IS NOT NULL
            AND definition_component_index IS NOT NULL
            AND definition_constructor_id IS NULL
          )
      WHEN 2 -- Currently Namespace
        THEN FALSE -- This object type will be deleted as part of migrating to relational namespaces.
      WHEN 3 -- Patch
        THEN (   definition_builtin IS NULL
             AND definition_component_index IS NULL
             AND definition_constructor_id IS NULL
             )
    END
  ),

  -- Ensure we can only have at most one of each kind of definition at each name.
  PRIMARY KEY (parent_namespace_hash_id, definition_kind, name_segment_id)
) WITHOUT ROWID;


-- Allow finding definition hashes by name and/or type.
CREATE INDEX namespace_definition_by_name ON namespace_definition (
  name_segment_id, definition_kind
);

-- Allow finding definition names by reference.
-- This index can be joined with a recursive namespace query to
-- limit its scope to a given namespace tree.
CREATE INDEX namespace_definition_by_ref ON namespace_definition (
  definition_builtin, definition_object_id, definition_component_index, definition_constructor_id
  -- Do I need this too?
  -- , parent_namespace_hash_id
);

-- Probably need EITHER this index or namespace_definition_by_ref, not BOTH.
-- It'll depend on the semantics we want for looking up historical names.
CREATE INDEX scoped_namespace_definition_by_ref ON namespace_definition (
  parent_namespace_hash_id, definition_builtin, definition_object_id, definition_component_index, definition_constructor_id
);

CREATE TABLE namespace_metadata (
  parent_namespace_hash_id INTEGER NOT NULL REFERENCES hash(id),
  metadata_kind INTEGER NOT NULL REFERENCES object_type_description(id),
  name_segment_id INTEGER NOT NULL REFERENCES text(id),

  metadata_builtin INTEGER NULL REFERENCES text(id),
  metadata_object_id INTEGER NULL REFERENCES object(id),
  metadata_component_index INTEGER NULL,
  metadata_constructor_id INTEGER NULL,


  CONSTRAINT namespace_metadata_validity CHECK (
    CASE metadata_kind
      WHEN 0 -- Term Component
        THEN
          (metadata_builtin IS NULL) =
          (metadata_object_id IS NOT NULL
            AND metadata_component_index IS NOT NULL
          )
      WHEN 1 -- Decl Component
        THEN
          (metadata_builtin IS NULL) =
          (metadata_object_id IS NOT NULL
            AND metadata_component_index IS NOT NULL
            AND metadata_constructor_id IS NULL
          )
      WHEN 2 -- Currently Namespace
        THEN FALSE -- This object type will be deleted as part of migrating to relational namespaces.
      WHEN 3 -- Patch
        THEN FALSE -- Patches are invalid metadata.
    END
  ),

  -- Is there any primary key that actually makes sense?
  -- We might just want to use row IDs here and make indexes for what we need, but maybe there's value in asserting uniqueness on the entire row?
  PRIMARY KEY(parent_namespace_hash_id, metadata_kind, name_segment_id, metadata_builtin, metadata_object_id, metadata_component_index),
  FOREIGN KEY(parent_namespace_hash_id, metadata_kind, name_segment_id) REFERENCES namespace_definition(parent_namespace_hash_id, metadata_kind, name_segment_id)
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
