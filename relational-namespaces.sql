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


-- Allow finding term hashes by name and/or type.
CREATE INDEX namespace_definition_by_name ON namespace_definition (
  name_segment_id, definition_kind
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
