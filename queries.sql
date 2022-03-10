-- Select the entity for a given name within the root namespace
-- Can either add an index over text to slightly speed this up, or just run with the assumption that a given branch will have a 'small' number 
-- entities as direct children, which is probably good enough.
SELECT entity.id
  FROM root_namespace 
    JOIN namespace_entity ON namespace_entity.parent_namespace_hash_id = root_namespace.parent_namespace_hash_id
    JOIN entity on entity.id = namespace_entity.entity_id
    JOIN text ON text.id = namespace_entity.name_segment_id
  WHERE
    root_namespace.path = ?
    AND text.text = ?
    ;

-- Select names and paths within the root namespace for a given set of entities.
SELECT root_namespace.path as path, text.text as name, entity_id
  FROM entity
    JOIN namespace_entity ON entity.id = entity_id
    JOIN root_namespace ON namespace_entity.parent_namespace_hash_id
    JOIN text ON text.id = namespace_entity.name_segment_id
    WHERE
      entity.id IN (1, 2, 3)
      ;

-- * Efficiently look up old names for an entity

-- What semantics do we actually want for this?

-- Just use the above query for finding names in the root namespace, but replace with the
-- root_namespace_with_history view instead (or any namespace view you like).

-- * Lazily load a sub-namespace by path.

SELECT parent_namespace_hash_id
  FROM root_namespace
  WHERE path = ?
  ;
