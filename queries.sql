-- * Allow querying for the names of a set of references/referents within the scope of a given branch.

WITH RECURSIVE child_namespaces(parent_namespace_hash_id, path) AS (
  SELECT ?, ""
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

-- Select all definitions which match the provided object/component/constructor/builtin; as long as it exists in the root namespace.
SELECT *
  namespace_definition_by_ref
    WHERE
          definition_builtin = ?
      AND definition_object_id = ?
      AND definition_component_index = ?
      AND definition_constructor_id = ?
      AND parent_namespace_hash_id
        IN (SELECT parent_namespace_hash_id FROM root_namespace)
;

-- * Efficiently look up old names.

-- What semantics do we actually want for this?

SELECT *
  FROM root_namespace
  JOIN scoped_namespace_definition_by_ref
    ON scoped_namespace_definition_by_ref.parent_namespace_hash_id = root_namespace.parent_namespace_hash_id
  WHERE
        definition_builtin = ?
    AND definition_object_id = ?
    AND definition_component_index = ?
    AND definition_constructor_id = ?

-- * Lazily load a sub-namespace by path.

SELECT parent_namespace_hash_id
  FROM root_namespace
  WHERE path = ?

---

-- Set up basic data

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

