-- * Allow querying for the names of a set of references/referents within the scope of a given branch.

WITH RECURSIVE child_namespaces(parent_namespace_hash_id, path) AS (
  SELECT 1, ""
  UNION
  SELECT child_hash_id, path || '.' || text.text
  FROM namespace_child
    JOIN child_namespaces
      ON child_namespaces.parent_namespace_hash_id = namespace_child.parent_namespace_hash_id
    JOIN text
      ON namespace_child.child_name_id = text.id
)

SELECT * FROM child_namespaces;
