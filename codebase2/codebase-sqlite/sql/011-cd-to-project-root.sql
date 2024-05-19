-- As part of deprecating cd, we want users who have cd'd deeper than their project root to be parked at the root instead.
UPDATE "most_recent_namespace"
SET "namespace" = json_array("namespace" -> 0, "namespace" -> 1, "namespace" -> 2, "namespace" -> 3)
WHERE "namespace" ->> 0 = '__projects'
  AND json_array_length("namespace") > 4;
