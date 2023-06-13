-- The most recent namespace that a user cd'd to.
-- This table should never have more than one row.
CREATE TABLE "most_recent_namespace" (
  -- A json array like ["foo", "bar"]; the root namespace is represented by the empty array
  "namespace" TEXT PRIMARY KEY NOT NULL
) WITHOUT ROWID;

INSERT INTO "most_recent_namespace" ("namespace") VALUES ('[]');
