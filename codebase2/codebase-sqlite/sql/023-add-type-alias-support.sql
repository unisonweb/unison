-- Bump schema version to signal that this codebase has been touched by a
-- UCM that knows about type aliases (ObjectType.TypeAliasComponent = 4,
-- TempEntityType.TypeAliasComponentType = 5).
--
-- These new enum values reuse the existing `object` and `temp_entity`
-- tables, so no schema change is required. This migration exists to mark
-- the version boundary; an older UCM that does not know how to handle the
-- new ObjectType values will refuse to open this codebase.
SELECT 1;
