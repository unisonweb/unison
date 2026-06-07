-- Add opaque declaration support. Introduces a new ObjectType variant
-- (OpaqueDeclarationComponent = 5) alongside the existing TermComponent (0),
-- DeclComponent (1), Namespace (2), Patch (3), and TypeAliasComponent (4).
-- The schema is otherwise unchanged — opaque declarations reuse the `object`
-- and `temp_entity` tables — but the new `type_id` value needs a corresponding
-- row in `object_type_description` to satisfy the foreign key.
--
-- TODO(opaque): the `opaque_body_membership` table from plan §2.2 lands in a
-- follow-up commit alongside the membership read/write API.
INSERT INTO object_type_description (id, description) VALUES
    (5, "Opaque Declaration");
