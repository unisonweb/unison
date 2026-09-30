-- Add type alias support. Introduces a new ObjectType variant
-- (TypeAliasComponent = 4) alongside the existing TermComponent (0),
-- DeclComponent (1), Namespace (2), and Patch (3). The schema is
-- otherwise unchanged — aliases reuse the `object` and `temp_entity`
-- tables — but the new `type_id` value needs a corresponding row in
-- `object_type_description` to satisfy the foreign key.
INSERT INTO object_type_description (id, description) VALUES
    (4, "Type Alias");
