-- This table doesn't seem to actually relate the term to the parent namespace.
CREATE TABLE namespace_term (
  id INTEGER PRIMARY KEY NOT NULL,
  namesegment_id INTEGER NOT NULL REFERENCES text(id),
  referent_object_id INTEGER NOT NULL REFERENCES object(id),
  referent_component_id INTEGER NOT NULL,
  referent_constructor_id INTEGER NULL,
);

CREATE INDEX namespace_term_name ON namespace_term (
  namesegment_id
);

-- Allows looking up names for a given object, is that what we really want?
-- I suspect we'll more often want to look up by hash_id or builtin_text_id.
CREATE INDEX namespace_term_ref ON namespace_term (
  referent_object_id,
  referent_component_id,
  referent_constructor_id,
  namesegment_id
);

CREATE TABLE namespace_type (
  id INTEGER PRIMARY KEY NOT NULL,
  namesegment_id INTEGER NOT NULL REFERENCES text(id),
  reference_object_id INTEGER NOT NULL REFERENCES object(id),
  reference_component_id INTEGER NOT NULL,
);

CREATE INDEX namespace_type_name ON namespace_type(
  namesegment_id
);

CREATE INDEX namespace_type_reference ON namespace_type (
  reference_object_id, 
  reference_component_id, 
  namesegment_id
);

CREATE TABLE namespace_patch (
  id INTEGER PRIMARY KEY NOT NULL,
  namesegment_id INTEGER NOT NULL REFERENCES text(id),
  patch_object_id INTEGER NOT NULL REFERENCES object(id)
);

CREATE INDEX namespace_patch_name ON namespace_patch(
  namesegment_id, patch_object_id
);

CREATE TABLE namespace_child (
  -- needs a name field
  namespace_hash_id INTEGER NOT NULL REFERENCES hash(id),
  child_hash_id INTEGER NOT NULL REFERENCES hash(id),
  -- This primary key doesn't allow the same child at multiple names.
  PRIMARY KEY (namespace_hash_id, child_hash_id)
) WITHOUT ROWID;

-- This allows us to find all parents from any histories of a given child, do we actually want this? Chris can't think
-- of a case where we'd need it.
CREATE INDEX namespace_child_child ON namespace_child(child_hash_id, namespace_hash_id);

CREATE TABLE namespace_term_metadata (
  namespace_hash_id INTEGER NOT NULL REFERENCES hash(id),
  namespace_term_id INTEGER NOT NULL REFERENCES namespace_term(id),
  metadata_object_id INTEGER NOT NULL REFERENCES object(id),
  metadata_component_id INTEGER NOT NULL,
  PRIMARY KEY (
    namespace_hash_id,
    namespace_term_id,
    metadata_object_id,
    metadata_component_id
  )
) WITHOUT ROWID;

CREATE INDEX namespace_term_metadata_metadata 
 ON namespace_term_metadata(
   metadata_object_id, metadata_component_id
 );

CREATE TABLE namespace_type_metadata (
  namespace_hash_id INTEGER NOT NULL REFERENCES hash(id),
  namespace_term_id INTEGER NOT NULL REFERENCES namespace_term(id),
  metadata_object_id INTEGER NOT NULL REFERENCES object(id),
  metadata_component_id INTEGER NOT NULL,
  PRIMARY KEY (
    namespace_hash_id,
    namespace_term_id,
    metadata_object_id,
    metadata_component_id
  )
) WITHOUT ROWID;

-- When do we currently need to look up all names linked to a given piece of metadata?
-- Maybe I want to see all the code I've authored?
-- It's easy to add that index when we add that feature so I'd advocate leaving it off for now.
CREATE INDEX namespace_type_metadata_metadata 
 ON namespace_type_metadata(
   metadata_object_id, metadata_component_id
 );
