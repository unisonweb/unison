-- Adds the ability to associate other name-lookup indexes to a particular mount path within an index.
-- E.g. you might specify that `.lib.base` is defined by the name lookup index for branch hash #123

-- This table is used to associate a mount point with a particular name lookup index.
CREATE TABLE name_lookup_mounts (
    -- The the parent index we're mounting inside of.
    parent_root_branch_hash_id INTEGER NOT NULL REFERENCES name_lookups(root_branch_hash_id) ON DELETE CASCADE,
    -- The index we're mounting.
    -- Don't allow deleting a mounted name lookup while it's still mounted in some other index.
    mounted_root_branch_hash_id INTEGER NOT NULL REFERENCES name_lookups(root_branch_hash_id) ON DELETE RESTRICT,
    -- The namespace which will point at the index, relative to the parent index.
    -- E.g. `lib.base`
    mount_path TEXT NOT NULL CHECK (mount_path <> ''),
    PRIMARY KEY (parent_root_branch_hash_id, mount_path)
);
