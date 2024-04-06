-- Adds the ability to associate other name-lookup indexes to a particular mount path within an index.
-- E.g. you might specify that `.lib.base` is defined by the name lookup index for branch hash #123

-- This table is used to associate a mount point with a particular name lookup index.
CREATE TABLE name_lookup_mounts (
    -- The parent index we're mounting inside of.
    parent_root_branch_hash_id INTEGER NOT NULL REFERENCES name_lookups(root_branch_hash_id) ON DELETE CASCADE,
    -- The index we're mounting.
    -- Don't allow deleting a mounted name lookup while it's still mounted in some other index,
    -- unless it's deleted in the same transaction.
    mounted_root_branch_hash_id INTEGER NOT NULL REFERENCES name_lookups(root_branch_hash_id) DEFERRABLE INITIALLY DEFERRED,

    -- The namespace which will point at the index, relative to the parent index, with a trailing dot.
    --
    -- E.g. `lib.base.`
    --
    -- The trailing '.' simplifies GLOB queries, so that 'base.*' matches both things in
    -- 'base' and 'base.List', but not 'base1', which allows us to avoid an OR in our where
    -- clauses which in turn helps the sqlite query planner use indexes more effectively.
    mount_path TEXT NOT NULL CHECK (mount_path <> ''),

    -- The reversed segments of the mount_path, with a trailing dot.
    --
    -- E.g. `base.lib.`
    --
    -- This allows us to reconstruct the full reversed names of definitions within this mount.
    --
    -- Like this:
    --
    -- reversed_name = "map.List.data."
    -- reversed_mount_path = "base.lib."
    -- full_reversed_name = reversed_name || reversed_mount_path
    -- i.e. "map.List.data.base.lib."
    reversed_mount_path TEXT NOT NULL CHECK (reversed_mount_path <> ''),
    PRIMARY KEY (parent_root_branch_hash_id, mount_path)
);
