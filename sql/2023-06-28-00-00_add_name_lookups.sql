CREATE TABLE branch_hashes (
    id SERIAL PRIMARY KEY NOT NULL,
    base32 TEXT NOT NULL
);
CREATE UNIQUE INDEX branch_hashes_base32 ON branch_hashes(base32);

-- Note: We don't necessarily have a component saved for every hash in here,
-- since we also store the component-hashes of arbitrary term-types here. (E.g. `Nat -> Nat`)
-- Refer to the `terms` and `types` tables for all saved components.
CREATE TABLE component_hashes (
    id SERIAL PRIMARY KEY NOT NULL,
    base32 TEXT NOT NULL
);
CREATE UNIQUE INDEX component_hashes_base_32 ON component_hashes(base32);


-- This table allows us to look up which branch hashes have a name lookup.
CREATE TABLE name_lookups (
    root_branch_hash_id INTEGER PRIMARY KEY REFERENCES branch_hashes(id) ON DELETE CASCADE
);


-- Create the new tables.
CREATE TABLE scoped_term_name_lookup (
  root_branch_hash_id INTEGER NOT NULL REFERENCES name_lookups(root_branch_hash_id) ON DELETE CASCADE,

  -- The name of the term in reversed form, with a trailing '.':
  -- E.g. map.List.base.
  --
  -- The trailing '.' is helpful when performing suffix queries where we may not know
  -- whether the suffix is complete or not, e.g. we could suffix search using any of the
  -- following globs and it would still find 'map.List.base.':
  --  map.List.base.*
  --  map.List.*
  --  map.*
  reversed_name TEXT NOT NULL,

  -- The last name segment of the name. This is used when looking up names for
  -- suffixification when building PPEs.
  -- E.g. for the name 'base.List.map' this would be 'map'
  last_name_segment TEXT NOT NULL,

  -- The namespace containing this definition, not reversed, with a trailing '.'
  -- The trailing '.' simplifies GLOB queries, so that 'base.*' matches both things in
  -- 'base' and 'base.List', but not 'base1', which allows us to avoid an OR in our where
  -- clauses which in turn helps the sqlite query planner use indexes more effectively.
  --
  -- example value: 'base.List.'
  namespace TEXT NOT NULL,
  referent_builtin TEXT NULL,
  referent_component_hash_id INTEGER NULL REFERENCES component_hashes(id) ON DELETE CASCADE,
  referent_component_index INTEGER NULL,
  referent_constructor_index INTEGER NULL,
  referent_constructor_type INTEGER NULL
);

-- Effectively the primary key for this table, but it's not a valid primary key because
-- some columns are nullable.
CREATE INDEX scoped_term_name_lookup_key ON scoped_term_name_lookup(root_branch_hash_id, reversed_name, referent_builtin, referent_component_hash_id, referent_component_index, referent_constructor_index);




-- This index allows finding all names we need to consider within a given namespace for
-- suffixification of a name.
-- It may seem strange to use last_name_segment rather than a suffix search over reversed_name name here
-- but SQLite will only optimize for a single prefix-glob at once, so we can't glob search
-- over both namespace and reversed_name, but we can EXACT match on last_name_segment and
-- then glob search on the namespace prefix, and have SQLite do the final glob search on
-- reversed_name over rows with a matching last segment without using an index and should be plenty fast.
CREATE INDEX scoped_term_names_by_namespace_and_last_name_segment ON scoped_term_name_lookup(root_branch_hash_id, last_name_segment, namespace);

  -- This index allows us to find all names with a given ref within a specific namespace
CREATE INDEX scoped_term_name_by_referent_lookup ON scoped_term_name_lookup(root_branch_hash_id, referent_builtin, referent_component_hash_id, referent_component_index, referent_constructor_index, namespace);

-- Allows fetching ALL names within a specific namespace prefix.
CREATE INDEX scoped_term_names_by_namespace ON scoped_term_name_lookup(root_branch_hash_id, namespace);


CREATE TABLE scoped_type_name_lookup (
  root_branch_hash_id INTEGER NOT NULL REFERENCES name_lookups(root_branch_hash_id) ON DELETE CASCADE,

  -- The name of the term: E.g. List.base
  reversed_name TEXT NOT NULL,

  -- The last name segment of the name. This is used when looking up names for
  -- suffixification when building PPEs.
  -- E.g. for the name 'base.List.map' this would be 'map'
  last_name_segment TEXT NOT NULL,
  -- The namespace containing this definition, not reversed, with a trailing '.'
  -- The trailing '.' simplifies GLOB queries, so that 'base.*' matches both things in
  -- 'base' and 'base.List', but not 'base1', which allows us to avoid an OR in our where
  -- clauses which in turn helps the sqlite query planner use indexes more effectively.
  --
  -- example value: 'base.List.'
  namespace TEXT NOT NULL,
  reference_builtin TEXT NULL,
  reference_component_hash_id INTEGER NULL REFERENCES component_hashes(id) ON DELETE CASCADE,
  reference_component_index INTEGER NULL
);

-- Effectively the primary key for this table, but it's not a valid primary key because
-- some columns are nullable.
CREATE INDEX scoped_type_name_lookup_key ON scoped_type_name_lookup(root_branch_hash_id, reversed_name, reference_builtin, reference_component_hash_id, reference_component_index);


-- This index allows finding all names we need to consider within a given namespace for
-- suffixification of a name.
-- It may seem strange to use last_name_segment rather than a suffix search over reversed_name name here
-- but SQLite will only optimize for a single prefix-glob at once, so we can't glob search
-- over both namespace and reversed_name, but we can EXACT match on last_name_segment and
-- then glob search on the namespace prefix, and have SQLite do the final glob search on
-- reversed_name over rows with a matching last segment without using an index and should be plenty fast.
CREATE INDEX scoped_type_names_by_namespace_and_last_name_segment ON scoped_type_name_lookup(root_branch_hash_id, last_name_segment, namespace);

-- This index allows us to find all names with a given ref within a specific namespace.
CREATE INDEX scoped_type_name_by_reference_lookup ON scoped_type_name_lookup(root_branch_hash_id, reference_builtin, reference_component_hash_id, reference_component_index, namespace);

-- Allows fetching ALL names within a specific namespace prefix.
CREATE INDEX scoped_type_names_by_namespace ON scoped_type_name_lookup(root_branch_hash_id, namespace);

-- Adds the ability to associate other name-lookup indexes to a particular mount path within an index.
-- E.g. you might specify that `.lib.base` is defined by the name lookup index for branch hash #123

-- This table is used to associate a mount point with a particular name lookup index.
CREATE TABLE name_lookup_mounts (
    -- The the parent index we're mounting inside of.
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
