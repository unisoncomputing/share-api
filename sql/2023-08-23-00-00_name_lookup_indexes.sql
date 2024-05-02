-- The previous name lookup indexes worked well in SQLite, but Postgres doesn't
-- handle NULLs in indexes very well, so we drop all of those indexes and use
-- partial indexes instead.

-- * TERM NAME LOOKUPS *

-- This index had nullable columns it didn't need, so we shrink it to what we actually need.
DROP INDEX scoped_term_name_lookup_key;
CREATE INDEX scoped_term_name_lookup_by_reversed_name ON scoped_term_name_lookup(root_branch_hash_id, reversed_name text_pattern_ops);

-- This index isn't working as intended because it has nullable columns, so we rebuild it as partial indexes.
DROP INDEX scoped_term_name_by_referent_lookup;

-- As long as we provide a condition on whether the referent is a builtin or not
-- these indexes work well.
CREATE INDEX scoped_term_name_by_referent_lookup_builtins ON scoped_term_name_lookup(root_branch_hash_id, referent_builtin, namespace text_pattern_ops)
  WHERE referent_builtin IS NOT NULL;
CREATE INDEX scoped_term_name_by_referent_lookup_non_builtins ON scoped_term_name_lookup(root_branch_hash_id, referent_component_hash_id, referent_component_index, referent_constructor_index, namespace text_pattern_ops)
  WHERE referent_builtin IS NULL;

-- Used for suffixification queries.
DROP INDEX scoped_term_names_by_namespace_and_last_name_segment;
CREATE INDEX scoped_term_names_by_namespace_and_last_name_segment ON scoped_term_name_lookup(root_branch_hash_id, last_name_segment text_pattern_ops, namespace text_pattern_ops);

-- This index remains unchanged, it allows fetching ALL names within a specific namespace prefix.
-- CREATE INDEX scoped_term_names_by_namespace ON scoped_term_name_lookup(root_branch_hash_id, namespace);

-- * TYPE NAME LOOKUPS *

-- This index had nullable columns it didn't need, so we shrink it to what we actually need.
DROP INDEX scoped_type_name_lookup_key;
CREATE INDEX scoped_type_name_lookup_by_reversed_name ON scoped_type_name_lookup(root_branch_hash_id, reversed_name text_pattern_ops);

-- As long as we provide a condition on whether the reference is a builtin or not
-- these indexes work well.
CREATE INDEX scoped_type_name_by_reference_lookup_builtins ON scoped_type_name_lookup(root_branch_hash_id, reference_builtin, namespace text_pattern_ops)
  WHERE reference_builtin IS NOT NULL;
CREATE INDEX scoped_type_name_by_reference_lookup_non_builtins ON scoped_type_name_lookup(root_branch_hash_id, reference_component_hash_id, reference_component_index, namespace text_pattern_ops)
  WHERE reference_builtin IS NULL;

-- This index remains unchanged, it's used for suffixification queries.
DROP INDEX scoped_type_names_by_namespace_and_last_name_segment;
CREATE INDEX scoped_type_names_by_namespace_and_last_name_segment ON scoped_type_name_lookup(root_branch_hash_id, last_name_segment text_pattern_ops, namespace text_pattern_ops);

DROP INDEX scoped_type_names_by_namespace;
CREATE INDEX scoped_type_names_by_namespace ON scoped_type_name_lookup(root_branch_hash_id, namespace text_pattern_ops);
