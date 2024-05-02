CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Function we use for generating text keys for indexes.
-- Use the function rather than digest directly so we can more easily change this if needed.
CREATE OR REPLACE FUNCTION text_hash(input text)
RETURNS bytea 
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT digest(input, 'sha256')
$$ LANGUAGE SQL;

-- Function we use for generating bytes keys for indexes.
-- Use the function rather than digest directly so we can more easily change this if needed.
CREATE OR REPLACE FUNCTION bytes_hash(input bytea)
RETURNS bytea 
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT digest(input, 'sha256')
$$ LANGUAGE SQL;

-- Table containing text chunks referenced by definitions.
-- Also contains names of builtins.
CREATE TABLE text (
  id SERIAL PRIMARY KEY,
  -- Some text values are too large to be part of an index, so we use a content-based hash instead.
  content_hash BYTEA NOT NULL GENERATED ALWAYS AS (text_hash(text)) STORED,
  text TEXT NOT NULL
);

CREATE UNIQUE INDEX text_by_content_hash ON text(content_hash);

-- Table containing raw bytes values, typically serialized terms or types.
-- We normalize them here by their content hash to avoid storing many copies of the same bytes
-- across codebases.
CREATE TABLE bytes (
    id SERIAL PRIMARY KEY,
    -- Some bytes values are too large to be part of an index, so we use a content-based hash instead.
    content_hash BYTEA NOT NULL GENERATED ALWAYS AS (bytes_hash(bytes)) STORED,
    bytes BYTEA NOT NULL
);

CREATE UNIQUE INDEX bytes_by_content_hash ON bytes(content_hash);

-- Table containing all terms we know about.
-- See sandboxed_terms for the actual contents of each term.
CREATE TABLE terms (
  id SERIAL PRIMARY KEY,

  component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE CASCADE,
  component_index INTEGER NOT NULL CHECK (component_index >= 0),

  -- Type Reference for the type of this term. Note: This is an arbitrary complex type,
  -- not necessarily a single decl, so there may not be an actual type-component saved for it.
  term_type_builtin INTEGER NULL REFERENCES text(id) ON DELETE NO ACTION,
  term_type_component_hash_id INTEGER NULL REFERENCES component_hashes(id) ON DELETE NO ACTION,
  term_type_component_index INTEGER NULL CHECK (term_type_component_index >= 0),

  CHECK (
    (term_type_builtin IS NOT NULL AND term_type_component_hash_id IS NULL AND term_type_component_index IS NULL) OR
    (term_type_builtin IS NULL AND term_type_component_hash_id IS NOT NULL AND term_type_component_index IS NOT NULL)
  )
);

CREATE UNIQUE INDEX terms_by_ref ON terms(component_hash_id, component_index)
  NULLS NOT DISTINCT;  -- These columns aren't nullable anyways, but this seems most future proof.

CREATE INDEX term_types_by_ref ON terms(term_type_builtin, term_type_component_hash_id, term_type_component_index)
  NULLS NOT DISTINCT;

CREATE TYPE decl_kind AS ENUM ('data', 'ability');
CREATE TYPE modifier_kind AS ENUM ('structural', 'unique');

-- Table containing all types (data-decls and abilities) we know about.
-- See sandboxed_types for the actual contents of each type.
CREATE TABLE types (
    id SERIAL PRIMARY KEY,

    component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE CASCADE,
    component_index INTEGER NOT NULL CHECK (component_index >= 0),

    kind decl_kind NOT NULL,
    modifier modifier_kind NOT NULL
);

CREATE UNIQUE INDEX types_by_ref ON types(component_hash_id, component_index)
  NULLS NOT DISTINCT;  -- These columns aren't nullable anyways, but this seems most future proof.

-- Additional information about the number and type of the constructors a given decl has.
-- All of this data can be derived from the type decl itself, but is useful
-- to have in a relational format.
CREATE TABLE constructors (
    id SERIAL PRIMARY KEY,

    type_id INTEGER NOT NULL REFERENCES types(id) ON DELETE CASCADE,
    constructor_index INTEGER NOT NULL CHECK (constructor_index >= 0),

    -- Type Reference for the type of this term. Note: This is an arbitrary complex type,
    -- not necessarily a single decl, so there may not be an actual type-component saved for it.
    constructor_type_builtin INTEGER NULL REFERENCES text(id) ON DELETE NO ACTION,
    constructor_type_component_hash_id INTEGER NULL REFERENCES component_hashes(id) ON DELETE NO ACTION,
    constructor_type_component_index INTEGER NULL CHECK (constructor_type_component_index >= 0),

    CHECK (
      (constructor_type_builtin IS NOT NULL AND constructor_type_component_hash_id IS NULL AND constructor_type_component_index IS NULL) OR
      (constructor_type_builtin IS NULL AND constructor_type_component_hash_id IS NOT NULL AND constructor_type_component_index IS NOT NULL)
    )
);

CREATE UNIQUE INDEX constructors_by_ref ON constructors(type_id, constructor_index)
  NULLS NOT DISTINCT; -- These columns aren't nullable anyways, but this seems most future proof.

-- Table containing the local component references for a term
CREATE TABLE term_local_component_references (
    term_id INTEGER NOT NULL REFERENCES terms(id) ON DELETE CASCADE,
    -- The local index the reference corresponds to.
    local_index INTEGER NOT NULL CHECK (local_index >= 0),
    -- The component hash the reference corresponds to.
    component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE NO ACTION,

    PRIMARY KEY (term_id, local_index)
);

CREATE INDEX term_local_component_references_by_dependency ON term_local_component_references(component_hash_id);

-- Table containing the local text references for a term
CREATE TABLE term_local_text_references (
    term_id INTEGER NOT NULL REFERENCES terms(id) ON DELETE CASCADE,
    -- The local index the reference corresponds to.
    local_index INTEGER NOT NULL CHECK (local_index >= 0),
    -- The text the text corresponds to.
    text_id INTEGER NOT NULL REFERENCES text(id) ON DELETE NO ACTION,

    PRIMARY KEY (term_id, local_index)
);

-- Index allowing finding terms by text search.
CREATE INDEX term_local_text_references_by_text_id ON term_local_text_references(text_id);

-- Table containing the actual bytes for a term.
CREATE TABLE sandboxed_terms (
    -- The user the term is sandboxed to.
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    -- This must point at a non-builtin term, but we don't have a way to enforce that.
    term_id INTEGER NOT NULL REFERENCES terms(id) ON DELETE CASCADE,

    -- A serialized SandboxedTermComponentElement
    bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,

    created_at TIMESTAMP NOT NULL DEFAULT NOW(),

    PRIMARY KEY (user_id, term_id) INCLUDE (bytes_id)
);

-- Table containing the local component references for a type
CREATE TABLE type_local_component_references (
    type_id INTEGER NOT NULL REFERENCES types(id) ON DELETE CASCADE,
    -- The local index the reference corresponds to.
    local_index INTEGER NOT NULL CHECK (local_index >= 0),
    -- The component hash the reference corresponds to.
    component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE NO ACTION,

    PRIMARY KEY (type_id, local_index)
);

CREATE INDEX type_local_component_references_by_dependency ON type_local_component_references(component_hash_id);

-- Table containing the local text references for a type
CREATE TABLE type_local_text_references (
    type_id INTEGER NOT NULL REFERENCES types(id) ON DELETE CASCADE,
    -- The local index the text corresponds to.
    local_index INTEGER NOT NULL CHECK (local_index >= 0),
    -- The text the reference corresponds to.
    text_id INTEGER NOT NULL REFERENCES text(id) ON DELETE NO ACTION,

    PRIMARY KEY (type_id, local_index)
);

-- Index allowing finding types by text search.
CREATE INDEX type_local_text_references_text_id_index ON type_local_text_references(text_id);

-- Table containing the actual bytes for a type.
CREATE TABLE sandboxed_types (
    -- The user the type is sandboxed to.
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    type_id INTEGER NOT NULL REFERENCES types(id) ON DELETE CASCADE,
    bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,

    created_at TIMESTAMP NOT NULL DEFAULT NOW(),

    PRIMARY KEY (user_id, type_id) INCLUDE (bytes_id)
);

CREATE TABLE patches (
    id SERIAL PRIMARY KEY,
    hash TEXT NOT NULL
);

CREATE UNIQUE INDEX patches_by_hash ON patches(hash);

CREATE TYPE patch_term_typing AS ENUM ('same', 'subtype', 'different');

CREATE TABLE patch_term_mappings (
    patch_id INTEGER NOT NULL REFERENCES patches(id) ON DELETE CASCADE,
    -- Can't use a term reference here because we need to be able to reference hashes which
    -- we may not actually have the code for.
    from_term_component_hash_id INTEGER NULL REFERENCES component_hashes(id) ON DELETE NO ACTION,
    from_term_component_index INTEGER NULL CHECK (from_term_component_index >= 0),
    from_term_builtin_id INTEGER NULL REFERENCES text(id) ON DELETE NO ACTION,

    CHECK (
      (from_term_component_hash_id IS NOT NULL AND from_term_component_index IS NOT NULL AND from_term_builtin_id IS NULL) OR
      (from_term_component_hash_id IS NULL AND from_term_component_index IS NULL AND from_term_builtin_id IS NOT NULL)
    ),

    -- The destination term MUST exist.
    to_term_id INTEGER NULL REFERENCES terms(id) ON DELETE NO ACTION,
    to_term_builtin_id INTEGER NULL REFERENCES text(id) ON DELETE NO ACTION,

    typing patch_term_typing NULL,
    deprecated BOOLEAN NOT NULL DEFAULT FALSE,

    CHECK (
        -- Deprecation
        (to_term_id IS NULL AND to_term_builtin_id IS NULL AND typing IS NULL AND deprecated = TRUE)
        OR
        -- Replacement
        ((to_term_id IS NOT NULL AND to_term_builtin_id IS NULL)
          OR (to_term_id IS NULL AND to_term_builtin_id IS NOT NULL))
           AND (typing IS NOT NULL AND deprecated = FALSE)
    )
);

CREATE UNIQUE INDEX patch_term_mappings_by_ref ON patch_term_mappings(patch_id, from_term_component_hash_id, from_term_component_index, from_term_builtin_id, to_term_id, to_term_builtin_id)
  NULLS NOT DISTINCT;

CREATE TABLE patch_type_mappings (
    patch_id INTEGER NOT NULL REFERENCES patches(id) ON DELETE CASCADE,
    from_type_component_hash_id INTEGER NULL REFERENCES component_hashes(id) ON DELETE NO ACTION,
    from_type_component_index INTEGER NULL CHECK (from_type_component_index >= 0),
    from_type_builtin_id INTEGER NULL REFERENCES text(id) ON DELETE NO ACTION,

    CHECK (
      (from_type_component_hash_id IS NOT NULL AND from_type_component_index IS NOT NULL AND from_type_builtin_id IS NULL) OR
      (from_type_component_hash_id IS NULL AND from_type_component_index IS NULL AND from_type_builtin_id IS NOT NULL)
    ),
    to_type_id INTEGER NULL REFERENCES types(id) ON DELETE NO ACTION,
    to_type_builtin_id INTEGER NULL REFERENCES text(id) ON DELETE NO ACTION,
    deprecated BOOLEAN NULL DEFAULT FALSE,

    CHECK (
      -- Deprecation
      (to_type_id IS NULL AND to_type_builtin_id IS NULL AND deprecated = TRUE)
      OR
      -- Replacement
      ((to_type_id IS NOT NULL AND to_type_builtin_id IS NULL)
        OR (to_type_id IS NULL AND to_type_builtin_id IS NOT NULL))
         AND deprecated = FALSE
    )
);

CREATE UNIQUE INDEX patch_type_mappings_by_ref ON patch_type_mappings(patch_id, from_type_component_hash_id, from_type_component_index, from_type_builtin_id, to_type_id, to_type_builtin_id)
  NULLS NOT DISTINCT;

-- Check on term -> constructor mappings
CREATE TABLE patch_constructor_mappings (
    patch_id INTEGER NOT NULL REFERENCES patches(id) ON DELETE CASCADE,

    from_constructor_component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE NO ACTION,
    from_constructor_component_index INTEGER NOT NULL CHECK (from_constructor_component_index >= 0),
    from_constructor_constructor_index INTEGER NOT NULL CHECK (from_constructor_constructor_index >= 0),

    to_constructor_id INTEGER NULL REFERENCES constructors(id) ON DELETE NO ACTION,

    typing patch_term_typing NULL,
    deprecated BOOLEAN NOT NULL DEFAULT FALSE,

    CHECK (
      (to_constructor_id IS NOT NULL AND typing IS NOT NULL AND deprecated = FALSE) OR
      (to_constructor_id IS NULL AND typing IS NULL AND deprecated = TRUE)
    )
);

CREATE UNIQUE INDEX patch_constructor_mappings_by_ref ON patch_constructor_mappings(patch_id, from_constructor_component_hash_id, from_constructor_component_index, from_constructor_constructor_index, to_constructor_id)
  NULLS NOT DISTINCT;

CREATE TABLE patch_ownership (
    patch_id INTEGER NOT NULL REFERENCES patches(id) ON DELETE NO ACTION,
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    PRIMARY KEY (user_id, patch_id)
);

-- Equivalent to the 'watch' tables in UCM, but Share doesn't really have
-- the concept of a 'watch' as such, so we just track evaluation results in general.
-- Useful for tracking test results and documentation evaluations.
CREATE TABLE eval_results (
  id SERIAL PRIMARY KEY,
  -- Not all source terms exist in the 'terms' table, (e.g. they may be sub-terms of a doc).
  -- so we use a reference instead.
  component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE CASCADE,
  component_index INTEGER NOT NULL CHECK (component_index >= 0)
);

CREATE UNIQUE INDEX eval_results_by_ref ON eval_results(component_hash_id, component_index)
  NULLS NOT DISTINCT; -- These columns aren't nullable anyways, but this seems most future proof.

-- Table containing the local component references for an eval result
CREATE TABLE eval_result_local_component_references (
    eval_result_id INTEGER NOT NULL REFERENCES eval_results(id) ON DELETE CASCADE,
    -- The local index the reference corresponds to.
    local_index INTEGER NOT NULL CHECK (local_index >= 0),
    -- The component hash the reference corresponds to.
    component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE NO ACTION,

    PRIMARY KEY (eval_result_id, local_index)
);

-- Table containing the local text references for an eval result
CREATE TABLE eval_result_local_text_references (
    eval_result_id INTEGER NOT NULL REFERENCES eval_results(id) ON DELETE CASCADE,
    -- The local index the reference corresponds to.
    local_index INTEGER NOT NULL CHECK (local_index >= 0),
    -- The text the text corresponds to.
    text_id INTEGER NOT NULL REFERENCES text(id) ON DELETE NO ACTION,

    PRIMARY KEY (eval_result_id, local_index)
);

CREATE TABLE sandboxed_eval_result (
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  eval_result_id INTEGER NOT NULL REFERENCES eval_results(id) ON DELETE CASCADE,
  result_bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,

  PRIMARY KEY (user_id, eval_result_id)
);


-- Links referents to their term or constructor.
-- TODO: Not sure if this is a good idea yet. Make sure to test performance if I use it.
CREATE VIEW derived_referents(component_hash_id, component_index, constructor_index, term_id, constructor_id, constructor_type_id) AS
SELECT component_hash_id, component_index, NULL, term.id, NULL, NULL
  FROM terms term
UNION ALL
SELECT typ.component_hash_id, typ.component_index, constr.constructor_index, NULL, constr.id, typ.id
  FROM constructors constr JOIN types typ ON (constr.type_id = typ.id);
