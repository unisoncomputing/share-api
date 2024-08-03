CREATE TABLE serialized_components (
    -- The user the term is sandboxed to.
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE CASCADE,

    -- The serialized component
    bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,

    created_at TIMESTAMP NOT NULL DEFAULT NOW(),

    PRIMARY KEY (user_id, component_hash_id) INCLUDE (bytes_id)
);

-- namespaces don't need to be sandboxed to user.
CREATE TABLE serialized_namespaces (
    namespace_hash_id NOT NULL REFERENCES branch_hashes(id) ON DELETE NO ACTION,

    -- The serialized namespace
    bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,

    created_at TIMESTAMP NOT NULL DEFAULT NOW(),

    PRIMARY KEY (namespace_hash_id) INCLUDE (bytes_id)
);

CREATE TABLE serialized_patches (
  patch_id INTEGER NOT NULL REFERENCES patches(id) ON DELETE CASCADE,
  bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
);

CREATE TABLE serialized_causals (
  causal_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE CASCADE,
  bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
);
