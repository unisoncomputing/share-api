
CREATE TABLE unfinished_causal_depths (
  id INTEGER PRIMARY KEY REFERENCES causals (id) ON DELETE CASCADE
);

CREATE TABLE unfinished_namespace_depths (
  id INTEGER PRIMARY KEY REFERENCES branch_hashes (id) ON DELETE CASCADE
);

CREATE TABLE unfinished_patch_depths (
  id INTEGER PRIMARY KEY REFERENCES patches (id) ON DELETE CASCADE
);

CREATE TABLE unfinished_component_depths (
  id INTEGER PRIMARY KEY REFERENCES component_hashes (id) ON DELETE CASCADE
);

INSERT INTO unfinished_causal_depths (id)
  SELECT c.id
    FROM causals c
    WHERE NOT EXISTS (
      SELECT FROM causal_depth cd WHERE cd.causal_id = c.id
    );

INSERT INTO unfinished_namespace_depths (id)
  SELECT n.namespace_hash_id
    FROM namespaces n
    WHERE NOT EXISTS (
      SELECT FROM namespace_depth nd WHERE nd.namespace_hash_id = n.namespace_hash_id
    );

INSERT INTO unfinished_patch_depths (id)
  SELECT p.id
    FROM patches p
    WHERE NOT EXISTS (
      SELECT FROM patch_depth pd WHERE pd.patch_id = p.id
    );

INSERT INTO unfinished_component_depths (id)
  SELECT ch.id
    FROM component_hashes ch
    WHERE NOT EXISTS (
      SELECT FROM component_depth cd WHERE cd.component_hash_id = ch.id
    );  

-- Afterwards
DROP TABLE unfinished_causal_depths;
DROP TABLE unfinished_namespace_depths;
DROP TABLE unfinished_patch_depths;
DROP TABLE unfinished_component_depths;
