CREATE TABLE personal_keys (
  id SERIAL PRIMARY KEY,
  -- The public JWK for this key.
  -- This may be null if the key is not yet registered.
  public_jwk jsonb NULL,
  thumbprint TEXT UNIQUE NOT NULL,
  -- The user registered to this key, which is proven by providing a signed 
  -- assertion using the key. 
  -- It may be null if the key is not yet registered to a user.
  user_id UUID NULL REFERENCES users(id) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

  -- Ensure that public_jwk and user_id are either both null or both non-null.
  CHECK ((public_jwk IS NULL) = (user_id IS NULL))
);

CREATE INDEX idx_personal_keys_user_id ON personal_keys(user_id, created_at)
  WHERE user_id IS NOT NULL;
CREATE INDEX idx_personal_keys_thumbprint ON personal_keys(thumbprint) INCLUDE (user_id);

CREATE TABLE history_comments (
  id SERIAL PRIMARY KEY,
  causal_id INTEGER NOT NULL REFERENCES causals(id),
  author TEXT NOT NULL,

  -- Milliseconds since epoch,
  -- This is stored as an exact integer rather than a TIMESTAMPTZ because we want
  -- to avoid floating point slop since it's used in hashing.
  created_at_ms BIGINT NOT NULL,

  -- This is the time we inserted the comment into our database, 
  -- NOT the time the comment was created by the author.
  discovered_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

  -- The Hash of the comment, SHA-512 over (hashing_version, causal_id, author, created_at_ms)
  comment_hash TEXT UNIQUE NOT NULL,
  author_key_id INTEGER NOT NULL REFERENCES personal_keys(id)
);

CREATE INDEX idx_history_comments_causal_id ON history_comments(causal_id);

CREATE TABLE history_comment_revisions (
  id INTEGER PRIMARY KEY,
  comment_id INTEGER NOT NULL REFERENCES history_comments(id),
  subject TEXT NOT NULL,
  contents TEXT NOT NULL,

  created_at_ms BIGINT NOT NULL,

  -- This is the time we inserted the comment revision into our database,
  -- NOT the time the comment revision was created by the author.
  discovered_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

  -- In a distributed system you really can’t ever truly delete comments,
  -- but you can ask to hide them.
  hidden BOOL NOT NULL DEFAULT FALSE,

  -- The Hash of the comment revision, SHA-512 over the canonical CBOR encoding of (hashing_version, comment-hash, subject, content, hidden, created_at_ms)
  revision_hash TEXT UNIQUE NOT NULL,

  --  The signature of the comment's author on the revision hash.
  author_signature BYTEA NOT NULL
);

CREATE INDEX idx_history_comment_revisions_comment_id ON history_comment_revisions(comment_id, created_at_ms DESC);

-- Tracks when each comment was discovered by each project, which allows us to
-- use a simple timestamp-based approach to filter for new comments since last sync
-- on a given project.
CREATE TABLE history_comment_revisions_project_discovery (
  comment_revision_id INTEGER NOT NULL REFERENCES history_comment_revisions(id),
  project_id UUID NOT NULL REFERENCES projects(id),
  discovered_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (project_id, comment_revision_id)
);

CREATE INDEX idx_history_comment_revisions_project_discovery_project_id ON history_comment_revisions_project_discovery(project_id, discovered_at);
