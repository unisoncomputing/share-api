CREATE TABLE loose_code_roots (
    -- The user this is the loose code root for
    user_id uuid PRIMARY KEY REFERENCES users (id) ON DELETE CASCADE,

    -- The hash of the loose code root
    causal_hash text NOT NULL,

    updated_at timestamptz NOT NULL DEFAULT NOW()
);


CREATE TRIGGER loose_code_roots_updated_at
  BEFORE UPDATE ON loose_code_roots
  FOR EACH ROW
  EXECUTE PROCEDURE moddatetime (updated_at);


CREATE TABLE loose_code_reflog (
    id uuid DEFAULT uuid_generate_v4() PRIMARY KEY,

    -- The user this is the loose code reflog for
    loose_code_user_id uuid NOT NULL REFERENCES users (id) ON DELETE CASCADE,

    -- A NULL old_causal_hash occurs for the first push
    old_causal_hash text NULL,

    new_causal_hash text NOT NULL,

    -- The user who made the change
    editor_user_id uuid NOT NULL REFERENCES users (id) ON DELETE SET NULL,

    -- What caused the change (fast forward, merge, push, etc.)
    description text NULL CHECK (description <> ''),

    created_at timestamptz NOT NULL DEFAULT NOW()
);

CREATE INDEX loose_code_reflog_by_loose_code_user_id ON loose_code_reflog(loose_code_user_id, created_at DESC);
