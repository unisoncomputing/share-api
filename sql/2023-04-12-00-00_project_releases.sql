-- Releases are just like branches, but with a few small differences:
-- 1. Releases have a 'status', one of 'draft', 'published', or 'deprecated'.
-- 2. Published releases are immutable, and can't be pushed to.
-- 3. There's no such thing as 'contributor' releases, they all belong to the project itself and must be pushed by
--    a project maintainer.
CREATE TABLE project_releases (
    id uuid DEFAULT uuid_generate_v4() PRIMARY KEY,

    -- The project this release belongs to
    project_id uuid NOT NULL REFERENCES projects (id) ON DELETE CASCADE,

    -- At the moment this is likely the version number, e.g. "1.2.3"
    -- but we haven't decided exactly how this will work yet.
    -- Once we nail down dependency management we may store the version number
    -- in a more structured format.
    version text NOT NULL CHECK (version <> ''),

    -- Status is deprecated if deprecated_at is set, published if published_at is set, and draft otherwise.
    status citext NOT NULL GENERATED ALWAYS AS (
        CASE WHEN deprecated_at IS NOT NULL THEN 'deprecated'
             WHEN published_at IS NOT NULL THEN 'published'
             ELSE 'draft'
        END
    ) STORED,

    -- The date the release was published, if it's published.
    published_at timestamptz NULL,

    published_by uuid NULL REFERENCES users (id) ON DELETE SET NULL,

    deprecated_at timestamptz NULL,
    deprecated_by uuid NULL REFERENCES users (id) ON DELETE SET NULL,

    -- The current HEAD of the release.
    causal_hash text NOT NULL,

    created_at timestamptz NOT NULL DEFAULT NOW(),

    -- Tracks who created this release.
    -- We keep releases around even if the creator is deleted.
    created_by uuid NULL REFERENCES users (id) ON DELETE SET NULL,


    updated_at timestamptz NOT NULL DEFAULT NOW(),
    -- whether (and when) the release was deleted
    deleted_at timestamptz NULL
);

-- Look up releases by version
-- No two living releases can have the same version within the same project
CREATE UNIQUE INDEX releases_by_project_id_and_version ON project_releases(project_id, version) WHERE deleted_at IS NULL;

-- Allow looking up releases within a project sorted by their updated_at time
CREATE INDEX releases_by_project_id_and_created_at ON project_releases(project_id, updated_at DESC) WHERE deleted_at IS NULL;

-- Allow looking up releases within a project sorted by their published_at time
CREATE INDEX releases_by_project_id ON project_releases(project_id, published_at DESC)
  WHERE published_at IS NOT NULL
        AND deleted_at IS NULL
        AND status = 'published';

CREATE TRIGGER project_releases_updated_at
  BEFORE UPDATE ON project_releases
  FOR EACH ROW
  EXECUTE PROCEDURE moddatetime (updated_at);

CREATE TABLE project_release_reflog (
    id uuid DEFAULT uuid_generate_v4() PRIMARY KEY,

    -- The release this reflog entry belongs to
    release_id uuid NOT NULL REFERENCES project_releases (id) ON DELETE CASCADE,

    -- A NULL old_causal_hash occurs when a release is first created.
    old_causal_hash text NULL,

    new_causal_hash text NOT NULL,

    -- The user who made the change
    user_id uuid NOT NULL REFERENCES users (id) ON DELETE SET NULL,

    -- What caused the change (fast forward, merge, push, etc.)
    description text NULL CHECK (description <> ''),

    created_at timestamptz NOT NULL DEFAULT NOW()
);

CREATE INDEX project_release_reflog_by_release_id ON project_release_reflog(release_id, created_at DESC);
