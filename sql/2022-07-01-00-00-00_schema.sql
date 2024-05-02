-- Allow auto-generating uuids
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE users (
  id uuid PRIMARY KEY,
  primary_email text NOT NULL CHECK (length(primary_email) > 0),
  email_verified boolean NOT NULL,
  avatar_url text NULL CHECK (length(avatar_url) > 0),
  name text NULL CHECK (length(name) > 0),
  -- Handles must be lowercase because we want to assert case-insensitive uniqueness.
  -- Alternatively we could add a unique index on the lowercased handle, but this is easier
  -- and saves us an index.
  handle text UNIQUE NOT NULL CHECK (length(handle) > 0 AND handle = lower(handle)),
  created_at timestamp NOT NULL DEFAULT NOW()
);


CREATE UNIQUE INDEX user_email ON users(lower(primary_email));
CREATE INDEX users_by_handle ON users (lower(handle) text_pattern_ops);
CREATE INDEX users_by_name ON users (lower(name) text_pattern_ops);


-- This is a relation between unison users and a linked github account.
CREATE TABLE github_users (
  github_user_id integer PRIMARY KEY UNIQUE,
  unison_user_id uuid NOT NULL UNIQUE,
  FOREIGN KEY (unison_user_id) REFERENCES users (id) ON DELETE CASCADE
);

-- Stores which tours/flows a user has accomplished, or notifications they've seen.
-- E.g. End-user license agreement, or welcome-tour
CREATE TABLE tours (
  user_id uuid NOT NULL,
  tour_id text NOT NULL,
  FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE,
  PRIMARY KEY(user_id, tour_id)
);


-- (organization_user_id, member_user_id) indicates that the user at member_user_id
CREATE TABLE org_members (
    organization_user_id uuid NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    member_user_id uuid NOT NULL REFERENCES users (id) ON DELETE CASCADE,

    PRIMARY KEY (organization_user_id, member_user_id),

    -- Orgs cannot be members of themselves.
    CHECK (organization_user_id <> member_user_id)
);

CREATE INDEX organizations_by_member ON org_members(member_user_id);

-- Stores the latest push for each user.
-- This table is used for tracking push metrics.
CREATE TABLE latest_push (
  user_id uuid PRIMARY KEY NOT NULL,
  pushed_at timestamp NOT NULL DEFAULT NOW(),

  FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE
);


CREATE INDEX latest_push_by_pushed_at ON latest_push(pushed_at, user_id);

-- This view is helpful for excluding unison employees from metrics.
CREATE VIEW external_users(id, handle)
  AS SELECT id, handle
  FROM users
    WHERE 
          -- exclude members of the 'unison' org
          NOT EXISTS (SELECT FROM org_members
                      JOIN users AS org_user ON organization_user_id = org_user.id
                      WHERE member_user_id = users.id
                            AND org_user.handle = 'unison'
                     )
          -- exclude orgs
          AND NOT EXISTS (SELECT FROM org_members WHERE organization_user_id = users.id);
