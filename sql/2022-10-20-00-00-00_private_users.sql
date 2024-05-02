-- Ideally, codebases would be private instead of the user itself, but for right now the two are synonymous.
-- If a proper codebase abstraction is added in the future, this field should be migrated there and deleted from the
-- user.
ALTER TABLE users
  ADD COLUMN private boolean NOT NULL DEFAULT false;
