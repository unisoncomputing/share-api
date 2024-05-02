-- Structured release versioning

-- drop the old unique index based on 'version'
DROP INDEX releases_by_project_id_and_version;

ALTER TABLE project_releases
  ADD COLUMN major_version INTEGER NULL,
  ADD COLUMN minor_version INTEGER NULL,
  ADD COLUMN patch_version INTEGER NULL
  ;

-- Set version numbers
UPDATE project_releases
  SET major_version = (string_to_array(version, '.')::int[])[1],
      minor_version = (string_to_array(version, '.')::int[])[2],
      patch_version = (string_to_array(version, '.')::int[])[3];

-- Set non-nullable versions.
ALTER TABLE project_releases
  ALTER COLUMN major_version SET NOT NULL,
  ALTER COLUMN minor_version SET NOT NULL,
  ALTER COLUMN patch_version SET NOT NULL,
  DROP COLUMN version
  ;

-- Recreate unique index
CREATE UNIQUE INDEX releases_by_project_id_and_version ON project_releases(project_id, major_version, minor_version, patch_version)
  WHERE deleted_at IS NULL;
