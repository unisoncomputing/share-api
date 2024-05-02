-- This view contains a mapping of users to all the private projects they have access to,
-- including those they have access to through organizations.
-- We don't include public project here because they are accessible to everyone.
-- and that predicate is better expressed using a `WHERE NOT p.private` clause

CREATE VIEW accessible_private_projects AS
SELECT
  p.owner_user_id AS user_id,
  p.id AS project_id,
  p.owner_user_id AS owner_user_id
    FROM projects p
UNION ALL -- We can UNION ALL because a user should never be a member of themself as an org
SELECT
  org.member_user_id AS user_id,
  p.id AS project_id,
  org.organization_user_id AS project_owner_user_id
  FROM org_members org JOIN projects p ON org.organization_user_id = p.owner_user_id;
