CREATE TABLE project_maintainers (
    project_id UUID NOT NULL REFERENCES projects (id) ON DELETE CASCADE,
    user_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    can_view BOOLEAN NOT NULL DEFAULT FALSE,
    can_maintain BOOLEAN NOT NULL DEFAULT FALSE,
    can_admin BOOLEAN NOT NULL DEFAULT FALSE,
    PRIMARY KEY (project_id, user_id)
);

CREATE INDEX project_maintainers_by_user_id ON project_maintainers (user_id) INCLUDE (project_id);

-- This view contains all projects which have access to premium features.
-- This currently means they are either:
--
-- * owned by a an active subscriber of cloud,
-- * are public
CREATE VIEW premium_projects AS
  SELECT project.id AS project_id, project.owner_user_id as project_owner_user_id
    FROM projects project
    WHERE NOT project.private
          OR EXISTS (SELECT FROM public.cloud_subscribers project_owner_subscription
                      WHERE project_owner_subscription.user_id = project.owner_user_id
                        AND project_owner_subscription.is_active
                     )
;

DROP VIEW accessible_private_projects;
CREATE VIEW accessible_private_projects AS
SELECT
  p.owner_user_id AS user_id,
  p.id AS project_id,
  p.owner_user_id AS owner_user_id
    FROM projects p
UNION
SELECT
  org.member_user_id AS user_id,
  p.id AS project_id,
  org.organization_user_id AS project_owner_user_id
  FROM org_members org JOIN projects p ON org.organization_user_id = p.owner_user_id
UNION
-- Include projects the user is a maintainer of
SELECT
  pm.user_id AS user_id,
  pm.project_id AS project_id,
  p.owner_user_id AS project_owner_user_id
  FROM project_maintainers pm JOIN projects p ON pm.project_id = p.id
    WHERE EXISTS (SELECT FROM premium_projects pp WHERE pp.project_id = pm.project_id)
;
