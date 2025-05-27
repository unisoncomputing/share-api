-- Include projects which are part of a commercial org as premium projects.

-- This view contains all projects which have access to premium features.
-- This currently means they are either:
--
-- * owned by a an active subscriber of cloud,
-- * are public
-- * are part of a commercial org
CREATE OR REPLACE VIEW premium_projects AS
  SELECT project.id AS project_id, project.owner_user_id as project_owner_user_id
    FROM projects project
    WHERE NOT project.private
          OR EXISTS (SELECT FROM public.cloud_subscribers project_owner_subscription
                      WHERE project_owner_subscription.user_id = project.owner_user_id
                        AND project_owner_subscription.is_active
                     )
          OR EXISTS (SELECT FROM orgs org
                      WHERE org.user_id = project.owner_user_id
                        AND org.is_commercial
                     );
