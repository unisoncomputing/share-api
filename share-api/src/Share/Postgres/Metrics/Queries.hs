module Share.Postgres.Metrics.Queries where

import Data.Time (UTCTime)
import Share.Postgres qualified as PG
import Share.Prelude

usersInteractedWithTickets :: PG.Transaction e Int64
usersInteractedWithTickets = do
  PG.queryExpect1Col
    [PG.sql|
        SELECT COUNT(DISTINCT(actor))
          FROM ticket_status_events
      |]

usersInteractedWithContributions :: PG.Transaction e Int64
usersInteractedWithContributions = do
  PG.queryExpect1Col
    [PG.sql|
        SELECT COUNT(DISTINCT(actor))
          FROM contribution_status_events
      |]

numUniqueUsersPushedSince :: UTCTime -> PG.Transaction e Int64
numUniqueUsersPushedSince sinceTime = do
  PG.queryExpect1Col
    [PG.sql|
    SELECT COUNT(DISTINCT user_id)
      FROM project_branch_reflog
      WHERE created_at >= #{sinceTime}
  |]

numUniqueUsersWithAPush :: PG.Transaction e Int64
numUniqueUsersWithAPush = do
  PG.queryExpect1Col
    [PG.sql|
      SELECT COUNT(DISTINCT user_id) FROM project_branch_reflog
  |]

allUsersCount :: PG.Transaction e Int64
allUsersCount =
  PG.queryExpect1Col [PG.sql| SELECT COUNT(*) FROM users |]

-- | Returns the number of total projects (private, public, total)
allProjectsCount :: PG.Transaction e (Int64, Int64, Int64)
allProjectsCount = do
  private <-
    PG.queryExpect1Col
      [PG.sql|
        SELECT COUNT(*)
          FROM projects
            WHERE private;
      |]
  public <-
    PG.queryExpect1Col
      [PG.sql|
        SELECT COUNT(*)
          FROM projects
            WHERE NOT private;
      |]
  pure (private, public, private + public)

-- | Returns the total number of total release downloads for the last (day, 7 days, 30 days)
releaseDownloadsGauges :: PG.Transaction e (Int64, Int64, Int64)
releaseDownloadsGauges = do
  daily <-
    PG.queryExpect1Col
      [PG.sql|
        SELECT COALESCE(SUM(downloads), 0)
          FROM project_release_daily_downloads
            WHERE day >= NOW() - INTERVAL '1 day';
      |]
  weekly <-
    PG.queryExpect1Col
      [PG.sql|
        SELECT COALESCE(SUM(downloads), 0)
          FROM project_release_daily_downloads
            WHERE day >= NOW() - INTERVAL '1 week';
      |]
  monthly <-
    PG.queryExpect1Col
      [PG.sql|
        SELECT COALESCE(SUM(downloads), 0)
          FROM project_release_daily_downloads
            WHERE day >= NOW() - INTERVAL '1 month';
      |]
  pure (daily, weekly, monthly)

-- | Users who have access to cloud.
numCloudUsers :: PG.Transaction e Int64
numCloudUsers =
  PG.queryExpect1Col [PG.sql| SELECT COUNT(*) FROM cloud_users |]

-- Number of total definitions on /main branch of all Share projects (including private)
numTotalPublicOrPrivateDefinitions :: PG.Transaction e Int64
numTotalPublicOrPrivateDefinitions =
  PG.queryExpect1Col
    [PG.sql|
WITH namespaces(handle, slug, name, namespace_hash_id, private) AS (
  SELECT u.handle, p.slug, pb.name, c.namespace_hash_id, p.private
    FROM project_branches pb
      JOIN projects p ON pb.project_id = p.id
      JOIN users u ON u.id = p.owner_user_id
      JOIN causals c ON pb.causal_id = c.id
      WHERE pb.name = 'main'
        AND pb.deleted_at IS NULL
        AND pb.contributor_id IS NULL
        -- We don't want to count the website among the definitions
        AND NOT (slug = 'website' AND handle = 'unison')
        -- Exclude private projects from team-arya members, there are a lot of test projects
        AND NOT (handle IN ('aryairani', 'chrispenner', 'mitchellwrosen', 'tstat') AND p.private)
        -- Exclude projects by unison org members which share a name with a unison project
        AND NOT (EXISTS (SELECT FROM org_members
                         JOIN users org ON org_members.organization_user_id = org.id
                         AND org_members.member_user_id = u.id WHERE org.handle = 'unison'
                        )
                 AND EXISTS(SELECT FROM projects unison_project
                            JOIN users unison_user on unison_project.owner_user_id = unison_user.id
                            WHERE unison_user.handle = 'unison' AND p.slug = unison_project.slug)
                )
  )
  SELECT COALESCE(sum(
    (SELECT count(*) FROM scoped_term_name_lookup WHERE namespace_hash_id = root_branch_hash_id)
      +
    (SELECT count(*) FROM scoped_type_name_lookup WHERE namespace_hash_id = root_branch_hash_id)
) :: bigint, 0 :: bigint) as num
  FROM namespaces
    |]

-- Number of total definitions on /main branch of all Share projects (excluding private)
numTotalPublicDefinitions :: PG.Transaction e Int64
numTotalPublicDefinitions =
  PG.queryExpect1Col
    [PG.sql|
WITH namespaces(handle, slug, name, namespace_hash_id, private) AS (
  SELECT u.handle, p.slug, pb.name, namespace_hash_id, p.private
    FROM project_branches pb
      JOIN projects p ON pb.project_id = p.id
      JOIN users u ON u.id = p.owner_user_id
      JOIN causals c ON pb.causal_id = c.id
      WHERE pb.name = 'main'
        AND pb.deleted_at IS NULL
        AND pb.contributor_id IS NULL
        AND NOT p.private
        -- We don't want to count the website among the definitions
        AND NOT (slug = 'website' AND handle = 'unison')
        -- Exclude private projects from team-arya members, there are a lot of test projects
        AND NOT (handle IN ('aryairani', 'chrispenner', 'mitchellwrosen', 'tstat') AND p.private)
        -- Exclude projects by unison org members which share a name with a unison project
        AND NOT (EXISTS (SELECT FROM org_members
                         JOIN users org ON org_members.organization_user_id = org.id
                         AND org_members.member_user_id = u.id WHERE org.handle = 'unison'
                        )
                 AND EXISTS(SELECT FROM projects unison_project
                            JOIN users unison_user on unison_project.owner_user_id = unison_user.id
                            WHERE unison_user.handle = 'unison' AND p.slug = unison_project.slug)
                )
  )
  SELECT COALESCE(sum(
    (SELECT count(*) FROM scoped_term_name_lookup WHERE namespace_hash_id = root_branch_hash_id)
      +
    (SELECT count(*) FROM scoped_type_name_lookup WHERE namespace_hash_id = root_branch_hash_id)
) :: bigint, 0 :: bigint) as num
  FROM namespaces
    |]

numCausalDiffQueueEntries :: PG.Transaction e Int64
numCausalDiffQueueEntries =
  PG.queryExpect1Col
    [PG.sql| SELECT COUNT(*) FROM causal_diff_queue |]
