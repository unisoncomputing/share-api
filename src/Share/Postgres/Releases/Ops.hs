module Share.Postgres.Releases.Ops (createRelease) where

import Share.IDs
import Share.Notifications.Queries qualified as NotifQ
import Share.Notifications.Types (NotificationEvent (..), NotificationEventData (..), ReleaseData (..))
import Share.Postgres qualified as PG
import Share.Postgres.IDs
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.Projects.Queries qualified as ProjectsQ
import Share.Postgres.Search.DefinitionSearch.Queries qualified as DDQ
import Share.Release

createRelease ::
  (PG.QueryM m) =>
  NameLookupReceipt ->
  ProjectId ->
  ReleaseVersion ->
  CausalId ->
  CausalId ->
  UserId ->
  m (Release CausalId UserId)
createRelease !_nlReceipt projectId (releaseVersion@ReleaseVersion {major, minor, patch}) squashedCausalId unsquashedCausalId creatorId = do
  release@Release {releaseId} <-
    PG.queryExpect1Row
      [PG.sql|
        INSERT INTO project_releases(
          project_id,
          created_by,
          squashed_causal_id,
          unsquashed_causal_id,
          major_version,
          minor_version,
          patch_version
        )
        VALUES (#{projectId}, #{creatorId}, #{squashedCausalId}, #{unsquashedCausalId}, #{major}, #{minor}, #{patch})
        RETURNING
          id,
          project_id,
          unsquashed_causal_id,
          squashed_causal_id,
          created_at,
          updated_at,
          created_at,
          created_by,
          deprecated_at,
          deprecated_by,
          created_by,
          major_version,
          minor_version,
          patch_version
      |]
  DDQ.submitReleaseToBeSynced releaseId
  (projectData, projectResourceId, projectOwnerUserId) <- ProjectsQ.projectNotificationData projectId
  let releaseData =
        ReleaseData
          { releaseId,
            releaseVersion
          }
  let notifEvent =
        NotificationEvent
          { eventId = (),
            eventOccurredAt = (),
            eventResourceId = projectResourceId,
            eventData = ProjectReleaseCreatedData projectData releaseData,
            eventScope = projectOwnerUserId,
            eventActor = creatorId
          }
  NotifQ.recordEvent notifEvent
  pure release
