module Share.Web.Share.Webhooks.Impl (server) where

import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Servant
import Share.BackgroundJobs.Webhooks.Types
import Share.Env qualified as Env
import Share.IDs
import Share.JWT
import Share.JWT qualified as JWT
import Share.Notifications.Types
import Share.Prelude
import Share.Web.App
import Share.Web.Errors (respondError)
import Share.Web.Share.Webhooks.API (WebhookPayloadExamples)
import Share.Web.Share.Webhooks.API qualified as WebhooksAPI
import Share.Web.UI.Links qualified as Links

server :: ServerT WebhooksAPI.API WebApp
server =
  WebhooksAPI.Routes
    { WebhooksAPI.payloadExamples = exampleEndpoint
    }

-- data NotificationTopic
--   = ProjectBranchUpdated
--   | ProjectContributionCreated
--   | ProjectContributionStatusUpdated
--   | ProjectContributionComment
--   | ProjectTicketCreated
--   | ProjectTicketStatusUpdated
--   | ProjectTicketComment
--   | ProjectReleaseCreated
--   deriving (Eq, Show, Ord)

-- data HydratedEventPayload
--   = HydratedProjectBranchUpdatedPayload ProjectPayload BranchPayload
--   | HydratedProjectContributionCreatedPayload ProjectPayload ContributionPayload
--   | HydratedProjectContributionStatusUpdatedPayload ProjectPayload ContributionPayload (StatusUpdatePayload ContributionStatus)
--   | HydratedProjectContributionCommentPayload ProjectPayload ContributionPayload CommentPayload
--   | HydratedProjectTicketCreatedPayload ProjectPayload TicketPayload
--   | HydratedProjectTicketStatusUpdatedPayload ProjectPayload TicketPayload (StatusUpdatePayload TicketStatus)
--   | HydratedProjectTicketCommentPayload ProjectPayload TicketPayload CommentPayload
--   | HydratedProjectReleaseCreatedPayload ProjectPayload ReleasePayload
--   deriving stock (Show, Eq)

exampleEndpoint :: WebApp WebhookPayloadExamples
exampleEndpoint = do
  let eventId = NotificationEventId UUID.nil
  let projectId = ProjectId UUID.nil
  let projectSlug = ProjectSlug "example-project"
  let userHandle = UserHandle "example-user"
  let projectShortHand = ProjectShortHand {userHandle, projectSlug}
  let projectOwnerHandle = UserHandle "project-owner"
  let projectOwnerUserId = UserId UUID.nil
  let branchId = BranchId UUID.nil
  let branchName = BranchName "main"
  let contributorHandle = UserHandle "contributor"
  let projectBranchShortHand =
        ProjectBranchShortHand
          { userHandle,
            projectSlug,
            contributorHandle = Just contributorHandle,
            branchName
          }
  let branchContributorUserId = Just (UserId UUID.nil)
  let contributorHandle = Just (UserHandle "branch-contributor")
  let branchShortHand = BranchShortHand {contributorHandle, branchName}
  now <- liftIO getCurrentTime
  let projectPayload =
        ProjectPayload
          { projectId,
            projectSlug,
            projectShortHand,
            projectOwnerHandle,
            projectOwnerUserId
          }
  let branchPayload =
        BranchPayload
          { branchId,
            branchName,
            branchShortHand,
            projectBranchShortHand,
            branchContributorUserId,
            branchContributorHandle = contributorHandle
          }
  let payloads = [(ProjectBranchUpdated, HydratedProjectBranchUpdatedPayload projectPayload branchPayload)]

  jwtSettings <- asks Env.jwtSettings
  examples <-
    for payloads \(topic, hydratedEventPayload) -> do
      hydratedEventLink <- Links.notificationLink hydratedEventPayload
      let eventPayload =
            WebhookEventPayload
              { eventId,
                occurredAt = now,
                topic,
                data_ =
                  HydratedEvent
                    { hydratedEventPayload,
                      hydratedEventLink
                    },
                jwt = ()
              }
      jwt <-
        JWT.signJWT jwtSettings eventPayload >>= \case
          Left jwtErr -> respondError $ JWTError eventId (NotificationWebhookId UUID.nil) jwtErr
          Right jwt -> pure $ JWTParam jwt
      pure $ eventPayload {jwt = jwt}

  pure examples
