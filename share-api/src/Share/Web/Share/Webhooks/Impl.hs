module Share.Web.Share.Webhooks.Impl (server) where

import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Servant
import Share.BackgroundJobs.Webhooks.Types
import Share.Contribution
import Share.Env.Types qualified as Env
import Share.IDs
import Share.JWT
import Share.JWT qualified as JWT
import Share.Notifications.Types
import Share.Prelude
import Share.Ticket
import Share.Ticket qualified as Ticket
import Share.Web.App
import Share.Web.Errors (respondError)
import Share.Web.Share.DisplayInfo.Types (UserDisplayInfo (..))
import Share.Web.Share.Webhooks.API (WebhookPayloadExamples)
import Share.Web.Share.Webhooks.API qualified as WebhooksAPI
import Share.Web.UI.Links qualified as Links

server :: ServerT WebhooksAPI.API WebApp
server =
  WebhooksAPI.Routes
    { WebhooksAPI.payloadExamples = exampleEndpoint
    }

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
  let contributorHandle = UserHandle "branch-contributor"
  let branchShortHand = BranchShortHand {contributorHandle = Just contributorHandle, branchName}
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
            branchContributorHandle = Just contributorHandle
          }
  let contributionBranchName = BranchName "contribution-branch"
  let contributionProjectBranchShortHand =
        ProjectBranchShortHand
          { userHandle,
            projectSlug,
            contributorHandle = Just contributorHandle,
            branchName = contributionBranchName
          }
  let contributionBranchPayload =
        BranchPayload
          { branchId,
            branchName = contributionBranchName,
            branchShortHand,
            projectBranchShortHand = contributionProjectBranchShortHand,
            branchContributorUserId = Just (UserId UUID.nil),
            branchContributorHandle = Just contributorHandle
          }
  let contributionId = ContributionId UUID.nil
  let contributionNumber = ContributionNumber 1
  let contributionTitle = "Add new feature"
  let contributionDescription = Just "This contribution adds a new feature."
  let contributionStatus = InReview
  let contributionAuthor =
        UserDisplayInfo
          { handle = contributorHandle,
            name = Just "Branch Contributor",
            avatarUrl = Nothing,
            userId = UserId UUID.nil
          }
  let contributionPayload =
        ContributionPayload
          { contributionId,
            contributionNumber,
            contributionTitle,
            contributionDescription,
            contributionStatus,
            contributionAuthor,
            contributionSourceBranch = contributionBranchPayload,
            contributionTargetBranch = branchPayload,
            contributionCreatedAt = now
          }
  let contributionStatusUpdatePayload =
        StatusUpdatePayload {oldStatus = Draft, newStatus = InReview}
  let ticketStatusUpdatePayload =
        StatusUpdatePayload {oldStatus = Open, newStatus = Ticket.Closed}
  let commentId = CommentId UUID.nil
  let commentContent = "This is a comment on the contribution."
  let userDisplayInfo =
        UserDisplayInfo
          { handle = userHandle,
            name = Just "User Name",
            avatarUrl = Nothing,
            userId = UserId UUID.nil
          }
  let commentPayload =
        CommentPayload
          { commentId,
            commentContent,
            commentCreatedAt = now,
            commentAuthor = userDisplayInfo
          }
  let ticketPayload =
        TicketPayload
          { ticketId = TicketId UUID.nil,
            ticketNumber = TicketNumber 1,
            ticketTitle = "Bug report",
            ticketDescription = Just "There is a bug in the system.",
            ticketStatus = Open,
            ticketAuthor = userDisplayInfo,
            ticketCreatedAt = now
          }
  let releaseVersion = ReleaseVersion {major = 1, minor = 2, patch = 3}
  let releasePayload =
        ReleasePayload
          { releaseId = ReleaseId UUID.nil,
            releaseVersion,
            releaseCreatedAt = now
          }
  let allTopics :: [NotificationTopic] = [minBound .. maxBound]
  let allPayloads :: [(NotificationTopic, HydratedEventPayload)] =
        allTopics
          <&> \case
            ProjectBranchUpdated -> HydratedProjectBranchUpdatedPayload projectPayload branchPayload
            ProjectContributionCreated ->
              HydratedProjectContributionCreatedPayload projectPayload contributionPayload
            ProjectContributionStatusUpdated ->
              HydratedProjectContributionStatusUpdatedPayload projectPayload contributionPayload contributionStatusUpdatePayload
            ProjectContributionComment ->
              HydratedProjectContributionCommentPayload
                projectPayload
                contributionPayload
                commentPayload
            ProjectTicketCreated ->
              HydratedProjectTicketCreatedPayload projectPayload ticketPayload
            ProjectTicketStatusUpdated ->
              HydratedProjectTicketStatusUpdatedPayload
                projectPayload
                ticketPayload
                ticketStatusUpdatePayload
            ProjectTicketComment ->
              HydratedProjectTicketCommentPayload
                projectPayload
                ticketPayload
                commentPayload
            ProjectReleaseCreated ->
              HydratedProjectReleaseCreatedPayload projectPayload releasePayload
          & zip allTopics

  jwtSettings <- asks Env.jwtSettings
  examples <-
    for allPayloads \(topic, hydratedEventPayload) -> do
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
      pure $ eventPayload {jwt}
  pure examples
