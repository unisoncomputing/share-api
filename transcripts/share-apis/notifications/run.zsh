#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

publictestproject_id=$(project_id_from_handle_and_slug 'test' 'publictestproject')

# Subscribe to project:contribution:created notifications for the test user's publictetestproject.
fetch "$test_user" POST create-subscription-for-project '/users/test/notifications/subscriptions' "{
  \"scope\": \"test\",
  \"topics\": [
    \"project:contribution:created\"
  ],
  \"filter\": {
    \"projectId\": \"$publictestproject_id\"
  }
}"

# Add a subscription within the transcripts user to notifications for contributions created in any project belonging to the test user.
# No filter is applied.
fetch "$transcripts_user" POST create-subscription-for-other-user-project '/users/transcripts/notifications/subscriptions' "{
  \"scope\": \"test\",
  \"topics\": [
    \"project:contribution:created\"
  ]
}"

fetch "$test_user" POST create-email-delivery '/users/test/notifications/delivery-methods/emails' '{
  "email": "me@example.com"
}'

fetch "$test_user" POST create-webhook-delivery '/users/test/notifications/delivery-methods/webhooks' '{
  "url": "https://example.com/webhook"
}'

fetch "$test_user" GET check-delivery-methods '/users/test/notifications/delivery-methods'

# Create a contribution in a public project, which should trigger a notification for both users
fetch "$test_user" POST public-contribution-create '/users/test/projects/publictestproject/contributions' '{
    "title": "My contribution",
    "description": "My description",
    "status": "draft",
    "sourceBranchRef": "feature",
    "targetBranchRef": "main"
}'

# Create a contribution in a private project, which shouldn't create a notification for either, because 'test' has
# a subscription filter and 'transcripts' doesn't have access.
fetch "$test_user" POST private-contribution-create '/users/test/projects/privatetestproject/contributions' '{
    "title": "My private contribution",
    "description": "My private description",
    "status": "draft",
    "sourceBranchRef": "privatebranch",
    "targetBranchRef": "main"
}'


# Notification APIs

fetch "$unauthorized_user" GET notifications-get-unauthorized '/users/test/notifications/hub' 

test_notification_id=$(fetch_data_jq "$test_user" GET list-notifications-test '/users/test/notifications/hub' '.notifications[0].id')
transcripts_notification_id=$(fetch_data_jq "$transcripts_user" GET list-notifications-transcripts '/users/transcripts/notifications/hub' '.notifications[0].id')

fetch "$transcripts_user" GET list-notifications-transcripts '/users/transcripts/notifications/hub' 

# Mark notifications as read
fetch "$test_user" PATCH mark-notifications-read-test '/users/test/notifications/hub' "{
  \"status\": \"read\",
  \"notificationIds\": [
    \"$test_notification_id\"
  ]
}"

fetch "$transcripts_user" PATCH mark-notifications-read-transcripts '/users/transcripts/notifications/hub' "{
  \"status\": \"read\",
  \"notificationIds\": [
    \"$transcripts_notification_id\"
  ]
}"

# Show only unread notifications (none should be unread):
fetch "$test_user" GET list-notifications-unread-test '/users/test/notifications/hub?status=unread' 

# Show only read notifications (the one we just marked as read):
fetch "$transcripts_user" GET list-notifications-read-transcripts '/users/transcripts/notifications/hub?status=read'
