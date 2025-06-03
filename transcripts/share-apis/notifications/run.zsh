#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

publictestproject_id=$(project_id_from_handle_and_slug 'test' 'publictestproject')

# Subscribe to project-related notifications for the test user's publictestproject.
# This subscription uses a topic group rather than a specific topic.
subscription_id=$(fetch_data_jq "$test_user" POST create-subscription-for-project '/users/test/notifications/subscriptions' '.subscription.id' "{
  \"scope\": \"test\",
  \"topics\": [],
  \"topicGroups\": [
    \"watch_project\"
  ],
  \"filter\": {
    \"projectId\": \"$publictestproject_id\"
  }
}" )

webhook_id=$(fetch_data_jq "$test_user" POST create-webhook  '/users/test/notifications/delivery-methods/webhooks' '.webhookId' "{
  \"url\": \"${echo_server}/good-webhook\",
  \"name\": \"Good Webhook\"
}" )

failing_webhook_id=$(fetch_data_jq "$test_user" POST create-webhook  '/users/test/notifications/delivery-methods/webhooks' '.webhookId' "{
  \"url\": \"${echo_server}/invalid?x-set-response-status-code=500\",
  \"name\": \"Bad Webhook\"
}" )

fetch "$test_user" POST add-webhook-to-subscription "/users/test/notifications/subscriptions/${subscription_id}/delivery-methods/add" "{
  \"deliveryMethods\": [{\"kind\": \"webhook\",  \"id\": \"${webhook_id}\"}, {\"kind\": \"webhook\",  \"id\": \"${failing_webhook_id}\"}]
}"

# Add a subscription within the transcripts user to notifications for contributions created in any project belonging to the test user.
# No filter is applied.
fetch "$transcripts_user" POST create-subscription-for-other-user-project '/users/transcripts/notifications/subscriptions' "{
  \"scope\": \"test\",
  \"topics\": [
    \"project:contribution:created\", \"project:branch:updated\"
  ],
  \"topicGroups\": []
}"

fetch "$test_user" POST create-email-delivery '/users/test/notifications/delivery-methods/emails' '{
  "email": "me@example.com"
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

# Add a comment to a contribution, which should trigger a notification for the test user, which
# follows "watch_project"
fetch "$transcripts_user" POST contribution-comment-create '/users/test/projects/publictestproject/contributions/1/timeline/comments' '{
    "content": "This is a new comment"
}'

# Create a ticket in the public project, which should trigger a notification for the test user, which
# follows "watch_project"

fetch "$transcripts_user" POST ticket-create '/users/test/projects/publictestproject/tickets' '{
    "title": "My Ticket",
    "description": "My description"
}'

# Add a comment to the ticket, which should trigger a notification for the test user, which
# follows "watch_project"
fetch "$transcripts_user" POST ticket-comment-create '/users/test/projects/publictestproject/tickets/1/timeline/comments' '{
    "content": "This is a new comment on the ticket"
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

# Create a new branch in the watched project, which should trigger a branch updated notification 
fetch "$test_user" POST branch-create '/ucm/v1/projects/create-project-branch' "{
  \"project-id\": \"${publictestproject_id}\",
  \"branch-name\": \"newbranch\",
  \"branch-head\": \"${empty_causal_hash}\"
}"

# Notification APIs

fetch "$unauthorized_user" GET notifications-get-unauthorized '/users/test/notifications/hub'

test_notification_id=$(fetch_data_jq "$test_user" GET list-notifications-test '/users/test/notifications/hub' '.items[0].id')
transcripts_notification_id=$(fetch_data_jq "$transcripts_user" GET list-notifications-transcripts '/users/transcripts/notifications/hub' '.items[0].id')

fetch "$test_user" GET list-notifications-test '/users/test/notifications/hub'

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

# loop until sql query returns 0;
while  [ $(pg_sql "SELECT COUNT(*) FROM notification_webhook_queue WHERE delivery_attempts_remaining > 0 AND NOT delivered;") -gt 0 ]; do \
  echo "Waiting for notification webhooks..." >&2; \
  sleep 1; \
done;

successful_webhooks=$(pg_sql "SELECT COUNT(*) FROM notification_webhook_queue WHERE delivered;")
unsuccessful_webhooks=$(pg_sql "SELECT COUNT(*) FROM notification_webhook_queue WHERE NOT delivered;")

echo "Successful webhooks: $successful_webhooks\nUnsuccessful webhooks: $unsuccessful_webhooks\n" > ./webhook_results.txt
