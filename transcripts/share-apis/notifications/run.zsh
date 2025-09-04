#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

publictestproject_id=$(project_id_from_handle_and_slug 'test' 'publictestproject')

# Can subscribe to project-related notifications for the test user's publictestproject.
subscription_id=$(fetch_data_jq "$test_user" PUT subscribe-to-watch-project '/users/test/projects/publictestproject/subscription' '.subscriptionId' '{
  "isSubscribed": true
}' )

fetch "$test_user" POST add-project-webhook "/users/test/projects/publictestproject/webhooks" "{
  \"uri\": \"${echo_server}/good-webhook\",
  \"topics\": {\"type\": \"all\"}
}"

# Subscribe the transcripts user to notifications for a project in the test user.
fetch "$transcripts_user" PUT subscribe-to-other-user-project '/users/test/projects/publictestproject/subscription' '{
  "isSubscribed": true
}'

# Create a contribution in a public project, which should trigger a notification for both users, but will be omitted
# from 'transcripts' notification list since it's a self-notification.
fetch "$transcripts_user" POST public-contribution-create '/users/test/projects/publictestproject/contributions' '{
    "title": "My contribution",
    "description": "My description",
    "status": "draft",
    "sourceBranchRef": "feature",
    "targetBranchRef": "main"
}'

# Create a release in a public project, which should trigger a notification for both users, but will be omitted
# from 'test' notification list since it's a self-notification.
fetch "$test_user" POST release-create '/users/test/projects/publictestproject/releases' '{
    "causalHash": "#sg60bvjo91fsoo7pkh9gejbn0qgc95vra87ap6l5d35ri0lkaudl7bs12d71sf3fh6p23teemuor7mk1i9n567m50ibakcghjec5ajg",
    "major": 2,
    "minor": 3,
    "patch": 4
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

# Update the contribution status, which should trigger a status update notification.
fetch "$test_user" PATCH contribution-update-status '/users/test/projects/publictestproject/contributions/1' '{
    "status": "closed"
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
# If we pass a limit, we should get a next cursor
fetch "$test_user" GET list-notifications-test-paging '/users/test/notifications/hub?limit=1'

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

# Show only unread notifications:
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

# List 'test' user's subscriptions
fetch "$test_user" GET list-subscriptions-test '/users/test/notifications/subscriptions'

# Can unsubscribe from project-related notifications for the test user's publictestproject.
fetch "$test_user" PUT unsubscribe-from-project '/users/test/projects/publictestproject/subscription' '{
  "isSubscribed": false
}'

# List 'test' user's subscriptions again
fetch "$test_user" GET list-subscriptions-test-after-unsubscribe '/users/test/notifications/subscriptions'
