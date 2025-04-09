#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

publictestproject_id=$(project_id_from_handle_and_slug 'test' 'publictestproject')

# Subscribe to project:contribution:created notifications for the test user's publictetestproject.
fetch "$test_user" POST create-subscription-for-project '/users/test/notifications/subscriptions' "{
  \"scope\": \"$test_user\",
  \"topics\": [
    \"project:contribution:created\"
  ],
  \"filter\": {
    \"project_id\": \"$publictestproject_id\"
  }
}"

# Subscribe to notifications for contribution creation for all projects belonging to another user.
fetch "$transcripts_user" POST create-subscription-for-other-user-project '/users/test/notifications/subscriptions' "{
  \"scope\": \"$test_user\",
  \"topics\": [
    \"project:contribution:created\"
  ],
  \"filter\": {
    \"project_id\": \"$publictestproject_id\"
  }
}"


# Create a contribution, which should trigger a notification
fetch "$test_user" POST contribution-create '/users/test/projects/publictestproject/contributions' '{
    "title": "My contribution",
    "description": "My description",
    "status": "draft",
    "sourceBranchRef": "feature",
    "targetBranchRef": "main"
}'


# Notification APIs

fetch "$unauthorized_user" GET notifications-get-unauthorized '/users/test/notifications' 

fetch "$test_user" GET list-notifications-test '/users/test/notifications' 

fetch "$transcripts_user" GET list-notifications-transcripts '/users/transcripts/notifications' 
