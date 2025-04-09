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
    \"project_id\": \"$publictestproject_id\"
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


# Create a contribution in a public project, which should trigger a notification for both users
fetch "$test_user" POST contribution-create '/users/test/projects/publictestproject/contributions' '{
    "title": "My contribution",
    "description": "My description",
    "status": "draft",
    "sourceBranchRef": "feature",
    "targetBranchRef": "main"
}'

# Create a contribution in a private project, which shouldn't create a notification for either, because 'test' has
# a subscription filter and 'transcripts' doesn't have access.
fetch "$test_user" POST contribution-create '/users/test/projects/privatetestproject/contributions' '{
    "title": "My private contribution",
    "description": "My private description",
    "status": "draft",
    "sourceBranchRef": "privatebranch",
    "targetBranchRef": "main"
}'


# Notification APIs

fetch "$unauthorized_user" GET notifications-get-unauthorized '/users/test/notifications/hub' 

fetch "$test_user" GET list-notifications-test '/users/test/notifications/hub' 

fetch "$transcripts_user" GET list-notifications-transcripts '/users/transcripts/notifications/hub' 
