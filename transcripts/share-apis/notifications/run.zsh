#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Subscribe the transcript user to contribution creation notifications
pg_sql "INSERT INTO notification_subscriptions (subscriber_user_id, scope_user_id, topic, filter) VALUES ('${transcript_user}', '${transcript_user}', 'project:contribution:created'::notification_topic, '{}'::jsonb)"

# Configure additional delivery mechanisms for the transcript user's notifications
pg_sql "INSERT INTO notification_subscriptions (subscriber_user_id, scope_user_id, topic, filter) VALUES ('${transcript_user}', '${transcript_user}', 'project:contribution:created'::notification_topic, '{}'::jsonb)"

# Create an event that should trigger a notification


# Notification APIs

fetch "$unauthorized_user" GET notifications-get-unauthorized '/users/transcripts/notifications' 

fetch "$transcripts_user" GET notifications-get '/users/transcripts/notifications' 
