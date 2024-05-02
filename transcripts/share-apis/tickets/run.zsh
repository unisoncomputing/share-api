#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Creating Tickets

## Create a ticket
fetch "$transcript_user" POST ticket-create '/users/test/projects/publictestproject/tickets' '{
    "title": "My Ticket",
    "description": "My description"
}'

## Should fail to create a ticket in private projects we don't have access to.
fetch "$unauthorized_user" POST ticket-create-unauthorized '/users/test/projects/privatetestproject/tickets' '{
    "title": "My ticket",
    "description": "My description"
}'

# Fetching Tickets

## List tickets by project
fetch "$transcript_user" GET ticket-list '/users/test/projects/publictestproject/tickets'

## List tickets by project, author filter
fetch "$transcript_user" GET ticket-list-author-filter '/users/test/projects/publictestproject/tickets?author=@test'

## List tickets by project, status filter
fetch "$transcript_user" GET ticket-list-status-filter '/users/test/projects/publictestproject/tickets?status=open'

fetch "$test_user" PATCH ticket-update '/users/test/projects/publictestproject/tickets/1' '{
    "title": "Updated Title",
    "description": "Updated description",
    "status": "closed"
}'

fetch "$transcript_user" GET ticket-get '/users/test/projects/publictestproject/tickets/1'


fetch "$transcript_user" POST ticket-comment-create '/users/test/projects/publictestproject/tickets/1/timeline/comments' '{
    "content": "This is a new comment"
}'

fetch "$test_user" PATCH ticket-comment-update-conflict '/users/test/projects/publictestproject/tickets/1/timeline/comments/CMT-25118f35-87f6-4429-bb43-69b0e5bb717a' '{
  "content": "This is an updated comment",
  "expectedRevision": 0
}'

fetch "$test_user" PATCH ticket-comment-update-success '/users/test/projects/publictestproject/tickets/1/timeline/comments/CMT-25118f35-87f6-4429-bb43-69b0e5bb717a' '{
  "content": "This is an updated comment",
  "expectedRevision": 1
}'

fetch "$test_user" DELETE ticket-comment-delete '/users/test/projects/publictestproject/tickets/1/timeline/comments/CMT-d9164a6f-706b-402d-a2b0-b55a7c19a4f5'

fetch "$transcript_user" GET ticket-timeline-get '/users/test/projects/publictestproject/tickets/1/timeline'
