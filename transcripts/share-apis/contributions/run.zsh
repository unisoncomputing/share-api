#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Creating Contributions

## Create a contribution
fetch "$transcripts_user" POST contribution-create-contribution '/users/test/projects/publictestproject/contributions' '{
    "title": "My contribution",
    "description": "My description",
    "status": "in_review",
    "sourceBranchRef": "@transcripts/contribution",
    "targetBranchRef": "main"
}'

## Draft contribution
fetch "$test_user" POST contribution-create-draft '/users/test/projects/publictestproject/contributions' '{
    "title": "My contribution",
    "description": "My description",
    "status": "draft",
    "sourceBranchRef": "@transcripts/contribution",
    "targetBranchRef": "main"
}'

## Core branch contribution
fetch "$test_user" POST contribution-create-core '/users/test/projects/publictestproject/contributions' '{
    "title": "My contribution",
    "description": "My description",
    "status": "draft",
    "sourceBranchRef": "feature",
    "targetBranchRef": "main"
}'


## Should fail to create a contribution to private projects we don't have access to.
fetch "$unauthorized_user" POST contribution-create-draft-unauthorized '/users/test/projects/privatetestproject/contributions' '{
    "title": "My contribution",
    "description": "My description",
    "status": "in_review",
    "sourceBranchRef": "@transcripts/contribution",
    "targetBranchRef": "main"
}'

# Fetching Contributions

## List contributions by project
fetch "$transcripts_user" GET contribution-list '/users/test/projects/publictestproject/contributions'

## List contributions by project, author filter
fetch "$transcripts_user" GET contribution-list-author-filter '/users/test/projects/publictestproject/contributions?author=@test'

## List contributions by project, status filter
fetch "$transcripts_user" GET contribution-list-status-filter '/users/test/projects/publictestproject/contributions?status=in_review'

## List contributions by project, kind filter
fetch "$transcripts_user" GET contribution-list-kind-filter '/users/test/projects/publictestproject/contributions?kind=core'

fetch "$test_user" PATCH contribution-update '/users/test/projects/publictestproject/contributions/3' '{
    "title": "Updated Title",
    "description": "Updated description",
    "status": "closed",
    "sourceBranchRef": "@transcripts/contribution",
    "targetBranchRef": "main"
}'

fetch "$transcripts_user" GET contribution-get '/users/test/projects/publictestproject/contributions/3'


fetch "$transcripts_user" POST contribution-comment-create '/users/test/projects/publictestproject/contributions/1/timeline/comments' '{
    "content": "This is a new comment"
}'

fetch "$test_user" PATCH contribution-comment-update-conflict '/users/test/projects/publictestproject/contributions/1/timeline/comments/CMT-b9dce6b4-f5b2-4b28-a485-988c0c7a56be' '{
  "content": "This is an updated comment",
  "expectedRevision": 0
}'

fetch "$test_user" PATCH contribution-comment-update-success '/users/test/projects/publictestproject/contributions/1/timeline/comments/CMT-b9dce6b4-f5b2-4b28-a485-988c0c7a56be' '{
  "content": "This is an updated comment",
  "expectedRevision": 1
}'

fetch "$test_user" DELETE contribution-comment-delete '/users/test/projects/publictestproject/contributions/1/timeline/comments/CMT-82c21bb5-6331-497f-b2e6-15414ebc63c4'

fetch "$transcripts_user" GET contribution-timeline-get '/users/test/projects/publictestproject/contributions/1/timeline'

################################################################################

# BCA and merge update testing

# Run setup transcript, saving the codebase so we can update it.
transcript_ucm transcript contribution-setup.md

# Create a contribution for merging feature-one into main
fetch "$transcripts_user" POST create-feature-one-contribution '/users/transcripts/projects/bca-updates/contributions' '{
    "title": "Feature One",
    "description": "description",
    "status": "in_review",
    "sourceBranchRef": "feature-one",
    "targetBranchRef": "main"
}'

# Create a contribution for merging feature-two into feature-one
fetch "$transcripts_user" POST create-feature-two-contribution '/users/transcripts/projects/bca-updates/contributions' '{
    "title": "Feature Two",
    "description": "description",
    "status": "in_review",
    "sourceBranchRef": "feature-two",
    "targetBranchRef": "feature-one"
}'


# Merge the feature branch into the main branch
transcript_ucm transcript merge-contribution-branches.md

# Fetch the contribution to see that it's been marked as merged.
fetch "$transcripts_user" GET merged-contribution '/users/transcripts/projects/bca-updates/contributions/1'

# Hacky, but since namespace diffs are computed asynchronously, we just block here until there are 5 (the number this
# test creates). Don't wait more than 10 seconds, just in case.
expectedNumberOfNamespaceDiffs=5
for i in {1..5}; do
  if [[ $(pg_sql "select count(*) from namespace_diffs;") -lt $expectedNumberOfNamespaceDiffs ]]; then
    sleep 1
  else
    break
  fi
done

# BCA of contribution diff should still be frozen at it's pre-merge hash. The bca and source hash should be different (or else we'd see no diff!)
fetch "$transcripts_user" GET merged-contribution-diff '/users/transcripts/projects/bca-updates/contributions/1/diff'

# Fetch the contribution for feature-two which was based on feature-one, which was just merged.
# It should now be marked as merging into main.
fetch "$transcripts_user" GET transitive-contribution '/users/transcripts/projects/bca-updates/contributions/2'

# BCA of transitive contribution should still be feature-one's hash, since that was merged into main, where we are now
# merging into.
fetch "$transcripts_user" GET transitive-contribution-diff '/users/transcripts/projects/bca-updates/contributions/2/diff'
