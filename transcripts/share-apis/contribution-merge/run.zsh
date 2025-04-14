#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Create a main branch and feature branch to merge into it.

login_user_for_ucm 'transcripts'
transcript_ucm transcript 'project-setup.md'

fetch "$transcripts_user" PATCH make-project-public '/users/transcripts/projects/merge' '{
    "summary": null,
    "visibility": "public"
}'

login_user_for_ucm 'test'
transcript_ucm transcript 'contribution-setup.md'

## Create a contribution
fetch "$test_user" POST contribution-create-contribution '/users/transcripts/projects/merge/contributions' '{
    "title": "Fast Forward Contribution",
    "description": "My description",
    "status": "in_review",
    "sourceBranchRef": "@test/fast-forward-feature",
    "targetBranchRef": "main"
}'

# And the main branch should be set to the branch's head
fetch "$transcripts_user" GET 'main-before-merge' '/users/transcripts/projects/merge/branches/main'
fetch_data "$transcripts_user" GET 'feature-branch' '/users/transcripts/projects/merge/branches/%40test%2Ffast-forward-feature'

# Get the contribution and its contribution state token
contribution_state_token=$(fetch_data "$transcripts_user" GET 'contribution' '/users/transcripts/projects/merge/contributions/1' 2>/dev/null | jq -r '.contributionStateToken')
echo "$contribution_state_token"

# Check whether we can merge the contribution
fetch "$transcripts_user" GET 'contribution-check-merge' '/users/transcripts/projects/merge/contributions/1/merge/check'

# Actually merge the contribution
fetch "$transcripts_user" POST 'contribution-merge' '/users/transcripts/projects/merge/contributions/1/merge' "{
    \"contributionStateToken\": \"${contribution_state_token}\"
}
"

# Contribution should now be merged
fetch "$transcripts_user" GET 'contribution-after-merge' '/users/transcripts/projects/merge/contributions/1'

# And the main branch should be set to the branch's head
fetch "$transcripts_user" GET 'main-after-merge' '/users/transcripts/projects/merge/branches/main'

login_user_for_ucm 'transcripts'
transcript_ucm transcript 'pull-after-merge.md'
