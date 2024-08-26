#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Create a main branch and feature branch to merge into it.
transcript_ucm transcript contribution-setup.md

## Create a contribution
fetch "$transcript_user" POST contribution-create-contribution '/users/transcripts/projects/merge/contributions' '{
    "title": "Fast Forward Contribution",
    "description": "My description",
    "status": "in_review",
    "sourceBranchRef": "fast-forward-feature",
    "targetBranchRef": "main"
}'

# And the main branch should be set to the branch's head
fetch "$transcript_user" GET 'main-before-merge' '/users/transcripts/projects/merge/branches/main'
fetch_data "$transcript_user" GET 'feature-branch' '/users/transcripts/projects/merge/branches/fast-forward-feature'

# Get the contribution and its contribution state token
contribution_state_token=$(fetch_data "$transcript_user" GET 'contribution' '/users/transcripts/projects/merge/contributions/1' 2>/dev/null | jq -r '.contributionStateToken')
echo "$contribution_state_token"

# Check whether we can merge the contribution
fetch "$transcript_user" GET 'contribution-check-merge' '/users/transcripts/projects/merge/contributions/1/merge/check'

# Actually merge the contribution
fetch "$transcript_user" POST 'contribution-merge' '/users/transcripts/projects/merge/contributions/1/merge' "{
    \"contributionStateToken\": \"${contribution_state_token}\"
}
"

# Contribution should now be merged
fetch "$transcript_user" GET 'contribution-after-merge' '/users/transcripts/projects/merge/contributions/1'

# And the main branch should be set to the branch's head
fetch "$transcript_user" GET 'main-after-merge' '/users/transcripts/projects/merge/branches/main'
