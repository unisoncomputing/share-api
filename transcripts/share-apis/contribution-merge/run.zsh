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

# Check whether we can merge the contribution
fetch "$transcript_user" GET contribution-check-merge '/users/transcripts/projects/merge/contributions/1/merge/check'
