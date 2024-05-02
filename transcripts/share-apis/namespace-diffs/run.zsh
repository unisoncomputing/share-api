#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

transcript_ucm transcript prelude.md

fetch "$transcript_user" GET namespace-diff '/users/transcripts/projects/namespace-diff/diff/namespaces?old=diff-start&new=diff-end'

fetch "$transcript_user" POST create-contribution-for-diff '/users/transcripts/projects/namespace-diff/contributions' '{
    "title": "My contribution",
    "description": "My description",
    "status": "in_review",
    "sourceBranchRef": "diff-end",
    "targetBranchRef": "diff-start"
}'

fetch "$transcript_user" GET contribution-diff '/users/transcripts/projects/namespace-diff/contributions/1/diff'
