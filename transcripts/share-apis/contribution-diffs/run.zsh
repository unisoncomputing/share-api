#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

transcript_ucm transcript prelude.md

fetch "$transcripts_user" GET namespace-diff '/users/transcripts/projects/contribution-diff/diff/namespaces?old=diff-start&new=diff-end'

fetch "$transcripts_user" POST create-contribution-for-diff '/users/transcripts/projects/contribution-diff/contributions' '{
    "title": "My contribution",
    "description": "My description",
    "status": "in_review",
    "sourceBranchRef": "diff-end",
    "targetBranchRef": "diff-start"
}'

fetch "$transcripts_user" GET contribution-diff '/users/transcripts/projects/contribution-diff/contributions/1/diff'

# Diffing a user-defined term against a user-defined term.
fetch "$transcripts_user" GET standard-term-diff '/users/transcripts/projects/contribution-diff/contributions/1/diff/terms?oldTerm=termUpdateMe&newTerm=termUpdateMe'

# Diffing a user-defined type against a user-defined type.
fetch "$transcripts_user" GET standard-type-diff '/users/transcripts/projects/contribution-diff/contributions/1/diff/types?oldType=DataUpdateMe&newType=DataUpdateMe'
