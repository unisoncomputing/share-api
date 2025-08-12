#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"
transcript_ucm transcript prelude.md

# Find everything in the Remote namespace and the ability too
fetch "$transcripts_user" GET branch-find "/users/transcripts/projects/branch-browse/branches/main/find?limit=9&renderWidth=100&query=Remote"

# Get definition by name
fetch "$transcripts_user" GET branch-definition-by-name "/users/transcripts/projects/branch-browse/branches/main/definitions/by-name/other.remoteMap?renderWidth=100"

# Get definition by name in history
fetch "$transcripts_user" GET branch-definition-by-name-in-history "/users/transcripts/projects/branch-browse/branches/main/definitions/by-name/other.remoteMap?renderWidth=100&rootHash=1jqrcqkhnqpgrkrnsn64r4ivvkap5eq8dto4l6l6galhgjf2mcqf9moqdlm9oh0m4753vbl7j1aev97r5h8bf2tudeudilaalfghjlg"
