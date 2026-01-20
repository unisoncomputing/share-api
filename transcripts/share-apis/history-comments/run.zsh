#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Currently this must be manually enabled
export UNISON_SYNC_HISTORY_COMMENTS=true

# Create some history
transcript_ucm transcript comment-push.md

# Pull the history
transcript_ucm transcript comment-pull.md
