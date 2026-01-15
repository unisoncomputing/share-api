#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Create some history
transcript_ucm transcript prelude.md

# Pull the history
transcript_ucm transcript comment-pull.md
