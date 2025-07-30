#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

transcript_ucm transcript prelude.md

# Find everything in the Remote namespace and the ability too
fetch "$transcripts_user" GET escaped-namespaces "/users/transcripts/projects/escaped-names/branches/main/namespaces/by-name/"
fetch "$transcripts_user" GET escaped-browse "/users/transcripts/projects/escaped-names/branches/main/browse"
