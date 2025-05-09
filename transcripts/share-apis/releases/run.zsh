#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

fetch "$transcripts_user" GET releases-list '/users/test/projects/publictestproject/releases'
fetch "$transcripts_user" GET releases-list-published-only '/users/test/projects/publictestproject/releases?status=published'
fetch "$transcripts_user" GET releases-list-version-search '/users/test/projects/publictestproject/releases?version-prefix=1.2'

# Create a release in the test project
fetch "$test_user" POST project-release-ucm-create '/ucm/v1/projects/create-project-branch' '{
    "project-id": "P-cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd",
    "branch-name": "releases/4.5.6",
    "branch-head": "sg60bvjo91fsoo7pkh9gejbn0qgc95vra87ap6l5d35ri0lkaudl7bs12d71sf3fh6p23teemuor7mk1i9n567m50ibakcghjec5ajg"
}'

fetch "$test_user" GET release-prefix-search '/users/test/projects/publictestproject/releases?version-prefix=4.5.'

# Deprecate a release
fetch "$test_user" PATCH release-deprecate '/users/test/projects/publictestproject/releases/4.5.6' '{
    "status": "deprecated"
}'

# Try to publish a deprecated release (should fail)
fetch "$test_user" PATCH release-publish-deprecated '/users/test/projects/publictestproject/releases/4.5.6' '{
    "status": "published"
}'

# Latest release should be set appropriately
fetch "$test_user" GET project-latest-release '/users/test/projects/publictestproject'

# Create a release in the unison project using the test user via the web API
fetch "$test_user" POST project-release-api-create '/users/unison/projects/privateorgproject/releases' '{
    "causalHash": "#sg60bvjo91fsoo7pkh9gejbn0qgc95vra87ap6l5d35ri0lkaudl7bs12d71sf3fh6p23teemuor7mk1i9n567m50ibakcghjec5ajg",
    "major": 1,
    "minor": 2,
    "patch": 3
}'
