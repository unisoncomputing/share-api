#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Should find all branches in the public project
fetch "$transcripts_user" GET branch-list '/users/test/projects/publictestproject/branches'
# Should not have access to private project
fetch "$transcripts_user" GET branch-list-private '/users/test/projects/privatetestproject/branches'
# Should show only contributor branches
fetch "$transcripts_user" GET branch-list-contributors-only '/users/test/projects/publictestproject/branches?kind=contributor'
# Should show only non-contributor branches
fetch "$transcripts_user" GET branch-list-core-only '/users/test/projects/publictestproject/branches?kind=core'
# Should filter by name-prefix
fetch "$transcripts_user" GET branch-list-name-filter '/users/test/projects/publictestproject/branches?name-prefix=ma'
# Should filter to a specific contributor
fetch "$transcripts_user" GET branch-list-contributor-filter '/users/test/projects/publictestproject/branches?contributor-handle=@transcripts'
# Should filter to a handle prefix
fetch "$transcripts_user" GET branch-list-contributor-prefix '/users/test/projects/publictestproject/branches?name-prefix=@transcr'
# Should filter to a handle + name prefix
fetch "$transcripts_user" GET branch-list-name-prefix '/users/test/projects/publictestproject/branches?name-prefix=@transcripts%2Fcontr'

# Should see public branches from other users
fetch "$transcripts_user" GET branch-list-by-user '/users/test/branches'

# Should see all branches by self
fetch "$transcripts_user" GET branch-list-by-user-self '/users/transcripts/branches'

# Can filter by name prefix
fetch "$transcripts_user" GET branch-list-by-self-name-prefix '/users/transcripts/branches?name-prefix=contr'

# Can filter by project shorthand
fetch "$transcripts_user" GET branch-list-by-self-project-ref '/users/transcripts/branches?project-ref=@test/publictestproject'

# Shouldn't find branches for private projects we can't access: (E.g. 'test's private project branches)
fetch "$transcripts_user" GET branch-list-inaccessible '/users/transcripts/branches?project-ref=@test/privatetestproject'

# Should find branches for private projects we can access: (E.g. Unison's private project branches)
fetch "$transcripts_user" GET branch-list-private '/users/transcripts/branches'

# Branch details
fetch "$transcripts_user" GET branch-details '/users/test/projects/publictestproject/branches/main'

# Paging tests
fetch "$transcripts_user" GET branch-list-page-1 '/users/test/projects/publictestproject/branches?limit=1'
next_cursor=$(fetch_data_jq "$transcripts_user" GET branch-list-paged '/users/test/projects/publictestproject/branches?limit=1' '.nextCursor')
fetch "$transcripts_user" GET branch-list-page-2 "/users/test/projects/publictestproject/branches?limit=1&cursor=$next_cursor"
prev_cursor=$(fetch_data_jq "$transcripts_user" GET branch-list-page-2 "/users/test/projects/publictestproject/branches?limit=1&cursor=$next_cursor" '.prevCursor')
fetch "$transcripts_user" GET branch-list-prev-page "/users/test/projects/publictestproject/branches?limit=1&cursor=$prev_cursor"

# Delete a branch
fetch "$test_user" DELETE branch-delete '/users/test/projects/publictestproject/branches/main'

# Branch should no longer exist
fetch "$test_user" GET branch-details-deleted '/users/test/projects/publictestproject/branches/main'


# Add some history to a branch.
transcript_ucm transcript prelude.md

fetch "$transcripts_user" GET branch-history '/users/transcripts/projects/branch-with-history/branches/main/history?limit=3'
