#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

transcript_ucm transcript prelude.md

fetch "$unauthenticated_user" GET project-get-simple '/users/test/projects/publictestproject'


fetch "$transcripts_user" GET project-readme "/users/transcripts/projects/transcriptproject/readme"
fetch "$transcripts_user" GET project-browse-definition "/users/transcripts/projects/transcriptproject/branches/main/definitions/by-name/someTerm"
fetch "$transcripts_user" GET project-browse-definition-in-dependency "/users/transcripts/projects/transcriptproject/branches/main/definitions/by-name/lib.someLib.depNum"

fetch "$transcripts_user" POST project-create '/users/transcripts/projects/containers' '{
    "summary": "This is my project",
    "visibility": "private",
    "tags": []
}'

# Now make some calls with an unauthorized user to ensure our auth checks work.

fetch "$unauthorized_user" GET project-get-private-unauthorized '/users/transcripts/projects/containers'

fetch "$unauthorized_user" PATCH project-update-unauthorized '/users/transcripts/projects/containers' '{
    "summary": "new summary"
}'

fetch "$transcripts_user" GET project-get-private-authorized '/users/transcripts/projects/containers'

fetch "$transcripts_user" GET project-list '/users/transcripts/projects'

fetch "$unauthorized_user" GET project-private-unauthorized-get '/users/transcripts/projects/containers'

fetch "$unauthenticated_user" GET project-private-unauthenticated-get '/users/transcripts/projects/containers'

fetch "$transcripts_user" PATCH project-update '/users/transcripts/projects/containers' '{
    "summary": null,
    "visibility": "public"
}'

fetch "$unauthenticated_user" GET project-get-public-unauthenticated '/users/transcripts/projects/containers'

fetch "$transcripts_user" PUT project-fav '/users/transcripts/projects/containers/fav' '{
  "isFaved": true
}'

fetch "$transcripts_user" GET project-get-after-update '/users/transcripts/projects/containers'

fetch "$transcripts_user" PUT project-unfav '/users/transcripts/projects/containers/fav' '{
  "isFaved": false
}'

fetch "$unauthorized_user" PATCH project-unauthorized-update '/users/transcripts/projects/containers' '{
    "summary": "new summary"
}'

fetch "$unauthorized_user" POST project-catalog-add-is-admin-only '/admin/catalog/category' '[
{ "categoryName": "data"
  , "userHandle": "transcripts"
  , "projectSlug": "containers"
}
]'

fetch "$admin_user" POST project-catalog-add '/admin/catalog/category' '[
    { "categoryName": "data"
    , "userHandle": "transcripts"
    , "projectSlug": "containers"
    }
]'

fetch "$transcripts_user" GET project-catalog-get '/catalog'


# Project Search

# Should find projects we have access to (e.g. Unison's private project), but none that we don't.
fetch "$transcripts_user" GET project-search '/search?query=test'

# Should find projects we have access to (e.g. Unison's private project), but none that we don't.
fetch "$transcripts_user" GET project-search-by-prefix '/search?query=@publicte'

# Should filter project search by user if provided a full valid handle:
fetch "$transcripts_user" GET project-search-with-user-and-project-query '/search?query=@test/public'

# Should return all projects in a user if provided a full valid handle, but no project query:
fetch "$transcripts_user" GET project-search-with-only-user '/search?query=@test/'

# Transcript user should not find 'test' user's private project
fetch "$transcripts_user" GET project-search-inaccessible '/search?query=privatetestproject'

# Should find projects based on terms in summary
fetch "$transcripts_user" GET project-search-by-summary '/search?query=summary'

# Delete project
fetch "$transcripts_user" DELETE project-delete '/users/transcripts/projects/containers'

# Should 404
fetch "$transcripts_user" GET check-project-deleted '/users/transcripts/projects/containers'
