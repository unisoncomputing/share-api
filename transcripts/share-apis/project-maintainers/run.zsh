#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

read_user="$(create_user read-user)"
maintain_user="$(create_user maintain-user)"
contributor_user="$(create_user contributor-user)"
admin_user="$(create_user admin-user)"

# Should fail
fetch "$unauthorized_user" GET non-maintainer-private-project-view '/users/test/projects/privatetestproject'
fetch "$unauthorized_user" PATCH non-maintainer-private-project-update '/users/test/projects/privatetestproject' '{
    "summary": "update"
}'


# Before adding permissions Project viewer should NOT see the private project in the owner's project list and search
fetch "$read_user" GET read-maintainer-project-list-before '/users/test/projects'
fetch "$read_user" GET read-maintainer-project-search-before '/search?query=%40private'

fetch "$test_user" POST add-roles '/users/test/projects/privatetestproject/roles' "
{
    \"role_assignments\": 
    [ { \"subject\": {\"kind\": \"user\", \"id\": \"${read_user}\"}
      , \"roles\": [\"project_viewer\"]
      }
      , { \"subject\": {\"kind\": \"user\", \"id\": \"${maintain_user}\"}
      , \"roles\": [\"project_maintainer\"]
      }
      , { \"subject\": {\"kind\": \"user\", \"id\": \"${contributor_user}\"}
      , \"roles\": [\"project_contributor\"]
      }
      , { \"subject\": {\"kind\": \"user\", \"id\": \"${admin_user}\"}
      , \"roles\": [\"project_owner\"]
      }
    ]
}"

# Non-owner should not be able to change roles
fetch "$maintain_user" POST non-owner-add-roles '/users/test/projects/privatetestproject/roles' "
{
    \"role_assignments\": 
    [ { \"subject\": {\"kind\": \"user\", \"id\": \"${read_user}\"}
      , \"roles\": [\"project_contributor\"]
      }
    ]
}"

# Project owner can list roles
fetch "$test_user" GET list-roles '/users/test/projects/privatetestproject/roles'

# Project owner can create tickets
fetch "$test_user" POST owner-ticket-create '/users/test/projects/privatetestproject/tickets' '{
    "title": "Ticket 1",
    "description": "My description"
}'

# Project viewer can view the project
fetch "$read_user" GET read-maintainer-project-view '/users/test/projects/privatetestproject'

# Project viewer should see the private project in the owner's project list and search
fetch "$read_user" GET read-maintainer-project-list-after '/users/test/projects'
fetch "$read_user" GET read-maintainer-project-search-after '/search?query=%40private'

# Contributor user should be able to create tickets
fetch "$contributor_user" POST contributor-maintainer-ticket-create '/users/test/projects/privatetestproject/tickets' '{
    "title": "Ticket 2",
    "description": "My description"
}'

# Read maintainers can't update the project
fetch "$read_user" PATCH read-maintainer-project-update '/users/test/projects/privatetestproject' '{
    "summary": "update"
}'

# Read maintainers can't close other's tickets
fetch "$read_user" PATCH read-maintainer-ticket-close '/users/test/projects/privatetestproject/tickets/1' '{
    "status": "closed"
}'

# Maintain maintainers can close any ticket
fetch "$maintain_user" PATCH maintain-maintainer-ticket-close '/users/test/projects/privatetestproject/tickets/1' '{
    "status": "closed"
}'

# Maintain maintainers can update the project
fetch "$maintain_user" PATCH maintain-maintainer-project-update '/users/test/projects/privatetestproject' '{
    "summary": "update"
}'

# Admin maintainers can update the project
fetch "$admin_user" PATCH admin-maintainer-project-update '/users/test/projects/privatetestproject' '{
    "summary": "update"
}'

# Expire the project owner's cloud subscription, thus disabling project-roles.
pg_sql "UPDATE public.cloud_subscribers SET is_active = false WHERE user_id = '${test_user##U-}'"

# All roles should still be listed.
fetch "$test_user" GET list-roles-non-premium '/users/test/projects/privatetestproject/roles'

# Should be unable to add new roles when the cloud subscription is expired.
fetch "$test_user" POST add-roles-non-premium '/users/test/projects/privatetestproject/roles' "
{
    \"role_assignments\": 
    [ { \"subject\": {\"kind\": \"user\", \"id\": \"${read_user}\"}
      , \"roles\": [\"project_maintainer\"]
      }
    ]
}"

# Can remove roles from users
# unmentioned users are left as-is,
fetch "$test_user" DELETE remove-roles '/users/test/projects/privatetestproject/roles' "
{
    \"role_assignments\": 
    [ { \"subject\": {\"kind\": \"user\", \"id\": \"${read_user}\"}
      , \"roles\": [\"project_viewer\"]
      }
      , { \"subject\": {\"kind\": \"user\", \"id\": \"${maintain_user}\"}
      , \"roles\": [\"project_maintainer\"]
      }
    ]
}"
