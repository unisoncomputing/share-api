#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

read_maintainer="$(create_user read-maintainer)"
maintain_maintainer="$(create_user maintain-maintainer)"
admin_maintainer="$(create_user admin-maintainer)"

# Should fail
fetch "$unauthorized_user" GET non-maintainer-project-view '/users/test/projects/privatetestproject'
fetch "$unauthorized_user" PATCH non-maintainer-project-update '/users/test/projects/privatetestproject' '{
    "summary": "update"
}'

fetch "$test_user" POST add-roles '/users/test/projects/privatetestproject/roles/add' "
{
    \"role_assignments\": 
    [ { \"subject\": {\"kind\": \"user\", \"id\": \"${read_maintainer}\"}
      , \"roles\": [\"project_viewer\"]
      }
      , { \"subject\": {\"kind\": \"user\", \"id\": \"${maintain_maintainer}\"}
      , \"roles\": [\"project_contributor\"]
      }
      , { \"subject\": {\"kind\": \"user\", \"id\": \"${admin_maintainer}\"}
      , \"roles\": [\"project_owner\"]
      }
    ]
}"

# Non-owner should not be able to change roles
fetch "$maintain_maintainer" POST non-owner-add-roles '/users/test/projects/privatetestproject/roles/add' "
{
    \"role_assignments\": 
    [ { \"subject\": {\"kind\": \"user\", \"id\": \"${read_maintainer}\"}
      , \"roles\": [\"project_contributor\"]
      }
    ]
}"

fetch "$test_user" GET list-roles '/users/test/projects/privatetestproject/roles'

# Project owner can create tickets
fetch "$test_user" POST owner-ticket-create '/users/test/projects/privatetestproject/tickets' '{
    "title": "Ticket 1",
    "description": "My description"
}'

fetch "$read_maintainer" GET read-maintainer-project-view '/users/test/projects/privatetestproject'

# Read maintainer should be able to create tickets
fetch "$read_maintainer" POST read-maintainer-ticket-create '/users/test/projects/privatetestproject/tickets' '{
    "title": "Ticket 2",
    "description": "My description"
}'

# Read maintainers can't update the project
fetch "$read_maintainer" PATCH read-maintainer-project-update '/users/test/projects/privatetestproject' '{
    "summary": "update"
}'

# Read maintainers can't close other's tickets
fetch "$read_maintainer" PATCH read-maintainer-ticket-close '/users/test/projects/privatetestproject/tickets/1' '{
    "status": "closed"
}'

# Maintain maintainers can close any ticket
fetch "$maintain_maintainer" PATCH maintain-maintainer-ticket-close '/users/test/projects/privatetestproject/tickets/1' '{
    "status": "closed"
}'

# Maintain maintainers can't update the project
fetch "$maintain_maintainer" PATCH maintain-maintainer-project-update '/users/test/projects/privatetestproject' '{
    "summary": "update"
}'

# Admin maintainers can update the project
fetch "$admin_maintainer" PATCH admin-maintainer-project-update '/users/test/projects/privatetestproject' '{
    "summary": "update"
}'

# Expire the project owner's cloud subscription, thus disabling project-roles.
pg_sql "UPDATE public.cloud_subscribers SET is_active = false WHERE user_id = '${test_user##U-}'"

# All roles should still be listed.
fetch "$test_user" GET list-roles-non-premium '/users/test/projects/privatetestproject/roles'

# Should be unable to add new roles when the cloud subscription is expired.
fetch "$test_user" POST add-roles-non-premium '/users/test/projects/privatetestproject/roles/add' "
{
    \"role_assignments\": 
    [ { \"subject\": {\"kind\": \"user\", \"id\": \"${read_maintainer}\"}
      , \"roles\": [\"project_contributor\"]
      }
    ]
}"

# Can remove roles from users
# unmentioned users are left as-is,
fetch "$test_user" POST remove-roles '/users/test/projects/privatetestproject/roles/remove' "
{
    \"role_assignments\": 
    [ { \"subject\": {\"kind\": \"user\", \"id\": \"${read_maintainer}\"}
      , \"roles\": [\"project_viewer\"]
      }
      , { \"subject\": {\"kind\": \"user\", \"id\": \"${maintain_maintainer}\"}
      , \"roles\": [\"project_contributor\"]
      }
    ]
}"
