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

fetch "$test_user" POST add-maintainers '/users/test/projects/privatetestproject/maintainers' "
{
    \"maintainers\": 
    [ { \"user\": \"${read_maintainer}\"
      , \"permissions\": 
        { \"canView\": true
        , \"canMaintain\": false
        , \"canAdmin\": false
        }
      }
    , {\"user\": \"${maintain_maintainer}\"
      , \"permissions\": 
        { \"canView\": true
        , \"canMaintain\": true
        , \"canAdmin\": false
        }
      }
    , {\"user\": \"${admin_maintainer}\"
      , \"permissions\": 
        { \"canView\": true
        , \"canMaintain\": true
        , \"canAdmin\": true
        }
      }
    ]
}"

fetch "$test_user" GET list-maintainers '/users/test/projects/privatetestproject/maintainers'

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

# Expire the project owner's cloud subscription, thus disabling project-maintainers.
pg_sql "UPDATE public.cloud_subscribers SET is_active = false WHERE user_id = '${test_user##U-}'"


# All maintainers should still be listed.
fetch "$test_user" GET list-maintainers-non-premium '/users/test/projects/privatetestproject/maintainers'

# maintainers should now lose their permissions.
# E.g. read-maintainer should no longer be able to create tickets
fetch "$read_maintainer" POST read-maintainer-non-premium-ticket-create '/users/test/projects/privatetestproject/tickets' '{
    "title": "Ticket 3",
    "description": "My description"
}'

# Can update maintainers' permissions
# unmentioned users are left as-is,
# maintainers with no valid permissions are removed.
fetch "$test_user" PATCH update-maintainers '/users/test/projects/privatetestproject/maintainers' "
{
    \"maintainers\": 
    [ { \"user\": \"${read_maintainer}\"
      , \"permissions\": 
        { \"canView\": true
        , \"canMaintain\": true
        , \"canAdmin\": false
        }
      }
    , {\"user\": \"${maintain_maintainer}\"
      , \"permissions\": 
        { \"canView\": false
        , \"canMaintain\": false
        , \"canAdmin\": false
        }
      }
    ]
}"
