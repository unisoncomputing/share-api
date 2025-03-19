#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

some_user="$(create_user some-user)"
read_maintainer="$(create_user read-maintainer)"
maintain_maintainer="$(create_user maintain-maintainer)"
admin_maintainer="$(create_user admin-maintainer)"

# User without permission shouldn't be able to access private project.

fetch "$some_user" GET non-maintainer-project-view '/users/unison/projects/privateorgproject'

fetch "$test_user" GET org-roles-list '/orgs/unison/roles'

# Giving this user the project_contributor role on the Unison org should give them access to the project owned by that org.
# Typically you'd add the user to the org, but this is a good way to test JUST the resource role hierarchy.
fetch "$test_user" POST grant-project-contributor '/orgs/unison/roles' "
{
    \"role_assignments\": 
    [ { \"subject\": {\"kind\": \"user\", \"id\": \"${some_user}\"}
      , \"roles\": [\"project_contributor\"]
      }
    ]
}"

fetch "$some_user" GET maintainer-project-view '/users/unison/projects/privateorgproject'

# Remove the user from the org again
fetch "$test_user" DELETE revoke-project-contributor '/orgs/unison/roles' "
{
    \"role_assignments\": 
    [ { \"subject\": {\"kind\": \"user\", \"id\": \"${some_user}\"}
      , \"roles\": [\"project_contributor\"]
      }
    ]
}"

fetch "$some_user" GET non-maintainer-project-view '/users/unison/projects/privateorgproject'
