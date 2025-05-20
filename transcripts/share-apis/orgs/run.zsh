#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

new_member="$(create_user newuser)"


# Org creation workflow

# Should fail
fetch "$unauthorized_user" POST org-create-unauthorized-commercial '/orgs' '{
  "name": "ACME",
  "handle": "acme",
  "avatarUrl": "https://example.com/anvil.png",
  "owner": "unauthorized",
  "email": "wile.e.coyote@example.com",
  "isCommercial": true
}'

# Should succeed, regular users can create non-commercial orgs
fetch "$unauthorized_user" POST org-create-non-commercial-non-admin '/orgs' '{
  "name": "OrgCo",
  "handle": "orgco",
  "avatarUrl": "https://example.com/staples.png",
  "owner": "unauthorized",
  "email": "orgco@example.com",
  "isCommercial": false
}'

# Admin can create an org and assign any owner.
fetch "$admin_user" POST org-create-by-admin-non-commercial '/orgs' '{
  "name": "Noncom",
  "handle": "noncom",
  "avatarUrl": "https://example.com/peace.png",
  "owner": "transcripts",
  "isCommercial": false
}'

fetch "$admin_user" POST org-create-by-admin-commercial '/orgs' '{
  "name": "ACME",
  "handle": "acme",
  "avatarUrl": "https://example.com/anvil.png",
  "owner": "transcripts",
  "isCommercial": true
}'


# Owner can add members
fetch "$transcripts_user" POST org-add-members '/orgs/acme/members' '{
  "members": [
    "test"
  ]
}'

# Permissions within an org should show in the get-user endpoint
fetch "$transcripts_user" GET org-get-profile '/users/acme' 

# Managing org members lists

# Org member lists are public
fetch "$unauthorized_user" GET org-get-members-public '/orgs/acme/members'

# Members without permission can't edit members lists.
fetch "$unauthorized_user" POST org-add-members-unauthorized '/orgs/acme/members' '{
  "members": [
    "unauthorized"
  ]
}'

fetch "$transcripts_user" POST org-add-members '/orgs/acme/members' '{
  "members": [
    "newuser"
  ]
}'

# Can't add an org to another org
fetch "$transcripts_user" POST org-cant-have-org-members '/orgs/acme/members' '{
  "members": [
    "unison"
  ]
}'

fetch "$transcripts_user" GET org-get-members-after-adding '/orgs/acme/members'

# Owner can remove members
fetch "$transcripts_user" DELETE org-remove-members '/orgs/acme/members' '{
  "members": [
    "newuser"
  ]
}'

fetch "$transcripts_user" GET org-get-members-after-removing '/orgs/acme/members'

# Create projects in each org.
transcript_ucm transcript create-org-projects.md

# Get projects for each org
# Commercial projects should be private by default
fetch "$transcripts_user" GET commercial-org-projects '/users/acme/projects'

# Non-commercial projects must be public by default
fetch "$transcripts_user" GET non-commercial-org-projects '/users/noncom/projects'

# Updating a non-commercial org's project to private should fail
fetch "$transcripts_user" PATCH non-com-project-privatization '/users/noncom/projects/proj' '{
    "visibility": "private"
}'
