#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

new_member="$(create_user newuser)"


# Org creation workflow

# Should fail
fetch "$unauthorized_user" POST org-create-unauthorized '/orgs' '{
  "name": "ACME",
  "handle": "acme",
  "avatarUrl": "https://example.com/anvil.png",
  "owner": "unauthorized",
  "email": "wile.e.coyote@example.com"
}'

# Admin can create an org and assign any owner.
fetch "$admin_user" POST org-create-by-admin '/orgs' '{
  "name": "ACME",
  "handle": "acme",
  "avatarUrl": "https://example.com/anvil.png",
  "owner": "transcripts",
  "email": "wile.e.coyote@example.com"
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
fetch "$transcript_user" POST org-cant-have-org-members '/orgs/acme/members' '{
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
