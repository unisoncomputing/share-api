#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

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

# Permissions within an org should show in the get-user endpoint
fetch "$transcript_user" GET org-get-profile '/users/acme' 
