#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Should find all branches in the public project
fetch "$transcript_user" GET user-profile '/users/transcripts'

fetch "$transcript_user" PATCH user-profile-update '/users/transcripts' '
{
  "name": "Updated Transcripts",
  "avatarUrl": "http://updated.com",
  "bio": "Updated bio",
  "website": "http://updated-website.com",
  "location": "Updated location",
  "twitterHandle": "updated-twitter",
  "pronouns" : "updated/pronouns"
}
'

