#!/usr/bin/env bash

transcripts_dir="${SHARE_PROJECT_ROOT}/transcripts"
source "${transcripts_dir}/transcript_functions.sh"


# Set up users so we can auth against them.
pg_reset_fixtures

echo "Getting access token for transcript setup"

transcripts_user="$(user_id_from_handle 'transcripts')"
export transcripts_user
curl -s --cookie-jar "$(cookie_jar_for_user_id "$transcripts_user")" http://localhost:5424/local/user/transcripts/login > /dev/null

test_user="$(user_id_from_handle 'test')"
export test_user
curl -s --cookie-jar "$(cookie_jar_for_user_id "$test_user")" http://localhost:5424/local/user/test/login > /dev/null

admin_user="$(user_id_from_handle 'admin')"
export admin_user
curl -s --cookie-jar "$(cookie_jar_for_user_id "$admin_user")" http://localhost:5424/local/user/admin/login > /dev/null

unison_user="$(user_id_from_handle 'unison')"
export unison_user
curl -s --cookie-jar "$(cookie_jar_for_user_id "$unison_user")" http://localhost:5424/local/user/unison/login > /dev/null

unauthorized_user="$(user_id_from_handle 'unauthorized')"
export unauthorized_user
curl -s --cookie-jar "$(cookie_jar_for_user_id "$unauthorized_user")" http://localhost:5424/local/user/unauthorized/login > /dev/null

# Empty cookie file
unauthenticated_user="$(cookie_jar_for_user_id 'unauthenticated')"
export unauthenticated_user

login_user_for_ucm 'transcripts'
