#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Create a cookie jar we can use to store cookies for the new user we're goin to create.
new_user_cookie_jar=$(cookie_jar_for_user_id "new_user")

# Should be able to create a new user via the login flow by following redirects.
# Note that the end of the redirect chain ends up on the Share UI (localhost:1234) which may or may not be running, so
# we just ignore bad status codes from that server.
curl -v -L -I -o /dev/null -w '{"result_url": "%{url_effective}"}' --request "GET" --cookie "$new_user_cookie_jar" --cookie-jar "$new_user_cookie_jar" "http://localhost:5424/login" || true

# user should now be logged in as the local github user.
fetch "new_user" GET new-user-profile /account
