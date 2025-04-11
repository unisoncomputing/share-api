#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Create a cookie jar we can use to store cookies for the new user we're goin to create.
new_user_cookie_jar=$(cookie_jar_for_user_id "new_user")

# Should be able to create a new user via the login flow by following redirects.
curl -s -L -I -o /dev/null -w '{"status_code": %{http_code}, "result_url": "%{url_effective}"}' --request "GET" --cookie "$new_user_cookie_jar" --cookie-jar "$new_user_cookie_jar" "localhost:5424/login" > ./new-user-login.json

# user should now be logged in as the local github user.
fetch "new_user" GET new-user-profile /account
