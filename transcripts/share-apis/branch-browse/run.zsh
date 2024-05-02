#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"
transcript_ucm transcript prelude.md

typeset -A tests
# Add tests here:
tests=(
    # Find everything in the Remote namespace and the ability too
    branch-find "/users/transcripts/projects/branch-browse/branches/main/find?limit=9&renderWidth=100&query=Remote"
    branch-definition-by-name "/users/transcripts/projects/branch-browse/branches/main/definitions/by-name/other.remoteMap?renderWidth=100"
    branch-definition-by-name-in-history "/users/transcripts/projects/branch-browse/branches/main/definitions/by-name/other.remoteMap?renderWidth=100&rootHash=61n6mn2ceb311k6v5b0muoarqivi7tdu44ma5d23g5egcdlo93f5v9trcpi2fd21hdkckcoho8i190qdq76jfvah9l29pb1i4bhtve0"
)

cookie_jar="$(mktemp)"

# Log in to the test user and save the credentials to use in tests.
echo "/local/user/transcripts/login"
curl -s --cookie-jar "$cookie_jar" http://localhost:5424/local/user/transcripts/login > /dev/null

exit_code=0

for testname api_path in "${(@kv)tests}"; do
    echo "${api_path}"
    url="http://localhost:5424${api_path}"
    result_file="$(mktemp)"
    curl -L -s --cookie "$cookie_jar" -H "Accept: application/json" -w '{"status_code":%{http_code}}' "http://localhost:5424${api_path}" > "${result_file}" 
    jq --sort-keys -s '{"status": .[1].status_code, "body": .[0]}' > "./$testname.json" <"${result_file}" || {
        echo "Failed to parse json response for ${url}:" 
        cat "${result_file}" >&2
        exit_code=1
    }
done

exit "$exit_code"
