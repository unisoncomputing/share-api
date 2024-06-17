#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

transcript_ucm transcript prelude.md

typeset -A tests
# Add tests here:
tests=(
    codebase-browse "/users/transcripts/projects/code-browse/branches/main/browse?namespace=names"
    codebase-definition-by-name "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/compoundTerm?relativeTo=names.apples"
    codebase-definition-by-name-encoded "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/%2F%2B%25?relativeTo=names"
    codebase-definition-by-name-should-not-find-names-in-other-namespaces "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/externalName?relativeTo=names"
    codebase-definition-by-name-should-work-with-empty-perspective "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/names.oranges.two"
    codebase-definition-by-name-should-pretty-print-external-names-if-no-local-name "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/referencesExternal?relativeTo=names"
    codebase-definition-by-name-should-minimally-suffix "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/oranges.two?relativeTo=names"
    codebase-definition-by-name-should-minimally-suffix-again "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/two?relativeTo=names.apples"
    codebase-definition-by-hash-term "/users/transcripts/projects/code-browse/branches/main/definitions/by-hash/@dcgdua2lj6upd1ah5v0qp09gjsej0d77d87fu6qn8e2qrssnlnmuinoio46hiu53magr7qn8vnqke8ndt0v76700o5u8gcvo7st28jg"
    codebase-definition-by-hash-constructor "/users/transcripts/projects/code-browse/branches/main/definitions/by-hash/@6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0@d0"
    codebase-find "/users/transcripts/projects/code-browse/branches/main/find?query=oranges.tw"
    codebase-namespace-by-name "/users/transcripts/projects/code-browse/branches/main/namespaces/by-name/names"
    codebase-namespace-by-name-root "/users/transcripts/projects/code-browse/branches/main/namespaces/by-name/"
    search "/search?query=te"
    account "/account"
    user-info "/user-info"
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
