#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

transcript_ucm transcript prelude.md

typeset -A tests
# Add tests here:
tests=(
    codebase-browse "/codebases/transcripts/browse?namespace=public.names"
    codebase-definition-by-name "/codebases/transcripts/definitions/by-name/compoundTerm?relativeTo=public.names.apples"
    codebase-definition-by-name-encoded "/codebases/transcripts/definitions/by-name/%2F%2B%25?relativeTo=public.names"
    codebase-definition-by-name-should-not-find-names-in-other-namespaces "/codebases/transcripts/definitions/by-name/externalName?relativeTo=public.names"
    codebase-definition-by-name-should-work-with-empty-perspective "/codebases/transcripts/definitions/by-name/public.names.oranges.two"
    codebase-definition-by-name-should-pretty-print-external-names-if-no-local-name "/codebases/transcripts/definitions/by-name/referencesExternal?relativeTo=public.names"
    codebase-definition-by-name-should-minimally-suffix "/codebases/transcripts/definitions/by-name/oranges.two?relativeTo=public.names"
    codebase-definition-by-name-should-minimally-suffix-again "/codebases/transcripts/definitions/by-name/two?relativeTo=public.names.apples"
    codebase-definition-by-hash-term "/codebases/transcripts/definitions/by-hash/@dcgdua2lj6upd1ah5v0qp09gjsej0d77d87fu6qn8e2qrssnlnmuinoio46hiu53magr7qn8vnqke8ndt0v76700o5u8gcvo7st28jg?relativeTo=public"
    codebase-definition-by-hash-constructor "/codebases/transcripts/definitions/by-hash/@6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0@d0?relativeTo=public"
    codebase-find "/codebases/transcripts/find?query=oranges.tw&relativeTo=public"
    codebase-namespace-by-name "/codebases/transcripts/namespaces/by-name/public.names"
    codebase-namespace-by-name-root "/codebases/transcripts/namespaces/by-name/"
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
