#!/usr/bin/env zsh

set -e
set -u

unset UNISON_SYNC_VERSION

while ! (  curl http://localhost:5424/health > /dev/null 2> /dev/null )  do \
    echo "Waiting for share server to come up, did you run 'make serve'?"; \
    sleep 1; \
done;

source "$(realpath "$(dirname "$0")")/transcript_helpers.sh"

typeset -A transcripts
transcripts=(
    contribution-merge transcripts/share-apis/contribution-merge/
    search transcripts/share-apis/search/
    users transcripts/share-apis/users/
    user-creation transcripts/share-apis/user-creation/
    contribution-diffs transcripts/share-apis/contribution-diffs/
    definition-diffs transcripts/share-apis/definition-diffs/
    tickets transcripts/share-apis/tickets/
    contributions transcripts/share-apis/contributions/
    projects-flow transcripts/share-apis/projects-flow/
    project-maintainers transcripts/share-apis/project-maintainers/
    release transcripts/share-apis/releases/
    branche transcripts/share-apis/branches/
    branch-browse transcripts/share-apis/branch-browse/
    code-browse transcripts/share-apis/code-browse/
    sync-apis transcripts/sync-apis/
    orgs transcripts/share-apis/orgs/
    notifications transcripts/share-apis/notifications/
)

for transcript dir in "${(@kv)transcripts}"; do
    # If the first argument is missing, run all transcripts, otherwise run only transcripts which match a prefix of the argument
    if [ -z "${1:-}" ] || [[ "$transcript" == "$1"* ]]; then
        pg_reset_fixtures
        echo "Running transcript $transcript"
        (cd "$dir" && {rm -f ./*.json(N) || true} && ./run.zsh);
    fi
done
