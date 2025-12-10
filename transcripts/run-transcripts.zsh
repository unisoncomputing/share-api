#!/usr/bin/env zsh

set -e
set -u

unset UNISON_SYNC_VERSION

while ! (  curl http://localhost:5424/health > /dev/null 2> /dev/null )  do \
    echo "Waiting for share server to come up, did you run 'make serve'?"; \
    sleep 1; \
done;

source "$(realpath "$(dirname "$0")")/transcript_helpers.sh"

echo "UCM Version: $(transcript_ucm --version)"

# Base directory containing share-api transcripts
transcripts_location="transcripts/share-apis"

# Find all directories within transcripts_location
for dir in "$transcripts_location"/*(/); do
    # Extract the directory name (transcript name)
    transcript="${dir:t}"
    
    # If the first argument is missing, run all transcripts, otherwise run only transcripts which match a prefix of the argument
    if [ -z "${1:-}" ] || [[ "$transcript" == "$1"* ]]; then
        pg_reset_fixtures
        echo "Running transcript $transcript"
        (cd "$dir" && rm -rf ./out && ./run.zsh)
    fi
done
