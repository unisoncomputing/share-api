#!/bin/zsh

set -e

# Set up database so helper scripts know it's a local db
PGPASSWORD="sekrit" psql -q -U postgres -p 5432 -h localhost -t -A -f "${SHARE_PROJECT_ROOT}/transcripts/sql/configure_local_database.sql" > /dev/null
source "${SHARE_PROJECT_ROOT}/transcripts/transcript_helpers.sh"
