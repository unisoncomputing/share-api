#!/bin/zsh

set -e

source "${SHARE_PROJECT_ROOT}/transcripts/transcript_functions.sh"
# Set up database so helper scripts know it's a local db
pg_file "${SHARE_PROJECT_ROOT}/transcripts/sql/configure_local_database.sql"
pg_init_fixtures
