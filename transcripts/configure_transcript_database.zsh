#!/bin/zsh

set -e

PGPASSWORD="sekrit" psql -q -U postgres -p 5432 -h localhost -t -A -f "${SHARE_PROJECT_ROOT}/transcripts/sql/configure_transcript_database.sql" > /dev/null
