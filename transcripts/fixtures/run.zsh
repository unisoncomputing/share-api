#!/bin/zsh

set -e

source "$(realpath "$(dirname "$0")")/../transcript_helpers.sh"

pg_file "${transcripts_dir}/sql/inserts.sql" > /dev/null
