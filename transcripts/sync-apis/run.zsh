#!/usr/bin/env zsh

set -e

source "../transcript_helpers.sh"

for transcript in $(ls *.md | grep -v '^prelude.md$' | grep -v 'output.md$') ; do
   # Reset DB to a known state before each transcript
   pg_reset_fixtures

   echo "Running $transcript"
   transcript_ucm transcript "$transcript"
done
