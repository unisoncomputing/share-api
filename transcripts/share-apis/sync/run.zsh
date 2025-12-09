#!/usr/bin/env zsh

set -e
source ../../transcript_helpers.sh

# Reset DB to a known state
login_user_for_ucm 'transcripts'

for transcript in $(ls *.md | grep -v '^prelude.md$' | grep -v 'output.md$') ; do
   # Reset DB to a known state before each transcript
   pg_reset_fixtures

   echo "Running $transcript"
   if [[ "$transcript" == error* ]]; then
       if transcript_ucm transcript "$transcript"; then
           echo "Transcript $transcript was expected to fail but succeeded"
           exit 1
       else
           echo "Transcript $transcript failed as expected"
       fi
   else
       # Add logic here for non-error transcripts
       transcript_ucm transcript "$transcript"
   fi
done
