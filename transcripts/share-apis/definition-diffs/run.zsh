#!/bin/zsh

set -e
source ../../transcript_helpers.sh

# Reset DB to a known state
pg_reset_fixtures

transcript_ucm transcript prelude.md

# Diffing a user-defined term against a user-defined term.
fetch "$transcript_user" GET standard-term-diff '/users/transcripts/projects/definition-diff/diff/terms?oldBranchRef=before&newBranchRef=after&oldTerm=beforeTerm&newTerm=afterTerm'

# Diffing a builtin against a user-defined term.
fetch "$transcript_user" GET mismatched-term-diff '/users/transcripts/projects/definition-diff/diff/terms?oldBranchRef=before&newBranchRef=after&oldTerm=builtin.bug&newTerm=newBug'

# Diffing a user-defined type against a user-defined type.
fetch "$transcript_user" GET standard-type-diff '/users/transcripts/projects/definition-diff/diff/types?oldBranchRef=before&newBranchRef=after&oldType=BeforeType&newType=AfterType'

# Diffing a user-defined type against a user-defined type.
fetch "$transcript_user" GET standard-ability-diff '/users/transcripts/projects/definition-diff/diff/types?oldBranchRef=before&newBranchRef=after&oldType=BeforeAbility&newType=AfterAbility'

# Diffing a builtin against a user-defined type.
fetch "$transcript_user" GET mismatched-type-diff '/users/transcripts/projects/definition-diff/diff/types?oldBranchRef=before&newBranchRef=after&oldType=builtin.io2.IO&newType=NewIO'
