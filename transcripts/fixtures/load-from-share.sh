#!/bin/zsh

# Note: this doesn't work as intended yet.

set -e
set -u
set -x

. "${SHARE_PROJECT_ROOT}/transcripts/transcript_functions.sh"

cache_dir="$HOME/.cache/share-api"

if [ ! -d "$cache_dir" ]; then
  mkdir -p "$cache_dir"
fi

project_list="${SHARE_PROJECT_ROOT}/transcripts/fixtures/projects.txt"

typeset -A projects
projects=(
    # pull from,                  push to
    '@unison/base/releases/4.8.0' '@test/base/main'
)

pull_transcript="$(mktemp).md"
push_transcript="$(mktemp).md"

while IFS= read -r line; do
  read -r project_source project_dest <<< "$line"
# Annoyingly clone will fail if it's already been cloned, but succeed otherwise, so we just add a bad command to
# force the block to always fail so we can use the :error directive.
cat << EOF
\`\`\`ucm:error
scratch/main> clone ${project_source}
scratch/main> force-failure
\`\`\`

EOF
done <"$project_list" >"$pull_transcript"

while IFS= read -r line; do
  read -r project_source project_dest <<< "$line"
cat << EOF
\`\`\`ucm
${project_source}> push ${project_dest}
\`\`\`

EOF
done <"$project_list"  >"$push_transcript"

UNISON_SHARE_HOST="https://api.unison-lang.org" ucm -C "${cache_dir}/code-cache" transcript.in-place "$pull_transcript"
UNISON_SHARE_HOST="http://localhost:5424" ucm -c "${cache_dir}/code-cache" transcript.in-place "$push_transcript"
