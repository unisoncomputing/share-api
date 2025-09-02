#!/bin/zsh

# Note: this doesn't work as intended yet.

set -e
set -u

. "${SHARE_PROJECT_ROOT}/transcripts/transcript_functions.sh"

cache_dir="$HOME/.cache/share-api"

if [ ! -d "$cache_dir" ]; then
  mkdir -p "$cache_dir"
fi

main_project_list="${SHARE_PROJECT_ROOT}/transcripts/fixtures/projects.txt"
custom_project_list="${SHARE_PROJECT_ROOT}/transcripts/fixtures/custom_projects.txt"
# create the custom projects file if it doesn't exist
if [ ! -f "$custom_project_list" ]; then
  touch "$custom_project_list"
fi

auth_transcript="$(mktemp).md"
pull_transcript="$(mktemp).md"
push_transcript="$(mktemp).md"

cat << EOF >"$auth_transcript"
\`\`\`ucm
scratch/main> auth.login
\`\`\`
EOF

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
done < <(cat "$main_project_list" "$custom_project_list") >"$pull_transcript"

while IFS= read -r line; do
  read -r project_source project_dest <<< "$line"
cat << EOF
\`\`\`ucm
${project_source}> push ${project_dest}
\`\`\`

EOF
done < <(cat "$main_project_list" "$custom_project_list")  >"$push_transcript"

echo "📫 Downloading projects from Share"
UNISON_SHARE_HOST="https://api.unison-lang.org" ucm -C "${cache_dir}/code-cache" transcript.in-place "$pull_transcript"
echo "🔑 Authenticating with local server..."
UNISON_SHARE_HOST="http://localhost:5424" ucm -c "${cache_dir}/code-cache" transcript.in-place "$auth_transcript"
echo "📦 Pushing projects to local server..."
UNISON_SHARE_HOST="http://localhost:5424" ucm -c "${cache_dir}/code-cache" transcript.in-place "$push_transcript"
