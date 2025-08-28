#!/bin/zsh

set -e
set -u
set -x

. "${SHARE_PROJECT_ROOT}/transcripts/transcript_functions.sh"

cache_dir="$HOME/.cache/share-api"

if [ ! -d "$cache_dir" ]; then
  mkdir -p "$cache_dir"
fi

typeset -A projects
projects=(
    base '@unison/base'
)

for project_name project_ref in "${(@kv)projects}"; do
    echo "Downloading sync file for $project_ref"
    output_file="$(mktemp)"
    curl -X GET --location "https://api.unison-lang.org/ucm/v1/projects/project?name=${project_ref}" \
    --header 'Content-Type: application/json' \
    >"$output_file"

    echo "Response for project $project_ref:"
    cat "$output_file"

    latest_release="$(jq -r '.payload."latest-release"' <"$output_file")"
    projectId="$(jq -r '.payload."project-id"' <"$output_file")"
    branch_ref="releases/${latest_release}"
    project_branch_ref="${project_ref}/${branch_ref}"

    curl -X GET --location "https://api.unison-lang.org/ucm/v1/projects/project-branch?projectId=${projectId}&branchName=releases/${latest_release}" \
    --header 'Content-Type: application/json' \
    >"$output_file"
    branch_head="$(jq -r '.payload."branch-head"' <"$output_file")"

    echo "Response for project branch $project_branch_ref:"
    cat "$output_file"

    sync_file="$cache_dir/${project_branch_ref}"
    
    if [ -f "$sync_file" ]; then
        echo "Sync file for $project_branch_ref already exists, skipping download."
        continue
      else
        mkdir -p "$(dirname "$sync_file")"
        echo "Downloading sync file for $project_branch_ref into $sync_file"
        curl -X POST --location 'https://api.unison-lang.org/ucm/v2/sync/entities/download' \
        --header 'Content-Type: application/json' \
        --data-raw "{\"branchRef\": \"${project_branch_ref}\", \"causalHash\": \"${branch_head}\", \"knownHashes\":[]}" >"$cache_dir/${project_branch_ref}"
    fi
done
