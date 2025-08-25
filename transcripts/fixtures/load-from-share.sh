#!/bin/sh

set -e
set -u

. "${SHARE_PROJECT_ROOT}/transcripts/transcript_functions.sh"

cache_dir="$HOME/.cache/share-api"

if [ ! -d "$cache_dir" ]; then
  mkdir -p "$cache_dir"
fi

if [ ! -f "$cache_dir/base" ]; then
  # Fetch the sync file for 
  curl -X POST --location 'api.unison-lang.org/ucm/v2/sync/entities/download' \
  --header 'Content-Type: application/json' \
  --header 'Accept-Encoding: deflate, gzip' -v -N \
  --data-raw '{"branchRef": "@unison/base/main", "causalHash": "eyJhbGciOiJIUzI1NiIsImtpZCI6IlIzNUJIQkI1eFY2RGZSekNCdGFKRXFQdjZDZGVDSGV6TzE5bTZpMnA2cGsifQ.eyJoIjoiNnZ1Z2wwczU2OGhjbWRmNHJoY29sdWNqa2RyMjE1YTNoMTF0MTFvYnNoMDdyYXNiMDBldTVobmp1NWVwbDBjcWQwcTFwbXFsZGRvN2VnbHBpcnZwb2FtMWd1Zm9sYmJzbHY3NG1nOCIsInQiOiJoaiIsInUiOm51bGx9.fi7wzrapDJtoYWpjCy3TNPnKeQikUlJF_6bSoHAbQnw", "knownHashes":[]}' >"$cache_dir/base"
fi

transcript_ucm transcript prelude.md
