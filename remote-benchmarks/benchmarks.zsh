#!/bin/zsh

set -e

# Bail if we don't have an argument
if [ -z "$1" ]; then
    echo "Usage: $0 <staging|prod> [benchmark-prefix]"
    exit 1
fi

benchmark_prefix="${2:-""}"

# set UCM_PATH to ucm if it's not already set
UCM_PATH="${UCM_PATH:-ucm}"


case "$1" in
    staging)
        export UNISON_SHARE_HOST="https://staging.api.unison-lang.org"
        # Postgres doesn't like multiple push workers
        export UNISON_SHARE_MAX_PUSH_WORKERS=1
        export env="staging"
        ;;
    prod*)
        export UNISON_SHARE_HOST="https://api.unison-lang.org"
        export env="prod"
        ;;
    *)
        echo "Usage: $0 [staging|prod] [UCM_PATH (e.g. unison-trunk)]"
        exit 1
        ;;
esac

typeset -A benchmarks
# Add benchmarks here:
benchmarks=(
    clone-base "clone-base.md"
    pull-base-release "pull-base-release.md"
)

cookie_jar="$(mktemp)"

exit_code=0

for benchmark transcript_path in "${(@kv)benchmarks}"; do
    # Skip benchmarks that don't match the prefix
    if [[ -n "$benchmark_prefix" && "$benchmark" != "${benchmark_prefix}-"* ]]; then
        continue
    fi
    timing_file="${benchmark}.${env}.time"
    echo "${transcript_path}"
    if ! /usr/bin/time -h -o "${timing_file}" "$UCM_PATH" transcript "${transcript_path}" ; then
        echo "Failed to run transcript ${transcript_path}" >&2
        exit_code=1
        continue
    fi
    date -u >> "${timing_file}"
done

exit "$exit_code"
