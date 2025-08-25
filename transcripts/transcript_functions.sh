#!/usr/bin/env bash

# Sets up a bunch of utility functions and environment variables, 
# but doesn't run anything.
set -e

transcripts_dir="${SHARE_PROJECT_ROOT}/transcripts"
ucm_xdg_data_dir=$(mktemp -d)
mkdir -p "${ucm_xdg_data_dir}/unisonlanguage"
ucm_credentials_file="${ucm_xdg_data_dir}/unisonlanguage/credentials.json"

# Executable to use when running unison transcripts
export UCM_PATH="${1:-"$(which ucm)"}"
export empty_causal_hash='sg60bvjo91fsoo7pkh9gejbn0qgc95vra87ap6l5d35ri0lkaudl7bs12d71sf3fh6p23teemuor7mk1i9n567m50ibakcghjec5ajg'
export echo_server_port=9999
export echo_server="http://localhost:${echo_server_port}"
# if CI=true, change the echo server url to a url from the docker network.
if [ "${CI:-false}" = "true" ]; then
  export echo_server="http://http-echo:${echo_server_port}"
fi

# UCM to use within transcripts
transcript_ucm() {
  XDG_DATA_HOME="${ucm_xdg_data_dir}" UNISON_SHARE_HOST="http://localhost:5424" "${UCM_PATH}" "$@"
}

cookie_jar_dir=$(mktemp -d)
export cookie_jar_dir

cookie_jar_for_user_id () {
  if [ -z "$1" ]; then
    echo "cookie_jar_for_user_id requires a user id" >&2
    exit 1
  fi
  echo "${cookie_jar_dir}/$1"
}

# SQL stuff

# Run sql against the local pg
pg_sql () {
        PGPASSWORD="sekrit" psql -q -U postgres -p 5432 -h localhost -t -A -c "$1"
}

pg_file () {
        PGPASSWORD="sekrit" psql -q -U postgres -p 5432 -h localhost -t -A -f "$1"
}

pg_init_fixtures() {
        # Initialize the database with the schema and inserts
        pg_file "${transcripts_dir}/sql/inserts.sql" > /dev/null
}


# Reset all the fixtures to the state in `inserts.sql`
pg_reset_fixtures () {
        pg_file "${transcripts_dir}/sql/clean.sql" > /dev/null
        pg_init_fixtures
}

user_id_from_handle () {
  if [ -z "$1" ]; then
    echo "user_id_from_handle requires a handle" >&2
    exit 1
  fi
  handle="$1"
  pg_sql "SELECT 'U-' || id FROM users WHERE handle = '${handle}';"
}

project_id_from_handle_and_slug () {
  if [ -z "$1" ]; then
    echo "project_id_from_handle_and_slug requires a handle" >&2
    exit 1
  fi
  if [ -z "$2" ]; then
    echo "project_id_from_handle_and_slug requires a slug" >&2
    exit 1
  fi
  handle="$1"
  slug="$2"
  pg_sql "SELECT 'P-' || p.id FROM projects p JOIN users u ON p.owner_user_id = u.id WHERE u.handle = '${handle}' AND p.slug = '${slug}';"
}

# Creates a user and returns the user id
create_user () {
  handle="$1"
  uid=$(pg_sql "INSERT INTO users (handle, primary_email, email_verified) VALUES ('${handle}', '${handle}@example.com', true) RETURNING 'U-' || id;")
  # Log the user in, storing credentials in their cookie jar.
  curl -s --cookie-jar "$(cookie_jar_for_user_id "$uid")" "http://localhost:5424/local/user/${handle}/login" > /dev/null
  echo "$uid"
}

clean_for_transcript() {
    # Replace all ISO8601 in stdin with the string "<TIMESTAMP>"
    sed -E 's/[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?Z?/<TIMESTAMP>/g' | \
    # Replace all uuids in stdin with the string "<UUID>"
    sed -E 's/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/<UUID>/g' | \
    # Replace all cursors in stdin with the string "<CURSOR>"
    sed -E 's/Cursor": ?"[^"]+"/Cursor": "<CURSOR>"/g' | \
    # Replace all JWTs in stdin with the string "<JWT>"
    sed -E 's/eyJ([[:alnum:]_.-]+)/<JWT>/g' | \
    # In docker we network things together with host names, but locally we just use localhost; so 
    # this normalizes everything in transcripts.
    sed -E 's/(localhost|http-echo):/<HOST>:/g'
}

fetch() {
    testname="$3"
    result_file="$(mktemp)"
    status_code_file="$(mktemp)"
    api_path="$4"
    echo "${testname}" "${api_path}"
    fetch_data "$@" 2> "${status_code_file}" | clean_for_transcript > "${result_file}"
    # Try embedding the json response as-is, but if it's not valid json (e.g. it's an error message instead), embed it as a string.
    jq --sort-keys -n --slurpfile status "${status_code_file}" --slurpfile body "${result_file}" '{"status": $status, "body": ($body | .[0])}' > "./$testname.json" 2> /dev/null || {
        jq --sort-keys -n --slurpfile status "${status_code_file}" --rawfile body "${result_file}" '{"status": $status, "body": $body}'  > "./$testname.json"
    }
    jq_exit=$?
    if [ $jq_exit -ne 0 ]; then
        echo "Failed to parse JSON response for test ${testname} with jq exit code ${jq_exit}" >&2
        echo "Response body: $(cat "${result_file}")" >&2
        exit 1
    fi
}

# fetch which returns the result,
# stderr gets '{"status_code:xxx"}'
# stdout gets the body
fetch_data() {
    if [ "$#" -lt 4 ]; then
        echo "fetch requires at least 4 arguments: user_id, method, testname, api_path, [data]" >&2
        exit 1
    fi
    if [ -z "$1" ]; then
        echo "fetch requires a user id" >&2
        exit 1
    fi
    cookie_jar="$(cookie_jar_for_user_id "$1")"
    method="$2"
    testname="$3"
    api_path="$4"
    data="$5"
    url="http://localhost:5424${api_path}"
    result_file="$(mktemp)"
    status_code_file="$(mktemp)"

    case $method in
        GET)
            curl --request "GET" -L -s --cookie "$cookie_jar" -H "Accept: application/json" -w '%{stderr} {"status_code":%{http_code}}' "$url"
            ;;
        *)
            curl --request "$method" -L -s --cookie "$cookie_jar" -H "Accept: application/json" -H "Content-Type: application/json" --data-raw "$data" -w '%{stderr} {"status_code":%{http_code}}' "$url"
            ;;
    esac
}

fetch_data_jq() {
    if [ "$#" -lt 5 ]; then
        echo "fetch requires at least 5 arguments: user_id, method, testname, api_path, jq_pattern, [data]" >&2
        exit 1
    fi
    if [ -z "$1" ]; then
        echo "fetch requires a user id" >&2
        exit 1
    fi
    cookie_jar="$1"
    method="$2"
    testname="$3"
    api_path="$4"
    jq_pattern="$5"
    data="$6"
    fetch_data "$cookie_jar" "$method" "$testname" "$api_path" "$data" 2> /dev/null | \
      jq --sort-keys -r "$jq_pattern" 
}

# Credentials setup 

login_user_for_ucm() {
  if [ -z "$1" ]; then
    echo "login_user_for_ucm requires a user handle" >&2
    exit 1
  fi
  user_handle="$1"
  user_id=$(user_id_from_handle "$user_handle")
  access_token=$(curl -L -s "http://localhost:5424/local/user/${user_handle}/access-token" )
  now=$(date -u "+%F")

  # Save the credentials to a file so that UCM can find them
cat << EOF > "${ucm_credentials_file}"
{
  "active_profile": "default",
  "credentials": {
    "default": {
      "localhost:5424": {
        "discovery_uri": "http://localhost:5424/.well-known/openid-configuration",
        "fetch_time": "${now}T00:00:00.000000Z",
        "tokens": {
          "access_token": "${access_token}",
          "expires_in": 2592000,
          "id_token": null,
          "refresh_token": null,
          "scope": "openid cloud sync",
          "token_type": "bearer"
        },
        "user_info": {
          "handle": "${user_handle}",
          "name": "${user_handle}",
          "user_id": "${user_id}"
        }
      }
    }
  }
}
EOF
}

wait_for_diffs() {
# Since namespace diffs are computed asynchronously, we just block here until there are no diffs left in
# the causal_diff_queue.
for i in {1..5}; do
  if [[ $(pg_sql "select count(*) from causal_diff_queue;") -ne 0 ]]; then
    sleep 1
  else
    break
  fi
done
}
