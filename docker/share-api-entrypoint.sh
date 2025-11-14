#!/bin/sh

set -ex

echo SHARE_REDIS: "$SHARE_REDIS"

if [ -n "$NOMAD_PORT_enlil_http" ]; then
    export SHARE_SERVER_PORT="$NOMAD_PORT_enlil_http"
fi

export SHARE_IP=0.0.0.0

exec 2>&1
exec /usr/local/bin/share
