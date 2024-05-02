#!/bin/sh

set -ex

echo ENLIL_REDIS: "$ENLIL_REDIS"

if [ -n "$NOMAD_PORT_enlil_tcp" ]; then
    export ENLIL_CLIENT_PORT="$NOMAD_PORT_enlil_tcp"
fi

if [ -n "$NOMAD_IP_enlil_tcp" ]; then
    export ENLIL_CLIENT_HOSTNAME="$NOMAD_IP_enlil_tcp"
fi

if [ -n "$NOMAD_PORT_enlil_http" ]; then
    export ENLIL_SERVER_PORT="$NOMAD_PORT_enlil_http"
fi

export ENLIL_IP=0.0.0.0

exec 2>&1
exec /usr/local/bin/enlil
