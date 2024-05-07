#!/bin/sh

set -ex

echo SHARE_REDIS: "$SHARE_REDIS"

if [ -n "$NOMAD_PORT_share_tcp" ]; then
    export SHARE_CLIENT_PORT="$NOMAD_PORT_share_tcp"
fi

if [ -n "$NOMAD_IP_share_tcp" ]; then
    export SHARE_CLIENT_HOSTNAME="$NOMAD_IP_share_tcp"
fi

if [ -n "$NOMAD_PORT_share_http" ]; then
    export SHARE_SERVER_PORT="$NOMAD_PORT_share_http"
fi

export SHARE_IP=0.0.0.0

exec 2>&1
exec /usr/local/bin/share
