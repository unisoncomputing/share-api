#!/bin/sh

set -ex

export SHARE_IP=0.0.0.0

exec 2>&1
exec /usr/local/bin/share-task-runner
