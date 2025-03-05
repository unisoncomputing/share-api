#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

read_maintainer="$(create_user read-maintainer)"
maintain_maintainer="$(create_user maintain-maintainer)"
admin_maintainer="$(create_user admin-maintainer)"
