#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

# Webhook, payload examples
fetch "$transcripts_user" GET webhook-examples '/webhooks/examples'
