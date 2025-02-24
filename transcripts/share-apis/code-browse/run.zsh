#!/usr/bin/env zsh

set -e

source "../../transcript_helpers.sh"

transcript_ucm transcript prelude.md

fetch "$transcript_user" GET codebase-browse "/users/transcripts/projects/code-browse/branches/main/browse?namespace=names"
fetch "$transcript_user" GET codebase-definition-by-name "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/compoundTerm?relativeTo=names.apples"
fetch "$transcript_user" GET codebase-definition-by-name-encoded "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/%2F%2B%25?relativeTo=names"
fetch "$transcript_user" GET codebase-definition-by-name-should-not-find-names-in-other-namespaces "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/externalName?relativeTo=names"
fetch "$transcript_user" GET codebase-definition-by-name-should-work-with-empty-perspective "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/names.oranges.two"
fetch "$transcript_user" GET codebase-definition-by-name-should-pretty-print-external-names-if-no-local-name "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/referencesExternal?relativeTo=names"
fetch "$transcript_user" GET codebase-definition-by-name-should-minimally-suffix "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/oranges.two?relativeTo=names"
fetch "$transcript_user" GET codebase-definition-by-name-should-minimally-suffix-again "/users/transcripts/projects/code-browse/branches/main/definitions/by-name/two?relativeTo=names.apples"
fetch "$transcript_user" GET codebase-definition-by-hash-term "/users/transcripts/projects/code-browse/branches/main/definitions/by-hash/@dcgdua2lj6upd1ah5v0qp09gjsej0d77d87fu6qn8e2qrssnlnmuinoio46hiu53magr7qn8vnqke8ndt0v76700o5u8gcvo7st28jg"
fetch "$transcript_user" GET codebase-definition-by-hash-constructor "/users/transcripts/projects/code-browse/branches/main/definitions/by-hash/@6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0@d0"
fetch "$transcript_user" GET codebase-find "/users/transcripts/projects/code-browse/branches/main/find?query=oranges.tw"
fetch "$transcript_user" GET codebase-namespace-by-name "/users/transcripts/projects/code-browse/branches/main/namespaces/by-name/names"
fetch "$transcript_user" GET codebase-namespace-by-name-root "/users/transcripts/projects/code-browse/branches/main/namespaces/by-name/"
fetch "$transcript_user" GET search "/search?query=te"
fetch "$transcript_user" GET account "/account"
fetch "$transcript_user" GET user-info "/user-info"
