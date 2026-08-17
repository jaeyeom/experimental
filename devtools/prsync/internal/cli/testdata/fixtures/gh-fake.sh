#!/bin/sh
set -eu

DIR=$(cd -- "$(dirname -- "$0")" && pwd)
cmd=${1-}
sub=${2-}

case "${cmd} ${sub}" in
  "auth status")
    exit 0
    ;;
  "api user")
    printf '%s\n' "alice"
    exit 0
    ;;
  "search prs")
    cat "${DIR}/gh-search-prs.json"
    exit 0
    ;;
  "pr list")
    cat "${DIR}/gh-pr-list.json"
    exit 0
    ;;
  "api graphql")
    cat "${DIR}/gh-review-threads.json"
    exit 0
    ;;
  *)
    printf '%s\n' "gh-fake: unexpected: $*" >&2
    exit 1
    ;;
esac
