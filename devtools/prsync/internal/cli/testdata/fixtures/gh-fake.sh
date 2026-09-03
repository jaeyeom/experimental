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
    # scan passes flags first (`search prs --author ...`); the orphan report
    # passes a ticket positional (`search prs PROJ-123 --author ...`) or a
    # batched OR query as separate argv (`search prs A OR B --author ...`).
    case "${3-}" in
      --*|"")
        cat "${DIR}/gh-search-prs.json"
        ;;
      *)
        if [ -n "${GH_FAKE_SEARCH_EMPTY-}" ]; then
          printf '%s\n' "[]"
        else
          cat "${DIR}/gh-search-authored-prs.json"
        fi
        ;;
    esac
    exit 0
    ;;
  "pr list")
    cat "${DIR}/gh-pr-list.json"
    exit 0
    ;;
  "pr comment")
    if [ -n "${GH_FAKE_COMMENT_SENTINEL-}" ]; then
      printf '%s\n' "$*" > "$GH_FAKE_COMMENT_SENTINEL"
    fi
    if [ -n "${GH_FAKE_COMMENT_FAIL-}" ]; then
      printf '%s\n' "gh-fake: comment failed" >&2
      exit 1
    fi
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
