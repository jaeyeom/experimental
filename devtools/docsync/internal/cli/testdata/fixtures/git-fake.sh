#!/bin/sh
set -eu
case "${1-} ${2-}" in
  "diff --name-only")
    printf '%s\n' "${GIT_FAKE_DIFF-}"
    exit 0
    ;;
  "rev-parse --show-toplevel")
    printf '%s\n' "${GIT_FAKE_ROOT-$PWD}"
    exit 0
    ;;
  *)
    printf '%s\n' "git-fake: unexpected: $*" >&2
    exit 1
    ;;
esac
