#!/bin/sh
set -eu

DIR=$(cd -- "$(dirname -- "$0")" && pwd)

case "${1-}" in
  --version)
    printf '%s\n' "herdr 0.8.1"
    exit 0
    ;;
esac

case "${1-} ${2-}" in
  "tab list")
    cat "${DIR}/herdr-tab-list.json"
    exit 0
    ;;
  "agent list")
    cat "${DIR}/herdr-agent-list.json"
    exit 0
    ;;
  "pane current")
    cat "${DIR}/herdr-pane-current.json"
    exit 0
    ;;
  "agent prompt")
    cat "${DIR}/herdr-agent-prompt.json"
    exit 0
    ;;
  *)
    printf '%s\n' "herdr-fake: unexpected: $*" >&2
    exit 1
    ;;
esac
