#!/bin/sh
set -eu

DIR=$(cd -- "$(dirname -- "$0")" && pwd)

case "${1-}" in
  --version)
    printf '%s\n' "${HERDR_FAKE_VERSION:-herdr 0.8.1}"
    exit 0
    ;;
esac

case "${1-} ${2-}" in
  "tab list")
    cat "${DIR}/herdr-tab-list.json"
    exit 0
    ;;
  "agent list")
    if [ -n "${HERDR_FAKE_STATUS_FILE-}" ] || [ -n "${HERDR_FAKE_AGENT_STATUS-}" ] || [ -n "${HERDR_FAKE_TAB_ID-}" ] || [ -n "${HERDR_FAKE_PANE_ID-}" ]; then
      status=${HERDR_FAKE_AGENT_STATUS:-idle}
      if [ -n "${HERDR_FAKE_STATUS_FILE-}" ] && [ -f "$HERDR_FAKE_STATUS_FILE" ]; then
        status=$(cat "$HERDR_FAKE_STATUS_FILE")
      fi
      tab_id=${HERDR_FAKE_TAB_ID:-w2:tC}
      pane_id=${HERDR_FAKE_PANE_ID:-w2:pC}
      printf '%s\n' "{\"result\":{\"agents\":[{\"pane_id\":\"${pane_id}\",\"tab_id\":\"${tab_id}\",\"agent\":\"codex\",\"agent_status\":\"${status}\"}]}}"
      exit 0
    fi
    cat "${DIR}/herdr-agent-list.json"
    exit 0
    ;;
  "pane current")
    if [ -n "${HERDR_FAKE_PANE_CURRENT_SENTINEL-}" ]; then
      printf '%s\n' "called" > "$HERDR_FAKE_PANE_CURRENT_SENTINEL"
    fi
    cat "${DIR}/herdr-pane-current.json"
    exit 0
    ;;
  "agent prompt")
    if [ -n "${HERDR_FAKE_PROMPT_SENTINEL-}" ]; then
      printf '%s\n' "called" > "$HERDR_FAKE_PROMPT_SENTINEL"
    fi
    if [ -n "${HERDR_FAKE_SETTLE-}" ] && [ -n "${HERDR_FAKE_STATUS_FILE-}" ]; then
      printf '%s\n' "$HERDR_FAKE_SETTLE" > "$HERDR_FAKE_STATUS_FILE"
    fi
    outcome=${HERDR_FAKE_PROMPT:-success}
    if [ -n "${HERDR_FAKE_PROMPT_SEQ-}" ]; then
      count=0
      if [ -n "${HERDR_FAKE_PROMPT_COUNT-}" ] && [ -f "$HERDR_FAKE_PROMPT_COUNT" ]; then
        count=$(cat "$HERDR_FAKE_PROMPT_COUNT")
      fi
      i=0
      rest=$HERDR_FAKE_PROMPT_SEQ
      while [ -n "$rest" ]; do
        tok=${rest%%,*}
        if [ "$rest" = "$tok" ]; then
          rest=
        else
          rest=${rest#*,}
        fi
        if [ "$i" -eq "$count" ]; then
          outcome=$tok
          break
        fi
        i=$((i + 1))
      done
      if [ -n "${HERDR_FAKE_PROMPT_COUNT-}" ]; then
        printf '%s\n' $((count + 1)) > "$HERDR_FAKE_PROMPT_COUNT"
      fi
    fi
    case "$outcome" in
      stall)
        cat "${DIR}/herdr-error-stalled.json" >&2
        exit 1
        ;;
      timeout)
        cat "${DIR}/herdr-error-timeout.json" >&2
        exit 1
        ;;
      kill)
        sleep 30
        exit 1
        ;;
      unparseable)
        printf '%s\n' "not-json"
        exit 0
        ;;
      done)
        cat "${DIR}/herdr-agent-prompt-done.json"
        exit 0
        ;;
      blocked)
        cat "${DIR}/herdr-agent-prompt-blocked.json"
        exit 0
        ;;
      success)
        cat "${DIR}/herdr-agent-prompt.json"
        exit 0
        ;;
      *)
        printf '%s\n' "herdr-fake: unknown prompt outcome: $outcome" >&2
        exit 1
        ;;
    esac
    ;;
  *)
    printf '%s\n' "herdr-fake: unexpected: $*" >&2
    exit 1
    ;;
esac
