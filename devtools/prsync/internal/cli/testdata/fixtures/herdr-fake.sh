#!/bin/sh
set -eu

DIR=$(cd -- "$(dirname -- "$0")" && pwd)
RUNTIME=${HERDR_FAKE_RUNTIME-}

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
  "tab close")
    if [ -n "${HERDR_FAKE_TAB_CLOSE_SENTINEL-}" ]; then
      printf '%s\n' "$*" > "$HERDR_FAKE_TAB_CLOSE_SENTINEL"
    fi
    case "${HERDR_FAKE_TAB_CLOSE-}" in
      not_found)
        printf '%s\n' "{\"error\":{\"code\":\"tab_not_found\",\"message\":\"tab ${3-} not found\"},\"id\":\"cli:tab:close\"}"
        exit 1
        ;;
      fail)
        printf '%s\n' "herdr-fake: tab close failed" >&2
        exit 1
        ;;
    esac
    printf '%s\n' '{"result":{"closed":true}}'
    exit 0
    ;;
  "agent list")
    status=${HERDR_FAKE_AGENT_STATUS:-idle}
    seq=1
    if [ -n "${HERDR_FAKE_STATUS_FILE-}" ] && [ -f "$HERDR_FAKE_STATUS_FILE" ]; then
      status=$(cat "$HERDR_FAKE_STATUS_FILE")
    fi
    tab_id=${HERDR_FAKE_TAB_ID:-w2:tC}
    pane_id=${HERDR_FAKE_PANE_ID:-w2:pC}
    if [ -n "$RUNTIME" ] && [ -f "$RUNTIME/prompted" ] && [ -z "${HERDR_FAKE_HOLD_IDLE-}" ]; then
      n=0
      if [ -f "$RUNTIME/listn" ]; then
        n=$(cat "$RUNTIME/listn")
      fi
      n=$((n + 1))
      printf '%s\n' "$n" > "$RUNTIME/listn"
      seq=$((n + 1))
      if [ "$n" -eq 1 ]; then
        status=working
      else
        status=idle
        if [ -n "${HERDR_FAKE_SETTLE-}" ]; then
          status=$HERDR_FAKE_SETTLE
        else
          case "${HERDR_FAKE_PROMPT-}" in
            blocked) status=blocked ;;
            'done') status='done' ;;
          esac
        fi
      fi
    fi
    use_dynamic=0
    if [ -n "${HERDR_FAKE_STATUS_FILE-}" ] || [ -n "${HERDR_FAKE_AGENT_STATUS-}" ] || [ -n "${HERDR_FAKE_TAB_ID-}" ] || [ -n "${HERDR_FAKE_PANE_ID-}" ]; then
      use_dynamic=1
    fi
    if [ -n "$RUNTIME" ] && [ -f "$RUNTIME/prompted" ]; then
      use_dynamic=1
    fi
    if [ "$use_dynamic" -eq 1 ]; then
      printf '%s\n' "{\"result\":{\"agents\":[{\"pane_id\":\"${pane_id}\",\"tab_id\":\"${tab_id}\",\"agent\":\"codex\",\"agent_status\":\"${status}\",\"state_change_seq\":${seq},\"revision\":${seq}}]}}"
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
    if [ -n "$RUNTIME" ]; then
      mkdir -p "$RUNTIME"
      printf '%s\n' "1" > "$RUNTIME/prompted"
      rm -f "$RUNTIME/listn"
    fi
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
