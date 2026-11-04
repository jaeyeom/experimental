#!/bin/sh
# Fake jira CLI for prsync tests. Reads JIRA_FAKE_DIR/<key>.json when present.
set -eu

if [ "${1-}" != "issue" ] || [ "${2-}" != "view" ] || [ "${4-}" != "--raw" ]; then
  printf '%s\n' "jira-fake: unexpected: $*" >&2
  exit 1
fi

key=${3-}
if [ -n "${JIRA_FAKE_LOG-}" ]; then
  printf '%s\n' "$key" >> "$JIRA_FAKE_LOG"
fi
if [ -n "${JIRA_FAKE_FAIL-}" ]; then
  printf '%s\n' "jira-fake: view failed" >&2
  exit 1
fi

dir=${JIRA_FAKE_DIR-}
if [ -n "$dir" ] && [ -f "$dir/$key.json" ]; then
  cat "$dir/$key.json"
  exit 0
fi
if [ -n "${JIRA_FAKE_STRICT-}" ]; then
  printf '%s\n' "jira-fake: no fixture for ${key}" >&2
  exit 1
fi

printf '%s\n' "{\"key\":\"${key}\",\"fields\":{\"summary\":\"${key}\",\"status\":{\"name\":\"To Do\",\"statusCategory\":{\"key\":\"new\"}},\"issuelinks\":[]}}"
exit 0
