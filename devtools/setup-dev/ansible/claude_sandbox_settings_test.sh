#!/bin/sh
# setup-claude-sandbox.yml writes sandbox.network.allowedDomains
# (jaeyeom/experimental#759). Only the claude-sandbox-settings tag runs,
# against a temp HOME, so the real ~/.claude and the tool installers are
# left alone.
#
#   - A fresh settings file gets the playbook hosts and no allowedHosts.
#   - allowedHosts, including a host the user added, joins allowedDomains
#     and the old key is removed. A backup is kept.
#   - Hosts already in allowedDomains stay, in front, with no duplicates.
#   - A second run changes nothing and writes no new backup.
set -eu

fail() {
	printf 'FAIL: %s\n' "$1" >&2
	exit 1
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
PLAYBOOK="${SCRIPT_DIR}/setup-claude-sandbox.yml"
INVENTORY="${SCRIPT_DIR}/inventory.ini"

if ! command -v ansible-playbook >/dev/null 2>&1; then
	echo "SKIP: ansible-playbook not found"
	exit 0
fi
ANSIBLE_BIN_DIR=$(dirname "$(command -v ansible-playbook)")

REAL_HOME=${HOME:-}
REAL_SETTINGS=""
REAL_SUM=""
if [ -n "$REAL_HOME" ]; then
	REAL_SETTINGS="${REAL_HOME}/.claude/settings.json"
	if [ -f "$REAL_SETTINGS" ]; then
		REAL_SUM=$(cksum "$REAL_SETTINGS")
	fi
fi

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT

export ANSIBLE_NOCOLOR=1
export ANSIBLE_STDOUT_CALLBACK=default
export ANSIBLE_RETRY_FILES_ENABLED=0
export ANSIBLE_LOCAL_TEMP="${ROOT}/ansible-local"
export ANSIBLE_REMOTE_TEMP="${ROOT}/ansible-remote"
export PATH="${ANSIBLE_BIN_DIR}:/usr/bin:/bin"

OUT="${ROOT}/out"

# playbook_domains prints the allowlist from the playbook, one host per line.
playbook_domains() {
	python3 -c '
import pathlib, re, sys
text = pathlib.Path(sys.argv[1]).read_text()
m = re.search(r"\n {10}allowedDomains:\n(.*?)(?=\n {10}[A-Za-z])", text, re.S)
if not m:
    sys.exit("allowedDomains block missing")
print("\n".join(re.findall(r"^ {12}- (\S+)", m.group(1), re.M)))
' "$PLAYBOOK"
}

# run uses HOME for ~/.claude/settings.json.
run() {
	if ! ansible-playbook -i "$INVENTORY" --tags claude-sandbox-settings "$PLAYBOOK" >"$OUT" 2>&1; then
		cat "$OUT" >&2
		fail "ansible-playbook failed"
	fi
	if grep -q 'libc.so' "$OUT"; then
		echo "SKIP: ansible-playbook cannot run in sandbox (libc issue)"
		exit 0
	fi
}

# use_home NAME switches HOME to a fresh temp directory.
use_home() {
	HOME="${ROOT}/$1"
	export HOME
	mkdir -p "$HOME"
}

settings_file() {
	printf '%s\n' "${HOME}/.claude/settings.json"
}

backup_count() {
	find "${HOME}/.claude" -maxdepth 1 -name 'settings.json.*' 2>/dev/null | wc -l | tr -d ' '
}

# assert_settings PATH KIND DOMAINS checks the written allowlist.
# DOMAINS is one host per line. migrate also requires theme dark.
assert_settings() {
	python3 -c '
import json, pathlib, sys
path, kind, raw = sys.argv[1], sys.argv[2], sys.argv[3]
want = [line for line in raw.splitlines() if line]
data = json.loads(pathlib.Path(path).read_text())
net = data["sandbox"]["network"]
if "allowedHosts" in net:
    sys.exit("allowedHosts still present: %s" % net)
got = net["allowedDomains"]
if kind not in ("fresh", "migrate"):
    sys.exit("unknown kind %s" % kind)
if got != want:
    sys.exit("domains %s, want %s" % (got, want))
if kind == "migrate" and data.get("theme") != "dark":
    sys.exit("theme lost: %s" % data.get("theme"))
' "$@"
}

DOMAINS=$(playbook_domains)
if [ -z "$DOMAINS" ]; then
	fail "playbook allowedDomains is empty"
fi

# Fresh settings.
use_home fresh
run
assert_settings "$(settings_file)" fresh "$DOMAINS"

# allowedHosts with a hand-added host, plus an unrelated key.
use_home hosts
mkdir -p "${HOME}/.claude"
python3 -c '
import json, pathlib, sys
hosts = [line for line in sys.argv[2].splitlines() if line]
path = pathlib.Path(sys.argv[1])
path.write_text(json.dumps({
    "theme": "dark",
    "sandbox": {"network": {"allowedHosts": ["extra.example", hosts[0]]}},
}))
' "${HOME}/.claude/settings.json" "$DOMAINS"
run
# extra.example is first, then every playbook host, including one that
# was also on allowedHosts.
assert_settings "$(settings_file)" migrate "$(printf 'extra.example\n%s\n' "$DOMAINS")"
if [ "$(backup_count)" -lt 1 ]; then
	fail "migration did not back up settings.json"
fi

# Existing allowedDomains stay in front. No duplicates.
use_home domains
mkdir -p "${HOME}/.claude"
python3 -c '
import json, pathlib, sys
hosts = [line for line in sys.argv[2].splitlines() if line]
path = pathlib.Path(sys.argv[1])
path.write_text(json.dumps({
    "theme": "dark",
    "sandbox": {"network": {"allowedDomains": ["extra.example", hosts[0]]}},
}))
' "${HOME}/.claude/settings.json" "$DOMAINS"
run
assert_settings "$(settings_file)" migrate "$(printf 'extra.example\n%s\n' "$DOMAINS")"
BEFORE=$(backup_count)
SUM=$(cksum "$(settings_file)")
run
if ! grep -q 'changed=0 ' "$OUT"; then
	cat "$OUT" >&2
	fail "second run changed something"
fi
if [ "$(cksum "$(settings_file)")" != "$SUM" ]; then
	fail "second run rewrote settings.json"
fi
if [ "$(backup_count)" != "$BEFORE" ]; then
	fail "second run wrote a new backup"
fi

if [ -n "$REAL_SUM" ] && [ "$(cksum "$REAL_SETTINGS")" != "$REAL_SUM" ]; then
	fail "real settings.json changed"
fi
