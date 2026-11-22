#!/bin/sh
# setup-claude-sandbox.yml writes sandbox.network.allowedDomains
# (jaeyeom/experimental#759), merges permissions.allow and
# permissions.deny (jaeyeom/experimental#820), and unions
# sandbox.network.allowUnixSockets (jaeyeom/experimental#850). Only the
# claude-sandbox-settings tag runs, against a temp HOME, so the real
# ~/.claude and the tool installers are left alone.
#
#   - A fresh settings file gets the playbook hosts and no allowedHosts.
#   - allowedHosts, including a host the user added, joins allowedDomains
#     and the old key is removed. A backup is kept.
#   - Hosts already in allowedDomains stay, in front, with no duplicates.
#   - A hand-added allow or deny rule stays, in front of the playbook's.
#   - A rule the user already has is not added again.
#   - A second run changes nothing and writes no new backup.
#   - A hand-added socket and the tunnel socket stay, in that order, and
#     docker, herdr, and the daemon socket follow. The tunnel is not
#     duplicated. The first run makes one backup; the second changes
#     nothing.
#   - claude_sandbox_os=Darwin uses ~/Library/Caches/agentbus/run.
#     claude_sandbox_os=Linux uses $XDG_RUNTIME_DIR, or /run/user/<uid>
#     when that variable is unset. HERDR_SOCKET_PATH overrides the
#     default herdr socket.
#   - A hand-added allowWrite entry stays first. Darwin adds
#     ~/Library/Caches/agentbus/state and ~/Library/Caches/agentbus/run.
#     A second run changes nothing and writes no new backup. Linux does
#     not add those two paths (jaeyeom/experimental#857).
#   - allowWrite gains the bazel output user root: the explicit variable,
#     else startup --output_user_root from ~/.bazelrc, then the system
#     rc, else the platform default. macOS adds the /private form of a
#     /var path. excludedCommands is a union that keeps hand-added
#     entries first; gh and jira join it when
#     claude_sandbox_exclude_tls_clients is on, which is the default on
#     Darwin only (jaeyeom/experimental#875).
#   - permissions.deny bans git worktree and EnterWorktree
#     (jaeyeom/experimental#982).
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
# The socket scenarios set these themselves.
unset HERDR_SOCKET_PATH XDG_RUNTIME_DIR

OUT="${ROOT}/out"

# The scenarios to run are the arguments. With none, all of them run.
# BUILD.bazel has one target per scenario, so editing a playbook reruns
# only the scenarios that use it.
SCENARIOS=" $* "
want() {
	case "$SCENARIOS" in
	"  ") return 0 ;;
	*" $1 "*) return 0 ;;
	esac
	return 1
}

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

# playbook_rules KEY prints permissions.allow or permissions.deny, one rule
# per line, without the surrounding quotes.
playbook_rules() {
	python3 -c '
import pathlib, re, sys
text = pathlib.Path(sys.argv[1]).read_text()
key = sys.argv[2]
if key == "allow":
    end = r"\n {8}deny:"
elif key == "deny":
    end = r"\n {2}tasks:"
else:
    sys.exit("unknown rules key")
m = re.search(r"\n {8}" + key + r":\n(.*?)" + end, text, re.S)
if not m:
    sys.exit(key + " block missing")
q = chr(34)
pat = r"^ {10}- " + q + "([^" + q + "]*)" + q
print("\n".join(re.findall(pat, m.group(1), re.M)))
' "$PLAYBOOK" "$1"
}

# assert_rules PATH ALLOW DENY checks permissions lists. ALLOW and DENY
# are one rule per line.
assert_rules() {
	python3 -c '
import json, pathlib, sys
path, allow_raw, deny_raw = sys.argv[1:]
want_allow = [line for line in allow_raw.splitlines() if line]
want_deny = [line for line in deny_raw.splitlines() if line]
data = json.loads(pathlib.Path(path).read_text())
perms = data.get("permissions") or {}
if perms.get("allow") != want_allow:
    sys.exit("allow %s, want %s" % (perms.get("allow"), want_allow))
if perms.get("deny") != want_deny:
    sys.exit("deny %s, want %s" % (perms.get("deny"), want_deny))
' "$@"
}

# union_rules EXISTING PLAYBOOK prints the order-preserving union.
union_rules() {
	python3 -c '
import sys
existing = [line for line in sys.argv[1].splitlines() if line]
playbook = [line for line in sys.argv[2].splitlines() if line]
out = list(existing)
for item in playbook:
    if item not in out:
        out.append(item)
print("\n".join(out))
' "$1" "$2"
}

# run uses HOME for ~/.claude/settings.json. Extra ansible-playbook
# arguments, such as -e claude_sandbox_os=Linux, come first.
run() {
	if ! ansible-playbook -i "$INVENTORY" --tags claude-sandbox-settings "$@" "$PLAYBOOK" >"$OUT" 2>&1; then
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

# assert_sockets PATH SOCKETS checks allowUnixSockets. SOCKETS is one
# path per line.
assert_sockets() {
	python3 -c '
import json, pathlib, sys
path, raw = sys.argv[1], sys.argv[2]
want = [line for line in raw.splitlines() if line]
data = json.loads(pathlib.Path(path).read_text())
got = data["sandbox"]["network"].get("allowUnixSockets")
if got != want:
    sys.exit("sockets %s, want %s" % (got, want))
' "$@"
}

DOMAINS=$(playbook_domains)
if [ -z "$DOMAINS" ]; then
	fail "playbook allowedDomains is empty"
fi
ALLOW=$(playbook_rules allow)
DENY=$(playbook_rules deny)
NPM_RULE='Bash(npx -y google-artifactregistry-auth)'
if [ -z "$ALLOW" ] || [ -z "$DENY" ]; then
	fail "playbook permission lists are empty"
fi
printf '%s\n' "$ALLOW" | grep -F -x "$NPM_RULE" >/dev/null || fail "playbook allow is missing the npm auth rule"
# Worktrees are banned (jaeyeom/experimental#982). The fresh-settings
# check below confirms both rules reach settings.json.
for rule in 'Bash(git worktree *)' 'EnterWorktree'; do
	printf '%s\n' "$DENY" | grep -F -x "$rule" >/dev/null || fail "playbook deny is missing $rule"
done

if want fresh; then
	# Fresh settings.
	use_home fresh
	run
	assert_settings "$(settings_file)" fresh "$DOMAINS"
	assert_rules "$(settings_file)" "$ALLOW" "$DENY"
fi

if want hosts; then
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
fi

if want domains; then
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
fi

if want perms; then
	# A hand-added allow and deny stay in front. The new rule is added once.
	use_home perms
	mkdir -p "${HOME}/.claude"
	python3 -c '
import json, pathlib, sys
path = pathlib.Path(sys.argv[1])
path.write_text(json.dumps({
    "theme": "dark",
    "permissions": {
        "allow": ["Bash(foo *)"],
        "deny": ["Bash(user-deny *)"],
    },
}))
' "${HOME}/.claude/settings.json"
	run
	assert_rules "$(settings_file)" \
		"$(union_rules "$(printf 'Bash(foo *)\n')" "$ALLOW")" \
		"$(union_rules "$(printf 'Bash(user-deny *)\n')" "$DENY")"
	if [ "$(backup_count)" -lt 1 ]; then
		fail "permission merge did not back up settings.json"
	fi
	BEFORE=$(backup_count)
	SUM=$(cksum "$(settings_file)")
	run
	if ! grep -q 'changed=0 ' "$OUT"; then
		cat "$OUT" >&2
		fail "permission rerun changed something"
	fi
	if [ "$(cksum "$(settings_file)")" != "$SUM" ]; then
		fail "permission rerun rewrote settings.json"
	fi
	if [ "$(backup_count)" != "$BEFORE" ]; then
		fail "permission rerun wrote a new backup"
	fi
fi

if want once; then
	# A rule the user already has is not duplicated.
	use_home once
	mkdir -p "${HOME}/.claude"
	python3 -c '
import json, pathlib, sys
path = pathlib.Path(sys.argv[1])
path.write_text(json.dumps({
    "permissions": {"allow": [sys.argv[2], "Bash(foo *)"]},
}))
' "${HOME}/.claude/settings.json" "$NPM_RULE"
	run
	GOT=$(python3 -c '
import json, pathlib, sys
data = json.loads(pathlib.Path(sys.argv[1]).read_text())
allow = data["permissions"]["allow"]
print(allow.count(sys.argv[2]))
print("Bash(foo *)" in allow)
' "$(settings_file)" "$NPM_RULE")
	case "$GOT" in
	"1
True") ;;
	*) fail "npm auth rule was not kept once: $GOT" ;;
	esac
fi

if want sockets; then
	# Hand-added socket and the tunnel stay in front. Docker, herdr, and
	# the daemon follow. The tunnel equals a playbook socket, so it is not
	# duplicated. Darwin paths come from HOME, not from a literal.
	use_home sockets
	unset HERDR_SOCKET_PATH
	unset XDG_RUNTIME_DIR
	mkdir -p "${HOME}/.claude"
	HERDR="${HOME}/.config/herdr/herdr.sock"
	DAEMON="${HOME}/Library/Caches/agentbus/run/agentbus/daemon.sock"
	TUNNEL="${HOME}/Library/Caches/agentbus/run/agentbus/tunnel.sock"
	python3 -c '
import json, pathlib, sys
path = pathlib.Path(sys.argv[1])
path.write_text(json.dumps({
    "theme": "dark",
    "sandbox": {"network": {"allowUnixSockets": [sys.argv[2], sys.argv[3]]}},
}))
' "${HOME}/.claude/settings.json" /tmp/custom.sock "$TUNNEL"
	run -e claude_sandbox_os=Darwin
	assert_sockets "$(settings_file)" "$(printf '%s\n' /tmp/custom.sock "$TUNNEL" /var/run/docker.sock "$HERDR" "$DAEMON")"
	assert_settings "$(settings_file)" migrate "$DOMAINS"
	if [ "$(backup_count)" -ne 1 ]; then
		fail "first socket run made $(backup_count) backups, want 1"
	fi
	BEFORE=$(backup_count)
	SUM=$(cksum "$(settings_file)")
	run -e claude_sandbox_os=Darwin
	if ! grep -q 'changed=0 ' "$OUT"; then
		cat "$OUT" >&2
		fail "socket rerun changed something"
	fi
	if [ "$(cksum "$(settings_file)")" != "$SUM" ]; then
		fail "socket rerun rewrote settings.json"
	fi
	if [ "$(backup_count)" != "$BEFORE" ]; then
		fail "socket rerun wrote a new backup"
	fi
fi

if want writes; then
	# A hand-added allowWrite stays first. Darwin adds the two agentbus dirs.
	use_home writes
	unset HERDR_SOCKET_PATH
	unset XDG_RUNTIME_DIR
	mkdir -p "${HOME}/.claude"
	STATE="${HOME}/Library/Caches/agentbus/state"
	RUNDIR="${HOME}/Library/Caches/agentbus/run"
	python3 -c '
import json, pathlib, sys
path = pathlib.Path(sys.argv[1])
path.write_text(json.dumps({
    "sandbox": {"filesystem": {"allowWrite": [sys.argv[2]]}},
}))
' "${HOME}/.claude/settings.json" /tmp/custom-write
	run -e claude_sandbox_os=Darwin
	python3 -c '
import json, pathlib, sys
data = json.loads(pathlib.Path(sys.argv[1]).read_text())
got = data["sandbox"]["filesystem"]["allowWrite"]
custom, state, rundir = sys.argv[2:]
if not got or got[0] != custom:
    sys.exit("custom write is not first: %s" % got)
if got.count(custom) != 1 or got.count(state) != 1 or got.count(rundir) != 1:
    sys.exit("write paths duplicated or missing: %s" % got)
' "$(settings_file)" /tmp/custom-write "$STATE" "$RUNDIR"
	if [ "$(backup_count)" -ne 1 ]; then
		fail "first write run made $(backup_count) backups, want 1"
	fi
	BEFORE=$(backup_count)
	SUM=$(cksum "$(settings_file)")
	run -e claude_sandbox_os=Darwin
	if ! grep -q 'changed=0 ' "$OUT"; then
		cat "$OUT" >&2
		fail "write rerun changed something"
	fi
	if [ "$(cksum "$(settings_file)")" != "$SUM" ]; then
		fail "write rerun rewrote settings.json"
	fi
	if [ "$(backup_count)" != "$BEFORE" ]; then
		fail "write rerun wrote a new backup"
	fi
fi

if want linux-xdg; then
	# Linux uses XDG_RUNTIME_DIR and HERDR_SOCKET_PATH when they are set.
	use_home linux-xdg
	export XDG_RUNTIME_DIR="${ROOT}/xdg"
	export HERDR_SOCKET_PATH="/tmp/herdr-from-env.sock"
	mkdir -p "${HOME}/.claude"
	printf '%s\n' '{}' >"${HOME}/.claude/settings.json"
	run -e claude_sandbox_os=Linux
	assert_sockets "$(settings_file)" "$(printf '%s\n' \
		/var/run/docker.sock \
		"$HERDR_SOCKET_PATH" \
		"${XDG_RUNTIME_DIR}/agentbus/daemon.sock" \
		"${XDG_RUNTIME_DIR}/agentbus/tunnel.sock")"
	python3 -c '
import json, pathlib, sys
data = json.loads(pathlib.Path(sys.argv[1]).read_text())
got = data["sandbox"]["filesystem"]["allowWrite"]
for path in sys.argv[2:]:
    if path in got:
        sys.exit("linux allowWrite has %s" % path)
' "$(settings_file)" \
		"${HOME}/Library/Caches/agentbus/state" \
		"${HOME}/Library/Caches/agentbus/run"
fi

if want linux-fallback; then
	# Linux without XDG_RUNTIME_DIR falls back to /run/user/<uid>.
	use_home linux-fallback
	unset HERDR_SOCKET_PATH
	unset XDG_RUNTIME_DIR
	mkdir -p "${HOME}/.claude"
	printf '%s\n' '{}' >"${HOME}/.claude/settings.json"
	USER_ID=$(id -u)
	run -e claude_sandbox_os=Linux
	assert_sockets "$(settings_file)" "$(printf '%s\n' \
		/var/run/docker.sock \
		"${HOME}/.config/herdr/herdr.sock" \
		"/run/user/${USER_ID}/agentbus/daemon.sock" \
		"/run/user/${USER_ID}/agentbus/tunnel.sock")"
	unset HERDR_SOCKET_PATH
	unset XDG_RUNTIME_DIR
fi

# assert_list PATH KEY WANT MISSING checks a sandbox list. KEY is
# excludedCommands or allowWrite. WANT is one entry per line and must
# appear in that order, not necessarily adjacent. MISSING is one entry
# per line that must not appear. No entry may appear twice.
assert_list() {
	python3 -c '
import json, pathlib, sys
path, key, want_raw, missing_raw = sys.argv[1:]
data = json.loads(pathlib.Path(path).read_text())
sandbox = data["sandbox"]
got = sandbox["filesystem"]["allowWrite"] if key == "allowWrite" else sandbox[key]
if len(got) != len(set(got)):
    sys.exit("%s has duplicates: %s" % (key, got))
pos = -1
for item in [line for line in want_raw.splitlines() if line]:
    if item not in got:
        sys.exit("%s is missing %s: %s" % (key, item, got))
    if got.index(item) < pos:
        sys.exit("%s has %s out of order: %s" % (key, item, got))
    pos = got.index(item)
for item in [line for line in missing_raw.splitlines() if line]:
    if item in got:
        sys.exit("%s has %s: %s" % (key, item, got))
' "$@"
}

# assert_rerun_clean LABEL ARGS... reruns with ARGS and checks that
# nothing changed and no backup was written.
assert_rerun_clean() {
	label=$1
	shift
	BEFORE=$(backup_count)
	SUM=$(cksum "$(settings_file)")
	run "$@"
	if ! grep -q 'changed=0 ' "$OUT"; then
		cat "$OUT" >&2
		fail "${label} rerun changed something"
	fi
	if [ "$(cksum "$(settings_file)")" != "$SUM" ]; then
		fail "${label} rerun rewrote settings.json"
	fi
	if [ "$(backup_count)" != "$BEFORE" ]; then
		fail "${label} rerun wrote a new backup"
	fi
}

# The bazel output user root joins allowWrite (jaeyeom/experimental#875).
# NO_SYSTEM_RC keeps a real /etc/bazel.bazelrc out of the test.
USER_NAME=$(id -un)
NO_SYSTEM_RC="${ROOT}/no-system-bazelrc"
printf '%s\n' 'startup --output_user_root=/opt/system-root' >"${ROOT}/system.bazelrc"

if want bazel-darwin; then
	# Darwin without a bazelrc uses /var/tmp and its /private form.
	use_home bazel-darwin
	mkdir -p "${HOME}/.claude"
	printf '%s\n' '{}' >"${HOME}/.claude/settings.json"
	run -e claude_sandbox_os=Darwin -e "claude_bazel_system_rc=${NO_SYSTEM_RC}"
	assert_list "$(settings_file)" allowWrite "$(printf '%s\n' \
		"${HOME}/.cache/bazel" \
		"/var/tmp/_bazel_${USER_NAME}" \
		"/private/var/tmp/_bazel_${USER_NAME}")" ""
	if [ "$(backup_count)" -ne 1 ]; then
		fail "first bazel run made $(backup_count) backups, want 1"
	fi
	assert_rerun_clean "bazel darwin" -e claude_sandbox_os=Darwin -e "claude_bazel_system_rc=${NO_SYSTEM_RC}"
fi

if want bazel-linux; then
	# Linux without a bazelrc uses ~/.cache/bazel/_bazel_<user>.
	use_home bazel-linux
	mkdir -p "${HOME}/.claude"
	printf '%s\n' '{}' >"${HOME}/.claude/settings.json"
	run -e claude_sandbox_os=Linux -e "claude_bazel_system_rc=${NO_SYSTEM_RC}"
	assert_list "$(settings_file)" allowWrite "$(printf '%s\n' \
		"${HOME}/.cache/bazel" \
		"${HOME}/.cache/bazel/_bazel_${USER_NAME}")" \
		"/var/tmp/_bazel_${USER_NAME}"
fi

if want bazel-rc; then
	# ~/.bazelrc --output_user_root wins over the system rc and the default.
	use_home bazel-rc
	mkdir -p "${HOME}/.claude"
	printf '%s\n' '{}' >"${HOME}/.claude/settings.json"
	printf '%s\n' '# comment' 'build --config=x' 'startup --output_user_root=/opt/bazel-root' >"${HOME}/.bazelrc"
	run -e claude_sandbox_os=Darwin -e "claude_bazel_system_rc=${ROOT}/system.bazelrc"
	assert_list "$(settings_file)" allowWrite "/opt/bazel-root" "$(printf '%s\n' \
		/opt/system-root \
		"/var/tmp/_bazel_${USER_NAME}")"
fi

if want bazel-system-rc; then
	# The system rc applies when ~/.bazelrc does not set it.
	use_home bazel-system-rc
	mkdir -p "${HOME}/.claude"
	printf '%s\n' '{}' >"${HOME}/.claude/settings.json"
	run -e claude_sandbox_os=Linux -e "claude_bazel_system_rc=${ROOT}/system.bazelrc"
	assert_list "$(settings_file)" allowWrite "/opt/system-root" \
		"${HOME}/.cache/bazel/_bazel_${USER_NAME}"
fi

if want bazel-explicit; then
	# An explicit claude_bazel_output_user_root wins over everything.
	use_home bazel-explicit
	mkdir -p "${HOME}/.claude"
	printf '%s\n' '{}' >"${HOME}/.claude/settings.json"
	printf '%s\n' 'startup --output_user_root=/opt/bazel-root' >"${HOME}/.bazelrc"
	run -e claude_sandbox_os=Darwin -e "claude_bazel_system_rc=${NO_SYSTEM_RC}" \
		-e claude_bazel_output_user_root=/var/folders/explicit
	assert_list "$(settings_file)" allowWrite "$(printf '%s\n' \
		/var/folders/explicit \
		/private/var/folders/explicit)" "/opt/bazel-root"
fi

if want excluded-darwin; then
	# excludedCommands is a union: hand-added entries stay first. Entries use
	# the "name *" form, because a bare name matches only a call with no
	# arguments. On Darwin the TLS flag defaults on and adds gh and jira.
	use_home excluded-darwin
	mkdir -p "${HOME}/.claude"
	python3 -c '
import json, pathlib, sys
path = pathlib.Path(sys.argv[1])
path.write_text(json.dumps({"sandbox": {"excludedCommands": ["mytool *", "git *"]}}))
' "${HOME}/.claude/settings.json"
	run -e claude_sandbox_os=Darwin -e "claude_bazel_system_rc=${NO_SYSTEM_RC}"
	assert_list "$(settings_file)" excludedCommands "$(printf '%s\n' \
		'mytool *' 'git *' 'bazel *' 'gh *' 'jira *')" "$(printf '%s\n' git gh)"
	assert_rerun_clean "excluded darwin" -e claude_sandbox_os=Darwin -e "claude_bazel_system_rc=${NO_SYSTEM_RC}"
fi

if want excluded-darwin-off; then
	# The flag off on Darwin leaves jira out.
	use_home excluded-darwin-off
	mkdir -p "${HOME}/.claude"
	printf '%s\n' '{}' >"${HOME}/.claude/settings.json"
	run -e claude_sandbox_os=Darwin -e "claude_bazel_system_rc=${NO_SYSTEM_RC}" \
		-e claude_sandbox_exclude_tls_clients=false
	assert_list "$(settings_file)" excludedCommands "$(printf '%s\n' 'bazel *' 'git *')" 'jira *'
fi

if want excluded-linux; then
	# Linux defaults the flag off. Turning it on adds jira.
	use_home excluded-linux
	mkdir -p "${HOME}/.claude"
	printf '%s\n' '{}' >"${HOME}/.claude/settings.json"
	run -e claude_sandbox_os=Linux -e "claude_bazel_system_rc=${NO_SYSTEM_RC}"
	assert_list "$(settings_file)" excludedCommands "$(printf '%s\n' 'bazel *' 'git *')" 'jira *'
	run -e claude_sandbox_os=Linux -e "claude_bazel_system_rc=${NO_SYSTEM_RC}" \
		-e claude_sandbox_exclude_tls_clients=true
	assert_list "$(settings_file)" excludedCommands "$(printf '%s\n' 'gh *' 'jira *')" ""
fi

if [ -n "$REAL_SUM" ] && [ "$(cksum "$REAL_SETTINGS")" != "$REAL_SUM" ]; then
	fail "real settings.json changed"
fi
