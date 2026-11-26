#!/bin/sh
# setup-decide.yml delegates skill installation to `decide skill install --agent all --force`
# using `--dry-run` and `skill status --json` for idempotency. The playbook runs
# only the skill tasks (--tags decide_skill) with a temp HOME and a PATH holding
# a fake go and, when wanted, a fake decide under the fake GOPATH, so the real
# ~/.claude and other agent directories are never read or written.
set -eu

fail() {
	printf 'FAIL: %s\n' "$1" >&2
	exit 1
}

if ! command -v ansible-playbook >/dev/null 2>&1; then
	printf '%s\n' "SKIP: ansible-playbook not found"
	exit 0
fi
ANSIBLE_BIN_DIR=$(dirname "$(command -v ansible-playbook)")
PYTHON=$(command -v python3 || true)
if [ -z "$PYTHON" ]; then
	fail "python3 is required"
fi

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
PLAYBOOK="${SCRIPT_DIR}/setup-decide.yml"
INVENTORY="${SCRIPT_DIR}/inventory.ini"

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT
export TMPDIR="$ROOT"

HOME="${ROOT}/home"
GOPATH_DIR="${ROOT}/gopath"
mkdir -p "$HOME" "${ROOT}/bin" "${GOPATH_DIR}/bin"
export HOME GOPATH_DIR
unset XDG_CONFIG_HOME GOPATH

# Fake go: only `go env GOPATH` is expected.
cat >"${ROOT}/bin/go" <<'EOF'
#!/bin/sh
if [ "$*" = "env GOPATH" ]; then
    printf '%s\n' "$GOPATH_DIR"
    exit 0
fi
printf 'fake go: unexpected call: %s\n' "$*" >&2
exit 1
EOF
chmod +x "${ROOT}/bin/go"

TRACE="${ROOT}/trace"
: >"$TRACE"
export TRACE
CURRENT_HASH="sha256:hash-v1"
export CURRENT_HASH
DRY_RUN_FAIL_RC=0
export DRY_RUN_FAIL_RC

# write_fake_decide installs a fake `decide` implementing `skill install` and
# `skill status --json` using the same agent root paths as pkg/toolcli.
write_fake_decide() {
	cat >"${GOPATH_DIR}/bin/decide" <<'EOF'
#!/bin/sh
set -eu
printf 'decide %s\n' "$*" >>"$TRACE"

if [ "$#" -ge 2 ] && [ "$1" = "skill" ] && [ "$2" = "install" ]; then
    shift 2
    dry_run=0
    force=0
    agent=""
    while [ "$#" -gt 0 ]; do
        case "$1" in
            --agent)
                agent="$2"
                shift 2
                ;;
            --force)
                force=1
                shift
                ;;
            --dry-run)
                dry_run=1
                shift
                ;;
            *)
                printf 'fake decide: unexpected flag: %s\n' "$1" >&2
                exit 2
                ;;
        esac
    done
    if [ "$agent" != "all" ]; then
        printf 'fake decide: expected --agent all, got %s\n' "$agent" >&2
        exit 2
    fi
    if [ "$dry_run" -eq 1 ] && [ "${DRY_RUN_FAIL_RC:-0}" -ne 0 ]; then
        printf 'fake decide: unexpected dry-run failure\n' >&2
        exit "$DRY_RUN_FAIL_RC"
    fi

    found=0
    for spec in \
        "claude|.claude|.claude/skills/decide/SKILL.md" \
        "codex|.agents|.agents/skills/decide/SKILL.md" \
        "antigravity|.gemini|.gemini/config/skills/decide/SKILL.md" \
        "grok|.grok|.grok/skills/decide/SKILL.md"; do
        ag=${spec%%|*}
        rest=${spec#*|}
        root_rel=${rest%%|*}
        skill_rel=${rest#*|}
        if [ -d "${HOME}/${root_rel}" ]; then
            found=1
            dest="${HOME}/${skill_rel}"
            if [ -f "$dest" ] && ! grep -q '^toolcli-tool: decide$' "$dest" && [ "$force" -ne 1 ]; then
                printf 'error: refusing to overwrite foreign file %s without --force\n' "$dest" >&2
                exit 2
            fi
            if [ "$dry_run" -eq 1 ]; then
                printf 'would install %s (user): %s\n' "$ag" "$dest"
            else
                mkdir -p "$(dirname "$dest")"
                printf '%s\n' \
                    '---' \
                    'name: decide' \
                    'toolcli-tool: decide' \
                    'toolcli-version: dev' \
                    "toolcli-hash: ${CURRENT_HASH}" \
                    '---' \
                    '' \
                    'Fake decide skill.' >"$dest"
                printf 'installed %s (user): %s\n' "$ag" "$dest"
            fi
        fi
    done

    if [ "$found" -eq 0 ]; then
        printf 'error: no supported agents found for user scope\n' >&2
        exit 2
    fi
    exit 0
fi

if [ "$*" = "skill status --json" ]; then
    "$PYTHON_BIN" - <<'PY'
import json
import os

home = os.environ["HOME"]
want_hash = os.environ["CURRENT_HASH"]
specs = [
    ("claude", "user", os.path.join(home, ".claude", "skills", "decide", "SKILL.md")),
    ("codex", "user", os.path.join(home, ".agents", "skills", "decide", "SKILL.md")),
    ("antigravity", "user", os.path.join(home, ".gemini", "config", "skills", "decide", "SKILL.md")),
    ("grok", "user", os.path.join(home, ".grok", "skills", "decide", "SKILL.md")),
    # Project entry stays foreign to verify user-scope filtering ignores it.
    ("claude", "project", os.path.join(home, "project", ".claude", "skills", "decide", "SKILL.md")),
]

def inspect(path, scope):
    if scope == "project":
        return "foreign", None
    if not os.path.exists(path):
        return "missing", None
    with open(path, "r", encoding="utf-8") as f:
        text = f.read()
    if "toolcli-tool: decide\n" not in text:
        return "foreign", None
    for line in text.splitlines():
        if line.startswith("toolcli-hash:"):
            h = line.split(":", 1)[1].strip()
            return ("current" if h == want_hash else "stale"), h
    return "stale", None

entries = []
for agent, scope, path in specs:
    status, h = inspect(path, scope)
    entry = {"agent": agent, "scope": scope, "path": path, "status": status}
    if h:
        entry["installedVersion"] = "dev"
        entry["installedHash"] = h
    entries.append(entry)

print(json.dumps({"tool": "decide", "currentHash": want_hash, "entries": entries}))
PY
    exit 0
fi

printf 'fake decide: unexpected call: %s\n' "$*" >&2
exit 1
EOF
	chmod +x "${GOPATH_DIR}/bin/decide"
}

PYTHON_BIN="$PYTHON"
export PYTHON_BIN

PATH="${ROOT}/bin:${ANSIBLE_BIN_DIR}:/usr/bin:/bin"
export PATH
if [ -e "${ANSIBLE_BIN_DIR}/decide" ]; then
	printf '%s\n' "SKIP: decide is next to ansible-playbook; cannot hide it"
	exit 0
fi

export ANSIBLE_HOME="${ROOT}/ansible"
export ANSIBLE_LOCAL_TEMP="${ROOT}/ansible/tmp"
export ANSIBLE_REMOTE_TEMP="${ROOT}/ansible/tmp"
export ANSIBLE_NOCOLOR=1
export ANSIBLE_STDOUT_CALLBACK=default
export ANSIBLE_DISPLAY_SKIPPED_HOSTS=1
export ANSIBLE_GATHERING=smart
export ANSIBLE_RETRY_FILES_ENABLED=0

OUT="${ROOT}/out"
CLAUDE_SKILL="${HOME}/.claude/skills/decide/SKILL.md"
GEMINI_SKILL="${HOME}/.gemini/config/skills/decide/SKILL.md"
CODEX_SKILL="${HOME}/.agents/skills/decide/SKILL.md"

try_playbook() {
	PLAYBOOK_RC=0
	ansible-playbook -i "$INVENTORY" --tags decide_skill "$@" "$PLAYBOOK" >"$OUT" 2>&1 || PLAYBOOK_RC=$?
	if grep -q 'libc.so' "$OUT"; then
		printf '%s\n' "SKIP: ansible-playbook cannot run in sandbox (libc issue)"
		exit 0
	fi
}

run_playbook() {
	try_playbook "$@"
	if [ "$PLAYBOOK_RC" -ne 0 ]; then
		cat "$OUT" >&2
		fail "ansible-playbook failed: $*"
	fi
}

run_playbook_fails() {
	try_playbook
	if [ "$PLAYBOOK_RC" -eq 0 ]; then
		cat "$OUT" >&2
		fail "ansible-playbook succeeded, want failure"
	fi
}

task_result() {
	awk -v name="TASK [$1]" '
        index($0, name) == 1 { found = 1; next }
        found && /^(ok|changed|skipping|fatal|failed):/ { sub(/:.*/, ""); print; exit }
        found && /^TASK \[/ { exit }
    ' "$OUT"
}

INSTALL="Install the decide agent skill"

expect_install() {
	_got=$(task_result "$INSTALL")
	if [ "$_got" != "$2" ]; then
		cat "$OUT" >&2
		fail "$1: install task got '${_got}', want $2"
	fi
}

assert_not_written() {
	if [ -e "$2" ]; then
		fail "$1: $2 was written"
	fi
}

assert_stamped() {
	[ -f "$2" ] || fail "$1: $2 was not written"
	grep -q '^toolcli-tool: decide$' "$2" || fail "$1: $2 missing toolcli-tool stamp"
	grep -q "^toolcli-hash: ${CURRENT_HASH}\$" "$2" || fail "$1: $2 missing hash ${CURRENT_HASH}"
}

trace_count() {
	grep -c "^$1\$" "$TRACE" 2>/dev/null || true
}

# --- 1. decide missing: warn, write nothing, do not fail ---
: >"$TRACE"
run_playbook
grep -q 'WARNING: Skipping decide skill install because decide is not on PATH' "$OUT" || {
	cat "$OUT" >&2
	fail "missing: expected missing binary warning"
}
expect_install "missing" skipping
assert_not_written "missing" "$CLAUDE_SKILL"

# --- 2. no supported agents: warn, write nothing, do not fail ---
write_fake_decide
: >"$TRACE"
run_playbook
grep -q 'WARNING: Skipping decide skill install because no supported agents were found' "$OUT" || {
	cat "$OUT" >&2
	fail "no-agents: expected no supported agents warning"
}
expect_install "no-agents" skipping
assert_not_written "no-agents" "$CLAUDE_SKILL"
if [ "$(trace_count 'decide skill install --agent all --force')" -ne 0 ]; then
	fail "no-agents: must not invoke non-dry-run install"
fi

# --- 3. --check mode: dry run writes nothing, never invokes non-dry-run install ---
mkdir -p "${HOME}/.claude" "${HOME}/.gemini"
: >"$TRACE"
run_playbook --check
expect_install "check" skipping
assert_not_written "check" "$CLAUDE_SKILL"
assert_not_written "check" "$GEMINI_SKILL"
if [ "$(trace_count 'decide skill install --agent all --force --dry-run')" -ne 1 ]; then
	fail "check: expected dry-run invocation"
fi
if [ "$(trace_count 'decide skill status --json')" -ne 1 ]; then
	fail "check: expected status --json invocation"
fi
if [ "$(trace_count 'decide skill install --agent all --force')" -ne 0 ]; then
	fail "check: must not invoke non-dry-run install"
fi

# --- 4. fresh install: installs for detected user-scope agents only ---
: >"$TRACE"
run_playbook
expect_install "fresh" changed
assert_stamped "fresh claude" "$CLAUDE_SKILL"
assert_stamped "fresh gemini" "$GEMINI_SKILL"
assert_not_written "fresh codex" "$CODEX_SKILL"
if [ "$(trace_count 'decide skill install --agent all --force')" -ne 1 ]; then
	fail "fresh: expected one install --agent all --force call"
fi

# --- 5. idempotent rerun when already current ---
: >"$TRACE"
run_playbook
expect_install "rerun" skipping
assert_stamped "rerun claude" "$CLAUDE_SKILL"
assert_stamped "rerun gemini" "$GEMINI_SKILL"
if [ "$(trace_count 'decide skill install --agent all --force')" -ne 0 ]; then
	fail "rerun: must not invoke install when all detected agents are current"
fi

# --- 6. --force migration of foreign SKILL.md ---
printf '%s\n' '---' 'name: decide' '---' '' 'Legacy verified-run text.' >"$CLAUDE_SKILL"
: >"$TRACE"
run_playbook
expect_install "foreign migration" changed
assert_stamped "foreign migration" "$CLAUDE_SKILL"
if [ "$(trace_count 'decide skill install --agent all --force')" -ne 1 ]; then
	fail "foreign migration: expected install --agent all --force call"
fi

# rerun after migration is idempotent again
: >"$TRACE"
run_playbook
expect_install "post-migration rerun" skipping
if [ "$(trace_count 'decide skill install --agent all --force')" -ne 0 ]; then
	fail "post-migration rerun: must not invoke install"
fi

# --- 7. stale skill triggers reinstall ---
CURRENT_HASH="sha256:hash-v2"
: >"$TRACE"
run_playbook
expect_install "stale update" changed
assert_stamped "stale update claude" "$CLAUDE_SKILL"
assert_stamped "stale update gemini" "$GEMINI_SKILL"

# --- 8. unexpected dry-run failure fails the playbook ---
DRY_RUN_FAIL_RC=3
run_playbook_fails
DRY_RUN_FAIL_RC=0

printf '%s\n' "PASS"
