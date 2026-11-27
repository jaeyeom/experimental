#!/bin/sh
# setup-factory.yml delegates skill installation to `factory skill install --agent all --force`
# using `--dry-run` and `skill status --json` for idempotency. The playbook runs
# only the skill tasks (--tags factory_skill) with a temp HOME and a PATH holding
# a fake go and, when wanted, a fake factory under the fake GOPATH, so the real
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
PLAYBOOK="${SCRIPT_DIR}/setup-factory.yml"
INVENTORY="${SCRIPT_DIR}/inventory.ini"

# shellcheck disable=SC1091  # Sourced from the same directory as this script.
. "${SCRIPT_DIR}/ansible_test_env.sh"
ansible_test_init_env 1

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT
export TMPDIR="$ROOT"

mkdir -p "${ROOT}/bin"

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

CURRENT_HASH="sha256:hash-v1"
export CURRENT_HASH
DRY_RUN_FAIL_RC=0
export DRY_RUN_FAIL_RC

# setup_case NAME initializes isolated HOME, GOPATH_DIR, TRACE, OUT, and Ansible dirs.
setup_case() {
	_case_dir="${ROOT}/cases/$1"
	HOME="${_case_dir}/home"
	GOPATH_DIR="${_case_dir}/gopath"
	TRACE="${_case_dir}/trace"
	OUT="${_case_dir}/out"
	FACTS_JSON="${_case_dir}/facts.json"
	mkdir -p "$HOME" "${GOPATH_DIR}/bin"
	: >"$TRACE"
	export HOME GOPATH_DIR TRACE
	unset XDG_CONFIG_HOME GOPATH
	export ANSIBLE_HOME="${_case_dir}/ansible"
	export ANSIBLE_LOCAL_TEMP="${_case_dir}/ansible-local"
	export ANSIBLE_REMOTE_TEMP="${_case_dir}/ansible-remote"
	CLAUDE_SKILL="${HOME}/.claude/skills/factory/SKILL.md"
	GEMINI_SKILL="${HOME}/.gemini/config/skills/factory/SKILL.md"
	CODEX_SKILL="${HOME}/.agents/skills/factory/SKILL.md"
}

seed_skill() {
	_seed_dest="$1"
	_seed_hash="$2"
	mkdir -p "$(dirname "$_seed_dest")"
	printf '%s\n' \
		'---' \
		'name: factory' \
		'toolcli-tool: factory' \
		'toolcli-version: dev' \
		"toolcli-hash: ${_seed_hash}" \
		'---' \
		'' \
		'Fake factory skill.' >"$_seed_dest"
}

# write_fake_factory installs a fake `factory` implementing `skill install` and
# `skill status --json` using the same agent root paths as pkg/toolcli.
write_fake_factory() {
	cat >"${GOPATH_DIR}/bin/factory" <<'EOF'
#!/bin/sh
set -eu
printf 'factory %s\n' "$*" >>"$TRACE"

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
                printf 'fake factory: unexpected flag: %s\n' "$1" >&2
                exit 2
                ;;
        esac
    done
    if [ "$agent" != "all" ]; then
        printf 'fake factory: expected --agent all, got %s\n' "$agent" >&2
        exit 2
    fi
    if [ "$dry_run" -eq 1 ] && [ "${DRY_RUN_FAIL_RC:-0}" -ne 0 ]; then
        printf 'fake factory: unexpected dry-run failure\n' >&2
        exit "$DRY_RUN_FAIL_RC"
    fi

    found=0
    for spec in \
        "claude|.claude|.claude/skills/factory/SKILL.md" \
        "codex|.agents|.agents/skills/factory/SKILL.md" \
        "antigravity|.gemini|.gemini/config/skills/factory/SKILL.md" \
        "grok|.grok|.grok/skills/factory/SKILL.md"; do
        ag=${spec%%|*}
        rest=${spec#*|}
        root_rel=${rest%%|*}
        skill_rel=${rest#*|}
        if [ -d "${HOME}/${root_rel}" ]; then
            found=1
            dest="${HOME}/${skill_rel}"
            if [ -f "$dest" ] && ! grep -q '^toolcli-tool: factory$' "$dest" && [ "$force" -ne 1 ]; then
                printf 'factory: refusing to overwrite foreign file %s without --force\n' "$dest" >&2
                exit 2
            fi
            if [ "$dry_run" -eq 1 ]; then
                printf 'would install %s (user): %s\n' "$ag" "$dest"
            else
                mkdir -p "$(dirname "$dest")"
                printf '%s\n' \
                    '---' \
                    'name: factory' \
                    'toolcli-tool: factory' \
                    'toolcli-version: dev' \
                    "toolcli-hash: ${CURRENT_HASH}" \
                    '---' \
                    '' \
                    'Fake factory skill.' >"$dest"
                printf 'installed %s (user): %s\n' "$ag" "$dest"
            fi
        fi
    done

    if [ "$found" -eq 0 ]; then
        printf 'factory: no supported agents found for user scope\n' >&2
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
    ("claude", "user", os.path.join(home, ".claude", "skills", "factory", "SKILL.md")),
    ("codex", "user", os.path.join(home, ".agents", "skills", "factory", "SKILL.md")),
    ("antigravity", "user", os.path.join(home, ".gemini", "config", "skills", "factory", "SKILL.md")),
    ("grok", "user", os.path.join(home, ".grok", "skills", "factory", "SKILL.md")),
    # Project entry stays foreign to verify user-scope filtering ignores it.
    ("claude", "project", os.path.join(home, "project", ".claude", "skills", "factory", "SKILL.md")),
]

def inspect(path, scope):
    if scope == "project":
        return "foreign", None
    if not os.path.exists(path):
        return "missing", None
    with open(path, "r", encoding="utf-8") as f:
        text = f.read()
    if "toolcli-tool: factory\n" not in text:
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

print(json.dumps({"tool": "factory", "currentHash": want_hash, "entries": entries}))
PY
    exit 0
fi

printf 'fake factory: unexpected call: %s\n' "$*" >&2
exit 1
EOF
	chmod +x "${GOPATH_DIR}/bin/factory"
}

PYTHON_BIN="$PYTHON"
export PYTHON_BIN

PATH="${ROOT}/bin:${ANSIBLE_BIN_DIR}:/usr/bin:/bin"
export PATH
if [ -e "${ANSIBLE_BIN_DIR}/factory" ]; then
	printf '%s\n' "SKIP: factory is next to ansible-playbook; cannot hide it"
	exit 0
fi

try_playbook() {
	PLAYBOOK_RC=0
	ansible_test_write_facts "$FACTS_JSON"
	ansible-playbook -i "$INVENTORY" --tags factory_skill -e "@${FACTS_JSON}" "$@" "$PLAYBOOK" >"$OUT" 2>&1 || PLAYBOOK_RC=$?
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

INSTALL="Install the factory agent skill"

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
	grep -q '^toolcli-tool: factory$' "$2" || fail "$1: $2 missing toolcli-tool stamp"
	grep -q "^toolcli-hash: ${CURRENT_HASH}\$" "$2" || fail "$1: $2 missing hash ${CURRENT_HASH}"
}

trace_count() {
	grep -c "^$1\$" "$TRACE" 2>/dev/null || true
}

PIDS=""

# --- 1 & 2. factory missing, then no supported agents ---
(
	setup_case missing_no_agents
	: >"$TRACE"
	run_playbook
	grep -q 'WARNING: Skipping factory skill install because factory is not on PATH' "$OUT" || {
		cat "$OUT" >&2
		fail "missing: expected missing binary warning"
	}
	expect_install "missing" skipping
	assert_not_written "missing" "$CLAUDE_SKILL"

	write_fake_factory
	: >"$TRACE"
	run_playbook
	grep -q 'WARNING: Skipping factory skill install because no supported agents were found' "$OUT" || {
		cat "$OUT" >&2
		fail "no-agents: expected no supported agents warning"
	}
	expect_install "no-agents" skipping
	assert_not_written "no-agents" "$CLAUDE_SKILL"
	if [ "$(trace_count 'factory skill install --agent all --force')" -ne 0 ]; then
		fail "no-agents: must not invoke non-dry-run install"
	fi
) &
PIDS="$PIDS $!"

# --- 3 & 8. --check mode and unexpected dry-run failure ---
(
	setup_case check_fail
	write_fake_factory
	mkdir -p "${HOME}/.claude" "${HOME}/.gemini"
	: >"$TRACE"
	run_playbook --check
	expect_install "check" skipping
	assert_not_written "check" "$CLAUDE_SKILL"
	assert_not_written "check" "$GEMINI_SKILL"
	if [ "$(trace_count 'factory skill install --agent all --force --dry-run')" -ne 1 ]; then
		fail "check: expected dry-run invocation"
	fi
	if [ "$(trace_count 'factory skill status --json')" -ne 1 ]; then
		fail "check: expected status --json invocation"
	fi
	if [ "$(trace_count 'factory skill install --agent all --force')" -ne 0 ]; then
		fail "check: must not invoke non-dry-run install"
	fi

	DRY_RUN_FAIL_RC=3
	run_playbook_fails
	DRY_RUN_FAIL_RC=0
) &
PIDS="$PIDS $!"

# --- 4 & 5. fresh install, then idempotent rerun ---
(
	setup_case fresh_rerun
	write_fake_factory
	mkdir -p "${HOME}/.claude" "${HOME}/.gemini"
	: >"$TRACE"
	run_playbook
	expect_install "fresh" changed
	assert_stamped "fresh claude" "$CLAUDE_SKILL"
	assert_stamped "fresh gemini" "$GEMINI_SKILL"
	assert_not_written "fresh codex" "$CODEX_SKILL"
	if [ "$(trace_count 'factory skill install --agent all --force')" -ne 1 ]; then
		fail "fresh: expected one install --agent all --force call"
	fi

	: >"$TRACE"
	run_playbook
	expect_install "rerun" skipping
	assert_stamped "rerun claude" "$CLAUDE_SKILL"
	assert_stamped "rerun gemini" "$GEMINI_SKILL"
	if [ "$(trace_count 'factory skill install --agent all --force')" -ne 0 ]; then
		fail "rerun: must not invoke install when all detected agents are current"
	fi
) &
PIDS="$PIDS $!"

# --- 6. --force migration of foreign SKILL.md ---
(
	setup_case foreign
	write_fake_factory
	mkdir -p "${HOME}/.claude/skills/factory" "${HOME}/.gemini"
	seed_skill "$GEMINI_SKILL" "$CURRENT_HASH"
	printf '%s\n' '---' 'name: factory' '---' '' 'Legacy verified-run text.' >"$CLAUDE_SKILL"
	: >"$TRACE"
	run_playbook
	expect_install "foreign migration" changed
	assert_stamped "foreign migration" "$CLAUDE_SKILL"
	if [ "$(trace_count 'factory skill install --agent all --force')" -ne 1 ]; then
		fail "foreign migration: expected install --agent all --force call"
	fi

	: >"$TRACE"
	run_playbook
	expect_install "post-migration rerun" skipping
	if [ "$(trace_count 'factory skill install --agent all --force')" -ne 0 ]; then
		fail "post-migration rerun: must not invoke install"
	fi
) &
PIDS="$PIDS $!"

# --- 7. stale skill triggers reinstall ---
(
	setup_case stale
	write_fake_factory
	mkdir -p "${HOME}/.claude" "${HOME}/.gemini"
	seed_skill "$CLAUDE_SKILL" "$CURRENT_HASH"
	seed_skill "$GEMINI_SKILL" "$CURRENT_HASH"
	CURRENT_HASH="sha256:hash-v2"
	: >"$TRACE"
	run_playbook
	expect_install "stale update" changed
	assert_stamped "stale update claude" "$CLAUDE_SKILL"
	assert_stamped "stale update gemini" "$GEMINI_SKILL"
) &
PIDS="$PIDS $!"

# shellcheck disable=SC2086  # PIDS is space-separated integer PIDs.
ansible_test_wait_pids $PIDS || exit 1

printf '%s\n' "PASS"
