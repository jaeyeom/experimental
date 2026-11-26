#!/bin/sh
# setup-slackwatch.yml delegates skill installation to `slackwatch skill install --agent all --force`
# using `--dry-run` and `skill status --json` for idempotency. The playbook runs
# only the skill tasks (--tags slackwatch_skill) with a temp HOME and a PATH holding
# a fake go and, when wanted, a fake slackwatch under the fake GOPATH, so the real
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
PLAYBOOK="${SCRIPT_DIR}/setup-slackwatch.yml"
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
TRIAGE_HASH="sha256:triage-v1"
export TRIAGE_HASH
WITH_TRIAGE=0
export WITH_TRIAGE
DRY_RUN_FAIL_RC=0
export DRY_RUN_FAIL_RC

# write_fake_slackwatch installs a fake `slackwatch` implementing `skill install` and
# `skill status --json` using the same agent root paths as pkg/toolcli.
# When WITH_TRIAGE=1, it also manages `slackwatch-triage`.
write_fake_slackwatch() {
	cat >"${GOPATH_DIR}/bin/slackwatch" <<'EOF'
#!/bin/sh
set -eu
printf 'slackwatch %s\n' "$*" >>"$TRACE"

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
                printf 'fake slackwatch: unexpected flag: %s\n' "$1" >&2
                exit 2
                ;;
        esac
    done
    if [ "$agent" != "all" ]; then
        printf 'fake slackwatch: expected --agent all, got %s\n' "$agent" >&2
        exit 2
    fi
    if [ "$dry_run" -eq 1 ] && [ "${DRY_RUN_FAIL_RC:-0}" -ne 0 ]; then
        printf 'fake slackwatch: unexpected dry-run failure\n' >&2
        exit "$DRY_RUN_FAIL_RC"
    fi

    skills="slackwatch"
    if [ "${WITH_TRIAGE:-0}" -eq 1 ]; then
        skills="slackwatch slackwatch-triage"
    fi

    found=0
    for skill in $skills; do
        skill_hash="$CURRENT_HASH"
        if [ "$skill" = "slackwatch-triage" ]; then
            skill_hash="$TRIAGE_HASH"
        fi
        for spec in \
            "claude|.claude|.claude/skills/${skill}/SKILL.md" \
            "codex|.agents|.agents/skills/${skill}/SKILL.md" \
            "antigravity|.gemini|.gemini/config/skills/${skill}/SKILL.md" \
            "grok|.grok|.grok/skills/${skill}/SKILL.md"; do
            ag=${spec%%|*}
            rest=${spec#*|}
            root_rel=${rest%%|*}
            skill_rel=${rest#*|}
            if [ -d "${HOME}/${root_rel}" ]; then
                found=1
                dest="${HOME}/${skill_rel}"
                if [ -f "$dest" ] && ! grep -q "^toolcli-tool: ${skill}\$" "$dest" && [ "$force" -ne 1 ]; then
                    printf 'error: refusing to overwrite foreign file %s without --force\n' "$dest" >&2
                    exit 2
                fi
                if [ "$dry_run" -eq 1 ]; then
                    printf 'would install %s (user): %s\n' "$ag" "$dest"
                else
                    mkdir -p "$(dirname "$dest")"
                    printf '%s\n' \
                        '---' \
                        "name: ${skill}" \
                        "toolcli-tool: ${skill}" \
                        'toolcli-version: dev' \
                        "toolcli-hash: ${skill_hash}" \
                        '---' \
                        '' \
                        "Fake ${skill} skill." >"$dest"
                    printf 'installed %s (user): %s\n' "$ag" "$dest"
                fi
            fi
        done
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
want_main = os.environ["CURRENT_HASH"]
want_triage = os.environ["TRIAGE_HASH"]
with_triage = os.environ.get("WITH_TRIAGE", "0") == "1"

skills = [("slackwatch", want_main)]
if with_triage:
    skills.append(("slackwatch-triage", want_triage))

def inspect(skill, want_hash, path, scope):
    if scope == "project":
        return "foreign", None
    if not os.path.exists(path):
        return "missing", None
    with open(path, "r", encoding="utf-8") as f:
        text = f.read()
    if f"toolcli-tool: {skill}\n" not in text:
        return "foreign", None
    for line in text.splitlines():
        if line.startswith("toolcli-hash:"):
            h = line.split(":", 1)[1].strip()
            return ("current" if h == want_hash else "stale"), h
    return "stale", None

entries = []
for skill, want_hash in skills:
    specs = [
        ("claude", "user", os.path.join(home, ".claude", "skills", skill, "SKILL.md")),
        ("codex", "user", os.path.join(home, ".agents", "skills", skill, "SKILL.md")),
        ("antigravity", "user", os.path.join(home, ".gemini", "config", "skills", skill, "SKILL.md")),
        ("grok", "user", os.path.join(home, ".grok", "skills", skill, "SKILL.md")),
        # Project entry stays foreign to verify user-scope filtering ignores it.
        ("claude", "project", os.path.join(home, "project", ".claude", "skills", skill, "SKILL.md")),
    ]
    for agent, scope, path in specs:
        status, h = inspect(skill, want_hash, path, scope)
        entry = {"agent": agent, "scope": scope, "path": path, "status": status}
        if with_triage:
            entry["skill"] = skill
        if h:
            entry["installedVersion"] = "dev"
            entry["installedHash"] = h
        entries.append(entry)

print(json.dumps({"tool": "slackwatch", "currentHash": want_main, "entries": entries}))
PY
    exit 0
fi

printf 'fake slackwatch: unexpected call: %s\n' "$*" >&2
exit 1
EOF
	chmod +x "${GOPATH_DIR}/bin/slackwatch"
}

PYTHON_BIN="$PYTHON"
export PYTHON_BIN

PATH="${ROOT}/bin:${ANSIBLE_BIN_DIR}:/usr/bin:/bin"
export PATH
if [ -e "${ANSIBLE_BIN_DIR}/slackwatch" ]; then
	printf '%s\n' "SKIP: slackwatch is next to ansible-playbook; cannot hide it"
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
CLAUDE_SKILL="${HOME}/.claude/skills/slackwatch/SKILL.md"
GEMINI_SKILL="${HOME}/.gemini/config/skills/slackwatch/SKILL.md"
CODEX_SKILL="${HOME}/.agents/skills/slackwatch/SKILL.md"
CLAUDE_TRIAGE="${HOME}/.claude/skills/slackwatch-triage/SKILL.md"
GEMINI_TRIAGE="${HOME}/.gemini/config/skills/slackwatch-triage/SKILL.md"

try_playbook() {
	PLAYBOOK_RC=0
	ansible-playbook -i "$INVENTORY" --tags slackwatch_skill "$@" "$PLAYBOOK" >"$OUT" 2>&1 || PLAYBOOK_RC=$?
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

INSTALL="Install the slackwatch agent skill"

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
	_skill_name=${3:-slackwatch}
	_want_hash=${4:-$CURRENT_HASH}
	[ -f "$2" ] || fail "$1: $2 was not written"
	grep -q "^toolcli-tool: ${_skill_name}\$" "$2" || fail "$1: $2 missing toolcli-tool stamp (${_skill_name})"
	grep -q "^toolcli-hash: ${_want_hash}\$" "$2" || fail "$1: $2 missing hash ${_want_hash}"
}

trace_count() {
	grep -c "^$1\$" "$TRACE" 2>/dev/null || true
}

# --- 1. slackwatch missing: warn, write nothing, do not fail ---
: >"$TRACE"
run_playbook
grep -q 'WARNING: Skipping slackwatch skill install because slackwatch is not on PATH' "$OUT" || {
	cat "$OUT" >&2
	fail "missing: expected missing binary warning"
}
expect_install "missing" skipping
assert_not_written "missing" "$CLAUDE_SKILL"

# --- 2. no supported agents: warn, write nothing, do not fail ---
write_fake_slackwatch
: >"$TRACE"
run_playbook
grep -q 'WARNING: Skipping slackwatch skill install because no supported agents were found' "$OUT" || {
	cat "$OUT" >&2
	fail "no-agents: expected no supported agents warning"
}
expect_install "no-agents" skipping
assert_not_written "no-agents" "$CLAUDE_SKILL"
if [ "$(trace_count 'slackwatch skill install --agent all --force')" -ne 0 ]; then
	fail "no-agents: must not invoke non-dry-run install"
fi

# --- 3. --check mode: dry run writes nothing, never invokes non-dry-run install ---
mkdir -p "${HOME}/.claude" "${HOME}/.gemini"
: >"$TRACE"
run_playbook --check
expect_install "check" skipping
assert_not_written "check" "$CLAUDE_SKILL"
assert_not_written "check" "$GEMINI_SKILL"
if [ "$(trace_count 'slackwatch skill install --agent all --force --dry-run')" -ne 1 ]; then
	fail "check: expected dry-run invocation"
fi
if [ "$(trace_count 'slackwatch skill status --json')" -ne 1 ]; then
	fail "check: expected status --json invocation"
fi
if [ "$(trace_count 'slackwatch skill install --agent all --force')" -ne 0 ]; then
	fail "check: must not invoke non-dry-run install"
fi

# --- 4. fresh install without inbound: installs slackwatch for detected user-scope agents only ---
: >"$TRACE"
run_playbook
expect_install "fresh" changed
assert_stamped "fresh claude" "$CLAUDE_SKILL"
assert_stamped "fresh gemini" "$GEMINI_SKILL"
assert_not_written "fresh codex" "$CODEX_SKILL"
assert_not_written "fresh triage" "$CLAUDE_TRIAGE"
if [ "$(trace_count 'slackwatch skill install --agent all --force')" -ne 1 ]; then
	fail "fresh: expected one install --agent all --force call"
fi

# --- 5. idempotent rerun when already current ---
: >"$TRACE"
run_playbook
expect_install "rerun" skipping
assert_stamped "rerun claude" "$CLAUDE_SKILL"
assert_stamped "rerun gemini" "$GEMINI_SKILL"
if [ "$(trace_count 'slackwatch skill install --agent all --force')" -ne 0 ]; then
	fail "rerun: must not invoke install when all detected agents are current"
fi

# --- 6. enabling inbound installs slackwatch-triage even when slackwatch is already current ---
WITH_TRIAGE=1
: >"$TRACE"
run_playbook
expect_install "inbound enabled" changed
assert_stamped "inbound claude triage" "$CLAUDE_TRIAGE" slackwatch-triage "$TRIAGE_HASH"
assert_stamped "inbound gemini triage" "$GEMINI_TRIAGE" slackwatch-triage "$TRIAGE_HASH"

# rerun with triage current is idempotent
: >"$TRACE"
run_playbook
expect_install "inbound rerun" skipping
if [ "$(trace_count 'slackwatch skill install --agent all --force')" -ne 0 ]; then
	fail "inbound rerun: must not invoke install when both skills are current"
fi

# --- 7. --force migration of foreign SKILL.md (both primary and triage) ---
printf '%s\n' '---' 'name: slackwatch' '---' '' 'Legacy verified-run text.' >"$CLAUDE_SKILL"
printf '%s\n' '---' 'name: slackwatch-triage' '---' '' 'Legacy verified-run triage text.' >"$CLAUDE_TRIAGE"
: >"$TRACE"
run_playbook
expect_install "foreign migration" changed
assert_stamped "foreign migration main" "$CLAUDE_SKILL"
assert_stamped "foreign migration triage" "$CLAUDE_TRIAGE" slackwatch-triage "$TRIAGE_HASH"
if [ "$(trace_count 'slackwatch skill install --agent all --force')" -ne 1 ]; then
	fail "foreign migration: expected install --agent all --force call"
fi

# rerun after migration is idempotent again
: >"$TRACE"
run_playbook
expect_install "post-migration rerun" skipping
if [ "$(trace_count 'slackwatch skill install --agent all --force')" -ne 0 ]; then
	fail "post-migration rerun: must not invoke install"
fi

# --- 8. stale triage skill alone triggers reinstall ---
TRIAGE_HASH="sha256:triage-v2"
: >"$TRACE"
run_playbook
expect_install "stale triage update" changed
assert_stamped "stale triage claude" "$CLAUDE_TRIAGE" slackwatch-triage "$TRIAGE_HASH"
assert_stamped "stale triage gemini" "$GEMINI_TRIAGE" slackwatch-triage "$TRIAGE_HASH"

# --- 9. stale primary skill triggers reinstall ---
CURRENT_HASH="sha256:hash-v2"
: >"$TRACE"
run_playbook
expect_install "stale update" changed
assert_stamped "stale update claude" "$CLAUDE_SKILL"
assert_stamped "stale update gemini" "$GEMINI_SKILL"

# --- 10. unexpected dry-run failure fails the playbook ---
DRY_RUN_FAIL_RC=3
run_playbook_fails
DRY_RUN_FAIL_RC=0

# --- 11. ensure.sh no longer references removed slackwatch skill approval hooks ---
if grep -qE 'approve_slackwatch_skill|slackwatch_skill_approve\.sh|setup-slackwatch\.yml' "${SCRIPT_DIR}/ensure.sh"; then
	fail "ensure.sh still references removed slackwatch skill approval hooks"
fi

printf '%s\n' "PASS"
