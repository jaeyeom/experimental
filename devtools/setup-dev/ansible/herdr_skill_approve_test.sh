#!/bin/sh
# ensure.sh reviews herdr --skill in this shell, after herdr.yml, because
# the Ansible command module has no controlling terminal.
# HOME is a temp directory. Nothing in the environment skips the prompt.
set -eu

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    exit 1
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HELPER="${SCRIPT_DIR}/herdr_skill_approve.sh"
PYTHON=$(command -v python3 || true)
if [ -z "$PYTHON" ]; then
    fail "python3 is required to drive the pty"
fi

if [ ! -f "$HELPER" ]; then
    fail "herdr_skill_approve.sh is missing"
fi

sh -n "$HELPER"
sh -n "$SCRIPT_DIR/herdr_skill_approve_test.sh"
# shellcheck disable=SC1091  # Sourced from the same directory as this script
. "$SCRIPT_DIR/playbook_resume.sh"
# shellcheck disable=SC1091  # Sourced from the same directory as this script
. "$SCRIPT_DIR/herdr_skill_approve.sh"

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT
export TMPDIR="$ROOT"
# delta pages on a tty. cat keeps the review in the capture. diff is the
# fallback when delta is off PATH.
export DELTA_PAGER=cat
export PAGER=cat
export GIT_PAGER=cat

CASE=""
TRACE=""
SKILL_BODY=""
SKILL_RC=""
FAIL_ANSIBLE=""

reset_case() {
    CASE=$(mktemp -d)
    HOME="${CASE}/home"
    mkdir -p "$HOME" "${CASE}/bin" "${CASE}/log"
    export HOME
    TRACE="${CASE}/trace"
    : >"$TRACE"
    export TRACE
    SKILL_BODY="${CASE}/skill-body"
    SKILL_RC="${CASE}/skill-rc"
    FAIL_ANSIBLE="${CASE}/fail-ansible"
    export SKILL_BODY SKILL_RC FAIL_ANSIBLE
    ANSIBLE_RUN_LOG="${CASE}/log/ansible.log"
    ANSIBLE_RUN_STATUS="${CASE}/log/ansible.status"
    export ANSIBLE_RUN_LOG ANSIBLE_RUN_STATUS
    # Keep the real herdr, if any, off PATH. The helper prepends ~/.local/bin
    # under this temp HOME.
    PATH="${CASE}/bin:/usr/bin:/bin"
    export PATH
    printf 'skill-bytes' >"$SKILL_BODY"

    cat >"${CASE}/bin/ansible-playbook" <<'EOF'
#!/bin/sh
printf 'ansible %s\n' "$*" >>"$TRACE"
if [ -n "${FAIL_ANSIBLE:-}" ] && [ -f "$FAIL_ANSIBLE" ]; then
    exit 1
fi
exit 0
EOF
    cat >"${CASE}/bin/herdr" <<'EOF'
#!/bin/sh
printf 'herdr %s\n' "$*" >>"$TRACE"
if [ "${1:-}" = "--version" ]; then
    printf '%s\n' "herdr 9.9.9"
    exit 0
fi
rc=0
if [ -n "${SKILL_RC:-}" ] && [ -f "$SKILL_RC" ]; then
    rc=$(cat "$SKILL_RC")
fi
if [ -n "${SKILL_BODY:-}" ] && [ -f "$SKILL_BODY" ]; then
    cat "$SKILL_BODY"
fi
exit "$rc"
EOF
    chmod +x "${CASE}/bin/ansible-playbook" "${CASE}/bin/herdr"
}

trace_count() {
    grep -c "^$1" "$TRACE" 2>/dev/null || true
}

assert_ansible_before_skill() {
    _abs_first=$(sed -n '1p' "$TRACE")
    case "$_abs_first" in
        ansible\ *) ;;
        *) fail "$1: first command was '${_abs_first}', want ansible" ;;
    esac
    if [ "$(trace_count 'herdr --skill')" -lt 1 ]; then
        fail "$1: herdr --skill did not run after herdr.yml"
    fi
}

assert_flags() {
    _af_line=$(sed -n '1p' "$TRACE")
    case "$_af_line" in
        *" -i inventory.ini -v --diff herdr.yml") ;;
        *) fail "$1: ansible args were '${_af_line}'" ;;
    esac
}

assert_no_approval() {
    _ana_idx="${HOME}/.cache/setup-dev/verified-scripts/texts/index.txt"
    if [ -f "$_ana_idx" ] && grep -q '	herdr-skill$' "$_ana_idx"; then
        fail "$1: approval was stored"
    fi
}

assert_approval_matches() {
    _aam_idx="${HOME}/.cache/setup-dev/verified-scripts/texts/index.txt"
    [ -f "$_aam_idx" ] || fail "$1: no text index"
    _aam_hash=$(awk -F '	' '$2 == "herdr-skill" { print $1; exit }' "$_aam_idx")
    [ -n "$_aam_hash" ] || fail "$1: no hash for herdr-skill"
    _aam_stored="${HOME}/.cache/setup-dev/verified-scripts/texts/${_aam_hash}"
    cmp -s "$_aam_stored" "$2" || fail "$1: stored body does not match"
}

# pty.fork makes the slave the child's controlling terminal, so review-text
# opens /dev/tty there. Answer only after the question is printed.
cat >"${ROOT}/pty_answer.py" <<'PY'
import os
import pty
import select
import sys

if len(sys.argv) < 3:
    sys.exit(2)
answer = sys.argv[1]
cmd = sys.argv[2:]
pid, fd = pty.fork()
if pid == 0:
    os.execvp(cmd[0], cmd)
    os._exit(127)

buf = b""
answered = False
idle = 0
status = 1
while True:
    wpid, status = os.waitpid(pid, os.WNOHANG)
    if wpid != 0:
        while True:
            ready, _, _ = select.select([fd], [], [], 0)
            if not ready:
                break
            try:
                chunk = os.read(fd, 4096)
            except OSError:
                chunk = b""
            if not chunk:
                break
            buf += chunk
        break
    ready, _, _ = select.select([fd], [], [], 1)
    if not ready:
        idle += 1
        if idle >= 20:
            os.kill(pid, 9)
            os.waitpid(pid, 0)
            sys.stderr.buffer.write(b"pty timeout\n")
            sys.stdout.buffer.write(buf)
            sys.exit(124)
        continue
    idle = 0
    try:
        chunk = os.read(fd, 4096)
    except OSError:
        continue
    if not chunk:
        continue
    buf += chunk
    if (not answered) and b"Approve this skill text? (y/N)" in buf:
        os.write(fd, (answer + "\n").encode())
        answered = True

sys.stdout.buffer.write(buf)
if os.WIFEXITED(status):
    sys.exit(os.WEXITSTATUS(status))
sys.exit(1)
PY

write_drive() {
    cat >"${CASE}/drive.sh" <<EOF
#!/bin/sh
set -eu
. "${SCRIPT_DIR}/playbook_resume.sh"
. "${HELPER}"
approve_herdr_skill "${SCRIPT_DIR}" " -v --diff" " setup-herdr"
EOF
    chmod +x "${CASE}/drive.sh"
}

run_approve() {
    approve_herdr_skill "$SCRIPT_DIR" "${1:- -v --diff}" " setup-herdr"
}

# --- check mode runs herdr.yml and does not review or store ---
reset_case
run_approve " --check" >/dev/null
_check_line=$(sed -n '1p' "$TRACE")
case "$_check_line" in
    *" -i inventory.ini --check herdr.yml") ;;
    *) fail "check mode ansible args were '${_check_line}'" ;;
esac
if [ "$(trace_count 'herdr ')" -ne 0 ]; then
    fail "check mode must not run herdr --skill"
fi
assert_no_approval "check mode"

reset_case
run_approve " -C" >/dev/null
_c_line=$(sed -n '1p' "$TRACE")
case "$_c_line" in
    *" -i inventory.ini -C herdr.yml") ;;
    *) fail "short check mode ansible args were '${_c_line}'" ;;
esac
if [ "$(trace_count 'herdr ')" -ne 0 ]; then
    fail "short check mode must not run herdr --skill"
fi
assert_no_approval "short check mode"

# --- herdr.yml failure stops before herdr --skill ---
reset_case
: >"$FAIL_ANSIBLE"
_fail_out="${CASE}/fail.out"
if run_approve >"$_fail_out" 2>&1; then
    fail "herdr.yml failure must stop the review"
fi
grep -q 'Error: playbook herdr.yml failed.' "$_fail_out" || fail "missing herdr.yml failure summary"
if [ "$(trace_count 'herdr ')" -ne 0 ]; then
    fail "herdr.yml failure must not run herdr"
fi
assert_flags "herdr.yml failure"
assert_no_approval "herdr.yml failure"

# --- herdr missing: warn, store nothing, succeed ---
reset_case
rm -f "${CASE}/bin/herdr"
_miss_out=$(run_approve)
case "$_miss_out" in
    *"herdr is not on PATH"*) ;;
    *) fail "missing herdr warning was: ${_miss_out}" ;;
esac
assert_no_approval "missing herdr"
if [ "$(trace_count 'herdr ')" -ne 0 ]; then
    fail "missing herdr must not be executed"
fi

# --- empty and whitespace-only stdout: warn, store nothing, succeed ---
reset_case
: >"$SKILL_BODY"
_empty_out=$(run_approve)
case "$_empty_out" in
    *"printed nothing"*) ;;
    *) fail "empty skill warning was: ${_empty_out}" ;;
esac
assert_no_approval "empty skill"
if [ "$(trace_count 'herdr --skill')" -ne 1 ]; then
    fail "empty skill must not be sent to review-text"
fi

reset_case
printf '\n \n' >"$SKILL_BODY"
_ws_out=$(run_approve)
case "$_ws_out" in
    *"printed nothing"*) ;;
    *) fail "whitespace skill warning was: ${_ws_out}" ;;
esac
assert_no_approval "whitespace skill"

# --- failing herdr --skill: warn, store nothing, succeed ---
reset_case
printf 'partial' >"$SKILL_BODY"
printf '3\n' >"$SKILL_RC"
_bad_out=$(run_approve)
case "$_bad_out" in
    *"exited 3"*) ;;
    *) fail "failing skill warning was: ${_bad_out}" ;;
esac
assert_no_approval "failing herdr --skill"
if [ "$(trace_count 'herdr --skill')" -ne 1 ]; then
    fail "failing herdr --skill must not be sent to review-text"
fi

# --- first approval through a pty stores the body; a later run does not ask ---
reset_case
write_drive
_yes_out="${CASE}/yes.out"
if ! "$PYTHON" "${ROOT}/pty_answer.py" y "${CASE}/drive.sh" >"$_yes_out"; then
    fail "approving the skill must succeed"
fi
grep -q 'needs approval' "$_yes_out" || fail "approval run did not say the text needs approval"
grep -q 'Skill text approved.' "$_yes_out" || fail "approval run did not confirm approval"
assert_ansible_before_skill "approval"
assert_flags "approval"
assert_approval_matches "approval" "$SKILL_BODY"
if [ "$(trace_count 'herdr --version')" -lt 1 ]; then
    fail "review-text did not ask herdr --version"
fi

: >"$TRACE"
_second_out=$(run_approve)
case "$_second_out" in
    *"needs approval"*) fail "matching skill text prompted again" ;;
esac
if [ "$(trace_count 'herdr --version')" -ne 0 ]; then
    fail "matching skill text called review-text"
fi
assert_approval_matches "second run" "$SKILL_BODY"

# --- changed text without a tty fails and keeps the stored body ---
cp "$SKILL_BODY" "${CASE}/original-body"
printf 'new-bytes' >"$SKILL_BODY"
_changed_err="${CASE}/changed.err"
if run_approve >"${CASE}/changed.out" 2>"$_changed_err"; then
    fail "changed skill text without a tty must fail"
fi
grep -q 'no terminal' "$_changed_err" || fail "changed skill text did not report a missing terminal"
assert_approval_matches "unchanged approval" "${CASE}/original-body"

# --- decline through a pty stores nothing and fails ---
reset_case
write_drive
_no_out="${CASE}/no.out"
if "$PYTHON" "${ROOT}/pty_answer.py" n "${CASE}/drive.sh" >"$_no_out"; then
    fail "declining the skill must fail"
fi
grep -q 'Skill text not approved.' "$_no_out" || fail "decline run did not report the refusal"
assert_no_approval "decline"
assert_ansible_before_skill "decline"

# --- ensure.sh runs this review after URL approval and before playbooks ---
_ensure="${SCRIPT_DIR}/ensure.sh"
_pre=$(grep -n 'pre_approve_urls "$@"' "$_ensure" | sed -n '1s/:.*//p')
_src=$(grep -n 'herdr_skill_approve.sh' "$_ensure" | sed -n '1s/:.*//p')
_guard=$(grep -n 'any_arg_imports setup-herdr.yml' "$_ensure" | sed -n '1s/:.*//p')
# shellcheck disable=SC2016  # Match the literal ensure.sh line, including its $.
_call=$(grep -n 'approve_herdr_skill "$(dirname "$0")" "$flags" "$playbooks" || exit 1' "$_ensure" | sed -n '1s/:.*//p')
# shellcheck disable=SC2016  # Match the literal ensure.sh line, including its $.
_loop=$(grep -n 'for playbook in $playbooks; do' "$_ensure" | sed -n '1s/:.*//p')
[ -n "$_pre" ] || fail "ensure.sh no longer calls pre_approve_urls"
[ -n "$_src" ] || fail "ensure.sh does not source herdr_skill_approve.sh"
[ -n "$_guard" ] || fail "ensure.sh does not limit review to setup-herdr.yml"
[ -n "$_call" ] || fail "ensure.sh does not call approve_herdr_skill"
[ -n "$_loop" ] || fail "ensure.sh playbook loop not found"
[ "$_src" -lt "$_call" ] || fail "helper is sourced after it is called"
[ "$_pre" -lt "$_guard" ] || fail "skill review runs before URL pre-approval"
[ "$_guard" -lt "$_call" ] || fail "setup-herdr guard is not the review call"
[ "$_call" -lt "$_loop" ] || fail "skill review runs inside the playbook loop"

printf '%s\n' "ok"
