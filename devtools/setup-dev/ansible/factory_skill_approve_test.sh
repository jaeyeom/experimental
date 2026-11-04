#!/bin/sh
# ensure.sh reviews `factory skill` in this shell, after factory.yml, because
# the Ansible command module has no controlling terminal.
# HOME is a temp directory. Nothing in the environment skips the prompt.
set -eu

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    exit 1
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HELPER="${SCRIPT_DIR}/factory_skill_approve.sh"
PYTHON=$(command -v python3 || true)
if [ -z "$PYTHON" ]; then
    fail "python3 is required to drive the pty"
fi

if [ ! -f "$HELPER" ]; then
    fail "factory_skill_approve.sh is missing"
fi

sh -n "$HELPER"
sh -n "$SCRIPT_DIR/factory_skill_approve_test.sh"
# shellcheck disable=SC1091  # Sourced from the same directory as this script
. "$SCRIPT_DIR/playbook_resume.sh"
# shellcheck disable=SC1091  # Sourced from the same directory as this script
. "$SCRIPT_DIR/factory_skill_approve.sh"

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
    export HOME
    # The helper prepends "$(go env GOPATH)/bin". Keep the fake factory
    # there, under this temp HOME, and off the default PATH.
    GOPATH="${HOME}/gopath"
    export GOPATH
    mkdir -p "$HOME" "${CASE}/bin" "${GOPATH}/bin" "${CASE}/log"
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
    PATH="${CASE}/bin:/usr/bin:/bin"
    export PATH
    printf 'skill-bytes' >"$SKILL_BODY"

    cat >"${CASE}/bin/go" <<'EOF'
#!/bin/sh
if [ "$1" = "env" ] && [ "$2" = "GOPATH" ]; then
    printf '%s\n' "$GOPATH"
    exit 0
fi
printf 'go %s\n' "$*" >>"$TRACE"
exit 1
EOF
    cat >"${CASE}/bin/ansible-playbook" <<'EOF'
#!/bin/sh
printf 'ansible %s\n' "$*" >>"$TRACE"
if [ -n "${FAIL_ANSIBLE:-}" ] && [ -f "$FAIL_ANSIBLE" ]; then
    exit 1
fi
exit 0
EOF
    cat >"${GOPATH}/bin/factory" <<'EOF'
#!/bin/sh
printf 'factory %s\n' "$*" >>"$TRACE"
rc=0
if [ -n "${SKILL_RC:-}" ] && [ -f "$SKILL_RC" ]; then
    rc=$(cat "$SKILL_RC")
fi
if [ -n "${SKILL_BODY:-}" ] && [ -f "$SKILL_BODY" ]; then
    cat "$SKILL_BODY"
fi
exit "$rc"
EOF
    chmod +x "${CASE}/bin/go" "${CASE}/bin/ansible-playbook" "${GOPATH}/bin/factory"
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
    if [ "$(trace_count 'factory skill')" -lt 1 ]; then
        fail "$1: factory skill did not run after factory.yml"
    fi
}

assert_flags() {
    _af_line=$(sed -n '1p' "$TRACE")
    case "$_af_line" in
        *" -i inventory.ini -v --diff factory.yml") ;;
        *) fail "$1: ansible args were '${_af_line}'" ;;
    esac
}

assert_no_approval() {
    _ana_idx="${HOME}/.cache/setup-dev/verified-scripts/texts/index.txt"
    if [ -f "$_ana_idx" ] && grep -q '	factory-skill$' "$_ana_idx"; then
        fail "$1: approval was stored"
    fi
}

assert_approval_matches() {
    _aam_idx="${HOME}/.cache/setup-dev/verified-scripts/texts/index.txt"
    [ -f "$_aam_idx" ] || fail "$1: no text index"
    _aam_hash=$(awk -F '	' '$2 == "factory-skill" { print $1; exit }' "$_aam_idx")
    [ -n "$_aam_hash" ] || fail "$1: no hash for factory-skill"
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
approve_factory_skill "${SCRIPT_DIR}" " -v --diff" " setup-factory"
EOF
    chmod +x "${CASE}/drive.sh"
}

run_approve() {
    approve_factory_skill "$SCRIPT_DIR" "${1:- -v --diff}" " setup-factory"
}

# --- check mode runs factory.yml and does not review or store ---
reset_case
run_approve " --check" >/dev/null
_check_line=$(sed -n '1p' "$TRACE")
case "$_check_line" in
    *" -i inventory.ini --check factory.yml") ;;
    *) fail "check mode ansible args were '${_check_line}'" ;;
esac
if [ "$(trace_count 'factory ')" -ne 0 ]; then
    fail "check mode must not run factory skill"
fi
assert_no_approval "check mode"

reset_case
run_approve " -C" >/dev/null
_c_line=$(sed -n '1p' "$TRACE")
case "$_c_line" in
    *" -i inventory.ini -C factory.yml") ;;
    *) fail "short check mode ansible args were '${_c_line}'" ;;
esac
if [ "$(trace_count 'factory ')" -ne 0 ]; then
    fail "short check mode must not run factory skill"
fi
assert_no_approval "short check mode"

# --- factory.yml failure stops before factory skill ---
reset_case
: >"$FAIL_ANSIBLE"
_fail_out="${CASE}/fail.out"
if run_approve >"$_fail_out" 2>&1; then
    fail "factory.yml failure must stop the review"
fi
grep -q 'Error: playbook factory.yml failed.' "$_fail_out" || fail "missing factory.yml failure summary"
if [ "$(trace_count 'factory ')" -ne 0 ]; then
    fail "factory.yml failure must not run factory"
fi
assert_flags "factory.yml failure"
assert_no_approval "factory.yml failure"

# --- factory missing: warn, store nothing, succeed ---
reset_case
rm -f "${GOPATH}/bin/factory"
_miss_out=$(run_approve)
case "$_miss_out" in
    *"factory is not on PATH"*) ;;
    *) fail "missing factory warning was: ${_miss_out}" ;;
esac
assert_no_approval "missing factory"
if [ "$(trace_count 'factory ')" -ne 0 ]; then
    fail "missing factory must not be executed"
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
if [ "$(trace_count 'factory skill')" -ne 1 ]; then
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

# --- failing factory skill: warn, store nothing, succeed ---
reset_case
printf 'partial' >"$SKILL_BODY"
printf '3\n' >"$SKILL_RC"
_bad_out=$(run_approve)
case "$_bad_out" in
    *"exited 3"*) ;;
    *) fail "failing skill warning was: ${_bad_out}" ;;
esac
assert_no_approval "failing factory skill"
if [ "$(trace_count 'factory skill')" -ne 1 ]; then
    fail "failing factory skill must not be sent to review-text"
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
# review-text runs factory skill again after the helper capture and check-text.
# verified-run prints a version only for herdr, so the extra invocation is the signal.
if [ "$(trace_count 'factory skill')" -lt 3 ]; then
    fail "review-text did not run factory skill"
fi

: >"$TRACE"
_second_out=$(run_approve)
case "$_second_out" in
    *"needs approval"*) fail "matching skill text prompted again" ;;
esac
if [ "$(trace_count 'factory skill')" -ne 2 ]; then
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
_src=$(grep -n 'factory_skill_approve.sh' "$_ensure" | sed -n '1s/:.*//p')
_guard=$(grep -n 'any_arg_imports setup-factory.yml' "$_ensure" | sed -n '1s/:.*//p')
# shellcheck disable=SC2016  # Match the literal ensure.sh line, including its $.
_call=$(grep -n 'approve_factory_skill "$(dirname "$0")" "$flags" "$playbooks" || exit 1' "$_ensure" | sed -n '1s/:.*//p')
# shellcheck disable=SC2016  # Match the literal ensure.sh line, including its $.
_loop=$(grep -n 'for playbook in $playbooks; do' "$_ensure" | sed -n '1s/:.*//p')
# shellcheck disable=SC2016  # Match the literal ensure.sh line, including its $.
_herdr_call=$(grep -n 'approve_herdr_skill "$(dirname "$0")" "$flags" "$playbooks" || exit 1' "$_ensure" | sed -n '1s/:.*//p')
[ -n "$_pre" ] || fail "ensure.sh no longer calls pre_approve_urls"
[ -n "$_src" ] || fail "ensure.sh does not source factory_skill_approve.sh"
[ -n "$_guard" ] || fail "ensure.sh does not limit review to setup-factory.yml"
[ -n "$_call" ] || fail "ensure.sh does not call approve_factory_skill"
[ -n "$_loop" ] || fail "ensure.sh playbook loop not found"
[ -n "$_herdr_call" ] || fail "ensure.sh no longer calls approve_herdr_skill"
[ "$_src" -lt "$_call" ] || fail "helper is sourced after it is called"
[ "$_pre" -lt "$_guard" ] || fail "skill review runs before URL pre-approval"
[ "$_guard" -lt "$_call" ] || fail "setup-factory guard is not the review call"
[ "$_herdr_call" -lt "$_call" ] || fail "factory review does not run after the herdr review"
[ "$_call" -lt "$_loop" ] || fail "skill review runs inside the playbook loop"

printf '%s\n' "ok"
