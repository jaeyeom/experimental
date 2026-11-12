#!/bin/sh
# setup-slackwatch.yml writes `slackwatch skill` to each skill path with
# verified-run apply-text. The playbook runs only the skill tasks
# (--tags slackwatch_skill) with a temp HOME and a PATH holding a fake go
# and, when wanted, a fake slackwatch under the fake GOPATH, so the real
# ~/.claude and ~/.cache/setup-dev are never read or written. Every run is
# detached from the terminal, like the Ansible command module, so
# apply-text cannot prompt.
set -eu

fail() {
	printf 'FAIL: %s\n' "$1" >&2
	exit 1
}

if ! command -v ansible-playbook >/dev/null 2>&1; then
	echo "SKIP: ansible-playbook not found"
	exit 0
fi
ANSIBLE_BIN_DIR=$(dirname "$(command -v ansible-playbook)")
PYTHON=$(command -v python3 || true)
if [ -z "$PYTHON" ]; then
	fail "python3 is required to drive the pty"
fi

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
PLAYBOOK="${SCRIPT_DIR}/setup-slackwatch.yml"
INVENTORY="${SCRIPT_DIR}/inventory.ini"

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT
export TMPDIR="$ROOT"
# delta pages on a tty. cat keeps the review in the capture.
export DELTA_PAGER=cat
export PAGER=cat
export GIT_PAGER=cat

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
echo "fake go: unexpected call: $*" >&2
exit 1
EOF
chmod +x "${ROOT}/bin/go"

SKILL_BODY="${ROOT}/skill-body"
export SKILL_BODY
printf '%s\n' '---' 'name: slackwatch' 'description: fake skill' '---' '' 'Body line.' >"$SKILL_BODY"

# write_fake_slackwatch EXIT_CODE installs a slackwatch whose `skill`
# prints $SKILL_BODY, or fails with EXIT_CODE when it is not 0.
write_fake_slackwatch() {
	cat >"${GOPATH_DIR}/bin/slackwatch" <<EOF
#!/bin/sh
if [ "\$*" = "skill" ]; then
    if [ "$1" -ne 0 ]; then
        echo "fake slackwatch: failing" >&2
        exit $1
    fi
    cat "\$SKILL_BODY"
    exit 0
fi
echo "fake slackwatch: unexpected call: \$*" >&2
exit 1
EOF
	chmod +x "${GOPATH_DIR}/bin/slackwatch"
}

PATH="${ROOT}/bin:${ANSIBLE_BIN_DIR}:/usr/bin:/bin"
export PATH
if [ -e "${ANSIBLE_BIN_DIR}/slackwatch" ]; then
	echo "SKIP: slackwatch is next to ansible-playbook; cannot hide it"
	exit 0
fi

export ANSIBLE_NOCOLOR=1
export ANSIBLE_STDOUT_CALLBACK=default
export ANSIBLE_DISPLAY_SKIPPED_HOSTS=1
export ANSIBLE_GATHERING=smart
export ANSIBLE_RETRY_FILES_ENABLED=0

OUT="${ROOT}/out"
SKILL_FILE="${HOME}/.claude/skills/slackwatch/SKILL.md"
TEXTS_INDEX="${HOME}/.cache/setup-dev/verified-scripts/texts/index.txt"

# no_tty.py runs a command in a new session, so /dev/tty cannot be opened.
cat >"${ROOT}/no_tty.py" <<'PY'
import os
import sys

pid = os.fork()
if pid == 0:
    os.setsid()
    os.execvp(sys.argv[1], sys.argv[1:])
    os._exit(127)
_, status = os.waitpid(pid, 0)
if os.WIFEXITED(status):
    sys.exit(os.WEXITSTATUS(status))
sys.exit(1)
PY

# pty_answer.py runs a command on a new pty and answers the review-text
# question with argv[1].
cat >"${ROOT}/pty_answer.py" <<'PY'
import os
import pty
import select
import sys

answer = sys.argv[1]
pid, fd = pty.fork()
if pid == 0:
    os.execvp(sys.argv[2], sys.argv[2:])
    os._exit(127)
buf = b""
answered = False
idle = 0
status = 1
while True:
    wpid, status = os.waitpid(pid, os.WNOHANG)
    if wpid != 0:
        break
    ready, _, _ = select.select([fd], [], [], 1)
    if not ready:
        idle += 1
        if idle >= 20:
            os.kill(pid, 9)
            os.waitpid(pid, 0)
            sys.stderr.buffer.write(b"pty timeout\n" + buf)
            sys.exit(124)
        continue
    idle = 0
    try:
        chunk = os.read(fd, 4096)
    except OSError:
        continue
    buf += chunk
    if not answered and b"Approve this skill text? (y/N)" in buf:
        os.write(fd, (answer + "\n").encode())
        answered = True
sys.stdout.buffer.write(buf)
if os.WIFEXITED(status):
    sys.exit(os.WEXITSTATUS(status))
sys.exit(1)
PY

# try_playbook runs the skill tasks without a terminal and stores the exit
# status in PLAYBOOK_RC; extra args go to ansible-playbook.
try_playbook() {
	PLAYBOOK_RC=0
	"$PYTHON" "${ROOT}/no_tty.py" ansible-playbook -i "$INVENTORY" \
		--tags slackwatch_skill "$@" "$PLAYBOOK" >"$OUT" 2>&1 || PLAYBOOK_RC=$?
	if grep -q 'libc.so' "$OUT"; then
		echo "SKIP: ansible-playbook cannot run in sandbox (libc issue)"
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

# run_playbook_fails runs a plain apply and expects it to fail.
run_playbook_fails() {
	try_playbook
	if [ "$PLAYBOOK_RC" -eq 0 ]; then
		cat "$OUT" >&2
		fail "ansible-playbook succeeded, want failure"
	fi
}

# task_result prints the first result word (ok, changed, skipping, fatal)
# after the TASK line with the given name.
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
	if [ -e "$SKILL_FILE" ]; then
		fail "$1: skill file was written"
	fi
}

assert_no_approval() {
	if [ -f "$TEXTS_INDEX" ] && grep -q '	slackwatch-skill$' "$TEXTS_INDEX"; then
		fail "$1: approval was stored"
	fi
}

assert_written() {
	if ! cmp -s "$2" "$SKILL_FILE"; then
		diff "$2" "$SKILL_FILE" >&2 || true
		fail "$1: skill file differs from the approved text"
	fi
}

expect_warning() {
	if ! grep -q "WARNING: Skipping slackwatch skill install because $2" "$OUT"; then
		cat "$OUT" >&2
		fail "$1: missing warning '$2'"
	fi
	assert_not_written "$1"
	assert_no_approval "$1"
}

# --- slackwatch missing: warn, write nothing, do not fail ---
run_playbook
expect_warning "missing" "slackwatch is not on PATH"
expect_install "missing" skipping

# --- slackwatch skill fails: warn, write nothing, do not fail ---
write_fake_slackwatch 3
run_playbook
# shellcheck disable=SC2016  # Backticks are literal warning text.
expect_warning "failing" '`slackwatch skill` exited 3'
expect_install "failing" skipping

write_fake_slackwatch 0

# --- check mode: the read-only lookups run, but the command module skips
# apply-text, so nothing is approved or written and the task reports
# skipping, not "would change" ---
run_playbook --check
expect_install "check" skipping
assert_not_written "check"
assert_no_approval "check"

# --- ensure.sh defers: warn, skip apply-text, write nothing ---
run_playbook -e setup_dev_defer_skill_apply=true
if ! grep -q 'WARNING: Skipping slackwatch skill apply-text because ensure.sh will' "$OUT"; then
	cat "$OUT" >&2
	fail "defer: missing apply-text deferral warning"
fi
expect_install "defer" skipping
assert_not_written "defer"
assert_no_approval "defer"

# --- direct run, unapproved text, no terminal: fails and names review-text ---
run_playbook_fails
expect_install "unapproved" failed
grep -q 'review-text slackwatch-skill -- slackwatch skill' "$OUT" ||
	fail "unapproved: output does not name the review-text command"
assert_not_written "unapproved"
assert_no_approval "unapproved"

# --- approve with review-text, as ensure.sh does; the playbook then writes it ---
if ! PATH="${GOPATH_DIR}/bin:${PATH}" "$PYTHON" "${ROOT}/pty_answer.py" y \
	"${SCRIPT_DIR}/verified-run" review-text slackwatch-skill -- slackwatch skill >"${ROOT}/review.out"; then
	cat "${ROOT}/review.out" >&2
	fail "review-text approval failed"
fi
run_playbook
expect_install "approved apply" changed
assert_written "approved apply" "$SKILL_BODY"

# --- rerun with the approved text: unchanged ---
run_playbook
expect_install "approved rerun" ok
assert_written "approved rerun" "$SKILL_BODY"

# --- changed text, not approved: fails and keeps the approved file ---
cp "$SKILL_BODY" "${ROOT}/approved-body"
printf '%s\n' 'Changed line.' >>"$SKILL_BODY"
run_playbook_fails
expect_install "changed" failed
grep -q 'review-text slackwatch-skill' "$OUT" ||
	fail "changed: output does not name the review-text command"
assert_written "changed" "${ROOT}/approved-body"

echo "PASS"
