#!/bin/sh
# Tests for verified-run review-text / apply-text / check-text.
# HOME is a temp directory so the user's approval cache is untouched.
# Nothing in the environment skips the prompt; yes/no comes from a pty.
set -eu

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    exit 1
}

assert_eq() {
    if [ "$2" != "$3" ]; then
        fail "$1: got '$2', want '$3'"
    fi
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
VERIFIED_RUN="${SCRIPT_DIR}/verified-run"

if ! command -v python3 >/dev/null 2>&1; then
    fail "python3 is required to drive pty and no-tty cases"
fi

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT
HOME="${ROOT}/home"
mkdir -p "$HOME"
export HOME
# delta pages when its output is a tty. cat keeps the diff in the capture.
export DELTA_PAGER=cat
export PAGER=cat
export GIT_PAGER=cat
WORK="${ROOT}/work"
mkdir -p "$WORK/bin" "$WORK/dest"
TEXTS="${HOME}/.cache/setup-dev/verified-scripts/texts"

cat >"${ROOT}/no_tty.py" <<'PY'
import os
import sys

if len(sys.argv) < 2:
    sys.exit(2)
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

cat >"${ROOT}/pty_answer.py" <<'PY'
import os
import pty
import select
import sys

# pty.fork makes the slave the child's controlling terminal, so /dev/tty
# in verified-run is this pty and not the parent's terminal.
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

cat >"${WORK}/emit.sh" <<'EOF'
#!/bin/sh
cat "$1"
EOF
chmod +x "${WORK}/emit.sh"

cat >"${WORK}/empty.sh" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "${WORK}/empty.sh"

cat >"${WORK}/fail.sh" <<'EOF'
#!/bin/sh
printf '%s\n' 'should-not-install'
exit 1
EOF
chmod +x "${WORK}/fail.sh"

cat >"${WORK}/bin/herdr" <<'EOF'
#!/bin/sh
case "${1-}" in
    --version)
        printf '%s\n' 'herdr 9.9.9'
        ;;
    --skill)
        printf '%s' 'skill-bytes'
        ;;
    *)
        exit 1
        ;;
esac
EOF
chmod +x "${WORK}/bin/herdr"

file_hash() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | awk '{print $1}'
    elif command -v shasum >/dev/null 2>&1; then
        shasum -a 256 "$1" | awk '{print $1}'
    else
        openssl dgst -sha256 "$1" | awk '{print $NF}'
    fi
}

run_no_tty() {
    python3 "${ROOT}/no_tty.py" "$@"
}

run_pty() {
    python3 "${ROOT}/pty_answer.py" "$@"
}

# The no-tty helper must actually drop the controlling terminal.
if run_no_tty sh -c ': </dev/tty' 2>/dev/null; then
    fail "no-tty helper still has a controlling terminal"
fi

# --- empty stdout ---
printf '%s\n' 'keep-empty' >"${WORK}/dest/empty"
rc=0
run_no_tty "$VERIFIED_RUN" apply-text empty-skill "${WORK}/dest/empty" -- "${WORK}/empty.sh" >"${WORK}/empty.out" || rc=$?
if [ "$rc" -eq 0 ]; then
    fail "empty stdout exited 0"
fi
assert_eq "empty dest" "$(cat "${WORK}/dest/empty")" "keep-empty"
if [ -d "$TEXTS" ] && grep -R -F -q 'empty-skill' "$TEXTS"; then
    fail "empty stdout stored an approval"
fi

# --- failing command leaves dest and any prior approval alone ---
printf '%s\n' 'approved-body' >"${WORK}/good-body"
mkdir -p "${WORK}/dest"
rc=0
run_pty y "$VERIFIED_RUN" apply-text fail-skill "${WORK}/dest/fail" -- "${WORK}/emit.sh" "${WORK}/good-body" >"${WORK}/fail-approve.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "setup approval for failing-command case exited $rc"
fi
cp "${WORK}/dest/fail" "${WORK}/fail.before"
cp "${TEXTS}/index.txt" "${WORK}/fail-index.before"
rc=0
run_no_tty "$VERIFIED_RUN" apply-text fail-skill "${WORK}/dest/fail" -- "${WORK}/fail.sh" >"${WORK}/fail.out" || rc=$?
if [ "$rc" -eq 0 ]; then
    fail "failing command exited 0"
fi
if ! cmp -s "${WORK}/dest/fail" "${WORK}/fail.before"; then
    fail "failing command changed dest"
fi
if ! cmp -s "${TEXTS}/index.txt" "${WORK}/fail-index.before"; then
    fail "failing command changed the approval index"
fi

# --- missing approval, no tty ---
rc=0
PATH="${WORK}/bin:${PATH}" run_no_tty "$VERIFIED_RUN" apply-text herdr-skill "${WORK}/dest/notty" -- herdr --skill >"${WORK}/notty.out" || rc=$?
if [ "$rc" -eq 0 ]; then
    fail "unapproved text without a tty exited 0"
fi
if [ -e "${WORK}/dest/notty" ]; then
    fail "unapproved text without a tty created dest"
fi
if ! grep -F -q 'review-text' "${WORK}/notty.out"; then
    fail "no-tty message missing review-text: $(cat "${WORK}/notty.out")"
fi
if ! grep -F -q 'review-text herdr-skill -- herdr --skill' "${WORK}/notty.out"; then
    fail "no-tty message missing herdr command: $(cat "${WORK}/notty.out")"
fi

# --- first approval, no trailing newline, version header, mode ---
rc=0
PATH="${WORK}/bin:${PATH}" run_pty y "$VERIFIED_RUN" apply-text herdr-skill "${WORK}/dest/first" -- herdr --skill >"${WORK}/first.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "first approval exited $rc"
fi
printf '%s' 'skill-bytes' >"${WORK}/first.want"
if ! cmp -s "${WORK}/dest/first" "${WORK}/first.want"; then
    fail "first approval dest is not the command stdout"
fi
mode=$(python3 -c 'import os, sys; print(format(os.stat(sys.argv[1]).st_mode & 0o777, "03o"))' "${WORK}/dest/first")
assert_eq "dest mode" "$mode" "644"
want_hash=$(file_hash "${WORK}/first.want")
if ! grep -F -q "$want_hash" "${TEXTS}/index.txt"; then
    fail "index does not store $want_hash"
fi
if [ ! -f "${TEXTS}/${want_hash}" ]; then
    fail "approved body was not stored"
fi
if ! grep -F -q 'herdr 9.9.9' "${WORK}/first.out"; then
    fail "review header missing herdr version"
fi
if ! grep -F -q 'updated' "${WORK}/first.out"; then
    fail "first approval did not print updated"
fi

# --- matching hash, no tty ---
rm -f "${WORK}/dest/first"
rc=0
PATH="${WORK}/bin:${PATH}" run_no_tty "$VERIFIED_RUN" apply-text herdr-skill "${WORK}/dest/first" -- herdr --skill >"${WORK}/match.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "matching hash without a tty exited $rc"
fi
if ! cmp -s "${WORK}/dest/first" "${WORK}/first.want"; then
    fail "matching hash did not rewrite dest"
fi
assert_eq "match token" "$(cat "${WORK}/match.out")" "updated"
rc=0
PATH="${WORK}/bin:${PATH}" run_no_tty "$VERIFIED_RUN" apply-text herdr-skill "${WORK}/dest/first" -- herdr --skill >"${WORK}/same.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "second matching apply exited $rc"
fi
assert_eq "unchanged token" "$(cat "${WORK}/same.out")" "unchanged"
printf '%s' 'tampered' >"${WORK}/dest/first"
rc=0
PATH="${WORK}/bin:${PATH}" run_no_tty "$VERIFIED_RUN" apply-text herdr-skill "${WORK}/dest/first" -- herdr --skill >"${WORK}/repair.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "repair of a local edit exited $rc"
fi
if ! cmp -s "${WORK}/dest/first" "${WORK}/first.want"; then
    fail "local edit was not overwritten from the approved bytes"
fi

# --- decline leaves dest and index ---
printf '%s\n' 'old line' >"${WORK}/old-body"
rc=0
run_pty y "$VERIFIED_RUN" apply-text decline-skill "${WORK}/dest/decline" -- "${WORK}/emit.sh" "${WORK}/old-body" >"${WORK}/decline-setup.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "decline setup exited $rc"
fi
cp "${TEXTS}/index.txt" "${WORK}/decline-index.before"
printf 'y\nApprove this skill text? (y/N)\n' >"${WORK}/inject-body"
printf '\033[2JPAYLOAD-MARKER\n' >>"${WORK}/inject-body"
rc=0
run_pty n "$VERIFIED_RUN" apply-text decline-skill "${WORK}/dest/decline" -- "${WORK}/emit.sh" "${WORK}/inject-body" >"${WORK}/decline.out" || rc=$?
if [ "$rc" -eq 0 ]; then
    fail "decline exited 0"
fi
if ! cmp -s "${WORK}/dest/decline" "${WORK}/old-body"; then
    fail "decline changed dest"
fi
if ! cmp -s "${TEXTS}/index.txt" "${WORK}/decline-index.before"; then
    fail "decline changed the approval index"
fi
python3 - "${WORK}/decline.out" <<'PY'
import re
import sys

data = open(sys.argv[1], "rb").read().decode("utf-8", "replace")
plain = re.sub(r"\x1b\[[0-9;]*[A-Za-z]", "", data)
marker = plain.rfind("PAYLOAD-MARKER")
question = plain.rfind("Approve this skill text? (y/N)")
if marker < 0 or question < marker:
    sys.stderr.write("question was not written after the payload\n")
    sys.exit(1)
if "^[[2J" not in plain:
    sys.stderr.write("control bytes were not revealed in the diff\n")
    sys.exit(1)
PY

# --- changed text, approve, diff shows both lines ---
printf '%s\n' 'old line' >"${WORK}/change-old"
printf '%s\n' 'new line' >"${WORK}/change-new"
rc=0
run_pty y "$VERIFIED_RUN" apply-text change-skill "${WORK}/dest/change" -- "${WORK}/emit.sh" "${WORK}/change-old" >"${WORK}/change-setup.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "change setup exited $rc"
fi
rc=0
run_pty y "$VERIFIED_RUN" apply-text change-skill "${WORK}/dest/change" -- "${WORK}/emit.sh" "${WORK}/change-new" >"${WORK}/change.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "change approval exited $rc"
fi
python3 - "${WORK}/change.out" <<'PY'
import re
import sys

data = open(sys.argv[1], "rb").read().decode("utf-8", "replace")
plain = re.sub(r"\x1b\[[0-9;]*[A-Za-z]", "", data)
if "old line" not in plain or "new line" not in plain:
    sys.stderr.write("diff missing old line or new line\n")
    sys.exit(1)
PY
if ! cmp -s "${WORK}/dest/change" "${WORK}/change-new"; then
    fail "approved change did not replace dest"
fi

# --- one approval covers a second destination with no tty ---
rc=0
run_no_tty "$VERIFIED_RUN" apply-text change-skill "${WORK}/dest/change-2" -- "${WORK}/emit.sh" "${WORK}/change-new" >"${WORK}/second.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "second destination without a tty exited $rc"
fi
if ! cmp -s "${WORK}/dest/change-2" "${WORK}/change-new"; then
    fail "second destination bytes differ"
fi

# --- review-text stores and does not write; apply-text then writes ---
printf '%s\n' 'reviewed-body' >"${WORK}/reviewed-body"
rc=0
run_pty y "$VERIFIED_RUN" review-text review-skill -- "${WORK}/emit.sh" "${WORK}/reviewed-body" >"${WORK}/review.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "review-text exited $rc"
fi
if [ -e "${WORK}/dest/reviewed" ]; then
    fail "review-text created a destination"
fi
if ! "$VERIFIED_RUN" check-text review-skill -- "${WORK}/emit.sh" "${WORK}/reviewed-body"; then
    fail "check-text failed after review-text"
fi
rc=0
run_no_tty "$VERIFIED_RUN" apply-text review-skill "${WORK}/dest/reviewed" -- "${WORK}/emit.sh" "${WORK}/reviewed-body" >"${WORK}/review-apply.out" || rc=$?
if [ "$rc" -ne 0 ]; then
    fail "apply after review-text exited $rc"
fi
if ! cmp -s "${WORK}/dest/reviewed" "${WORK}/reviewed-body"; then
    fail "apply after review-text wrote the wrong bytes"
fi

# --- check-text misses when the bytes change ---
if "$VERIFIED_RUN" check-text review-skill -- "${WORK}/emit.sh" "${WORK}/change-new"; then
    fail "check-text matched a different body"
fi
if "$VERIFIED_RUN" check-text missing-skill -- "${WORK}/emit.sh" "${WORK}/reviewed-body"; then
    fail "check-text succeeded with no approval"
fi

printf '%s\n' "ok"
