#!/bin/sh
# Tests for the pre-commit/gitleaks.sh Githooks hook, using a fake gitleaks
# on PATH so the test does not depend on a real install or a Git repo.
set -eu

fail() {
	echo "FAIL: $1" >&2
	exit 1
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/pre-commit/gitleaks.sh"
sh -n "$HOOK"

_tmp=$(mktemp -d)
trap 'rm -rf "$_tmp"' EXIT

# Missing gitleaks: print the install hint and do not block the commit.
mkdir "$_tmp/empty"
if ! PATH="$_tmp/empty" /bin/sh "$HOOK" >"$_tmp/out" 2>&1; then
	fail "hook must exit 0 when gitleaks is not installed"
fi
grep -q "./ensure.sh gitleaks" "$_tmp/out" ||
	fail "missing-gitleaks output must include the install hint"

# Fake gitleaks records its arguments and exits with FAKE_GITLEAKS_EXIT.
mkdir "$_tmp/bin"
cat >"$_tmp/bin/gitleaks" <<'EOF'
#!/bin/sh
printf '%s\n' "$@" >"$FAKE_GITLEAKS_ARGS"
exit "${FAKE_GITLEAKS_EXIT:-0}"
EOF
chmod +x "$_tmp/bin/gitleaks"

export FAKE_GITLEAKS_ARGS="$_tmp/args"

# Clean staged changes: the commit goes through.
if ! FAKE_GITLEAKS_EXIT=0 PATH="$_tmp/bin:$PATH" /bin/sh "$HOOK"; then
	fail "hook must exit 0 when gitleaks finds nothing"
fi
for _arg in git --pre-commit --staged --redact --no-banner --verbose; do
	grep -qx -- "$_arg" "$_tmp/args" ||
		fail "gitleaks must be called with $_arg; got: $(tr '\n' ' ' <"$_tmp/args")"
done

# A finding blocks the commit.
if FAKE_GITLEAKS_EXIT=1 PATH="$_tmp/bin:$PATH" /bin/sh "$HOOK"; then
	fail "hook must exit non-zero when gitleaks reports a leak"
fi

echo "PASS"
