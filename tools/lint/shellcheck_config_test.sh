#!/usr/bin/env bash
# Verifies that the hermetic ShellCheck binary respects //:.shellcheckrc
# (shell=bash) and -x (follow sourced files).
set -euo pipefail

shellcheck_bin="$(realpath "$1")"
rcfile="$(realpath "$2")"

workdir="$(mktemp -d "${TEST_TMPDIR:-/tmp}/shellcheck_config_test.XXXXXX")"
trap 'rm -rf "$workdir"' EXIT

# 1. Verify //:.shellcheckrc (shell=bash) is honored for shebang-less scripts
#    and that real violations are still caught.
cat >"$workdir/no_shebang_ok.sh" <<'EOF'
arr=(one two)
printf '%s\n' "${arr[0]}"
EOF

if "$shellcheck_bin" --norc "$workdir/no_shebang_ok.sh" >/dev/null 2>&1; then
  echo "FAIL: expected shellcheck --norc to reject shebang-less script (SC2148)" >&2
  exit 1
fi

"$shellcheck_bin" --rcfile "$rcfile" "$workdir/no_shebang_ok.sh"

cat >"$workdir/no_shebang_bad.sh" <<'EOF'
msg="hello world"
echo $msg
EOF

output=""
if output=$("$shellcheck_bin" --rcfile "$rcfile" "$workdir/no_shebang_bad.sh" 2>&1); then
  echo "FAIL: expected shellcheck to report SC2086 on no_shebang_bad.sh" >&2
  exit 1
fi

if ! printf '%s\n' "$output" | grep -q "SC2086"; then
  echo "FAIL: expected SC2086 in shellcheck output:" >&2
  printf '%s\n' "$output" >&2
  exit 1
fi

# 2. Verify -x follows sourced files so variables from sourced scripts resolve
#    without SC1091, while omitting -x fails with SC1091.
cat >"$workdir/lib.sh" <<'EOF'
#!/usr/bin/env bash
export HELPER_VAR="ok"
EOF

cat >"$workdir/main.sh" <<'EOF'
#!/usr/bin/env bash
# shellcheck source=lib.sh
. ./lib.sh
printf '%s\n' "$HELPER_VAR"
EOF

output=""
if output=$(cd "$workdir" && "$shellcheck_bin" --rcfile "$rcfile" main.sh 2>&1); then
  echo "FAIL: expected shellcheck without -x to report SC1091 on main.sh" >&2
  exit 1
fi

if ! printf '%s\n' "$output" | grep -q "SC1091"; then
  echo "FAIL: expected SC1091 without -x in shellcheck output:" >&2
  printf '%s\n' "$output" >&2
  exit 1
fi

(cd "$workdir" && "$shellcheck_bin" --rcfile "$rcfile" -x main.sh)
