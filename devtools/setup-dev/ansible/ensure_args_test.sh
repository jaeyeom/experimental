#!/bin/sh
# Tests for ensure.sh argument parsing (ensure_args.sh).
set -eu

fail() {
    echo "FAIL: $1" >&2
    exit 1
}

assert_eq() {
    _label="$1"
    _got="$2"
    _want="$3"
    if [ "$_got" != "$_want" ]; then
        fail "$_label: got '$_got', want '$_want'"
    fi
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
sh -n "$SCRIPT_DIR/ensure_args.sh"
sh -n "$SCRIPT_DIR/ensure_args_test.sh"
# shellcheck source=devtools/setup-dev/ansible/ensure_args.sh
. "$SCRIPT_DIR/ensure_args.sh"

# Stub for ansible-playbook: print each argument in brackets on one line.
record_args() {
    _ra_out=""
    for _ra in "$@"; do
        _ra_out="${_ra_out}[${_ra}]"
    done
    printf '%s\n' "$_ra_out"
}

# ansible_argv prints the arguments ensure.sh passes to ansible-playbook.
ansible_argv() {
    eval "record_args $ansible_args"
}

# trim drops the leading space ensure.sh string lists start with.
trim() {
    printf '%s\n' "$1" | sed 's/^ *//'
}

# --- No arguments ---
ensure_parse_args
assert_eq "empty playbooks" "$(trim "$playbooks")" ""
assert_eq "empty flags" "$(trim "$flags")" ""
assert_eq "empty argv" "$(ansible_argv)" ""
assert_eq "empty from" "$from_playbook" ""

# --- Playbooks and plain flags ---
ensure_parse_args --check -v all emacs
assert_eq "plain playbooks" "$(trim "$playbooks")" "all emacs"
assert_eq "plain flags" "$(trim "$flags")" "--check -v"
assert_eq "plain argv" "$(ansible_argv)" "[--check][-v]"

# --- -e VALUE is kept out of playbooks ---
ensure_parse_args -e foo=bar all
assert_eq "-e playbooks" "$(trim "$playbooks")" "all"
assert_eq "-e argv" "$(ansible_argv)" "[-e][foo=bar]"
assert_eq "-e not in flags" "$(trim "$flags")" ""

# --- A value with spaces stays one argument ---
ensure_parse_args -e 'msg="hello world"' all
assert_eq "spaces playbooks" "$(trim "$playbooks")" "all"
assert_eq "spaces argv" "$(ansible_argv)" '[-e][msg="hello world"]'

# --- JSON, single quotes, and @file values ---
ensure_parse_args -e '{"a": 1, "b": "it'\''s"}' --extra-vars @vars.yml all
assert_eq "json argv" "$(ansible_argv)" '[-e][{"a": 1, "b": "it'\''s"}][--extra-vars][@vars.yml]'
assert_eq "json playbooks" "$(trim "$playbooks")" "all"

# --- Glob characters are not expanded ---
ensure_parse_args -e 'x=*' all
assert_eq "glob argv" "$(ansible_argv)" '[-e][x=*]'

# --- Repeated -e and --extra-vars, combined with --from ---
ensure_parse_args -e a=1 --from setup-git --extra-vars b=2 -e 'c=3 4' -- all
assert_eq "repeat argv" "$(ansible_argv)" "[-e][a=1][--extra-vars][b=2][-e][c=3 4]"
assert_eq "repeat playbooks" "$(trim "$playbooks")" "all"
assert_eq "repeat from" "$from_playbook" "setup-git"

ensure_parse_args --from setup-git -e a=1 all
assert_eq "from first argv" "$(ansible_argv)" "[-e][a=1]"
assert_eq "from first playbooks" "$(trim "$playbooks")" "all"
assert_eq "from first from" "$from_playbook" "setup-git"

# --- Other value-taking flags ---
ensure_parse_args --tags setup profile-cloudflare-dev
assert_eq "--tags argv" "$(ansible_argv)" "[--tags][setup]"
assert_eq "--tags playbooks" "$(trim "$playbooks")" "profile-cloudflare-dev"

ensure_parse_args -t a,b --skip-tags c -l localhost --limit host2 -i extra.ini all
assert_eq "value flags argv" "$(ansible_argv)" "[-t][a,b][--skip-tags][c][-l][localhost][--limit][host2][-i][extra.ini]"
assert_eq "value flags playbooks" "$(trim "$playbooks")" "all"

# --- Joined forms pass through as one argument ---
ensure_parse_args '--extra-vars=msg=hello world' -efoo=bar --tags=setup all
assert_eq "joined argv" "$(ansible_argv)" "[--extra-vars=msg=hello world][-efoo=bar][--tags=setup]"
assert_eq "joined playbooks" "$(trim "$playbooks")" "all"
# A joined value with spaces must not leak words into the flag list that
# --check / -K detection splits on.
ensure_parse_args '--extra-vars=x --check' all
assert_eq "joined spaces flags" "$(trim "$flags")" ""

# --- -- still separates playbooks ---
ensure_parse_args --check -- -weird all
assert_eq "separator playbooks" "$(trim "$playbooks")" "-weird all"
assert_eq "separator argv" "$(ansible_argv)" "[--check]"

# --- Missing values are errors ---
if ensure_parse_args all -e 2>/dev/null; then
    fail "-e without a value must fail"
fi
if ensure_parse_args all --from 2>/dev/null; then
    fail "--from without a value must fail"
fi
_err=$(ensure_parse_args --extra-vars 2>&1 || true)
case "$_err" in
    *--extra-vars*) ;;
    *) fail "missing value error does not name the flag: $_err" ;;
esac

# --- ensure_args_append adds internal arguments ---
ensure_parse_args -e 'a=b c' all
ensure_args_append -e setup_dev_defer_skill_apply=true
assert_eq "append argv" "$(ansible_argv)" "[-e][a=b c][-e][setup_dev_defer_skill_apply=true]"

# --- ensure.sh resets "$@" to playbook names before using it ---
_ensure="$SCRIPT_DIR/ensure.sh"
# shellcheck disable=SC2016  # Match the literal ensure.sh lines, including $.
_parse=$(grep -n 'ensure_parse_args "$@"' "$_ensure" | sed -n '1s/:.*//p')
# shellcheck disable=SC2016  # Match the literal ensure.sh lines, including $.
_reset=$(grep -n '^set -- $playbooks$' "$_ensure" | sed -n '1s/:.*//p')
# shellcheck disable=SC2016  # Match the literal ensure.sh lines, including $.
_first_use=$(grep -n 'any_arg_imports [a-z-]*\.yml "$@"' "$_ensure" | sed -n '1s/:.*//p')
[ -n "$_parse" ] || fail "ensure.sh does not call ensure_parse_args"
[ -n "$_reset" ] || fail "ensure.sh does not reset \"\$@\" to playbooks"
[ -n "$_first_use" ] || fail "ensure.sh any_arg_imports \"\$@\" not found"
[ "$_parse" -lt "$_reset" ] || fail "\"\$@\" is reset before parsing"
[ "$_reset" -lt "$_first_use" ] || fail "any_arg_imports sees raw arguments"
# shellcheck disable=SC2016  # Match the literal ensure.sh lines, including $.
grep -q 'ansible-playbook -i inventory.ini $ansible_args' "$_ensure" ||
    fail "ensure.sh does not pass \$ansible_args to ansible-playbook"
# shellcheck disable=SC2016  # Match the literal ensure.sh lines, including $.
if grep -q 'ansible-playbook -i inventory.ini $flags' "$_ensure"; then
    fail "ensure.sh still word-splits \$flags into ansible-playbook"
fi

echo "PASS"
