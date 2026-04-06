#!/bin/bash

set -euo pipefail

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_contains() {
    local needle="$1"
    local haystack="$2"

    if [[ "$haystack" != *"$needle"* ]]; then
        fail "expected output to contain '$needle'"
    fi
}

assert_equals() {
    local expected="$1"
    local actual="$2"

    if [ "$expected" != "$actual" ]; then
        fail "expected '$expected', got '$actual'"
    fi
}

workspace_root="${TEST_SRCDIR}/${TEST_WORKSPACE}"
script_path="${workspace_root}/scripts/review-and-push-loop.sh"
tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

git_stub="$tmpdir/git-stub.sh"
codex_stub="$tmpdir/codex-stub.sh"
repo_dir="$tmpdir/repo"
state_dir="$tmpdir/state"
mkdir -p "$repo_dir" "$state_dir"

cat >"$git_stub" <<'EOF'
#!/bin/bash

set -euo pipefail

state_dir="${TEST_STATE_DIR:?}"

if [ "${1:-}" = "-C" ]; then
    shift 2
fi

command="$1"
shift

case "$command" in
    fetch)
        printf '%s\n' "${2:-}" >>"$state_dir/fetch.log"
        ;;
    symbolic-ref)
        printf 'origin/main\n'
        ;;
    show-ref)
        exit 1
        ;;
    rev-list)
        if [ "${1:-}" = "--count" ]; then
            cat "$state_dir/ahead_count"
            exit 0
        fi

        case "$(cat "$state_dir/ahead_count")" in
            0)
                ;;
            1)
                printf 'commit-1\n'
                ;;
            2)
                printf 'commit-2\ncommit-1\n'
                ;;
            *)
                printf 'commit-3\ncommit-2\ncommit-1\n'
                ;;
        esac
        ;;
    *)
        printf 'unexpected git command: %s\n' "$command" >&2
        exit 1
        ;;
esac
EOF
chmod +x "$git_stub"

cat >"$codex_stub" <<'EOF'
#!/bin/bash

set -euo pipefail

state_dir="${TEST_STATE_DIR:?}"
status_mode="${TEST_CODEX_MODE:?}"
output_file=""

while [ "$#" -gt 0 ]; do
    case "$1" in
        -o|--output-last-message)
            output_file="$2"
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done

[ -n "$output_file" ] || {
    printf 'missing output file\n' >&2
    exit 1
}

count_file="$state_dir/codex_calls"
if [ ! -f "$count_file" ]; then
    printf '0' >"$count_file"
fi
count=$(cat "$count_file")
count=$((count + 1))
printf '%s' "$count" >"$count_file"

case "$status_mode" in
    loop)
        ahead=$(cat "$state_dir/ahead_count")
        printf '{"status":"APPROVE_PUSHED","summary":"pushed"}\n' >"$output_file"
        printf '%s' $((ahead - 1)) >"$state_dir/ahead_count"
        ;;
    blocked)
        printf '{"status":"BLOCKED","summary":"blocked","blocked_reason":"hard blocker"}\n' >"$output_file"
        ;;
    no-progress)
        printf '{"status":"APPROVE_PUSHED","summary":"claimed push"}\n' >"$output_file"
        ;;
    *)
        printf 'unexpected test codex mode: %s\n' "$status_mode" >&2
        exit 1
        ;;
esac
EOF
chmod +x "$codex_stub"

run_script() {
    TEST_STATE_DIR="$state_dir" \
    GIT_BIN="$git_stub" \
    CODEX_BIN="$codex_stub" \
    "$script_path" --repo "$repo_dir" "$@"
}

printf '2' >"$state_dir/ahead_count"
printf 'loop' >"$state_dir/mode"
output=$(TEST_CODEX_MODE=loop run_script 2>&1)
assert_contains "DONE: HEAD is not ahead of origin/main." "$output"
assert_equals "0" "$(cat "$state_dir/ahead_count")"
assert_equals "2" "$(cat "$state_dir/codex_calls")"

printf '1' >"$state_dir/ahead_count"
rm -f "$state_dir/codex_calls"
set +e
blocked_output=$(TEST_CODEX_MODE=blocked run_script 2>&1)
blocked_status=$?
set -e
assert_equals "2" "$blocked_status"
assert_contains "BLOCKED: review-and-push stopped on commit-1." "$blocked_output"
assert_equals "1" "$(cat "$state_dir/codex_calls")"

printf '1' >"$state_dir/ahead_count"
rm -f "$state_dir/codex_calls"
set +e
no_progress_output=$(TEST_CODEX_MODE=no-progress run_script 2>&1)
no_progress_status=$?
set -e
assert_equals "2" "$no_progress_status"
assert_contains "ahead count did not decrease" "$no_progress_output"

printf 'PASS\n'
