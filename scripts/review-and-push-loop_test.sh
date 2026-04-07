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
codex_args_log="$state_dir/codex-args.log"

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
    push)
        refspec="${2:-}"
        commit="${refspec%%:*}"
        current_ahead=$(cat "$state_dir/ahead_count")
        printf '%s\n' "$refspec" >>"$state_dir/push.log"
        case "$commit" in
            commit-1)
                printf '%s' $((current_ahead - 1)) >"$state_dir/ahead_count"
                ;;
            commit-2)
                printf '%s' $((current_ahead - 2)) >"$state_dir/ahead_count"
                ;;
            commit-3)
                printf '%s' $((current_ahead - 3)) >"$state_dir/ahead_count"
                ;;
            *)
                printf 'unexpected push target: %s\n' "$commit" >&2
                exit 1
                ;;
        esac
        ;;
    rev-parse)
        candidate="${3:-}"
        candidate="${candidate%\^\{commit\}}"
        case "$candidate" in
            commit-1|commit-2|commit-3)
                printf '%s\n' "$candidate"
                ;;
            *)
                exit 1
                ;;
        esac
        ;;
    merge-base)
        if [ "${1:-}" != "--is-ancestor" ]; then
            printf 'unexpected merge-base mode\n' >&2
            exit 1
        fi
        left="${2:-}"
        right="${3:-}"
        if [ "$right" = "HEAD" ]; then
            case "$left" in
                commit-1|commit-2|commit-3)
                    exit 0
                    ;;
                *)
                    exit 1
                    ;;
            esac
        fi
        case "$left:$right" in
            origin/main:commit-1|origin/main:commit-2|origin/main:commit-3)
                exit 0
                ;;
            commit-1:commit-1|commit-1:commit-2|commit-1:commit-3)
                exit 0
                ;;
            commit-2:commit-2|commit-2:commit-3)
                exit 0
                ;;
            commit-3:commit-3)
                exit 0
                ;;
            *)
                exit 1
                ;;
        esac
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
help_mode="${TEST_CODEX_HELP_MODE:-modern}"
stdout_mode="${TEST_CODEX_STDOUT_MODE:-quiet}"
output_file=""
schema_file=""
printf '%s\n' "$*" >>"$state_dir/codex-args.log"

if [ "${1:-}" = "exec" ] && [ "${2:-}" = "--help" ]; then
    case "$help_mode" in
        modern)
            cat <<'MODERN_HELP'
Usage: codex exec [OPTIONS] [PROMPT] [COMMAND]
      --dangerously-bypass-approvals-and-sandbox
      --ask-for-approval <APPROVAL_POLICY>
  -s, --sandbox <SANDBOX_MODE>
      --full-auto
      --ephemeral
      --output-schema <FILE>
  -o, --output-last-message <FILE>
      --color <COLOR>
MODERN_HELP
            ;;
        legacy)
            cat <<'LEGACY_HELP'
Usage: codex exec --cd <DIR> [PROMPT]
      --full-auto
      --ephemeral
      --output-schema <FILE>
  -o, --output-last-message <FILE>
      --color <COLOR>
LEGACY_HELP
            ;;
        *)
            printf 'unexpected test codex help mode: %s\n' "$help_mode" >&2
            exit 1
            ;;
    esac
    exit 0
fi

while [ "$#" -gt 0 ]; do
    case "$1" in
        --output-schema)
            schema_file="$2"
            shift 2
            ;;
        -o|--output-last-message)
            output_file="$2"
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done

if [ -n "$schema_file" ]; then
    cp "$schema_file" "$state_dir/last-schema.json"
fi

case "$stdout_mode" in
    quiet)
        ;;
    noisy)
        printf 'OpenAI Codex vtest\n'
        printf '{"status":"BLOCKED","summary":"stdout noise","blocked_reason":"ignore me"}\n'
        printf 'tokens used\n'
        ;;
    *)
        printf 'unexpected test codex stdout mode: %s\n' "$stdout_mode" >&2
        exit 1
        ;;
esac

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
        printf '{"status":"PUSH","summary":"push oldest","push_target":"commit-1","blocked_reason":null}\n' >"$output_file"
        ;;
    push-all)
        ahead=$(cat "$state_dir/ahead_count")
        if [ "$ahead" -ge 2 ]; then
            printf '{"status":"PUSH","summary":"push two commits","push_target":"commit-2","blocked_reason":null}\n' >"$output_file"
        else
            printf '{"status":"PUSH","summary":"push oldest","push_target":"commit-1","blocked_reason":null}\n' >"$output_file"
        fi
        ;;
    blocked)
        printf '{"status":"BLOCKED","summary":"blocked","push_target":null,"blocked_reason":"hard blocker"}\n' >"$output_file"
        ;;
    no-progress)
        printf '{"status":"PUSH","summary":"bad target","push_target":"not-a-commit","blocked_reason":null}\n' >"$output_file"
        ;;
    hang)
        sleep 2
        printf '{"status":"PUSH","summary":"late","push_target":"commit-1","blocked_reason":null}\n' >"$output_file"
        ;;
    *)
        printf 'unexpected test codex mode: %s\n' "$status_mode" >&2
        exit 1
        ;;
esac
EOF
chmod +x "$codex_stub"

# shellcheck disable=SC2120
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
assert_contains "exec --help" "$(head -n 1 "$codex_args_log")"
assert_contains "--ask-for-approval never" "$(cat "$codex_args_log")"
assert_contains "--sandbox workspace-write" "$(cat "$codex_args_log")"
assert_contains '"required": ["status", "summary", "push_target", "blocked_reason"]' "$(cat "$state_dir/last-schema.json")"
assert_contains '"type": ["string", "null"]' "$(cat "$state_dir/last-schema.json")"

printf '1' >"$state_dir/ahead_count"
rm -f "$state_dir/codex_calls" "$state_dir/push.log"
set +e
blocked_output=$(TEST_CODEX_MODE=blocked run_script 2>&1)
blocked_status=$?
set -e
assert_equals "2" "$blocked_status"
assert_contains "BLOCKED: review-and-push stopped on commit-1." "$blocked_output"
assert_equals "1" "$(cat "$state_dir/codex_calls")"
if [ -f "$state_dir/push.log" ]; then
	fail "did not expect blocked mode to push"
fi

printf '1' >"$state_dir/ahead_count"
rm -f "$state_dir/codex_calls"
set +e
blocked_noisy_output=$(TEST_CODEX_MODE=blocked TEST_CODEX_STDOUT_MODE=noisy run_script 2>&1)
blocked_noisy_status=$?
set -e
assert_equals "2" "$blocked_noisy_status"
assert_contains "BLOCKED: review-and-push stopped on commit-1." "$blocked_noisy_output"

printf '1' >"$state_dir/ahead_count"
rm -f "$state_dir/codex_calls"
set +e
no_progress_output=$(TEST_CODEX_MODE=no-progress run_script 2>&1)
no_progress_status=$?
set -e
assert_equals "2" "$no_progress_status"
assert_contains "proposed invalid push target" "$no_progress_output"

printf '2' >"$state_dir/ahead_count"
rm -f "$state_dir/codex_calls" "$state_dir/push.log"
push_all_output=$(TEST_CODEX_MODE=push-all run_script 2>&1)
assert_contains "DONE: HEAD is not ahead of origin/main." "$push_all_output"
assert_contains "Pushed through commit-2. Remaining ahead count: 0." "$push_all_output"
assert_equals "1" "$(cat "$state_dir/codex_calls")"
assert_contains "commit-2:refs/heads/main" "$(cat "$state_dir/push.log")"

printf '1' >"$state_dir/ahead_count"
rm -f "$state_dir/codex_calls" "$state_dir/codex-args.log"
legacy_output=$(TEST_CODEX_MODE=loop TEST_CODEX_HELP_MODE=legacy run_script 2>&1)
assert_contains "DONE: HEAD is not ahead of origin/main." "$legacy_output"
assert_contains "--full-auto" "$(cat "$codex_args_log")"
if grep -Fq -- "--ask-for-approval never" "$codex_args_log"; then
	fail "did not expect legacy codex invocation to use --ask-for-approval"
fi

printf '1' >"$state_dir/ahead_count"
rm -f "$state_dir/codex_calls" "$state_dir/codex-args.log"
set +e
timeout_output=$(TEST_CODEX_MODE=hang CODEX_TIMEOUT_SECONDS=1 run_script 2>&1)
timeout_status=$?
set -e
assert_equals "2" "$timeout_status"
assert_contains "Codex execution timed out after 1s" "$timeout_output"
assert_contains "BLOCKED: Codex execution failed during iteration 1." "$timeout_output"

printf 'PASS\n'
