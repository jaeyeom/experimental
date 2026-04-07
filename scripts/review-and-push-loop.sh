#!/bin/bash

set -euo pipefail

usage() {
    cat <<'EOF'
Usage: review-and-push-loop.sh [--repo DIR] [--base BRANCH] [--max-iterations N]

Review and push the oldest commit ahead of the remote base branch in a fresh
Codex process, then repeat until there are no local commits left to review or
the workflow is blocked.
EOF
}

log() {
    printf '%s\n' "$*"
}

fail() {
    printf 'Error: %s\n' "$*" >&2
    exit 1
}

repo_dir=$(pwd)
base_branch_override=""
max_iterations=100
git_bin="${GIT_BIN:-git}"
codex_bin="${CODEX_BIN:-codex}"
codex_timeout_seconds="${CODEX_TIMEOUT_SECONDS:-300}"
codex_exec_help_cache=""

while [ "$#" -gt 0 ]; do
    case "$1" in
        --repo)
            [ "$#" -ge 2 ] || fail "--repo requires a value"
            repo_dir="$2"
            shift 2
            ;;
        --base)
            [ "$#" -ge 2 ] || fail "--base requires a value"
            base_branch_override="$2"
            shift 2
            ;;
        --max-iterations)
            [ "$#" -ge 2 ] || fail "--max-iterations requires a value"
            max_iterations="$2"
            shift 2
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        *)
            fail "unknown argument: $1"
            ;;
    esac
done

case "$max_iterations" in
    ''|*[!0-9]*)
        fail "--max-iterations must be a positive integer"
        ;;
    0)
        fail "--max-iterations must be greater than zero"
        ;;
esac

case "$codex_timeout_seconds" in
    ''|*[!0-9]*)
        fail "CODEX_TIMEOUT_SECONDS must be a non-negative integer"
        ;;
esac

if [ ! -d "$repo_dir" ]; then
    fail "repo directory does not exist: $repo_dir"
fi

git_cmd() {
    "$git_bin" -C "$repo_dir" "$@"
}

codex_exec_help() {
    if [ -z "$codex_exec_help_cache" ]; then
        codex_exec_help_cache=$("$codex_bin" exec --help 2>&1 || true)
    fi
    printf '%s\n' "$codex_exec_help_cache"
}

codex_exec_supports_flag() {
    local flag="$1"
    codex_exec_help | grep -Fq -- "$flag"
}

build_codex_exec_args() {
    codex_args=(exec --cd "$repo_dir")

    if codex_exec_supports_flag "--ask-for-approval"; then
        codex_args+=(--ask-for-approval never)
    elif codex_exec_supports_flag "--full-auto"; then
        codex_args+=(--full-auto)
    fi

    if codex_exec_supports_flag "--sandbox"; then
        codex_args+=(--sandbox workspace-write)
    fi

    if codex_exec_supports_flag "--ephemeral"; then
        codex_args+=(--ephemeral)
    fi

    if codex_exec_supports_flag "--color"; then
        codex_args+=(--color never)
    fi

    if codex_exec_supports_flag "--output-schema"; then
        codex_args+=(--output-schema "$schema_file")
    fi

    if codex_exec_supports_flag "--output-last-message"; then
        codex_args+=(--output-last-message "$output_file")
    fi

    codex_args+=(-)
}

resolve_base_branch() {
    if [ -n "$base_branch_override" ]; then
        printf '%s\n' "$base_branch_override"
        return
    fi

    local detected
    detected=$(git_cmd symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null || true)
    detected=${detected#origin/}
    if [ -n "$detected" ]; then
        printf '%s\n' "$detected"
        return
    fi

    local candidate
    for candidate in main master trunk; do
        if git_cmd show-ref --verify --quiet "refs/remotes/origin/$candidate"; then
            printf '%s\n' "$candidate"
            return
        fi
    done

    fail "could not determine remote base branch"
}

ahead_count() {
    local remote_ref="$1"
    git_cmd rev-list --count "$remote_ref..HEAD"
}

oldest_ahead_commit() {
    local remote_ref="$1"
    git_cmd rev-list "$remote_ref..HEAD" | tail -n 1
}

parse_json_string_field() {
    local file="$1"
    local field="$2"

    sed -n "s/.*\"$field\"[[:space:]]*:[[:space:]]*\"\\([^\"]*\\)\".*/\\1/p" "$file" | head -n 1
}

codex_result_status=""
codex_result_push_target=""

validate_push_target() {
    local push_target="$1"
    local remote_ref="$2"
    local oldest_target="$3"

    [ -n "$push_target" ] || return 1
    git_cmd rev-parse --verify --quiet "$push_target^{commit}" >/dev/null 2>&1 || return 1
    git_cmd merge-base --is-ancestor "$remote_ref" "$push_target" >/dev/null 2>&1 || return 1
    git_cmd merge-base --is-ancestor "$oldest_target" "$push_target" >/dev/null 2>&1 || return 1
    git_cmd merge-base --is-ancestor "$push_target" HEAD >/dev/null 2>&1 || return 1
}

run_codex_pass() {
    local iteration="$1"
    local base_branch="$2"
    local remote_ref="$3"
    local ahead="$4"
    local target_commit="$5"
    local schema_file
    local output_file
    local prompt_file
    local -a codex_args
    local codex_status
    local codex_pid
    local elapsed

    schema_file=$(mktemp)
    output_file=$(mktemp)
    prompt_file=$(mktemp)

    cat >"$schema_file" <<'EOF'
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "additionalProperties": false,
  "required": ["status", "summary", "push_target", "blocked_reason"],
  "properties": {
    "status": {
      "type": "string",
      "enum": ["PUSH", "BLOCKED"]
    },
    "summary": {
      "type": "string"
    },
    "push_target": {
      "type": ["string", "null"]
    },
    "blocked_reason": {
      "type": ["string", "null"]
    }
  }
}
EOF

    cat >"$prompt_file" <<EOF
You are running one isolated pass of the review-and-push workflow.

Repository: $repo_dir
Base branch: $base_branch
Remote ref: $remote_ref
Iteration: $iteration
Ahead count before review: $ahead
Target commit: $target_commit

Instructions:
- Review the target commit above. You may also inspect its descendants through HEAD if needed to decide whether the wrapper should push exactly one commit or several contiguous commits.
- Treat the current working tree as potentially dirty and ignore unrelated untracked or unstaged files unless they directly affect the target commit review.
- Keep the review phase read-only and inspect Git objects directly.
- Do not run git push or any other network-changing command. The wrapper will validate your JSON response and perform any push itself.
- Treat only hard blockers as push-stopping issues: security bugs, leaked secrets, obvious destructive data loss or corruption, clearly broken startup or primary-path behavior, or similarly severe issues.
- Non-blocking concerns should become concise follow-up items, not a reason to stop the push.
- Your final response must be a JSON object and nothing else.
- Use status PUSH when there are no hard blockers and the wrapper should push commits through push_target.
- Set push_target to $target_commit to push exactly one commit, or to a descendant commit hash to push multiple contiguous commits in one step.
- Use status BLOCKED if a hard blocker or execution failure stopped progress.
- Always include push_target. Use null when status is BLOCKED.
- Always include blocked_reason. Use null when status is PUSH.
EOF

    build_codex_exec_args

    if [ "$codex_timeout_seconds" -gt 0 ]; then
        set +e
        "$codex_bin" "${codex_args[@]}" <"$prompt_file" >&2 &
        codex_pid=$!
        elapsed=0

        while kill -0 "$codex_pid" 2>/dev/null; do
            if [ "$elapsed" -ge "$codex_timeout_seconds" ]; then
                kill "$codex_pid" 2>/dev/null || true
                wait "$codex_pid" 2>/dev/null || true
                set -e
                printf 'Error: Codex execution timed out after %ss\n' "$codex_timeout_seconds" >&2
                rm -f "$schema_file" "$output_file" "$prompt_file"
                return 1
            fi
            sleep 1
            elapsed=$((elapsed + 1))
        done

        wait "$codex_pid"
        codex_status=$?
        set -e
    else
        set +e
        "$codex_bin" "${codex_args[@]}" <"$prompt_file" >&2
        codex_status=$?
        set -e
    fi

    if [ "$codex_status" -ne 0 ]; then
        rm -f "$schema_file" "$output_file" "$prompt_file"
        return 1
    fi

    codex_result_status=$(parse_json_string_field "$output_file" status)
    codex_result_push_target=$(parse_json_string_field "$output_file" push_target)
    rm -f "$schema_file" "$output_file" "$prompt_file"
}

base_branch=$(resolve_base_branch)
remote_ref="origin/$base_branch"

iteration=1
while [ "$iteration" -le "$max_iterations" ]; do
    log "Fetching $remote_ref before iteration $iteration..."
    git_cmd fetch origin

    ahead=$(ahead_count "$remote_ref")
    if [ "$ahead" -eq 0 ]; then
        log "DONE: HEAD is not ahead of $remote_ref."
        exit 0
    fi

    target_commit=$(oldest_ahead_commit "$remote_ref")
    if [ -z "$target_commit" ]; then
        fail "failed to resolve the oldest ahead commit"
    fi

    log "Iteration $iteration: reviewing and pushing $target_commit ($ahead commit(s) ahead of $remote_ref)."

    run_codex_pass "$iteration" "$base_branch" "$remote_ref" "$ahead" "$target_commit" || {
        log "BLOCKED: Codex execution failed during iteration $iteration."
        exit 2
    }

    case "$codex_result_status" in
        PUSH)
            if ! validate_push_target "$codex_result_push_target" "$remote_ref" "$target_commit"; then
                log "BLOCKED: Codex proposed invalid push target '$codex_result_push_target' for oldest commit $target_commit."
                exit 2
            fi

            if ! git_cmd push origin "$codex_result_push_target:refs/heads/$base_branch"; then
                log "BLOCKED: push failed for $codex_result_push_target."
                exit 2
            fi

            ahead_after=$(ahead_count "$remote_ref")
            if [ "$ahead_after" -ge "$ahead" ]; then
                log "BLOCKED: push to $codex_result_push_target completed, but the ahead count did not decrease."
                exit 2
            fi
            log "Pushed through $codex_result_push_target. Remaining ahead count: $ahead_after."
            ;;
        BLOCKED)
            log "BLOCKED: review-and-push stopped on $target_commit."
            exit 2
            ;;
        *)
            log "BLOCKED: unexpected Codex status '$codex_result_status' for $target_commit."
            exit 2
            ;;
    esac

    iteration=$((iteration + 1))
done

log "BLOCKED: reached max iterations ($max_iterations) before the branch was fully pushed."
exit 2
