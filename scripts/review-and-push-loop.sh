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

if [ ! -d "$repo_dir" ]; then
    fail "repo directory does not exist: $repo_dir"
fi

git_cmd() {
    "$git_bin" -C "$repo_dir" "$@"
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

run_codex_pass() {
    local iteration="$1"
    local base_branch="$2"
    local remote_ref="$3"
    local ahead="$4"
    local target_commit="$5"
    local schema_file
    local output_file
    local prompt_file

    schema_file=$(mktemp)
    output_file=$(mktemp)
    prompt_file=$(mktemp)

    cat >"$schema_file" <<'EOF'
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "additionalProperties": false,
  "required": ["status", "summary"],
  "properties": {
    "status": {
      "type": "string",
      "enum": ["APPROVE_PUSHED", "BLOCKED"]
    },
    "summary": {
      "type": "string"
    },
    "blocked_reason": {
      "type": "string"
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
- Review exactly the target commit above, not later commits.
- Treat the current working tree as potentially dirty and ignore unrelated untracked or unstaged files unless they directly affect the target commit review.
- Keep the review phase read-only and inspect Git objects directly.
- Treat only hard blockers as push-stopping issues: security bugs, leaked secrets, obvious destructive data loss or corruption, clearly broken startup or primary-path behavior, or similarly severe issues.
- Non-blocking concerns should become concise follow-up items, not a reason to stop the push.
- If there are no hard blockers, push exactly this commit with:
  git push origin "$target_commit:refs/heads/$base_branch"
- If the push fails because it is not a fast-forward or because required auth/network actions fail, treat that as blocked.
- Your final response must be a JSON object and nothing else.
- Use status APPROVE_PUSHED only if you reviewed this commit and the exact push succeeded.
- Use status BLOCKED if a hard blocker or execution failure stopped progress.
EOF

    if ! "$codex_bin" exec \
        --cd "$repo_dir" \
        --full-auto \
        --ephemeral \
        --color never \
        --output-schema "$schema_file" \
        --output-last-message "$output_file" \
        - <"$prompt_file"; then
        rm -f "$schema_file" "$output_file" "$prompt_file"
        return 1
    fi

    parse_json_string_field "$output_file" status
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

    status=$(run_codex_pass "$iteration" "$base_branch" "$remote_ref" "$ahead" "$target_commit") || {
        log "BLOCKED: Codex execution failed during iteration $iteration."
        exit 2
    }

    case "$status" in
        APPROVE_PUSHED)
            ahead_after=$(ahead_count "$remote_ref")
            if [ "$ahead_after" -ge "$ahead" ]; then
                log "BLOCKED: Codex reported APPROVE_PUSHED for $target_commit, but the ahead count did not decrease."
                exit 2
            fi
            log "Pushed $target_commit. Remaining ahead count: $ahead_after."
            ;;
        BLOCKED)
            log "BLOCKED: review-and-push stopped on $target_commit."
            exit 2
            ;;
        *)
            log "BLOCKED: unexpected Codex status '$status' for $target_commit."
            exit 2
            ;;
    esac

    iteration=$((iteration + 1))
done

log "BLOCKED: reached max iterations ($max_iterations) before the branch was fully pushed."
exit 2
