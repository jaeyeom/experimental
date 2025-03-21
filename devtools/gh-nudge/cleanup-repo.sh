#!/usr/bin/env bash
#
# This script cleans up local branches that are "effectively merged"
# into main (i.e., they become identical to main after a rebase).
# We check two conditions:
#   1) There's no diff between the branch and main.
#   2) The branch has no commits not already in main (via git cherry).
#
# If both are true, we delete the branch.
#
# NOTE: Change MAIN_BRANCH to your trunk branch if needed.
#
# Usage: ./cleanup-repo.sh [--dry-run]

set -e

# Check for dry-run flag
DRY_RUN=0
if [ "$1" == "--dry-run" ]; then
    DRY_RUN=1
    echo "Dry run mode activated: No branches will be deleted."
fi

MAIN_BRANCH=$(git remote show origin | sed -n '/HEAD branch/s/.*: //p')

# Ensure we're on main so we don't accidentally delete the checked-out branch
git checkout "$MAIN_BRANCH" >/dev/null 2>&1 || {
    echo "Error: Could not check out branch '$MAIN_BRANCH'." >&2
    exit 1
}

# (Optional) Ensure main is up-to-date with remote
# git pull --ff-only

# Collect branches to delete
branches_to_delete=()

# List local branches except main
for branch in $(git for-each-ref --format='%(refname:short)' refs/heads/); do
    # Skip if it's main
    if [ "$branch" = "$MAIN_BRANCH" ]; then
        continue
    fi

    # Skip if currently checked out (just to avoid confusion)
    current_branch=$(git branch --show-current)
    if [ "$branch" = "$current_branch" ]; then
        continue
    fi

    # Check for commits that aren't in main.
    # `git cherry main branch` outputs:
    #   + <commit hash>  => commit not in main
    #   - <commit hash>  => commit is in main
    # If there's no lines starting with '+', then no unique commits.
    if git cherry "$MAIN_BRANCH" "$branch" | grep -q -v '^+'; then
        branches_to_delete+=("$branch")
    fi
done

if [ "${#branches_to_delete[@]}" -eq 0 ]; then
    echo "No branches found that are fully contained in '$MAIN_BRANCH'."
    exit 0
fi

if [ $DRY_RUN -eq 1 ]; then
    echo "Dry run: Branches that would be deleted:"
    for br in "${branches_to_delete[@]}"; do
        echo "  $br"
    done
    echo "Dry run complete. ${#branches_to_delete[@]} branches would be deleted."
    exit 0
fi

echo "Branches identical to '$MAIN_BRANCH' (will be deleted):"
for br in "${branches_to_delete[@]}"; do
    echo "  $br"
    # Use -D (force) in case Git doesn't recognize them as merged
    git branch -D "$br"
    # Also remove the remote tracking branch
    git update-ref -d "refs/remotes/origin/$br"
done

echo "Done! Deleted ${#branches_to_delete[@]} redundant branches."
