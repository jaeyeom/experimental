#!/bin/bash
set -euo pipefail

# Script to copy review comments from one PR to another
# Usage: ./copy-pr-comments.sh <source_pr> <target_pr>

SCRIPT_NAME=$(basename "$0")

usage() {
    echo "Usage: $SCRIPT_NAME <source_pr> <target_pr>"
    echo ""
    echo "Copy all review comments from source PR to target PR"
    echo ""
    echo "Examples:"
    echo "  $SCRIPT_NAME 15779 15781"
    echo "  $SCRIPT_NAME 123 456"
    echo ""
    echo "Requirements:"
    echo "  - gh CLI tool must be installed and authenticated"
    echo "  - Must be run from within a git repository"
    echo "  - User must have write access to the target PR"
    exit 1
}

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >&2
}

error() {
    echo "[ERROR] $*" >&2
    exit 1
}

# Check arguments
if [[ $# -ne 2 ]]; then
    usage
fi

SOURCE_PR="$1"
TARGET_PR="$2"

# Validate PR numbers are numeric
if ! [[ "$SOURCE_PR" =~ ^[0-9]+$ ]] || ! [[ "$TARGET_PR" =~ ^[0-9]+$ ]]; then
    error "PR numbers must be numeric"
fi

# Check if gh CLI is available
if ! command -v gh &> /dev/null; then
    error "gh CLI tool is required but not installed. Install it from https://cli.github.com/"
fi

# Check if we're in a git repository
if ! git rev-parse --git-dir &> /dev/null; then
    error "Must be run from within a git repository"
fi

# Get repository information
REPO=$(gh repo view --json owner,name --jq '.owner.login + "/" + .name')
log "Working with repository: $REPO"

# Create temporary files
TEMP_DIR=$(mktemp -d)
COMMENTS_FILE="$TEMP_DIR/comments.json"
REVIEW_FILE="$TEMP_DIR/review.json"

cleanup() {
    rm -rf "$TEMP_DIR"
}
trap cleanup EXIT

log "Fetching comments from PR #$SOURCE_PR..."

# Fetch comments from source PR
if ! gh api "repos/$REPO/pulls/$SOURCE_PR/comments" > "$COMMENTS_FILE"; then
    error "Failed to fetch comments from PR #$SOURCE_PR. Check if the PR exists and you have access."
fi

# Check if there are any comments
COMMENT_COUNT=$(jq length "$COMMENTS_FILE")
if [[ "$COMMENT_COUNT" -eq 0 ]]; then
    log "No comments found in PR #$SOURCE_PR"
    exit 0
fi

log "Found $COMMENT_COUNT comment(s) in PR #$SOURCE_PR"

# Check if target PR exists
if ! gh pr view "$TARGET_PR" --json number &> /dev/null; then
    error "Target PR #$TARGET_PR does not exist or is not accessible"
fi

log "Processing comments for PR #$TARGET_PR..."

# Transform comments for the new PR
jq '{
    body: "",
    comments: [
        .[] | {
            path: .path,
            line: .line,
            body: .body
        }
    ]
}' "$COMMENTS_FILE" > "$REVIEW_FILE"

# Validate the review file
if ! jq empty "$REVIEW_FILE" 2>/dev/null; then
    error "Failed to create valid review JSON"
fi

# Create the review on target PR
log "Creating review on PR #$TARGET_PR..."
if REVIEW_RESULT=$(gh api "repos/$REPO/pulls/$TARGET_PR/reviews" --method POST --input "$REVIEW_FILE" 2>&1); then
    REVIEW_ID=$(echo "$REVIEW_RESULT" | jq -r '.id')
    REVIEW_URL=$(echo "$REVIEW_RESULT" | jq -r '.html_url')
    log "✅ Successfully created review #$REVIEW_ID"
    log "Review URL: $REVIEW_URL"
    log "Note: Review is in PENDING state. Submit it from the GitHub web interface when ready."
else
    error "Failed to create review on PR #$TARGET_PR: $REVIEW_RESULT"
fi

# Summary
log "Summary:"
log "  Source PR: #$SOURCE_PR"
log "  Target PR: #$TARGET_PR"
log "  Comments copied: $COMMENT_COUNT"
log "  Review created: #$REVIEW_ID (PENDING)"