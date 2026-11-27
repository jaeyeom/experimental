#!/bin/sh
set -eu

DIR=$(cd -- "$(dirname -- "$0")" && pwd)
cmd=${1-}
sub=${2-}

case "${cmd} ${sub}" in
  "auth status")
    exit 0
    ;;
  "api user")
    printf '%s\n' "alice"
    exit 0
    ;;
  "search prs")
    if [ -n "${GH_FAKE_LIST_FAIL-}" ]; then
      printf '%s\n' "gh-fake: search prs should not run" >&2
      exit 1
    fi
    # scan passes flags first (`search prs --author ...`); the orphan report
    # passes a ticket positional (`search prs PROJ-123 --author ...`) or a
    # batched OR query as separate argv (`search prs A OR B --author ...`).
    case "${3-}" in
      --*|"")
        cat "${DIR}/gh-search-prs.json"
        ;;
      *)
        if [ -n "${GH_FAKE_SEARCH_FILE-}" ]; then
          cat "$GH_FAKE_SEARCH_FILE"
          exit 0
        fi
        if [ -n "${GH_FAKE_SEARCH_EMPTY-}" ]; then
          printf '%s\n' "[]"
        else
          cat "${DIR}/gh-search-authored-prs.json"
        fi
        ;;
    esac
    exit 0
    ;;
  "pr list")
    if [ -n "${GH_FAKE_LIST_FAIL-}" ]; then
      printf '%s\n' "gh-fake: pr list should not run" >&2
      exit 1
    fi
    cat "${DIR}/gh-pr-list.json"
    exit 0
    ;;
  "pr view")
    if [ -n "${GH_FAKE_VIEW_LOG-}" ]; then
      printf '%s\n' "$*" >> "${GH_FAKE_VIEW_LOG}"
    fi
    number=${3-}
    if [ "${number}" != "123" ]; then
      printf '%s\n' "GraphQL: Could not resolve to a PullRequest with the number of ${number}." >&2
      exit 1
    fi
    if [ -n "${GH_FAKE_PR_VIEW_FILE-}" ]; then
      cat "$GH_FAKE_PR_VIEW_FILE"
    else
      cat "${DIR}/gh-pr-view.json"
    fi
    exit 0
    ;;
  "pr comment")
    if [ -n "${GH_FAKE_COMMENT_SENTINEL-}" ]; then
      printf '%s\n' "$*" > "$GH_FAKE_COMMENT_SENTINEL"
    fi
    if [ -n "${GH_FAKE_COMMENT_BODY_FILE-}" ]; then
      # Extract exact --body argument byte-for-byte without adding a newline.
      while [ $# -gt 0 ]; do
        if [ "$1" = "--body" ] && [ $# -ge 2 ]; then
          shift
          printf '%s' "$1" > "$GH_FAKE_COMMENT_BODY_FILE"
          break
        fi
        shift
      done
    fi
    if [ -n "${GH_FAKE_COMMENT_FAIL-}" ]; then
      printf '%s\n' "gh-fake: comment failed" >&2
      exit 1
    fi
    if [ -n "${GH_FAKE_COMMENT_URL-}" ]; then
      printf '%s\n' "$GH_FAKE_COMMENT_URL"
    fi
    exit 0
    ;;
  "api graphql")
    # Path history, its reviews, review threads, waive timeline, the open-PR
    # search, and the single-PR view share `gh api graphql`.
    case "$*" in
      *HEAD_REF_FORCE_PUSHED_EVENT*)
        if [ -n "${GH_FAKE_WAIVE_TIMELINE_FILE-}" ]; then
          cat "$GH_FAKE_WAIVE_TIMELINE_FILE"
        else
          printf '%s\n' '{"data":{"repository":{"pullRequest":{"comments":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]},"commits":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]},"timelineItems":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]}}}}}'
        fi
        ;;
      *reviewThreads*reviews*|*reviews*reviewThreads*)
        if [ -n "${GH_FAKE_BOT_REREVIEW_FILE-}" ]; then
          cat "$GH_FAKE_BOT_REREVIEW_FILE"
        else
          printf '%s\n' '{"data":{"repository":{"pullRequest":{"commits":{"nodes":[]},"reviews":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]},"reviewThreads":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]},"comments":{"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]}}}}}'
        fi
        ;;
      *committedDate*)
        cat "${DIR}/gh-rca-comments.json"
        ;;
      *':pullRequest(number:'*)
        cat "${DIR}/gh-pr-reviews.json"
        ;;
      *associatedPullRequests*)
        cat "${DIR}/gh-path-history.json"
        ;;
      *requestedReviewer*)
        case "$*" in
          *'search(query:'*)
            if [ -n "${GH_FAKE_LIST_FAIL-}" ]; then
              printf '%s\n' "gh-fake: pr list should not run" >&2
              exit 1
            fi
            if [ -n "${GH_FAKE_OPEN_PR_SEARCH_FILE-}" ]; then
              cat "$GH_FAKE_OPEN_PR_SEARCH_FILE"
            else
              cat "${DIR}/gh-open-pr-search.json"
            fi
            ;;
          *)
            if [ -n "${GH_FAKE_VIEW_LOG-}" ]; then
              printf '%s\n' "$*" >> "${GH_FAKE_VIEW_LOG}"
            fi
            case "$*" in
              *'num=123'*)
                if [ -n "${GH_FAKE_OPEN_PR_VIEW_FILE-}" ]; then
                  cat "$GH_FAKE_OPEN_PR_VIEW_FILE"
                else
                  cat "${DIR}/gh-open-pr-view.json"
                fi
                ;;
              *)
                printf '%s\n' "GraphQL: Could not resolve to a PullRequest." >&2
                exit 1
                ;;
            esac
            ;;
        esac
        ;;
      *)
        if [ -n "${GH_FAKE_REVIEW_THREADS_FILE-}" ]; then
          cat "$GH_FAKE_REVIEW_THREADS_FILE"
        else
          cat "${DIR}/gh-review-threads.json"
        fi
        ;;
    esac
    exit 0
    ;;
  "api --paginate")
    case "${3-}" in
      orgs/acme/teams/platform-reviewers/members)
        cat "${DIR}/gh-team-members.json"
        ;;
      repos/acme/widgets/pulls/123/files)
        printf '%s\n' "src/foo/special.go"
        ;;
      *)
        printf '%s\n' "gh-fake: unexpected: $*" >&2
        exit 1
        ;;
    esac
    exit 0
    ;;
  "api search/issues")
    # Like GitHub: search is GET only, and gh api sends -f fields as a POST
    # unless --method GET is given.
    case "$*" in
      *"--method GET"*) ;;
      *)
        printf '%s\n' "gh: Not Found (HTTP 404)" >&2
        exit 1
        ;;
    esac
    count=1
    case "$*" in
      *user-review-requested:carol*)
        count=5
        ;;
      *user-review-requested:bob*)
        count=1
        ;;
    esac
    printf '%s\n' "{\"total_count\":${count}}"
    exit 0
    ;;
  "api repos/acme/widgets/git/ref/heads/"*)
    printf '%s\n' "base1"
    exit 0
    ;;
  "api repos/acme/widgets/contents/"*)
    cat "${DIR}/gh-codeowners.json"
    exit 0
    ;;
  *)
    printf '%s\n' "gh-fake: unexpected: $*" >&2
    exit 1
    ;;
esac
