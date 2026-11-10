#!/usr/bin/env bash
# review-range.sh - Work out what fork-keeper must review, from git refs only.
#
# Usage: review-range.sh [-u <upstream-remote>] [-b <branch>]
#   Run inside a fork checkout that has already fetched:
#     origin                                   (the fork)
#     <upstream-remote>                        (refs/remotes/<u>/<b>)
#     +refs/fork-keeper/*:refs/fork-keeper/*   (from origin; optional)
#     +refs/tags/*:refs/upstream-tags/*        (from upstream; optional)
#
# Prints one JSON object on stdout (see references/verdict-schema.md, `range`).
# Exit 0 on success, 2 on missing refs or bad usage.
#
# Read-only: never modifies refs, the index, or the working tree.

set -euo pipefail

upstream=upstream
branch=main

die() {
	echo "review-range: $*" >&2
	exit 2
}

while getopts "u:b:" opt; do
	case $opt in
	u) upstream=$OPTARG ;;
	b) branch=$OPTARG ;;
	*) die "usage: review-range.sh [-u <upstream-remote>] [-b <branch>]" ;;
	esac
done

command -v jq >/dev/null 2>&1 || die "jq is required"

new=$(git rev-parse -q --verify "refs/remotes/$upstream/$branch^{commit}") ||
	die "missing refs/remotes/$upstream/$branch (fetch $upstream first)"
fork=$(git rev-parse -q --verify "refs/remotes/origin/$branch^{commit}") ||
	die "missing refs/remotes/origin/$branch (fetch origin first)"

if old=$(git rev-parse -q --verify "refs/fork-keeper/upstream-head^{commit}"); then
	first_run=false
else
	old=$(git merge-base "$fork" "$new") || die "fork and upstream share no history"
	first_run=true
fi

up_to_date=false
if git merge-base --is-ancestor "$new" "$fork"; then up_to_date=true; fi

rewritten=false
review_base=$old
if [[ $first_run == false ]] && ! git merge-base --is-ancestor "$old" "$new"; then
	rewritten=true
	review_base=$(git merge-base "$old" "$new" || true)
fi

# The fork must contain the recorded head; if it doesn't (rolled back, or a
# promotion that never landed), commits between the fork and the recorded head
# were never accepted, so review everything upstream has that the fork lacks.
fork_behind_recorded_head=false
if [[ $first_run == false ]] && ! git merge-base --is-ancestor "$old" "$fork"; then
	fork_behind_recorded_head=true
	review_base=$(git merge-base "$fork" "$new" || true)
fi

# The user's patch stack: fork commits whose patch upstream doesn't already
# have. Commits reachable from the recorded upstream head are upstream's, even
# if upstream has since dropped them (a rewrite), so never treat them as the
# user's.
exclude_old=()
if [[ $first_run == false ]]; then exclude_old=("^$old"); fi
user_commits=$(git rev-list --reverse --no-merges --right-only --cherry-pick \
	"$new...$fork" ${exclude_old[@]+"${exclude_old[@]}"})

fork_has_merges=false
if [[ -n $(git rev-list --merges "$new..$fork") ]]; then fork_has_merges=true; fi

mirror=$(git for-each-ref --format='%(refname:lstrip=3)%09%(objectname)' refs/fork-keeper/upstream-tags/)
fetched=$(git for-each-ref --format='%(refname:lstrip=2)%09%(objectname)' refs/upstream-tags/)

jq -cn \
	--arg old "$old" --arg new "$new" --arg base "$review_base" \
	--argjson first_run "$first_run" --argjson up_to_date "$up_to_date" \
	--argjson rewritten "$rewritten" --argjson fork_has_merges "$fork_has_merges" \
	--argjson fork_behind_recorded_head "$fork_behind_recorded_head" \
	--arg user_commits "$user_commits" --arg mirror "$mirror" --arg fetched "$fetched" '
	def lines: split("\n") | map(select(length > 0));
	def tagmap: lines | map(split("\t") | {(.[0]): .[1]}) | add // {};
	($mirror | tagmap) as $m | ($fetched | tagmap) as $f |
	{
		upstream_old: $old,
		upstream_new: $new,
		first_run: $first_run,
		up_to_date: $up_to_date,
		rewritten: $rewritten,
		review_base: (if $base == "" then null else $base end),
		user_commits: ($user_commits | lines),
		fork_has_merges: $fork_has_merges,
		fork_behind_recorded_head: $fork_behind_recorded_head,
		moved_tags: [$m | to_entries[]
			| select($f[.key] != null and $f[.key] != .value)
			| {name: .key, was: .value, now: $f[.key]}],
		deleted_tags: [$m | keys[] | select($f[.] == null)]
	}'
