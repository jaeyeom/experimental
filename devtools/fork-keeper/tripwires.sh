#!/usr/bin/env bash
# tripwires.sh - fork-keeper's deterministic checks on an upstream sync.
#
# Usage: tripwires.sh <git-dir> <range.json> <candidate-sha>
#
#   <git-dir>        repository holding the fetched upstream, fork, and
#                    candidate objects
#   <range.json>     review-range.sh output computed by the scheduler (not the
#                    agent's copy)
#   <candidate-sha>  the candidate head that would be promoted
#
# Prints one "tripwire: <kind>: <detail>" line per hit. Exit 0 when nothing
# hit, 1 on any hit, 2 on bad usage. fork-keeper.sh promotes only on exit 0.
#
# Kinds:
#   range          a review-range flag that always needs the user
#   tags           upstream moved or deleted a mirrored tag
#   url            an added line names a host the old tree never mentions
#   blob           an added line holds a long base64-like run
#   binary         a binary file changed
#   ci-or-install  a CI config, hook, Makefile, or install/setup script changed
#   manifest       a dependency manifest or lock file changed
#   stack          the candidate is not exactly the user's patches on top of
#                  upstream_new (stricter than validate-verdict.sh, which
#                  accepts confident conflict resolutions)
#
# Read-only: never modifies refs, the index, or the working tree, and never
# runs anything from the repository.

set -euo pipefail

[[ $# -eq 3 ]] || {
	echo "usage: tripwires.sh <git-dir> <range.json> <candidate-sha>" >&2
	exit 2
}
git_dir=$1
range=$2
cand=$3
G() { git --git-dir="$git_dir" "$@"; }

hits=0
hit() {
	printf 'tripwire: %s: %s\n' "$1" "$2"
	hits=$((hits + 1))
}

ci_re='^\.(github|circleci|buildkite|githooks|husky)/|^\.gitlab-ci|^\.travis\.yml$|(^|/)\.pre-commit-|(^|/)(GNUmakefile|[Mm]akefile|[^/]*\.mk|Jenkinsfile|[Jj]ustfile|Rakefile|Taskfile\.ya?ml)$|(^|/)(install|setup|bootstrap)[^/]*$'
manifest_re='(^|/)(package\.json|package-lock\.json|npm-shrinkwrap\.json|yarn\.lock|pnpm-lock\.yaml|bun\.lockb?|go\.mod|go\.sum|Cargo\.toml|Cargo\.lock|requirements[^/]*\.txt|pyproject\.toml|poetry\.lock|uv\.lock|Pipfile(\.lock)?|setup\.cfg|Gemfile(\.lock)?|[^/]*\.gemspec|composer\.(json|lock)|Eask|Cask|[^/]*-pkg\.el|MODULE\.bazel(\.lock)?|WORKSPACE(\.bazel)?|pom\.xml|build\.gradle(\.kts)?|Package\.swift|flake\.(nix|lock))$'

for flag in first_run rewritten fork_has_merges fork_behind_recorded_head; do
	if ! jq -e --arg f "$flag" '.[$f] == false' "$range" >/dev/null; then
		hit range "$flag"
	fi
done
while IFS= read -r t; do
	if [[ -n $t ]]; then hit tags "$t"; fi
done < <(jq -r '(.moved_tags[]? | "moved \(.name)"), (.deleted_tags[]? | "deleted \(.)")' "$range")

new=$(jq -r .upstream_new "$range")
base=$(jq -r '.review_base // empty' "$range")

if [[ -z $base ]]; then
	hit range "no review base (unrelated histories)"
else
	while IFS=$'\t' read -r added _ path; do
		if [[ $added == - ]]; then hit binary "$path"; fi
	done < <(G diff --no-renames --numstat "$base" "$new")

	while IFS= read -r path; do
		if [[ $path =~ $ci_re ]]; then hit ci-or-install "$path"; fi
		if [[ $path =~ $manifest_re ]]; then hit manifest "$path"; fi
	done < <(G diff --no-renames --name-only "$base" "$new")

	# Added lines as "path<TAB>text".
	added=$(G diff --no-renames --no-ext-diff --unified=0 "$base" "$new" |
		awk '/^\+\+\+ /{f = substr($0, 7); next} /^\+/{print f "\t" substr($0, 2)}')

	while IFS= read -r path; do
		if [[ -n $path ]]; then hit blob "$path"; fi
	done < <(grep -E $'\t.*[A-Za-z0-9+/]{80,}' <<<"$added" | cut -f1 | sort -u || true)

	while IFS= read -r host; do
		if [[ -n $host ]] && ! G grep -q -F -e "$host" "$base" -- 2>/dev/null; then
			hit url "$host"
		fi
	done < <({
		grep -oE '[A-Za-z][A-Za-z0-9+.-]*://[^/[:space:]"'"'"'<>()`]+' <<<"$added" |
			sed -E 's#^[^:]*://##; s#^[^@]*@##; s#:[0-9]*$##' || true
		grep -oE '[A-Za-z0-9._-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}:' <<<"$added" |
			sed -E 's#^[^@]*@##; s#:$##' || true
	} | sort -u)
fi

patch_ids() {
	local c
	for c in "$@"; do
		G show --no-ext-diff "$c" | git patch-id --stable | cut -d' ' -f1
	done
}

if ! G merge-base --is-ancestor "$new" "$cand" 2>/dev/null; then
	hit stack "candidate $cand is not based on upstream_new $new"
else
	if [[ -n $(G rev-list --merges "$new..$cand") ]]; then hit stack "candidate has merge commits"; fi
	# shellcheck disable=SC2046 # one sha per word
	want=$(patch_ids $(jq -r '.user_commits[]' "$range"))
	# shellcheck disable=SC2046
	got=$(patch_ids $(G rev-list --reverse "$new..$cand"))
	if [[ $want != "$got" ]]; then hit stack "candidate patches differ from the fork's own commits"; fi
fi

[[ $hits -eq 0 ]]
