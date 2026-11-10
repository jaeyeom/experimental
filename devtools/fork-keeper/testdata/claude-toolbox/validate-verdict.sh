#!/usr/bin/env bash
# validate-verdict.sh - Check a fork-keeper verdict.json against the contract.
#
# Usage: validate-verdict.sh <verdict.json> [--candidate-head <sha>] [--range <range.json>]
#
#   --candidate-head  require verdict.candidate_head to equal <sha>
#   --range           require verdict.range to equal the review-range.sh output
#                     the caller computed itself (don't trust the agent's copy)
#
# Exit 0: the verdict is well-formed (and, if "clean", truly eligible for
#         promotion). Exit 1: anything else, with reasons on stderr.
#
# The promotion script (jaeyeom/experimental#598) must treat any non-zero exit
# as "never promote". See ../references/verdict-schema.md.

set -uo pipefail

fail() {
	echo "validate-verdict: $*" >&2
	exit 1
}

[[ $# -ge 1 ]] || fail "usage: validate-verdict.sh <verdict.json> [--candidate-head <sha>] [--range <range.json>]"
file=$1
shift
want_head=""
range_file=""
while [[ $# -gt 0 ]]; do
	case $1 in
	--candidate-head)
		[[ $# -ge 2 ]] || fail "--candidate-head needs a value"
		want_head=$2
		[[ $want_head =~ ^[0-9a-f]{40}$ ]] || fail "--candidate-head must be a 40-hex sha, got '$want_head'"
		shift 2
		;;
	--range)
		[[ $# -ge 2 ]] || fail "--range needs a file"
		range_file=$2
		shift 2
		;;
	*) fail "unknown argument: $1" ;;
	esac
done

command -v jq >/dev/null 2>&1 || fail "jq is required"
[[ -f $file ]] || fail "no such file: $file"
jq -se 'length == 1 and (.[0] | type) == "object"' "$file" >/dev/null 2>&1 ||
	fail "not exactly one JSON object: $file"
range_json=null
if [[ -n $range_file ]]; then
	[[ -f $range_file ]] || fail "no such range file: $range_file"
	jq -se 'length == 1 and (.[0] | type) == "object"' "$range_file" >/dev/null 2>&1 ||
		fail "range file is not exactly one JSON object: $range_file"
	range_json=$(jq -c . "$range_file")
fi

# Each rule yields an error string when violated; the list must be empty.
errors=$(jq -r --arg want "$want_head" --argjson want_range "$range_json" '
	def sha: type == "string" and test("^[0-9a-f]{40}$");
	def cats: ["secret-leak","destructive","concealment","supply-chain",
		"repo-signal","agent-manipulation","uncertain"];
	. as $v | [
		(if $v.schema != 1 then "schema must be 1" else empty end),
		(if ($v.verdict | IN("clean","needs-review")) | not
			then "verdict must be \"clean\" or \"needs-review\"" else empty end),
		(if ($v.candidate_head | sha) | not then "candidate_head must be a 40-hex sha" else empty end),
		(if ($v.range | type) != "object" then "range must be an object" else empty end),
		(if ($v.range.upstream_old | sha) | not then "range.upstream_old must be a 40-hex sha" else empty end),
		(if ($v.range.upstream_new | sha) | not then "range.upstream_new must be a 40-hex sha" else empty end),
		(if ($v.range.user_commits | type == "array" and all(.[]; sha)) | not
			then "range.user_commits must be an array of 40-hex shas" else empty end),
		(if ($v.range.moved_tags | type) != "array" then "range.moved_tags must be an array" else empty end),
		(if ($v.range.deleted_tags | type) != "array" then "range.deleted_tags must be an array" else empty end),
		(if $want_range != null and $v.range != $want_range
			then "range does not match the independently computed --range" else empty end),
		(if ($v.findings | type) != "array" then "findings must be an array" else empty end),
		(if ($v.conflicts | type) != "array" then "conflicts must be an array" else empty end),
		(($v.findings // [])[]? | select((.category | IN(cats[])) | not)
			| "unknown finding category: \(.category)"),
		(if $want != "" and $v.candidate_head != $want
			then "candidate_head \($v.candidate_head) != expected \($want)" else empty end),
		(if $v.verdict == "clean" then
			(if ($v.findings | length) > 0 then "clean verdict has findings" else empty end),
			(if any(($v.conflicts // [])[]; .confident != true)
				then "clean verdict has an unconfident conflict" else empty end),
			(if $v.range.rewritten != false then "clean verdict but upstream history was rewritten" else empty end),
			(if $v.range.fork_has_merges != false then "clean verdict but fork has merge commits" else empty end),
			(if $v.range.first_run != false then "clean verdict on a first review" else empty end),
			(if $v.range.fork_behind_recorded_head != false
				then "clean verdict but fork does not contain the recorded upstream head" else empty end),
			(if $v.range.moved_tags != [] then "clean verdict but upstream tags moved" else empty end),
			(if $v.range.deleted_tags != [] then "clean verdict but upstream tags were deleted" else empty end)
		else empty end)
	] | .[]' "$file" 2>&1) || fail "could not evaluate $file: $errors"

if [[ -n $errors ]]; then
	while IFS= read -r e; do echo "validate-verdict: $e" >&2; done <<<"$errors"
	exit 1
fi
exit 0
