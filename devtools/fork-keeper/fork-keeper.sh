#!/usr/bin/env bash
# fork-keeper.sh - Scheduler side of fork-keeper (jaeyeom/experimental#598).
#
# Usage: fork-keeper.sh [<fork-name>...]
#
# For each configured fork (default: all): fetch, pre-check, run the
# reviewing agent with a fresh context, push `sync/<date>`, open or refresh
# the PR, and promote the fork's default branch only when the verdict
# validates as clean and tripwires.sh finds nothing. The agent half is the
# fork-keeper plugin in jaeyeom/claude-toolbox; its
# skills/fork-keeper/references/verdict-schema.md is the contract.
#
# Environment:
#   FORK_KEEPER_CONFIG   fork list (default ~/.config/fork-keeper/forks.json)
#   FORK_KEEPER_CACHE    scratch repos and agent output
#                        (default ~/.cache/fork-keeper); safe to delete
#   FORK_KEEPER_SCRIPTS  directory holding review-range.sh and
#                        validate-verdict.sh (default: the claude-toolbox
#                        checkout at ~/.cache/claude-toolbox)
#   FORK_KEEPER_DATE     date used in branch and tag names (default: UTC today)
#
# Config:
#   {"forks": [{
#     "name": "emacs-herdr",                       # cache key, log prefix
#     "repo": "jaeyeom/emacs-herdr",               # for gh -R
#     "fork_url": "https://github.com/...",        # optional; from repo
#     "upstream_url": "https://github.com/baongoc124/emacs-herdr.git",
#     "branch": "main",                            # optional; default main
#     "agent": ["claude", "-p", "/fork-keeper:fork-keeper sync --out {out} -b {branch}"]
#   }]}
#   In "agent", {out} and {branch} are replaced; FORK_KEEPER_OUT is also set.
#
# The agent never gets write access through this script: it runs in a
# throwaway clone with no remotes, without gh or push credentials in its
# environment, and its only outputs are the local branch
# fork-keeper/candidate plus $OUT/verdict.json and $OUT/report.md. This
# process does every push and gh call. The environment scrub is not a
# sandbox: run the agent command under an OS or Claude sandbox as well.
#
# State lives in the fork, not on this machine:
#   refs/fork-keeper/upstream-head       last promoted upstream head
#   refs/fork-keeper/upstream-tags/*     upstream tags as of that promotion
#   refs/fork-keeper/sync-base/<branch>  upstream head a sync PR was built on
#   refs/fork-keeper/sync-tags/<branch>/* upstream tags at that time

set -euo pipefail

config=${FORK_KEEPER_CONFIG:-$HOME/.config/fork-keeper/forks.json}
cache=${FORK_KEEPER_CACHE:-$HOME/.cache/fork-keeper}
scripts=${FORK_KEEPER_SCRIPTS:-$HOME/.cache/claude-toolbox/plugins/fork-keeper/skills/fork-keeper/scripts}
today=${FORK_KEEPER_DATE:-$(date -u +%F)}
self="${BASH_SOURCE[0]}"
here="$(cd "$(dirname "$self")" && pwd)"

APPROVED="fork-keeper:approved"
NEEDS_REVIEW=needs-review

name=-
log() { printf 'fork-keeper[%s]: %s\n' "$name" "$*"; }
die() {
	log "error: $*" >&2
	exit 1
}

for tool in git jq gh; do
	command -v "$tool" >/dev/null 2>&1 || die "$tool is required"
done
[[ -f $config ]] || die "no config at $config"
for s in "$scripts/review-range.sh" "$scripts/validate-verdict.sh" "$here/tripwires.sh"; do
	[[ -f $s ]] || die "missing $s"
done

# More than one fork: one child process each, so a failure in one fork
# neither stops the others nor leaks state between them.
if [[ $# -ne 1 ]]; then
	if [[ $# -eq 0 ]]; then
		names=$(jq -r '.forks[].name' "$config")
	else
		names=$(printf '%s\n' "$@")
	fi
	rc=0
	for n in $names; do
		bash "$self" "$n" || rc=1
	done
	exit $rc
fi

name=$1
fork=$(jq -ce --arg n "$name" '.forks[] | select(.name == $n)' "$config") ||
	die "no fork named $name in $config"
field() { jq -r "$1" <<<"$fork"; }
repo=$(field .repo)
branch=$(field '.branch // "main"')
upstream_url=$(field .upstream_url)
fork_url=$(field '.fork_url // "https://github.com/\(.repo).git"')
agent=()
while IFS= read -r a; do agent+=("$a"); done < <(
	field '(.agent // ["claude", "-p", "/fork-keeper:fork-keeper sync --out {out} -b {branch}"])[]'
)

mkdir -p "$cache/no-hooks"
git_dir="$cache/$name.git"
work="$cache/$name.work"
out="$cache/$name.out"

# Private copies of every check, taken before any agent runs.
priv=$(mktemp -d "${TMPDIR:-/tmp}/fork-keeper.XXXXXX")
trap 'rm -rf "$priv"' EXIT
cp "$scripts/review-range.sh" "$scripts/validate-verdict.sh" "$here/tripwires.sh" "$priv/"
range="$priv/range.json"

# The scheduler's own repository. Hooks are off: nothing from the fork or
# upstream may run here.
G() { git --git-dir="$git_dir" "$@"; }
[[ -d $git_dir ]] || git init -q --bare "$git_dir"
G config core.hooksPath "$cache/no-hooks"

G fetch -q --prune "$fork_url" '+refs/heads/*:refs/remotes/origin/*' '+refs/fork-keeper/*:refs/fork-keeper/*' ||
	die "cannot fetch the fork $fork_url"
G fetch -q --no-tags --prune "$upstream_url" '+refs/heads/*:refs/remotes/upstream/*' '+refs/tags/*:refs/upstream-tags/*' ||
	die "cannot fetch upstream $upstream_url"

# Computed here, before the agent, and never taken from the agent.
GIT_DIR="$git_dir" bash "$priv/review-range.sh" -u upstream -b "$branch" >"$range" ||
	die "review-range.sh failed"
if [[ $(jq -r .up_to_date "$range") == true ]]; then
	log "up-to-date: upstream is already in $branch"
	exit 0
fi
upstream_new=$(jq -r .upstream_new "$range")

# promote <new-head> <upstream-head> <tags-prefix>
promote() {
	local head=$1 up=$2 tags=$3 old
	old=$(G rev-parse "refs/remotes/origin/$branch")
	# The rollback tag and the default branch land together or not at all.
	G push -q --atomic --force-with-lease="refs/heads/$branch:$old" "$fork_url" \
		"$old:refs/tags/pre-sync/$today" "$head:refs/heads/$branch" ||
		die "force-push of $branch failed; fork-keeper refs left unchanged"
	# Only after the default branch moved: otherwise the next run would skip
	# reviewing these commits.
	G push -q --prune "$fork_url" "+$up:refs/fork-keeper/upstream-head" \
		"+$tags/*:refs/fork-keeper/upstream-tags/*" ||
		die "promoted $branch but could not record refs/fork-keeper/upstream-head"
}

has_label() { jq -e --arg l "$1" 'any(.labels[]; .name == $l)' <<<"$pr" >/dev/null; }

pr=$(gh pr list -R "$repo" --state open --json number,headRefName,headRefOid,labels,url) ||
	die "gh pr list failed"
pr=$(jq -c '[.[] | select(.headRefName | startswith("sync/"))] | sort_by(.number) | last // empty' <<<"$pr")
sync_branch="sync/$today"
sticky=false
pr_url=
# at: the sync branch, followed by its PR's URL once there is one.
at() { printf '%s%s' "$sync_branch" "${pr_url:+ $pr_url}"; }
if [[ -n $pr ]]; then
	sync_branch=$(jq -r .headRefName <<<"$pr")
	pr_url=$(jq -r '.url // empty' <<<"$pr")
	pr_head=$(jq -r .headRefOid <<<"$pr")
	fetched=$(G rev-parse -q --verify "refs/remotes/origin/$sync_branch^{commit}" || true)
	if has_label "$APPROVED"; then
		if [[ -n $fetched && $pr_head == "$fetched" ]]; then
			base=$(G rev-parse -q --verify "refs/fork-keeper/sync-base/$sync_branch^{commit}") ||
				die "approved $sync_branch has no refs/fork-keeper/sync-base/$sync_branch"
			promote "$pr_head" "$base" "refs/fork-keeper/sync-tags/$sync_branch"
			log "promoted approved $(at) ($pr_head) to $branch"
			exit 0
		fi
		log "ignoring $APPROVED: PR head $pr_head is not the fetched $sync_branch"
	fi
	if has_label "$NEEDS_REVIEW"; then
		sticky=true
		if [[ -n $fetched ]] && G merge-base --is-ancestor "$upstream_new" "$fetched"; then
			log "waiting for review: $(at) already has upstream $upstream_new"
			exit 0
		fi
	fi
fi

# Fresh agent checkout: only the refs the skill reads, no remotes, no hooks.
rm -rf "$work" "$out"
mkdir -p "$out" "$cache/agent-gh"
git init -q "$work"
git -C "$work" config core.hooksPath "$cache/no-hooks"
git -C "$work" fetch -q "$git_dir" '+refs/remotes/*:refs/remotes/*' \
	'+refs/upstream-tags/*:refs/upstream-tags/*' '+refs/fork-keeper/*:refs/fork-keeper/*'
git -C "$work" checkout -q --detach "refs/remotes/origin/$branch"
agent_gitconfig="$cache/agent-gitconfig"
{
	printf '[user]\n\tname = %s\n\temail = %s\n' \
		"$(git config user.name || echo fork-keeper)" \
		"$(git config user.email || echo fork-keeper@localhost)"
	printf '[core]\n\thooksPath = %s\n' "$cache/no-hooks"
} >"$agent_gitconfig"

args=()
for a in "${agent[@]}"; do
	a=${a//\{out\}/$out}
	args+=("${a//\{branch\}/$branch}")
done
log "running agent: ${args[0]}"
agent_rc=0
(
	cd "$work"
	exec env -u GH_TOKEN -u GITHUB_TOKEN -u GH_ENTERPRISE_TOKEN -u GITHUB_ENTERPRISE_TOKEN \
		-u SSH_AUTH_SOCK -u GIT_ASKPASS -u SSH_ASKPASS \
		GH_CONFIG_DIR="$cache/agent-gh" GIT_CONFIG_GLOBAL="$agent_gitconfig" \
		GIT_CONFIG_NOSYSTEM=1 GIT_TERMINAL_PROMPT=0 FORK_KEEPER_OUT="$out" \
		"${args[@]}"
) >"$cache/$name.agent.log" 2>&1 || agent_rc=$?
[[ $agent_rc -eq 0 ]] || log "agent exited $agent_rc (see $cache/$name.agent.log)"

candidate=$(git -C "$work" rev-parse -q --verify 'refs/heads/fork-keeper/candidate^{commit}') ||
	die "agent left no fork-keeper/candidate branch"
G fetch -q "$work" "+refs/heads/fork-keeper/candidate:refs/fork-keeper-run/candidate" ||
	die "cannot import the candidate"
[[ $(G rev-parse refs/fork-keeper-run/candidate) == "$candidate" ]] ||
	die "fork-keeper/candidate moved while importing it"

# Every promotion check reads only the scheduler's repo and copies.
valid=true
vmsg=$(bash "$priv/validate-verdict.sh" "$out/verdict.json" --candidate-head "$candidate" --range "$range" 2>&1) ||
	valid=false
verdict=$(jq -r '.verdict // "none"' "$out/verdict.json" 2>/dev/null || echo none)
tw_rc=0
tw=$(bash "$priv/tripwires.sh" "$git_dir" "$range" "$candidate") || tw_rc=$?

body="$priv/body.md"
{
	if [[ -s $out/report.md ]]; then cat "$out/report.md"; else echo "The agent wrote no report.md."; fi
	echo
	echo "## Scheduler checks"
	echo
	echo "- Verdict: \`$verdict\`"
	if [[ $valid == true ]]; then echo "- validate-verdict.sh: passed"; else
		echo "- validate-verdict.sh: failed"
		echo "  - ${vmsg//$'\n'/$'\n'  - }"
	fi
	if [[ $tw_rc -eq 0 ]]; then echo "- Tripwires: none"; else
		echo "- Tripwires:"
		echo "  - ${tw//$'\n'/$'\n'  - }"
	fi
	if [[ $sticky == true ]]; then echo "- Already \`$NEEDS_REVIEW\`: waits for \`$APPROVED\`."; fi
} >"$body"

# A head change voids any earlier approval.
if [[ -n $pr ]] && has_label "$APPROVED"; then
	gh pr edit "$sync_branch" -R "$repo" --remove-label "$APPROVED" >/dev/null ||
		die "cannot remove $APPROVED before refreshing $sync_branch"
fi
G push -q --prune "$fork_url" "+$candidate:refs/heads/$sync_branch" \
	"+$upstream_new:refs/fork-keeper/sync-base/$sync_branch" \
	"+refs/upstream-tags/*:refs/fork-keeper/sync-tags/$sync_branch/*" ||
	die "cannot push $sync_branch"
if [[ -n $pr ]]; then
	gh pr edit "$sync_branch" -R "$repo" --body-file "$body" >/dev/null ||
		die "cannot refresh the PR for $sync_branch"
	log "refreshed ${pr_url:-$sync_branch}"
else
	pr_url=$(gh pr create -R "$repo" --base "$branch" --head "$sync_branch" \
		--title "fork-keeper: sync upstream $today" --body-file "$body") ||
		die "cannot open a PR for $sync_branch"
	log "opened ${pr_url:-$sync_branch}"
fi

if [[ $valid == true && $verdict == clean && $tw_rc -eq 0 && $sticky == false ]]; then
	promote "$candidate" "$upstream_new" refs/upstream-tags
	log "promoted $(at) ($candidate) to $branch"
	exit 0
fi
gh pr edit "$sync_branch" -R "$repo" --add-label "$NEEDS_REVIEW" >/dev/null ||
	die "cannot label $sync_branch $NEEDS_REVIEW"
log "needs-review: $(at) (verdict $verdict, validator $valid, tripwires rc $tw_rc, sticky $sticky)"
