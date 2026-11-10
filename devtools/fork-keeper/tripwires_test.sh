#!/usr/bin/env bash
# Tests for tripwires.sh: the deterministic diff checks that must find nothing
# before fork-keeper promotes a clean verdict.
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=devtools/fork-keeper/testlib.sh
. "$DIR/testlib.sh"
TW="$DIR/tripwires.sh"
RR="$DIR/testdata/claude-toolbox/review-range.sh"

# prep: fetch the fixture the way fork-keeper.sh does, write range.json, and
# rebuild the user's stack on upstream as the agent would. Sets W, RANGE,
# CAND.
prep() {
	W="$FX/w"
	rm -rf "$W"
	git init -q "$W"
	g "$W" fetch -q "$FX/origin.git" '+refs/heads/*:refs/remotes/origin/*' '+refs/fork-keeper/*:refs/fork-keeper/*'
	g "$W" fetch -q --no-tags "$FX/upstream.git" '+refs/heads/*:refs/remotes/upstream/*' '+refs/tags/*:refs/upstream-tags/*'
	RANGE="$FX/range.json"
	(cd "$W" && bash "$RR") >"$RANGE"
	g "$W" checkout -q -B cand "$(jq -r .upstream_new "$RANGE")"
	local c
	for c in $(jq -r '.user_commits[]' "$RANGE"); do
		g "$W" cherry-pick "$c" >/dev/null
	done
	CAND="$(g "$W" rev-parse HEAD)"
}

# tw: run tripwires.sh; sets OUT and RC.
tw() {
	RC=0
	OUT="$(bash "$TW" "$W/.git" "$RANGE" "${1:-$CAND}" 2>&1)" || RC=$?
}

hit() { [[ $RC -eq 1 ]] && grep -q "$1" <<<"$OUT"; }
quiet() { [[ $RC -eq 0 && -z $OUT ]]; }

# Harmless upstream change with a recorded head: nothing to report.
CUR=harmless
mk_fixture
u0="$(uref refs/heads/main)"
record_head "$u0"
fcommit mine.txt "my patch" >/dev/null
ucommit README "more docs" >/dev/null
prep
tw
check "harmless change must be quiet: rc=$RC $OUT" quiet

# A host that appears in the old tree is not new.
CUR=known-host
mk_fixture
ucommit README "see https://example.org/docs" >/dev/null
fork_at_upstream
record_head "$(uref refs/heads/main)"
ucommit notes.txt "mirror at https://example.org/other" >/dev/null
prep
tw
check "known host must be quiet: rc=$RC $OUT" quiet

CUR=new-host
mk_fixture
record_head "$(uref refs/heads/main)"
ucommit lisp/x.el '(url-retrieve "https://evil.example.net:8443/p")' >/dev/null
prep
tw
check "new host must hit: rc=$RC $OUT" hit "url.*evil.example.net"

CUR=scp-host
mk_fixture
record_head "$(uref refs/heads/main)"
ucommit notes.txt "clone git@evil.example.net:x/y.git" >/dev/null
prep
tw
check "scp-style host must hit: rc=$RC $OUT" hit "url.*evil.example.net"

CUR=blob
mk_fixture
record_head "$(uref refs/heads/main)"
ucommit data.txt "$(printf 'QUJD%.0s' $(seq 1 30))" >/dev/null
prep
tw
check "base64-like blob must hit: rc=$RC $OUT" hit "blob.*data.txt"

CUR=binary
mk_fixture
record_head "$(uref refs/heads/main)"
printf '\000\001\002' >"$FX/useed/tool.bin"
g "$FX/useed" add tool.bin
g "$FX/useed" commit -q -m bin
g "$FX/useed" push -q origin HEAD:main
prep
tw
check "binary must hit: rc=$RC $OUT" hit "binary.*tool.bin"

for f in .github/workflows/ci.yml Makefile install.sh scripts/setup-env.sh .githooks/pre-commit; do
	CUR="ci:$f"
	mk_fixture
	record_head "$(uref refs/heads/main)"
	ucommit "$f" "echo hi" >/dev/null
	prep
	tw
	check "CI/install script $f must hit: rc=$RC $OUT" hit "ci-or-install.*$f"
done

for f in package.json go.mod Cargo.lock requirements-dev.txt herdr-pkg.el Eask; do
	CUR="manifest:$f"
	mk_fixture
	record_head "$(uref refs/heads/main)"
	ucommit "$f" "dep" >/dev/null
	prep
	tw
	check "dependency manifest $f must hit: rc=$RC $OUT" hit "manifest.*$f"
done

# Range flags are re-checked here, independent of the validator.
CUR=first-run
mk_fixture
ucommit README "docs" >/dev/null
prep
tw
check "first run must hit: rc=$RC $OUT" hit "range.*first_run"

CUR=rewritten
mk_fixture
ucommit README "one" >/dev/null
fork_at_upstream
record_head "$(uref refs/heads/main)"
g "$FX/useed" reset -q --hard HEAD~1
ucommit README "two" >/dev/null
prep
tw
check "rewritten upstream must hit: rc=$RC $OUT" hit "range.*rewritten"

CUR=moved-tag
mk_fixture
u0="$(uref refs/heads/main)"
record_head "$u0"
git --git-dir="$FX/origin.git" update-ref refs/fork-keeper/upstream-tags/v1 "$u0"
u1="$(ucommit README "docs")"
git --git-dir="$FX/upstream.git" update-ref refs/tags/v1 "$u1"
prep
tw
check "moved tag must hit: rc=$RC $OUT" hit "tags.*v1"

# The candidate must be exactly the user's patches on top of upstream_new.
CUR=patch-changed
mk_fixture
record_head "$(uref refs/heads/main)"
fcommit mine.txt "my patch" >/dev/null
ucommit README "docs" >/dev/null
prep
echo "sneaky" >>"$W/mine.txt"
g "$W" commit -q -a --amend --no-edit
tw "$(g "$W" rev-parse HEAD)"
check "changed user patch must hit: rc=$RC $OUT" hit "stack.*differ"

CUR=extra-commit
prep
echo extra >"$W/extra.txt"
g "$W" add extra.txt
g "$W" commit -q -m extra
tw "$(g "$W" rev-parse HEAD)"
check "extra candidate commit must hit: rc=$RC $OUT" hit "stack.*differ"

CUR=not-on-upstream
tw "$(jq -r .upstream_old "$RANGE")"
check "candidate not based on upstream_new must hit: rc=$RC $OUT" hit "stack.*not based"

finish
