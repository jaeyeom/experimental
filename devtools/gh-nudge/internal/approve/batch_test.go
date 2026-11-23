package approve

import (
	"bytes"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestBatchLoadChunkingAndWarmCacheZeroPerPRCalls(t *testing.T) {
	var prs []github.ApprovalPullRequest
	for i := 1; i <= 7; i++ {
		pr := prNumber(i)
		pr.BaseRef = "main"
		pr.Files = []models.File{{Path: "deploy/values.yaml"}}
		pr.ChangedFiles = 1
		prs = append(prs, pr)
	}
	fake := newFake(prs...)
	fake.codeowners = mineCodeowners
	fake.viewerTeams = map[string][]string{"acme": {"core"}}
	fake.teams = map[string][]string{"acme/core": {"me", "bob"}}
	fake.patches = make(map[int][]github.PullFile, 7)
	for i := 1; i <= 7; i++ {
		fake.patches[i] = []github.PullFile{patchFile("deploy/values.yaml", yamlCommentPatch)}
	}

	cache := &PRCache{Dir: t.TempDir(), TTL: 7 * 24 * time.Hour, Now: fixedNow}
	runPass := func() runResult {
		var out, logBuf bytes.Buffer
		runner := &Runner{
			Client: fake,
			Config: config.ApproveConfig{
				Body:                "LGTM",
				SkipAlreadyApproved: true,
				Rules:               []config.ApproveRule{mineRule},
			},
			Out:       &out,
			Log:       &logBuf,
			Now:       fixedNow,
			PRCache:   cache,
			BatchSize: 3,
		}
		err := runner.Run(Options{Explain: true})
		return runResult{out: out.String(), log: decodeLog(t, &logBuf), err: err}
	}

	// Cold pass populates PRCache.
	res1 := runPass()
	if res1.err != nil {
		t.Fatal(res1.err)
	}
	if fake.batchReads != 3 {
		t.Fatalf("cold batchReads = %d, want 3 (ceil(7/3))", fake.batchReads)
	}

	// Warm pass: reset counters and verify 0 per-PR loads, 1 viewerTeams, 1 codeowners.
	fake.batchReads = 0
	fake.batchSizes = nil
	fake.loadReads = 0
	fake.patchReads = 0
	fake.codeownersReads = 0
	fake.viewerTeamReads = 0
	fake.teamCountReads = 0
	fake.teamListReads = 0

	res2 := runPass()
	if res2.err != nil {
		t.Fatal(res2.err)
	}
	if fake.batchReads != 3 {
		t.Fatalf("warm batchReads = %d, want 3 (ceil(7/3))", fake.batchReads)
	}
	if got := fmt.Sprint(fake.batchSizes); got != "[3 3 1]" {
		t.Fatalf("warm batchSizes = %s, want [3 3 1]", got)
	}
	if fake.loadReads != 0 || fake.patchReads != 0 {
		t.Fatalf("warm per-PR reads: load=%d patch=%d, want 0", fake.loadReads, fake.patchReads)
	}
	if fake.viewerTeamReads != 1 || fake.teamCountReads != 0 || fake.teamListReads != 0 {
		t.Fatalf("warm team reads: viewer=%d count=%d list=%d, want 1/0/0",
			fake.viewerTeamReads, fake.teamCountReads, fake.teamListReads)
	}
	if fake.codeownersReads != 1 {
		t.Fatalf("warm codeownersReads = %d, want 1", fake.codeownersReads)
	}
	for _, want := range []string{"(batch 1/3)", "(batch 2/3)", "(batch 3/3)", "GraphQL calls:"} {
		if !strings.Contains(res2.out, want) {
			t.Fatalf("explain output missing %q:\n%s", want, res2.out)
		}
	}
}

func TestBatchPartialFailureReportsErrorAndEvaluatesOthers(t *testing.T) {
	prs := []github.ApprovalPullRequest{prNumber(1), prNumber(2), prNumber(3)}
	fake := newFake(prs...)
	fake.loadErr = map[int]error{2: errors.New("load pull request acme/widgets#2: not found")}

	res := run(t, fake, []config.ApproveRule{dashboardRule}, Options{})
	if res.err == nil {
		t.Fatal("expected error for partial failure, got nil")
	}
	if len(res.log) != 3 {
		t.Fatalf("log len = %d, want 3", len(res.log))
	}
	if res.log[0].Action != ActionWouldApprove {
		t.Fatalf("PR 1 action = %s, want would_approve", res.log[0].Action)
	}
	if res.log[1].Action != ActionError || !strings.Contains(res.log[1].Reason, "not found") {
		t.Fatalf("PR 2 = %+v, want error with not found", res.log[1])
	}
	if res.log[2].Action != ActionWouldApprove {
		t.Fatalf("PR 3 action = %s, want would_approve", res.log[2].Action)
	}
}

func TestWarmCacheCodeownersFetchedOncePerRepoAndBaseBranch(t *testing.T) {
	var prs []github.ApprovalPullRequest
	for i, base := range []string{"main", "main", "release", "release"} {
		pr := prNumber(i + 1)
		pr.BaseRef = base
		pr.Files = []models.File{{Path: "deploy/values.yaml"}}
		pr.ChangedFiles = 1
		prs = append(prs, pr)
	}
	fake := newFake(prs...)
	fake.codeowners = mineCodeowners
	fake.viewerTeams = map[string][]string{"acme": {"core"}}
	fake.teams = map[string][]string{"acme/core": {"me", "bob"}}
	fake.patches = make(map[int][]github.PullFile, 4)
	for i := 1; i <= 4; i++ {
		fake.patches[i] = []github.PullFile{patchFile("deploy/values.yaml", yamlCommentPatch)}
	}

	cache := &PRCache{Dir: t.TempDir(), TTL: 7 * 24 * time.Hour, Now: fixedNow}
	if res := runWithCache(t, fake, []config.ApproveRule{mineRule}, cache, Options{}); res.err != nil {
		t.Fatal(res.err)
	}
	fake.codeownersReads = 0
	fake.viewerTeamReads = 0
	fake.loadReads = 0
	res := runWithCache(t, fake, []config.ApproveRule{mineRule}, cache, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if fake.codeownersReads != 2 {
		t.Fatalf("warm codeownersReads = %d, want 2 (once per base branch)", fake.codeownersReads)
	}
	if fake.viewerTeamReads != 1 {
		t.Fatalf("warm viewerTeamReads = %d, want 1", fake.viewerTeamReads)
	}
	if fake.loadReads != 0 {
		t.Fatalf("warm loadReads = %d, want 0", fake.loadReads)
	}
}

func TestBatchPreservesCandidateOrderAndOutcomes(t *testing.T) {
	pr1 := prNumber(1)
	pr2 := prNumber(2)
	pr2.Draft = true // cheap skip
	pr3 := prNumber(3)
	pr3.Files = []models.File{{Path: "other/a.go"}} // manual
	pr4 := prNumber(4)

	fake := newFake(pr1, pr2, pr3, pr4)
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: fake,
		Config: config.ApproveConfig{
			Body:                "LGTM",
			SkipAlreadyApproved: true,
			Rules:               []config.ApproveRule{dashboardRule},
		},
		Out:       &out,
		Log:       &logBuf,
		Now:       fixedNow,
		BatchSize: 2,
	}
	if err := runner.Run(Options{}); err != nil {
		t.Fatal(err)
	}
	entries := decodeLog(t, &logBuf)
	wantOrder := []string{"acme/widgets#1", "acme/widgets#2", "acme/widgets#3", "acme/widgets#4"}
	wantActions := []string{ActionWouldApprove, ActionSkipped, ActionManual, ActionWouldApprove}
	if len(entries) != len(wantOrder) {
		t.Fatalf("entries = %d, want %d", len(entries), len(wantOrder))
	}
	for i := range entries {
		if entries[i].PR != wantOrder[i] || entries[i].Action != wantActions[i] {
			t.Fatalf("entry[%d] = %s (%s), want %s (%s)", i, entries[i].PR, entries[i].Action, wantOrder[i], wantActions[i])
		}
	}
}
