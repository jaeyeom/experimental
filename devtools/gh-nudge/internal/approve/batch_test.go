package approve

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"strings"
	"sync"
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

func TestBatchRetrySucceedsAfterTransient504(t *testing.T) {
	prs := []github.ApprovalPullRequest{prNumber(1), prNumber(2), prNumber(3), prNumber(4)}
	fake := newFake(prs...)
	attempts := 0
	fake.batchErrFn = func(refs []github.PullRef) error {
		attempts++
		if attempts == 1 {
			return errors.New("load pull requests: gh api graphql: failed to execute gh: exit status 1: gh: HTTP 504")
		}
		return nil
	}

	var sleeps []time.Duration
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: fake,
		Config: config.ApproveConfig{
			Body:                "LGTM",
			SkipAlreadyApproved: true,
			Rules:               []config.ApproveRule{dashboardRule},
		},
		Out:         &out,
		Log:         &logBuf,
		Now:         fixedNow,
		BatchSize:   4,
		BackoffBase: time.Millisecond,
		Sleep: func(d time.Duration) {
			sleeps = append(sleeps, d)
		},
	}
	if err := runner.Run(Options{Explain: true}); err != nil {
		t.Fatalf("expected retry to succeed, got err: %v", err)
	}
	if fake.batchReads != 2 {
		t.Fatalf("batchReads = %d, want 2", fake.batchReads)
	}
	if len(sleeps) != 1 {
		t.Fatalf("sleeps = %v, want 1 pause", sleeps)
	}
	entries := decodeLog(t, &logBuf)
	if len(entries) != 4 {
		t.Fatalf("entries = %d, want 4", len(entries))
	}
	for i, e := range entries {
		if e.Action != ActionWouldApprove {
			t.Fatalf("entry[%d] action = %s, want %s", i, e.Action, ActionWouldApprove)
		}
	}
	if !strings.Contains(out.String(), "Batch retry: chunk size 4, attempt 1, error:") || !strings.Contains(out.String(), "HTTP 504") {
		t.Fatalf("output missing retry log:\n%s", out.String())
	}
}

func TestBatchSplitsChunkToIsolateFailingPR(t *testing.T) {
	prs := []github.ApprovalPullRequest{prNumber(1), prNumber(2), prNumber(3), prNumber(4), prNumber(5)}
	fake := newFake(prs...)
	fake.batchErrFn = func(refs []github.PullRef) error {
		for _, ref := range refs {
			if ref.Number == 3 {
				return errors.New("load pull requests: gh api graphql: exit status 1: gh: HTTP 504")
			}
		}
		return nil
	}

	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: fake,
		Config: config.ApproveConfig{
			Body:                "LGTM",
			SkipAlreadyApproved: true,
			Rules:               []config.ApproveRule{dashboardRule},
		},
		Out:         &out,
		Log:         &logBuf,
		Now:         fixedNow,
		BatchSize:   5,
		BackoffBase: time.Millisecond,
		Sleep:       func(time.Duration) {},
	}
	err := runner.Run(Options{Explain: true})
	if err == nil || !strings.Contains(err.Error(), "1 pull request(s) failed") {
		t.Fatalf("err = %v, want 1 pull request(s) failed", err)
	}
	entries := decodeLog(t, &logBuf)
	if len(entries) != 5 {
		t.Fatalf("entries = %d, want 5", len(entries))
	}
	for i, e := range entries {
		wantAction := ActionWouldApprove
		if i == 2 { // PR #3
			wantAction = ActionError
		}
		if e.Action != wantAction {
			t.Fatalf("PR #%d action = %s, want %s", i+1, e.Action, wantAction)
		}
	}
	for _, want := range []string{"Batch retry: chunk size 5, attempt 1", "Batch split: chunk size 5, attempt 2"} {
		if !strings.Contains(out.String(), want) {
			t.Fatalf("output missing %q:\n%s", want, out.String())
		}
	}
}

func TestBatchNonTransientErrorNotRetriedOrSplit(t *testing.T) {
	prs := []github.ApprovalPullRequest{prNumber(1), prNumber(2), prNumber(3)}
	fake := newFake(prs...)
	fake.batchErr = errors.New("load pull requests: GraphQL: bad query")

	var sleeps []time.Duration
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: fake,
		Config: config.ApproveConfig{
			Body:                "LGTM",
			SkipAlreadyApproved: true,
			Rules:               []config.ApproveRule{dashboardRule},
		},
		Out:         &out,
		Log:         &logBuf,
		Now:         fixedNow,
		BatchSize:   3,
		BackoffBase: time.Millisecond,
		Sleep: func(d time.Duration) {
			sleeps = append(sleeps, d)
		},
	}
	err := runner.Run(Options{Explain: true})
	if err == nil || !strings.Contains(err.Error(), "3 pull request(s) failed") {
		t.Fatalf("err = %v, want 3 pull request(s) failed", err)
	}
	if fake.batchReads != 1 {
		t.Fatalf("batchReads = %d, want 1 (no retry or split)", fake.batchReads)
	}
	if len(sleeps) != 0 {
		t.Fatalf("sleeps = %v, want none", sleeps)
	}
	if strings.Contains(out.String(), "Batch retry:") || strings.Contains(out.String(), "Batch split:") {
		t.Fatalf("unexpected retry/split log for non-transient error:\n%s", out.String())
	}
}

func TestBatchRateLimitErrorBehavesAsToday(t *testing.T) {
	t.Run("retries with backoff and succeeds without splitting", func(t *testing.T) {
		prs := []github.ApprovalPullRequest{prNumber(1), prNumber(2), prNumber(3)}
		fake := newFake(prs...)
		attempts := 0
		fake.batchErrFn = func(refs []github.PullRef) error {
			attempts++
			if attempts <= 2 {
				return errors.New("HTTP 403: You have exceeded a secondary rate limit")
			}
			return nil
		}

		var mu sync.Mutex
		var sleeps []time.Duration
		now := fixedNow()
		var out, logBuf bytes.Buffer
		runner := &Runner{
			Client: fake,
			Config: config.ApproveConfig{
				Body:                "LGTM",
				SkipAlreadyApproved: true,
				Rules:               []config.ApproveRule{dashboardRule},
			},
			Out: &out,
			Log: &logBuf,
			Now: func() time.Time {
				mu.Lock()
				defer mu.Unlock()
				return now
			},
			BatchSize:   3,
			BackoffBase: time.Millisecond,
			Sleep: func(d time.Duration) {
				mu.Lock()
				sleeps = append(sleeps, d)
				now = now.Add(d)
				mu.Unlock()
			},
		}
		if err := runner.Run(Options{Explain: true}); err != nil {
			t.Fatalf("unexpected err: %v", err)
		}
		if fake.batchReads != 3 {
			t.Fatalf("batchReads = %d, want 3", fake.batchReads)
		}
		if got := fmt.Sprint(fake.batchSizes); got != "[3 3 3]" {
			t.Fatalf("batchSizes = %s, want [3 3 3]", got)
		}
		if len(sleeps) != 2 {
			t.Fatalf("sleeps = %v, want 2 backoff sleeps", sleeps)
		}
		if strings.Contains(out.String(), "Batch retry:") || strings.Contains(out.String(), "Batch split:") {
			t.Fatalf("unexpected transient retry/split log for rate limit:\n%s", out.String())
		}
	})

	t.Run("exhausted rate limit does not split chunk", func(t *testing.T) {
		prs := []github.ApprovalPullRequest{prNumber(1), prNumber(2), prNumber(3)}
		fake := newFake(prs...)
		fake.batchErr = errors.New("HTTP 403: You have exceeded a secondary rate limit")

		var out, logBuf bytes.Buffer
		runner := &Runner{
			Client: fake,
			Config: config.ApproveConfig{
				Body:                "LGTM",
				SkipAlreadyApproved: true,
				Rules:               []config.ApproveRule{dashboardRule},
			},
			Out:         &out,
			Log:         &logBuf,
			Now:         fixedNow,
			BatchSize:   3,
			BackoffBase: time.Millisecond,
			Sleep:       func(time.Duration) {},
		}
		err := runner.Run(Options{})
		if err == nil || !strings.Contains(err.Error(), "3 pull request(s) failed") {
			t.Fatalf("err = %v, want 3 pull request(s) failed", err)
		}
		if fake.batchReads != maxRateLimitRetries+1 {
			t.Fatalf("batchReads = %d, want %d", fake.batchReads, maxRateLimitRetries+1)
		}
		for i, sz := range fake.batchSizes {
			if sz != 3 {
				t.Fatalf("batchSizes[%d] = %d, want 3 (no split)", i, sz)
			}
		}
	})
}

func TestBatchAllWouldBeLeftFailedCountsLeftForYouAndUnevaluated(t *testing.T) {
	pr1 := prNumber(1)
	pr1.Files = []models.File{{Path: "infra/main.tf"}} // would be left for you
	pr2 := prNumber(2)
	pr2.Files = []models.File{{Path: "infra/other.tf"}} // would be left for you
	fake := newFake(pr1, pr2)
	fake.batchErr = errors.New("load pull requests: gh api graphql: exit status 1: gh: HTTP 504")

	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: fake,
		Config: config.ApproveConfig{
			Body:                "LGTM",
			SkipAlreadyApproved: true,
			Rules:               []config.ApproveRule{dashboardRule},
		},
		Out:         &out,
		Log:         &logBuf,
		Now:         fixedNow,
		BatchSize:   2,
		BackoffBase: time.Millisecond,
		Sleep:       func(time.Duration) {},
	}
	err := runner.Run(Options{})
	if err == nil || !strings.Contains(err.Error(), "2 pull request(s) failed") {
		t.Fatalf("err = %v, want 2 pull request(s) failed", err)
	}
	wantSummary := "👀 Left for you (2)\n" +
		"  2 unevaluated (rerun to check):\n" +
		"    acme/widgets#1 - unevaluated: load pull requests: gh api graphql: exit status 1: gh: HTTP 504\n" +
		"    acme/widgets#2 - unevaluated: load pull requests: gh api graphql: exit status 1: gh: HTTP 504\n"
	if !strings.Contains(out.String(), wantSummary) {
		t.Fatalf("output missing Left for you (2) and unevaluated lines:\n%s", out.String())
	}
}

func TestBatchOnePRAPIErrorCountedAsManualInSummary(t *testing.T) {
	manual := prNumber(1)
	manual.Files = []models.File{{Path: "infra/main.tf"}}
	failed := prNumber(2)
	approvable := prNumber(3)
	fake := newFake(manual, failed, approvable)
	fake.loadErr = map[int]error{2: errors.New("gh: HTTP 502")}

	res := run(t, fake, []config.ApproveRule{dashboardRule}, Options{Apply: true})
	if res.err == nil || !strings.Contains(res.err.Error(), "1 pull request(s) failed") {
		t.Fatalf("err = %v, want 1 pull request(s) failed", res.err)
	}
	if len(res.fake.approvals) != 1 || res.fake.approvals[0].ref.Number != 3 {
		t.Fatalf("approvals = %+v, want only PR #3", res.fake.approvals)
	}
	if res.log[1].Action != ActionError || !strings.HasPrefix(res.log[1].Reason, "unevaluated: ") || !strings.Contains(res.log[1].Reason, "HTTP 502") {
		t.Fatalf("PR #2 log entry = %+v", res.log[1])
	}
	want := "👀 Left for you (2)\n" +
		"  acme/widgets#1  Tidy dashboard - dashboard: paths_all_match failed (outside globs: infra/main.tf)\n" +
		"  1 unevaluated (rerun to check):\n" +
		"    acme/widgets#2 - unevaluated: gh: HTTP 502\n"
	if !strings.Contains(res.out, want) {
		t.Fatalf("summary missing manual + unevaluated counts:\n%s\nwant substring:\n%s", res.out, want)
	}
}

func TestBatchRetrySucceedsAfterTruncatedJSON(t *testing.T) {
	prs := []github.ApprovalPullRequest{prNumber(1), prNumber(2), prNumber(3), prNumber(4)}
	fake := newFake(prs...)
	attempts := 0
	fake.batchErrFn = func(refs []github.PullRef) error {
		attempts++
		if attempts == 1 {
			return errors.New("load pull requests: gh api graphql: failed to execute gh: exit status 1: unexpected end of JSON input")
		}
		return nil
	}

	var sleeps []time.Duration
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: fake,
		Config: config.ApproveConfig{
			Body:                "LGTM",
			SkipAlreadyApproved: true,
			Rules:               []config.ApproveRule{dashboardRule},
		},
		Out:         &out,
		Log:         &logBuf,
		Now:         fixedNow,
		BatchSize:   4,
		BackoffBase: time.Millisecond,
		Sleep: func(d time.Duration) {
			sleeps = append(sleeps, d)
		},
	}
	if err := runner.Run(Options{Explain: true}); err != nil {
		t.Fatalf("expected retry to succeed, got err: %v", err)
	}
	if fake.batchReads != 2 {
		t.Fatalf("batchReads = %d, want 2", fake.batchReads)
	}
	if len(sleeps) != 1 {
		t.Fatalf("sleeps = %v, want 1 pause", sleeps)
	}
	entries := decodeLog(t, &logBuf)
	if len(entries) != 4 {
		t.Fatalf("entries = %d, want 4", len(entries))
	}
	for i, e := range entries {
		if e.Action != ActionWouldApprove {
			t.Fatalf("entry[%d] action = %s, want %s", i, e.Action, ActionWouldApprove)
		}
	}
	if !strings.Contains(out.String(), "Batch retry: chunk size 4, attempt 1, error:") || !strings.Contains(out.String(), "unexpected end of JSON input") {
		t.Fatalf("output missing retry log:\n%s", out.String())
	}
}

func TestBatchSplitsChunkOnPersistentTruncatedJSON(t *testing.T) {
	prs := []github.ApprovalPullRequest{prNumber(1), prNumber(2), prNumber(3), prNumber(4), prNumber(5)}
	fake := newFake(prs...)
	fake.batchErrFn = func(refs []github.PullRef) error {
		for _, ref := range refs {
			if ref.Number == 3 {
				return errors.New("load pull requests: gh api graphql: failed to execute gh: exit status 1: unexpected end of JSON input")
			}
		}
		return nil
	}

	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: fake,
		Config: config.ApproveConfig{
			Body:                "LGTM",
			SkipAlreadyApproved: true,
			Rules:               []config.ApproveRule{dashboardRule},
		},
		Out:         &out,
		Log:         &logBuf,
		Now:         fixedNow,
		BatchSize:   5,
		BackoffBase: time.Millisecond,
		Sleep:       func(time.Duration) {},
	}
	err := runner.Run(Options{Explain: true})
	if err == nil || !strings.Contains(err.Error(), "1 pull request(s) failed") {
		t.Fatalf("err = %v, want 1 pull request(s) failed", err)
	}
	entries := decodeLog(t, &logBuf)
	if len(entries) != 5 {
		t.Fatalf("entries = %d, want 5", len(entries))
	}
	for i, e := range entries {
		wantAction := ActionWouldApprove
		if i == 2 { // PR #3
			wantAction = ActionError
		}
		if e.Action != wantAction {
			t.Fatalf("PR #%d action = %s, want %s", i+1, e.Action, wantAction)
		}
	}
	for _, want := range []string{"Batch retry: chunk size 5, attempt 1", "Batch split: chunk size 5, attempt 2"} {
		if !strings.Contains(out.String(), want) {
			t.Fatalf("output missing %q:\n%s", want, out.String())
		}
	}
}

func TestIsTransientBatchError(t *testing.T) {
	var v any
	jsonSyntaxErr := json.Unmarshal([]byte(`{"data": 1,}`), &v)
	if jsonSyntaxErr == nil {
		t.Fatal("expected json.Unmarshal error for garbled JSON")
	}

	tests := []struct {
		name string
		err  error
		want bool
	}{
		{name: "nil", err: nil, want: false},
		{name: "http 502", err: errors.New("gh: HTTP 502"), want: true},
		{name: "http 503", err: errors.New("gh: HTTP 503"), want: true},
		{name: "http 504", err: errors.New("load pull requests: gh api graphql: exit status 1: gh: HTTP 504"), want: true},
		{name: "deadline exceeded", err: fmt.Errorf("graphql: %w", context.DeadlineExceeded), want: true},
		{name: "timeout text", err: errors.New("dial tcp: i/o timeout"), want: true},
		{name: "unexpected end of json input", err: errors.New("load pull requests: gh api graphql: failed to execute gh: exit status 1: unexpected end of JSON input"), want: true},
		{name: "unexpected EOF text", err: errors.New("load pull requests: read body: unexpected EOF"), want: true},
		{name: "wrapped io.ErrUnexpectedEOF", err: fmt.Errorf("load pull requests: %w", io.ErrUnexpectedEOF), want: true},
		{name: "wrapped json.SyntaxError", err: fmt.Errorf("load pull requests: %w", jsonSyntaxErr), want: true},
		{name: "connection reset by peer", err: errors.New("read tcp 10.0.0.1:443: connection reset by peer"), want: true},
		{name: "use of closed network connection", err: errors.New("read tcp 10.0.0.1:443: use of closed network connection"), want: true},
		{name: "wrapped net.ErrClosed", err: fmt.Errorf("read body: %w", net.ErrClosed), want: true},
		{name: "response body closed", err: errors.New("http2: response body closed"), want: true},
		{name: "http2 connection force closed", err: errors.New("http2: client connection force closed"), want: true},
		{name: "connection closed phrase", err: errors.New("connection closed before response completed"), want: true},
		{name: "invalid character html body", err: errors.New("invalid character '<' looking for beginning of value"), want: true},
		{name: "invalid character non-json body", err: errors.New("load pull requests: invalid character 'e' looking for beginning of value"), want: true},
		{name: "bad query", err: errors.New("GraphQL: Field 'foo' doesn't exist"), want: false},
		{name: "graphql not found", err: errors.New("load pull requests: Could not resolve to a PullRequest with the number of 99."), want: false},
		{name: "primary rate limit", err: errors.New("HTTP 403: API rate limit exceeded"), want: false},
		{name: "secondary rate limit", err: errors.New("HTTP 403: You have exceeded a secondary rate limit"), want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := isTransientBatchError(tc.err); got != tc.want {
				t.Errorf("isTransientBatchError(%v) = %v, want %v", tc.err, got, tc.want)
			}
		})
	}
}
