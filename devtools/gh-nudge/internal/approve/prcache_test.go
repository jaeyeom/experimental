package approve

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func decodeLog(t *testing.T, buf *bytes.Buffer) []LogEntry {
	t.Helper()
	var entries []LogEntry
	for _, line := range strings.Split(strings.TrimSpace(buf.String()), "\n") {
		if line == "" {
			continue
		}
		var entry LogEntry
		if err := json.Unmarshal([]byte(line), &entry); err != nil {
			t.Fatalf("decode log line %q: %v", line, err)
		}
		entries = append(entries, entry)
	}
	return entries
}

func runWithCache(t *testing.T, fake Client, rules []config.ApproveRule, cache *PRCache, opts Options) runResult {
	t.Helper()
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: fake,
		Config: config.ApproveConfig{
			Body:                "LGTM",
			SkipAlreadyApproved: true,
			Rules:               rules,
		},
		Out:     &out,
		Log:     &logBuf,
		Now:     fixedNow,
		PRCache: cache,
	}
	err := runner.Run(opts)
	return runResult{out: out.String(), log: decodeLog(t, &logBuf), err: err}
}

func TestPRCacheReusesFactsAndReevaluatesMutableFields(t *testing.T) {
	pr1, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch))
	pr1.Labels = []string{"ready"}
	pr1.Checks = []github.ApprovalCheck{{Name: "ci", Outcome: models.ChecksPass}}
	fake.prs[1] = pr1
	fake.viewerTeams = map[string][]string{"acme": {"core"}}
	fake.teams["acme/core"] = []string{"me", "bob"}

	cache := &PRCache{Dir: t.TempDir(), TTL: 7 * 24 * time.Hour, Now: fixedNow}
	rule := mineRule
	rule.LabelsRequired = []string{"ready"}
	rule.Checks = config.ChecksGreen

	// First pass: cold cache, loads patches, CODEOWNERS, and teams.
	res1 := runWithCache(t, fake, []config.ApproveRule{rule}, cache, Options{Explain: true})
	if res1.err != nil {
		t.Fatal(res1.err)
	}
	if res1.log[0].Action != ActionWouldApprove {
		t.Fatalf("pass 1 action = %s, want would_approve", res1.log[0].Action)
	}
	if !strings.Contains(res1.out, "cache: miss (not cached)") {
		t.Fatalf("pass 1 explain missing cache miss:\n%s", res1.out)
	}
	if fake.patchReads != 1 || fake.codeownersReads != 1 || fake.teamCountReads != 1 {
		t.Fatalf("pass 1 reads: patches=%d codeowners=%d teamCounts=%d",
			fake.patchReads, fake.codeownersReads, fake.teamCountReads)
	}

	// Second pass with unchanged head SHA: no patch or team-count reads,
	// and mutable fields (draft state, labels, checks) are still evaluated live.
	fake.patchReads = 0
	fake.codeownersReads = 0
	fake.teamCountReads = 0
	pr1.Checks = []github.ApprovalCheck{{Name: "ci", Outcome: models.ChecksFail}}
	fake.prs[1] = pr1

	res2 := runWithCache(t, fake, []config.ApproveRule{rule}, cache, Options{Explain: true})
	if res2.err != nil {
		t.Fatal(res2.err)
	}
	if fake.patchReads != 0 {
		t.Fatalf("pass 2 patchReads = %d, want 0", fake.patchReads)
	}
	if fake.teamCountReads != 0 {
		t.Fatalf("pass 2 teamCountReads = %d, want 0", fake.teamCountReads)
	}
	if res2.log[0].Action != ActionManual {
		t.Fatalf("pass 2 action = %s, want manual when check fails", res2.log[0].Action)
	}
	if !strings.Contains(res2.out, "cache: hit") {
		t.Fatalf("pass 2 explain missing cache: hit:\n%s", res2.out)
	}

	// Changing labels and draft state on pass 3 is also evaluated live.
	pr1.Checks = nil
	pr1.Draft = true
	pr1.Labels = nil
	fake.prs[1] = pr1
	res3 := runWithCache(t, fake, []config.ApproveRule{rule}, cache, Options{Explain: true})
	if res3.err != nil {
		t.Fatal(res3.err)
	}
	if res3.log[0].Action != ActionSkipped {
		t.Fatalf("pass 3 action = %s, want skipped when label removed and draft=true", res3.log[0].Action)
	}

	// Restore label/draft/checks and verify cache hit approves again.
	pr1.Checks = []github.ApprovalCheck{{Name: "ci", Outcome: models.ChecksPass}}
	pr1.Draft = false
	pr1.Labels = []string{"ready"}
	fake.prs[1] = pr1
	res4 := runWithCache(t, fake, []config.ApproveRule{rule}, cache, Options{Explain: true})
	if res4.err != nil {
		t.Fatal(res4.err)
	}
	if res4.log[0].Action != ActionWouldApprove {
		t.Fatalf("pass 4 action = %s, want would_approve", res4.log[0].Action)
	}
	if !strings.Contains(res4.out, "cache: hit") {
		t.Fatalf("pass 4 explain missing cache: hit:\n%s", res4.out)
	}
	if fake.patchReads != 0 || fake.teamCountReads != 0 {
		t.Fatalf("pass 4 reads: patches=%d teamCounts=%d, want 0", fake.patchReads, fake.teamCountReads)
	}
}

func TestPRCacheInvalidations(t *testing.T) {
	setup := func(t *testing.T) (github.ApprovalPullRequest, *fakeClient, *PRCache) {
		t.Helper()
		pr, fake := minePR(patchFile("deploy/values.yaml", yamlCommentPatch))
		fake.viewerTeams = map[string][]string{"acme": {"core"}}
		fake.teams["acme/core"] = []string{"me", "bob"}
		cache := &PRCache{Dir: t.TempDir(), TTL: 7 * 24 * time.Hour, Now: fixedNow}
		res := runWithCache(t, fake, []config.ApproveRule{mineRule}, cache, Options{Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		fake.patchReads = 0
		fake.codeownersReads = 0
		fake.teamCountReads = 0
		return pr, fake, cache
	}

	t.Run("new head SHA", func(t *testing.T) {
		pr, fake, cache := setup(t)
		pr.HeadSHA = "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
		fake.prs[1] = pr
		res := runWithCache(t, fake, []config.ApproveRule{mineRule}, cache, Options{Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if fake.patchReads != 1 || !strings.Contains(res.out, "cache: miss (head changed)") {
			t.Fatalf("patchReads=%d, out:\n%s", fake.patchReads, res.out)
		}
	})

	t.Run("changed CODEOWNERS blob", func(t *testing.T) {
		_, fake, cache := setup(t)
		fake.codeownersOID = "new-blob-sha"
		res := runWithCache(t, fake, []config.ApproveRule{mineRule}, cache, Options{Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if fake.teamCountReads != 1 || !strings.Contains(res.out, "cache: miss (CODEOWNERS changed)") {
			t.Fatalf("teamCountReads=%d, out:\n%s", fake.teamCountReads, res.out)
		}
	})

	t.Run("changed team list", func(t *testing.T) {
		_, fake, cache := setup(t)
		fake.viewerTeams = map[string][]string{"acme": {"core", "extra"}}
		res := runWithCache(t, fake, []config.ApproveRule{mineRule}, cache, Options{Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if fake.teamCountReads != 1 || !strings.Contains(res.out, "cache: miss (teams changed)") {
			t.Fatalf("teamCountReads=%d, out:\n%s", fake.teamCountReads, res.out)
		}
	})

	t.Run("schema bump", func(t *testing.T) {
		_, fake, cache := setup(t)
		cache.SchemaVersion = prCacheSchemaVersion + 1
		res := runWithCache(t, fake, []config.ApproveRule{mineRule}, cache, Options{Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if fake.patchReads != 1 || !strings.Contains(res.out, "cache: miss (schema changed)") {
			t.Fatalf("patchReads=%d, out:\n%s", fake.patchReads, res.out)
		}
	})

	t.Run("corrupt entry", func(t *testing.T) {
		pr, fake, cache := setup(t)
		if err := os.WriteFile(cache.path(pr.Ref), []byte("{corrupt json"), 0o600); err != nil {
			t.Fatal(err)
		}
		res := runWithCache(t, fake, []config.ApproveRule{mineRule}, cache, Options{Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if fake.patchReads != 1 || !strings.Contains(res.out, "cache: miss (corrupt)") {
			t.Fatalf("patchReads=%d, out:\n%s", fake.patchReads, res.out)
		}
	})

	t.Run("no-cache bypasses cache", func(t *testing.T) {
		_, fake, _ := setup(t)
		res := runWithCache(t, fake, []config.ApproveRule{mineRule}, nil, Options{Explain: true})
		if res.err != nil {
			t.Fatal(res.err)
		}
		if fake.patchReads != 1 {
			t.Fatalf("patchReads = %d, want 1 when cache is nil (--no-cache)", fake.patchReads)
		}
	})
}

func TestPRCachePrunesOldNonCandidates(t *testing.T) {
	now := fixedNow()
	cache := &PRCache{Dir: t.TempDir(), TTL: 24 * time.Hour, Now: func() time.Time { return now }}
	staleRef := github.PullRef{Owner: "acme", Repo: "widgets", Number: 99}
	if err := cache.put(prCacheEntry{Owner: "acme", Repo: "widgets", Number: 99, HeadSHA: "old"}); err != nil {
		t.Fatal(err)
	}
	activeRef := github.PullRef{Owner: "acme", Repo: "widgets", Number: 1}
	if err := cache.put(prCacheEntry{Owner: "acme", Repo: "widgets", Number: 1, HeadSHA: "cur"}); err != nil {
		t.Fatal(err)
	}
	now = now.Add(48 * time.Hour)
	if err := cache.prune(map[string]bool{prCacheKey(activeRef): true}); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(cache.path(staleRef)); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("stale entry still exists: %v", err)
	}
	if _, err := os.Stat(cache.path(activeRef)); err != nil {
		t.Fatalf("active entry was pruned: %v", err)
	}
}

type parallelFakeClient struct {
	*fakeClient
	inFlight    atomic.Int32
	maxInFlight atomic.Int32
	approving   atomic.Int32
	maxApprove  atomic.Int32
	loadDelay   time.Duration
	rateLimitN  atomic.Int32
}

func (p *parallelFakeClient) LoadApprovalPullRequest(ref github.PullRef) (github.ApprovalPullRequest, error) {
	cur := p.inFlight.Add(1)
	for {
		prev := p.maxInFlight.Load()
		if cur <= prev || p.maxInFlight.CompareAndSwap(prev, cur) {
			break
		}
	}
	defer p.inFlight.Add(-1)
	if p.loadDelay > 0 {
		time.Sleep(p.loadDelay)
	}
	if p.rateLimitN.Load() > 0 && p.rateLimitN.Add(-1) >= 0 {
		return github.ApprovalPullRequest{}, errors.New("HTTP 403: You have exceeded a secondary rate limit")
	}
	return p.fakeClient.LoadApprovalPullRequest(ref)
}

func (p *parallelFakeClient) ApprovePullRequest(ref github.PullRef, sha, body string) error {
	cur := p.approving.Add(1)
	for {
		prev := p.maxApprove.Load()
		if cur <= prev || p.maxApprove.CompareAndSwap(prev, cur) {
			break
		}
	}
	defer p.approving.Add(-1)
	time.Sleep(2 * time.Millisecond)
	return p.fakeClient.ApprovePullRequest(ref, sha, body)
}

func TestParallelLoadRespectsConcurrencyOrderAndSequentialApprovals(t *testing.T) {
	var prs []github.ApprovalPullRequest
	for i := 1; i <= 8; i++ {
		prs = append(prs, prNumber(i))
	}
	pFake := &parallelFakeClient{
		fakeClient: newFake(prs...),
		loadDelay:  10 * time.Millisecond,
	}
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: pFake,
		Config: config.ApproveConfig{
			Body:            "LGTM",
			LoadConcurrency: 3,
			Rules:           []config.ApproveRule{dashboardRule},
		},
		Out: &out,
		Log: &logBuf,
		Now: fixedNow,
	}
	if err := runner.Run(Options{Apply: true}); err != nil {
		t.Fatal(err)
	}
	if pFake.maxInFlight.Load() > 3 {
		t.Fatalf("max concurrent loads = %d, want <= 3", pFake.maxInFlight.Load())
	}
	if pFake.maxInFlight.Load() < 2 {
		t.Fatalf("max concurrent loads = %d, expected parallel execution", pFake.maxInFlight.Load())
	}
	if pFake.maxApprove.Load() != 1 {
		t.Fatalf("max concurrent approvals = %d, want 1", pFake.maxApprove.Load())
	}
	entries := decodeLog(t, &logBuf)
	if len(entries) != 8 {
		t.Fatalf("entries = %d, want 8", len(entries))
	}
	for i, e := range entries {
		wantPR := fmt.Sprintf("acme/widgets#%d", i+1)
		if e.PR != wantPR {
			t.Fatalf("entry[%d].PR = %s, want %s (candidate order)", i, e.PR, wantPR)
		}
		if e.Action != ActionApproved {
			t.Fatalf("entry[%d].Action = %s, want approved", i, e.Action)
		}
	}
}

func TestSecondaryRateLimitBacksOffAndKeepsGoing(t *testing.T) {
	prs := []github.ApprovalPullRequest{prNumber(1), prNumber(2), prNumber(3), prNumber(4)}
	pFake := &parallelFakeClient{
		fakeClient: newFake(prs...),
	}
	pFake.rateLimitN.Store(2)

	var sleeps []time.Duration
	var sleepMu sync.Mutex
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: pFake,
		Config: config.ApproveConfig{
			Body:            "LGTM",
			LoadConcurrency: 4,
			Rules:           []config.ApproveRule{dashboardRule},
		},
		Out:         &out,
		Log:         &logBuf,
		Now:         fixedNow,
		BackoffBase: time.Millisecond,
		Sleep: func(d time.Duration) {
			sleepMu.Lock()
			sleeps = append(sleeps, d)
			sleepMu.Unlock()
		},
	}
	if err := runner.Run(Options{}); err != nil {
		t.Fatalf("pass failed on secondary rate limit: %v", err)
	}
	if len(sleeps) == 0 {
		t.Fatalf("expected backoff sleep on secondary rate limit")
	}
	entries := decodeLog(t, &logBuf)
	if len(entries) != 4 {
		t.Fatalf("entries = %d, want 4", len(entries))
	}
	for i, e := range entries {
		if e.Action != ActionWouldApprove {
			t.Fatalf("entry[%d].Action = %s, want would_approve", i, e.Action)
		}
	}
}

func TestMaxDurationWithParallelWorkersInFlight(t *testing.T) {
	var prs []github.ApprovalPullRequest
	for i := 1; i <= 6; i++ {
		prs = append(prs, prNumber(i))
	}
	fake := newFake(prs...)
	var mu sync.Mutex
	now := fixedNow()
	started := make(chan struct{}, 6)
	release := make(chan struct{})
	pFake := &parallelFakeClient{fakeClient: fake}
	fake.onCall = func(op string) {
		if op == "pull request" {
			started <- struct{}{}
			<-release
		}
	}

	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: pFake,
		Config: config.ApproveConfig{
			Body:            "LGTM",
			LoadConcurrency: 2,
			Rules:           []config.ApproveRule{dashboardRule},
		},
		Out: &out,
		Log: &logBuf,
		Now: func() time.Time {
			mu.Lock()
			defer mu.Unlock()
			return now
		},
	}

	done := make(chan error, 1)
	go func() {
		done <- runner.Run(Options{MaxDuration: 5 * time.Minute})
	}()

	// Wait for the first 2 workers to be in flight, then advance past MaxDuration
	// before releasing them so no further candidates are started.
	<-started
	<-started
	mu.Lock()
	now = now.Add(6 * time.Minute)
	mu.Unlock()
	close(release)

	if err := <-done; err != nil {
		t.Fatal(err)
	}
	if fake.loadReads != 2 {
		t.Fatalf("loadReads = %d, want 2 in-flight loads to finish", fake.loadReads)
	}
	if !strings.Contains(out.String(), "fully loaded: 2;") || !strings.Contains(out.String(), "4 candidate(s) not evaluated") {
		t.Fatalf("summary = %s", out.String())
	}
}
