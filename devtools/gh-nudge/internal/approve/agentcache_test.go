package approve

import (
	"bytes"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// fakeClock is a settable clock for cache tests.
type fakeClock struct{ now time.Time }

func (c *fakeClock) Now() time.Time { return c.now }

func newTestCache(t *testing.T) (*AgentCache, *fakeClock) {
	t.Helper()
	clock := &fakeClock{now: fixedNow()}
	return &AgentCache{
		Dir:      filepath.Join(t.TempDir(), "gh-approve", "agent"),
		TTL:      config.DefaultAgentCacheTTL,
		MaxBytes: int64(config.DefaultAgentCacheMaxMB) << 20,
		Now:      clock.Now,
	}, clock
}

// cacheRunner is a Runner with an agent cache. Each run resets the output
// and the log.
type cacheRunner struct {
	t      *testing.T
	runner *Runner
	out    bytes.Buffer
	log    bytes.Buffer
}

func newCacheRunner(t *testing.T, fake *fakeClient, rules []config.ApproveRule, agent *fakeAgent, cache *AgentCache) *cacheRunner {
	t.Helper()
	cr := &cacheRunner{t: t}
	cr.runner = &Runner{
		Client:     fake,
		Config:     config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: rules},
		Out:        &cr.out,
		Log:        &cr.log,
		Now:        fixedNow,
		RunCommand: agent.run,
		AgentCache: cache,
	}
	return cr
}

func (cr *cacheRunner) run(opts Options) (string, LogEntry) {
	cr.t.Helper()
	cr.out.Reset()
	cr.log.Reset()
	if err := cr.runner.Run(opts); err != nil {
		cr.t.Fatal(err)
	}
	var entry LogEntry
	if err := json.Unmarshal(cr.log.Bytes(), &entry); err != nil {
		cr.t.Fatalf("log %q: %v", cr.log.String(), err)
	}
	return cr.out.String(), entry
}

func cacheFiles(t *testing.T, cache *AgentCache) []string {
	t.Helper()
	entries, err := os.ReadDir(cache.Dir)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		t.Fatal(err)
	}
	var names []string
	for _, e := range entries {
		names = append(names, e.Name())
	}
	return names
}

func TestAgentCacheHitSkipsSecondRun(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve", "reason": "only dashboards"}`}
	cache, clock := newTestCache(t)
	cr := newCacheRunner(t, newFake(prNumber(1)), []config.ApproveRule{agentRule("agent")}, agent, cache)

	out, entry := cr.run(Options{})
	got := entry.Rules[0].Agent
	if got == nil || got.Cache != CacheMiss || len(got.CacheKey) != 12 {
		t.Fatalf("first agent = %+v", got)
	}
	if !strings.Contains(out, "agent: approve (fresh)") {
		t.Fatalf("first out = %s", out)
	}
	files := cacheFiles(t, cache)
	if len(files) != 1 {
		t.Fatalf("cache files = %q", files)
	}
	info, err := os.Stat(filepath.Join(cache.Dir, files[0]))
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode().Perm() != 0o600 {
		t.Fatalf("cache file mode = %v", info.Mode())
	}

	clock.now = clock.now.Add(49 * time.Hour)
	out, entry = cr.run(Options{})
	if len(agent.calls) != 1 {
		t.Fatalf("agent ran %d times, want 1", len(agent.calls))
	}
	hit := entry.Rules[0].Agent
	if hit == nil || hit.Cache != CacheHit || hit.CacheKey != got.CacheKey ||
		hit.Decision != AgentApprove || hit.Reason != "only dashboards" || entry.Action != ActionWouldApprove {
		t.Fatalf("second entry = %+v agent = %+v", entry, hit)
	}
	if !strings.Contains(out, "agent: approve (cached 2d ago)") {
		t.Fatalf("second out = %s", out)
	}
}

func TestAgentCacheDeferHitShowsCached(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "defer", "reason": "touches billing"}`}
	cache, _ := newTestCache(t)
	cr := newCacheRunner(t, newFake(prNumber(1)), []config.ApproveRule{agentRule("agent")}, agent, cache)
	cr.run(Options{})
	out, entry := cr.run(Options{})
	if len(agent.calls) != 1 || entry.Action != ActionManual || entry.Rules[0].Agent.Cache != CacheHit {
		t.Fatalf("calls = %d entry = %+v", len(agent.calls), entry)
	}
	if !strings.Contains(out, "agent: defer (cached)") || !strings.Contains(out, "touches billing") {
		t.Fatalf("out = %s", out)
	}
}

func TestAgentCacheExplainAndLogShowCacheAndKey(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve"}`}
	cache, _ := newTestCache(t)
	cr := newCacheRunner(t, newFake(prNumber(1)), []config.ApproveRule{agentRule("agent")}, agent, cache)
	out, entry := cr.run(Options{Explain: true})
	key := entry.Rules[0].Agent.CacheKey
	if !strings.Contains(out, "agent_cache: miss, key "+key) {
		t.Fatalf("explain = %s", out)
	}
	if !strings.Contains(cr.log.String(), `"agent_cache":"miss"`) || !strings.Contains(cr.log.String(), `"agent_cache_key":"`+key+`"`) {
		t.Fatalf("log = %s", cr.log.String())
	}
	out, _ = cr.run(Options{Explain: true})
	if !strings.Contains(out, "agent_cache: hit, key "+key) || !strings.Contains(cr.log.String(), `"agent_cache":"hit"`) {
		t.Fatalf("explain = %s log = %s", out, cr.log.String())
	}
}

func TestAgentCacheKeyChanges(t *testing.T) {
	tests := map[string]func(fake *fakeClient, rule *config.ApproveRule){
		"diff": func(fake *fakeClient, _ *config.ApproveRule) { fake.diff = "diff changed\n" },
		"argv": func(_ *fakeClient, rule *config.ApproveRule) {
			rule.AgentCommand = []string{"review-agent", "--lenient"}
		},
		"rule name": func(_ *fakeClient, rule *config.ApproveRule) { rule.Name = "renamed" },
		"head sha": func(fake *fakeClient, _ *config.ApproveRule) {
			pr := fake.prs[1]
			pr.HeadSHA = "h2"
			fake.prs[1] = pr
		},
	}
	for name, change := range tests {
		t.Run(name, func(t *testing.T) {
			agent := &fakeAgent{stdout: `{"decision": "approve"}`}
			cache, _ := newTestCache(t)
			fake := newFake(prNumber(1))
			rule := agentRule("agent")
			cr := newCacheRunner(t, fake, []config.ApproveRule{rule}, agent, cache)
			cr.run(Options{})
			change(fake, &rule)
			cr.runner.Config.Rules = []config.ApproveRule{rule}
			_, entry := cr.run(Options{})
			if len(agent.calls) != 2 || entry.Rules[0].Agent.Cache != CacheMiss {
				t.Fatalf("calls = %d agent = %+v", len(agent.calls), entry.Rules[0].Agent)
			}
		})
	}
}

func TestAgentCacheNeverStoresFailures(t *testing.T) {
	tests := map[string]fakeAgent{
		"timeout":          {stdout: `{"decision": "approve"}`, timedOut: true},
		"non-zero exit":    {stdout: `{"decision": "approve"}`, code: 1},
		"invalid output":   {stdout: `not json`},
		"unknown decision": {stdout: `{"decision": "maybe"}`},
		"start error":      {startErr: true},
	}
	for name, agent := range tests {
		t.Run(name, func(t *testing.T) {
			cache, _ := newTestCache(t)
			cr := newCacheRunner(t, newFake(prNumber(1)), []config.ApproveRule{agentRule("agent")}, &agent, cache)
			cr.run(Options{})
			_, entry := cr.run(Options{})
			got := entry.Rules[0].Agent
			if len(agent.calls) != 2 || got.Decision != AgentDefer || got.Cache != CacheMiss {
				t.Fatalf("calls = %d agent = %+v", len(agent.calls), got)
			}
			if files := cacheFiles(t, cache); len(files) != 0 {
				t.Fatalf("cache files = %q", files)
			}
		})
	}
}

func TestAgentCacheTTLExpires(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve"}`}
	cache, clock := newTestCache(t)
	cr := newCacheRunner(t, newFake(prNumber(1)), []config.ApproveRule{agentRule("agent")}, agent, cache)
	cr.run(Options{})
	clock.now = clock.now.Add(config.DefaultAgentCacheTTL + time.Second)
	_, entry := cr.run(Options{})
	if len(agent.calls) != 2 || entry.Rules[0].Agent.Cache != CacheMiss {
		t.Fatalf("calls = %d agent = %+v", len(agent.calls), entry.Rules[0].Agent)
	}
}

func TestAgentCacheGetRemovesExpiredFile(t *testing.T) {
	cache, clock := newTestCache(t)
	if err := cache.put("k1", agentCacheEntry{Decision: AgentApprove, Time: clock.now}); err != nil {
		t.Fatal(err)
	}
	if _, ok := cache.get("k1"); !ok {
		t.Fatal("fresh entry missed")
	}
	clock.now = clock.now.Add(cache.TTL + time.Second)
	if _, ok := cache.get("k1"); ok {
		t.Fatal("expired entry hit")
	}
	if files := cacheFiles(t, cache); len(files) != 0 {
		t.Fatalf("expired file kept: %q", files)
	}
}

func TestAgentCacheEvictsOldestOverLimit(t *testing.T) {
	cache, clock := newTestCache(t)
	entry := agentCacheEntry{Decision: AgentApprove, Reason: strings.Repeat("x", 100)}
	at := func(minutes int) agentCacheEntry {
		e := entry
		e.Time = clock.now.Add(time.Duration(minutes) * time.Minute)
		return e
	}
	if err := cache.put("first", at(0)); err != nil {
		t.Fatal(err)
	}
	info, err := os.Stat(filepath.Join(cache.Dir, "first.json"))
	if err != nil {
		t.Fatal(err)
	}
	cache.MaxBytes = 2*info.Size() + info.Size()/2
	if err := cache.put("second", at(1)); err != nil {
		t.Fatal(err)
	}
	if err := cache.put("third", at(2)); err != nil {
		t.Fatal(err)
	}
	if _, ok := cache.get("first"); ok {
		t.Fatal("oldest entry kept over the size limit")
	}
	for _, key := range []string{"second", "third"} {
		if _, ok := cache.get(key); !ok {
			t.Fatalf("entry %s evicted", key)
		}
	}
}

func TestAgentCacheBypass(t *testing.T) {
	disabled := agentRule("agent")
	disabled.AgentCacheDisabled = true
	tests := map[string]struct {
		rule     config.ApproveRule
		useCache bool
	}{
		"no cache (--no-agent-cache)": {agentRule("agent"), false},
		"agent_cache = false":         {disabled, true},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			agent := &fakeAgent{stdout: `{"decision": "approve"}`}
			cache, _ := newTestCache(t)
			var runnerCache *AgentCache
			if tt.useCache {
				runnerCache = cache
			}
			cr := newCacheRunner(t, newFake(prNumber(1)), []config.ApproveRule{tt.rule}, agent, runnerCache)
			cr.run(Options{})
			out, entry := cr.run(Options{})
			if len(agent.calls) != 2 || entry.Rules[0].Agent.Cache != CacheBypass {
				t.Fatalf("calls = %d agent = %+v", len(agent.calls), entry.Rules[0].Agent)
			}
			if !strings.Contains(out, "agent: approve (fresh)") {
				t.Fatalf("out = %s", out)
			}
			if files := cacheFiles(t, cache); len(files) != 0 {
				t.Fatalf("cache files = %q", files)
			}
		})
	}
}

func TestAgentCacheHitStillRechecksHeadAtApply(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve"}`}
	cache, _ := newTestCache(t)
	fake := newFake(prNumber(1))
	cr := newCacheRunner(t, fake, []config.ApproveRule{agentRule("agent")}, agent, cache)
	cr.run(Options{})
	fake.heads = map[int]string{1: "moved"}
	_, entry := cr.run(Options{Apply: true})
	if entry.Rules[0].Agent.Cache != CacheHit || entry.Action != ActionSkipped || !strings.Contains(entry.Reason, "head moved") {
		t.Fatalf("entry = %+v", entry)
	}
	if fake.headReads != 1 || len(fake.approvals) != 0 {
		t.Fatalf("head reads = %d approvals = %+v", fake.headReads, fake.approvals)
	}
}

func TestAgentStdinIsStable(t *testing.T) {
	input := sampleInput()
	input.Files = []models.File{{Path: "z.go"}, {Path: "a.go", Additions: 1}}
	input.PR.Labels = []string{"wip", "docs"}
	first, err := agentStdin(input)
	if err != nil {
		t.Fatal(err)
	}
	again, err := agentStdin(input)
	if err != nil {
		t.Fatal(err)
	}
	reordered := input
	reordered.Files = []models.File{{Path: "a.go", Additions: 1}, {Path: "z.go"}}
	reordered.PR.Labels = []string{"docs", "wip"}
	second, err := agentStdin(reordered)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(first, again) || !bytes.Equal(first, second) {
		t.Fatalf("stdin differs:\n%s\n%s\n%s", first, again, second)
	}
	if bytes.Index(first, []byte(`"a.go"`)) > bytes.Index(first, []byte(`"z.go"`)) ||
		bytes.Index(first, []byte(`"docs"`)) > bytes.Index(first, []byte(`"wip"`)) {
		t.Fatalf("stdin not sorted: %s", first)
	}
	if input.Files[0].Path != "z.go" || input.PR.Labels[0] != "wip" {
		t.Fatal("agentStdin reordered the caller's slices")
	}
	if !bytes.Contains(first, []byte(`"headSha":"h1"`)) {
		t.Fatalf("stdin missing head SHA: %s", first)
	}
}

func TestAgentCacheKeyDependsOnEachPart(t *testing.T) {
	base := agentCacheKey([]byte("in"), []string{"a", "b"}, "r")
	for name, key := range map[string]string{
		"stdin": agentCacheKey([]byte("in2"), []string{"a", "b"}, "r"),
		"argv":  agentCacheKey([]byte("in"), []string{"a b"}, "r"),
		"rule":  agentCacheKey([]byte("in"), []string{"a", "b"}, "r2"),
	} {
		if key == base {
			t.Fatalf("%s change kept key %s", name, key)
		}
	}
	if base != agentCacheKey([]byte("in"), []string{"a", "b"}, "r") || len(base) != 64 {
		t.Fatalf("key = %q", base)
	}
}
