package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

type fakeQueue struct {
	pr           github.QueuePullRequest
	byRef        map[string]github.QueuePullRequest
	loadErr      error
	enqueueErr   error
	enqueued     []string
	enqueueCalls int
	loadedRefs   []string
	downstack    []github.StackPR
	downCalls    int
	evidence     github.FailureEvidence
	landed       github.StackPR
	landedOK     bool
	mergedAsync  []string
	loads        int
	snapshots    []github.QueuePullRequest
}

func (f *fakeQueue) LoadPullRequest(ref string) (github.QueuePullRequest, error) {
	f.loads++
	f.loadedRefs = append(f.loadedRefs, ref)
	if f.loadErr != nil {
		return github.QueuePullRequest{}, f.loadErr
	}
	if len(f.snapshots) > 0 {
		index := f.loads - 1
		if index >= len(f.snapshots) {
			index = len(f.snapshots) - 1
		}
		pr := f.snapshots[index]
		if pr.Ref.Raw == "" {
			pr.Ref.Raw = ref
		}
		return pr, nil
	}
	if f.byRef != nil {
		pr, ok := f.byRef[ref]
		if !ok {
			return github.QueuePullRequest{}, errors.New("unknown pull request")
		}
		if pr.Ref.Raw == "" {
			pr.Ref.Raw = ref
		}
		return pr, nil
	}
	pr := f.pr
	if pr.Ref.Raw == "" {
		pr.Ref.Raw = ref
	}
	return pr, nil
}

func (f *fakeQueue) EnqueuePullRequest(nodeID string) error {
	f.enqueueCalls++
	if f.enqueueErr != nil {
		return f.enqueueErr
	}
	f.enqueued = append(f.enqueued, nodeID)
	return nil
}

func (f *fakeQueue) OpenDownstack(_, _ string, _ int) ([]github.StackPR, error) {
	f.downCalls++
	return f.downstack, nil
}

func (f *fakeQueue) FailureEvidence(_, _, _ string) (github.FailureEvidence, error) {
	return f.evidence, nil
}

func (f *fakeQueue) LandedFirst(_, _, _ string, _ int) (github.StackPR, bool, error) {
	return f.landed, f.landedOK, nil
}

func (f *fakeQueue) MergeAsync(owner, repo string, number int, sha string) error {
	f.mergedAsync = append(f.mergedAsync, fmt.Sprintf("%s/%s#%d@%s", owner, repo, number, sha))
	return nil
}

func readyPR() github.QueuePullRequest {
	return github.QueuePullRequest{
		Ref: github.PullRef{
			Owner:  "acme",
			Repo:   "widgets",
			Number: 12,
			Raw:    "acme/widgets#12",
		},
		NodeID:         "PR_NODE",
		ReviewDecision: "APPROVED",
		HeadSHA:        "abc123",
		Checks: []github.QueueCheck{{
			Name:       "ci",
			Green:      true,
			Conclusion: "SUCCESS",
		}},
	}
}

func queueConfig() *config.Config {
	return &config.Config{
		Path:   "testdata/config.pkl",
		GitHub: config.GitHubConfig{Owner: "acme", Repos: []string{"widgets"}},
		Settings: config.SettingsConfig{
			RequiredChecks: []string{"ci"},
		},
	}
}

func queueSettings() config.SettingsConfig {
	return config.SettingsConfig{
		BlockMergeLabels: []string{"do-not-merge"},
		RequiredChecks:   []string{"ci", "lint"},
		FlakeSignatures:  []string{"connection reset"},
		RCAMarker:        "RCA {event_id} class={class}",
	}
}

func TestQueueListsEveryPreflightProblemAndDoesNotEnqueue(t *testing.T) {
	pr := readyPR()
	pr.Draft = true
	pr.Labels = []string{"do-not-merge"}
	pr.ReviewDecision = "REVIEW_REQUIRED"
	pr.UnresolvedThreads = 2
	pr.Checks = []github.QueueCheck{{Name: "ci", Green: false, Conclusion: "FAILURE"}}
	client := &fakeQueue{pr: pr}
	cfg := &config.Config{
		GitHub:   config.GitHubConfig{Owner: "acme", Repos: []string{"widgets"}},
		Settings: queueSettings(),
	}

	output := captureStdout(t, func() {
		err := queuePullRequest(client, cfg, pr.Ref.Raw, false)
		if err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})

	for _, want := range []string{
		"pull request is a draft",
		`label "do-not-merge" blocks merge`,
		"pull request is not approved",
		`required check "ci" is not green (FAILURE)`,
		`required check "lint" is missing`,
		"unresolved review thread",
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	if len(client.enqueued) != 0 || len(client.mergedAsync) != 0 {
		t.Errorf("enqueued = %v async = %v, want no enqueue", client.enqueued, client.mergedAsync)
	}
}

func TestQueueEmptyRequiredChecksRefuses(t *testing.T) {
	pr := readyPR()
	pr.Checks = []github.QueueCheck{{Name: "ci", Green: false, Conclusion: "FAILURE"}}
	cfg := queueConfig()
	cfg.Settings.RequiredChecks = nil
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	if !strings.Contains(output, "required_checks is empty") {
		t.Fatalf("output = %s", output)
	}
	if len(client.enqueued) != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueued = %v async = %v", client.enqueued, client.mergedAsync)
	}

	withAllowNoRequiredChecks(t, true)
	allowed := &fakeQueue{pr: pr}
	if err := queuePullRequest(allowed, cfg, pr.Ref.Raw, false); err != nil {
		t.Fatalf("allowed queuePullRequest() error = %v", err)
	}
	if len(allowed.enqueued) != 1 {
		t.Fatalf("enqueued = %v, want one", allowed.enqueued)
	}
}

func TestQueueRemovalPrintsEvidenceAndFlakeHint(t *testing.T) {
	pr := readyPR()
	pr.Events = []github.QueueEvent{{
		ID:        "EVT1",
		Kind:      github.QueueEventRemoved,
		CreatedAt: time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC),
		Reason:    "merge_conflict",
		CommitSHA: "deadbeef",
	}}
	client := &fakeQueue{
		pr: pr,
		evidence: github.FailureEvidence{
			CheckRunURL: "https://github.com/acme/widgets/runs/1",
			JobURL:      "https://github.com/acme/widgets/actions/runs/9/job/8",
			StepName:    "test",
			Log:         "setup ok\nconnection reset by peer\nexit 1",
		},
		landed:   github.StackPR{Number: 10},
		landedOK: true,
	}
	settings := queueSettings()
	settings.RequiredChecks = []string{"ci"}
	cfg := &config.Config{
		GitHub:   config.GitHubConfig{Owner: "acme", Repos: []string{"widgets"}},
		Settings: settings,
	}

	output := captureStdout(t, func() {
		err := queuePullRequest(client, cfg, pr.Ref.Raw, false)
		if err == nil {
			t.Fatal("queuePullRequest() error = nil, want RCA refusal")
		}
	})

	for _, want := range []string{
		"Removal reason: merge_conflict",
		"https://github.com/acme/widgets/runs/1",
		"https://github.com/acme/widgets/actions/runs/9/job/8",
		"connection reset by peer",
		`Flake hint: pattern "connection reset" matched "connection reset" (not a verdict)`,
		"Landed first: acme/widgets#10",
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	if len(client.enqueued) != 0 {
		t.Errorf("enqueued = %v, want none without an RCA marker", client.enqueued)
	}
}

func TestQueueRCAGate(t *testing.T) {
	marker := "RCA {event_id} class={class}"
	removed := func(id string, at time.Time) github.QueueEvent {
		return github.QueueEvent{ID: id, Kind: github.QueueEventRemoved, CreatedAt: at, Reason: "failed_checks"}
	}
	added := func(id string, at time.Time) github.QueueEvent {
		return github.QueueEvent{ID: id, Kind: github.QueueEventAdded, CreatedAt: at}
	}
	base := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	tests := []struct {
		name     string
		events   []github.QueueEvent
		comment  string
		limit    int
		wantCall bool
	}{
		{
			name: "no marker",
			events: []github.QueueEvent{
				added("A0", base),
				removed("EVT", base.Add(time.Minute)),
			},
			wantCall: false,
		},
		{
			name: "our-code",
			events: []github.QueueEvent{
				added("A0", base),
				removed("EVT", base.Add(time.Minute)),
			},
			comment:  "RCA EVT class=our-code",
			wantCall: false,
		},
		{
			name: "flake under the cap",
			events: []github.QueueEvent{
				added("A0", base),
				removed("R1", base.Add(time.Minute)),
				added("A1", base.Add(2*time.Minute)),
				removed("EVT", base.Add(3*time.Minute)),
			},
			comment:  "RCA EVT class=flake",
			limit:    2,
			wantCall: true,
		},
		{
			name: "infra at the cap",
			events: []github.QueueEvent{
				added("A0", base),
				removed("R1", base.Add(time.Minute)),
				added("A1", base.Add(2*time.Minute)),
				removed("R2", base.Add(3*time.Minute)),
				added("A2", base.Add(4*time.Minute)),
				removed("EVT", base.Add(5*time.Minute)),
			},
			comment:  "RCA EVT class=infra",
			limit:    2,
			wantCall: true,
		},
		{
			name: "flake over the cap",
			events: []github.QueueEvent{
				added("A0", base),
				removed("R1", base.Add(time.Minute)),
				added("A1", base.Add(2*time.Minute)),
				removed("R2", base.Add(3*time.Minute)),
				added("A2", base.Add(4*time.Minute)),
				removed("R3", base.Add(5*time.Minute)),
				added("A3", base.Add(6*time.Minute)),
				removed("EVT", base.Add(7*time.Minute)),
			},
			comment:  "RCA EVT class=flake",
			limit:    2,
			wantCall: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			pr := readyPR()
			pr.Events = tc.events
			if tc.comment != "" {
				pr.Comments = []string{tc.comment}
			}
			client := &fakeQueue{pr: pr}
			settings := config.SettingsConfig{RCAMarker: marker, RequiredChecks: []string{"ci"}}
			if tc.limit != 0 {
				limit := tc.limit
				settings.MaxRequeues = &limit
			}
			cfg := &config.Config{
				GitHub:   config.GitHubConfig{Owner: "acme", Repos: []string{"widgets"}},
				Settings: settings,
			}
			err := queuePullRequest(client, cfg, pr.Ref.Raw, false)
			if tc.wantCall && err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
			if !tc.wantCall && err == nil {
				t.Fatal("queuePullRequest() error = nil, want refusal")
			}
			if tc.wantCall && (len(client.enqueued) != 1 || client.enqueued[0] != "PR_NODE") {
				t.Errorf("enqueued = %v, want PR_NODE", client.enqueued)
			}
			if !tc.wantCall && len(client.enqueued) != 0 {
				t.Errorf("enqueued = %v, want none", client.enqueued)
			}
		})
	}
}

func TestQueueStackUsesMergeAsyncOnlyWithDownstack(t *testing.T) {
	pr := readyPR()
	stackErr := errors.New("this pull request is part of a stack and must be merged sequentially using the stack merge API")
	below := []github.StackPR{{Number: 10, State: "open"}, {Number: 11, State: "open"}}

	t.Run("refuses without the flag", func(t *testing.T) {
		client := &fakeQueue{pr: pr, enqueueErr: stackErr, downstack: below}
		output := captureStdout(t, func() {
			err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false)
			if err == nil {
				t.Fatal("queuePullRequest() error = nil, want refusal")
			}
		})
		for _, want := range []string{"acme/widgets#10", "acme/widgets#11"} {
			if !strings.Contains(output, want) {
				t.Errorf("output missing %q:\n%s", want, output)
			}
		}
		if len(client.mergedAsync) != 0 {
			t.Errorf("mergedAsync = %v, want none", client.mergedAsync)
		}
		if len(client.enqueued) != 0 {
			t.Errorf("enqueued = %v, want the mutation to fail first", client.enqueued)
		}
	})

	t.Run("calls merge-async when asked", func(t *testing.T) {
		client := &fakeQueue{pr: pr, enqueueErr: stackErr, downstack: below}
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, true); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
		if len(client.mergedAsync) != 1 || client.mergedAsync[0] != "acme/widgets#12@abc123" {
			t.Errorf("mergedAsync = %v", client.mergedAsync)
		}
	})
}

func TestQueueOtherEnqueueErrorDoesNotFallBack(t *testing.T) {
	pr := readyPR()
	client := &fakeQueue{pr: pr, enqueueErr: errors.New("required status check failed")}
	err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, true)
	if err == nil {
		t.Fatal("queuePullRequest() error = nil")
	}
	if client.downCalls != 0 || len(client.mergedAsync) != 0 {
		t.Errorf("downCalls = %d async = %v, want no fallback", client.downCalls, client.mergedAsync)
	}
}

func TestWatchIgnoresTransientNotInQueue(t *testing.T) {
	base := readyPR()
	base.InMergeQueue = false
	merged := base
	merged.Merged = true
	client := &fakeQueue{snapshots: []github.QueuePullRequest{base, base, merged}}
	start := time.Date(2026, 3, 1, 0, 0, 0, 0, time.UTC)
	now := start
	output := captureStdout(t, func() {
		err := watchPullRequests(client, []string{base.Ref.Raw}, watchOptions{
			timeout:  time.Minute,
			interval: 15 * time.Second,
			now:      func() time.Time { return now },
			sleep:    func(d time.Duration) { now = now.Add(d) },
		})
		if err != nil {
			t.Fatalf("watchPullRequests() error = %v", err)
		}
	})
	if !strings.Contains(output, "acme/widgets#12 merged") {
		t.Errorf("output = %q", output)
	}
	if strings.Contains(output, "ejected") || strings.Contains(output, "timeout") {
		t.Errorf("output = %q, want merged", output)
	}
	if client.loads < 3 {
		t.Errorf("loads = %d, want at least 3", client.loads)
	}
}

func TestWatchJSONReportsEachResult(t *testing.T) {
	ejected := readyPR()
	ejected.Ref.Raw = "acme/widgets#2"
	ejected.Merged = false
	ejected.InMergeQueue = false
	ejected.Events = []github.QueueEvent{{
		ID:        "R1",
		Kind:      github.QueueEventRemoved,
		CreatedAt: time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC),
		Reason:    "failed_checks",
	}}
	merged := readyPR()
	merged.Ref.Raw = "acme/widgets#1"
	merged.Merged = true
	pending := readyPR()
	pending.Ref.Raw = "acme/widgets#3"
	pending.Merged = false
	client := &fakeQueue{byRef: map[string]github.QueuePullRequest{
		"acme/widgets#1": merged,
		"acme/widgets#2": ejected,
		"acme/widgets#3": pending,
	}}
	output := captureStdout(t, func() {
		err := watchPullRequests(client, []string{
			"acme/widgets#1",
			"acme/widgets#2",
			"acme/widgets#3",
		}, watchOptions{timeout: 0, asJSON: true})
		if err == nil {
			t.Fatal("watchPullRequests() error = nil, want a non-merge result")
		}
	})
	for _, want := range []string{`"result": "merged"`, `"result": "ejected:failed_checks"`, `"result": "timeout"`} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
}

func TestEarlierRequeuesIgnoresTheFirstEnqueue(t *testing.T) {
	base := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	events := []github.QueueEvent{
		{Kind: github.QueueEventAdded, CreatedAt: base},
		{Kind: github.QueueEventRemoved, CreatedAt: base.Add(time.Minute)},
	}
	if got := earlierRequeues(events); got != 0 {
		t.Errorf("earlierRequeues() = %d, want 0", got)
	}
}

func TestQueueHelpAndReadinessNameDraft(t *testing.T) {
	if !strings.Contains(queueCommandHelp(), "draft") {
		t.Fatalf("help = %q", queueCommandHelp())
	}
	pr := readyPR()
	pr.Draft = true
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false)
		if err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	if !strings.Contains(output, "pull request is a draft") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "Would enqueue") || strings.Contains(output, "Enqueued") {
		t.Fatalf("refused draft still enqueued:\n%s", output)
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}
}

func TestQueueDryRunDoesNotEnqueue(t *testing.T) {
	withMergeFlags(t, true, false, false)
	pr := readyPR()
	client := &fakeQueue{
		pr:         pr,
		enqueueErr: errors.New("this pull request is part of a stack and must be merged sequentially using merge-async"),
	}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, true); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v, want neither API", client.enqueueCalls, client.mergedAsync)
	}
	if !strings.Contains(output, "Would enqueue acme/widgets#12.") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "Enqueued") {
		t.Fatalf("dry run said Enqueued:\n%s", output)
	}
	for _, want := range []string{
		"pull request is not a draft",
		"no block label",
		"pull request is approved",
		`required check "ci" is green`,
		"no unresolved review threads",
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
}

func TestQueueEnqueueStillPrintsEnqueued(t *testing.T) {
	pr := readyPR()
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if !strings.Contains(output, "Enqueued acme/widgets#12.") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "Would enqueue") {
		t.Fatalf("real enqueue said Would enqueue:\n%s", output)
	}
	if client.enqueueCalls != 1 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}
}

func TestQueueJSONReportsReadinessAndResult(t *testing.T) {
	pr := readyPR()
	t.Run("enqueued", func(t *testing.T) {
		jsonOutput = true
		t.Cleanup(func() { jsonOutput = false })
		client := &fakeQueue{pr: pr}
		output := captureStdout(t, func() {
			if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
		})
		assertQueueJSON(t, output, "enqueued")
		if strings.Contains(output, "Enqueued") {
			t.Fatalf("json output included text:\n%s", output)
		}
	})
	t.Run("would-enqueue", func(t *testing.T) {
		withMergeFlags(t, true, false, false)
		jsonOutput = true
		t.Cleanup(func() { jsonOutput = false })
		client := &fakeQueue{
			pr:         pr,
			enqueueErr: errors.New("stack merge-async sequentially"),
		}
		output := captureStdout(t, func() {
			if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, true); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
		})
		assertQueueJSON(t, output, "would-enqueue")
		if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
			t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
		}
	})
}

func assertQueueJSON(t *testing.T, output, result string) {
	t.Helper()
	var got queueJSONResult
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("json.Unmarshal() error = %v\n%s", err, output)
	}
	if got.PullRequest != "acme/widgets#12" || got.Result != result {
		t.Fatalf("result = %+v", got)
	}
	if len(got.Readiness) == 0 || !containsAll(got.Readiness, "pull request is not a draft", "pull request is approved") {
		t.Fatalf("readiness = %v", got.Readiness)
	}
}

func containsAll(lines []string, wants ...string) bool {
	for _, want := range wants {
		found := false
		for _, line := range lines {
			if line == want {
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}
	return true
}

func TestWatchDryRunDoesNotEnqueue(t *testing.T) {
	withMergeFlags(t, true, false, false)
	pr := readyPR()
	pr.Merged = true
	client := &fakeQueue{pr: pr}
	if err := watchPullRequests(client, []string{pr.Ref.Raw}, watchOptions{timeout: 0}); err != nil {
		t.Fatalf("watchPullRequests() error = %v", err)
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}
}

func TestQueueAndWatchRunWithoutSlackBlock(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "config.yaml")
	content := "github:\n  owner: acme\n  repos:\n    - widgets\nsettings:\n  required_checks:\n    - ci\n"
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg, err := config.LoadConfig(path)
	if err != nil {
		t.Fatalf("LoadConfig() error = %v", err)
	}
	if cfg.Slack.Token != "" || cfg.Slack.DefaultChannel != "" {
		t.Fatalf("slack = %+v, want empty", cfg.Slack)
	}
	client := &fakeQueue{pr: readyPR()}
	if err := queuePullRequest(client, cfg, readyPR().Ref.Raw, false); err != nil {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if len(client.enqueued) != 1 {
		t.Fatalf("enqueued = %v", client.enqueued)
	}
	merged := readyPR()
	merged.Merged = true
	watcher := &fakeQueue{pr: merged}
	if err := watchPullRequests(watcher, []string{merged.Ref.Raw}, watchOptions{timeout: 0}); err != nil {
		t.Fatalf("watchPullRequests() error = %v", err)
	}
	if watcher.enqueueCalls != 0 || len(watcher.mergedAsync) != 0 {
		t.Fatalf("watch enqueueCalls = %d async = %v", watcher.enqueueCalls, watcher.mergedAsync)
	}
}

func TestWatchBareNumberUsesCheckoutRemote(t *testing.T) {
	dir := initGitRemote(t, "origin", "https://github.com/acme/widgets.git")
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name == "gh" {
			t.Errorf("gh was called: %v", args)
			return "", errors.New("gh should not be called")
		}
		return defaultRunCommand(name, args...)
	})
	t.Chdir(dir)
	pr := readyPR()
	pr.Merged = true
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := watchPullRequests(client, []string{"12"}, watchOptions{timeout: 0}); err != nil {
			t.Fatalf("watchPullRequests() error = %v", err)
		}
	})
	if len(client.loadedRefs) != 1 || client.loadedRefs[0] != "acme/widgets#12" {
		t.Fatalf("loaded = %v, want acme/widgets#12", client.loadedRefs)
	}
	if !strings.Contains(output, "acme/widgets#12 merged") {
		t.Fatalf("output = %q", output)
	}
}

func TestWatchExplicitRefDoesNotLookUpRemote(t *testing.T) {
	withCommandStub(t, func(name string, args ...string) (string, error) {
		t.Errorf("unexpected %s %v", name, args)
		return "", errors.New("unexpected command")
	})
	for _, ref := range []string{"acme/widgets#12", "https://github.com/acme/widgets/pull/12"} {
		t.Run(ref, func(t *testing.T) {
			pr := readyPR()
			pr.Merged = true
			client := &fakeQueue{pr: pr}
			if err := watchPullRequests(client, []string{ref}, watchOptions{timeout: 0}); err != nil {
				t.Fatalf("watchPullRequests() error = %v", err)
			}
			if len(client.loadedRefs) != 1 || client.loadedRefs[0] != ref {
				t.Fatalf("loaded = %v, want %s", client.loadedRefs, ref)
			}
		})
	}
}

func TestWatchNoArgumentPrintsUsageWithoutAPI(t *testing.T) {
	client := &fakeQueue{pr: readyPR()}
	err := watchPullRequests(client, nil, watchOptions{})
	if !errors.Is(err, errWatchUsage) {
		t.Fatalf("watchPullRequests() error = %v, want usage", err)
	}
	if client.loads != 0 || len(client.loadedRefs) != 0 {
		t.Fatalf("loads = %d refs = %v", client.loads, client.loadedRefs)
	}
	for _, help := range []string{queueUsage(), watchUsage()} {
		for _, part := range []string{"owner/repo#n", "url", "number"} {
			if !strings.Contains(help, part) {
				t.Errorf("%q missing %s", help, part)
			}
		}
	}
	for _, help := range []string{queueCommandHelp(), watchCommandHelp()} {
		for _, part := range []string{"owner/repo#n", "a URL", "a bare number"} {
			if !strings.Contains(help, part) {
				t.Errorf("%q missing %s", help, part)
			}
		}
	}
}

func TestQueueBareNumberUsesCheckoutRemote(t *testing.T) {
	dir := initGitRemote(t, "origin", "https://github.com/acme/widgets.git")
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name == "gh" {
			t.Errorf("gh was called: %v", args)
			return "", errors.New("gh should not be called")
		}
		return defaultRunCommand(name, args...)
	})
	t.Chdir(dir)
	client := &fakeQueue{pr: readyPR()}
	if err := queuePullRequest(client, queueConfig(), "12", false); err != nil {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if len(client.loadedRefs) != 1 || client.loadedRefs[0] != "acme/widgets#12" {
		t.Fatalf("loaded = %v, want acme/widgets#12", client.loadedRefs)
	}
}

func TestQueueBareNumberUsesFirstRemoteWhenOriginIsMissing(t *testing.T) {
	dir := initGitRemote(t, "upstream", "git@github.com:acme/widgets.git")
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name == "gh" {
			t.Errorf("gh was called: %v", args)
			return "", errors.New("gh should not be called")
		}
		return defaultRunCommand(name, args...)
	})
	t.Chdir(dir)
	client := &fakeQueue{pr: readyPR()}
	if err := queuePullRequest(client, queueConfig(), "12", false); err != nil {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if len(client.loadedRefs) != 1 || client.loadedRefs[0] != "acme/widgets#12" {
		t.Fatalf("loaded = %v, want acme/widgets#12", client.loadedRefs)
	}
}

func TestQueueBareNumberUsesGHWhenRemoteIsNotGitHub(t *testing.T) {
	dir := initGitRemote(t, "origin", "https://example.com/acme/widgets.git")
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name == "gh" {
			return "acme/widgets\n", nil
		}
		return defaultRunCommand(name, args...)
	})
	t.Chdir(dir)
	client := &fakeQueue{pr: readyPR()}
	if err := queuePullRequest(client, queueConfig(), "12", false); err != nil {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if len(client.loadedRefs) != 1 || client.loadedRefs[0] != "acme/widgets#12" {
		t.Fatalf("loaded = %v, want acme/widgets#12", client.loadedRefs)
	}
}

func TestQueueExplicitRefDoesNotLookUpRemote(t *testing.T) {
	withCommandStub(t, func(name string, args ...string) (string, error) {
		t.Errorf("unexpected %s %v", name, args)
		return "", errors.New("unexpected command")
	})
	for _, ref := range []string{"acme/widgets#12", "https://github.com/acme/widgets/pull/12"} {
		t.Run(ref, func(t *testing.T) {
			client := &fakeQueue{pr: readyPR()}
			if err := queuePullRequest(client, queueConfig(), ref, false); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
			if len(client.loadedRefs) != 1 || client.loadedRefs[0] != ref {
				t.Fatalf("loaded = %v, want %s", client.loadedRefs, ref)
			}
		})
	}
}

func TestQueueUnknownRepositoryRefuses(t *testing.T) {
	pr := readyPR()
	cfg := queueConfig()
	cfg.GitHub.Repos = []string{"other"}
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	if !strings.Contains(output, "acme/widgets is not a configured repository") {
		t.Fatalf("output = %s", output)
	}
	if len(client.enqueued) != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueued = %v async = %v", client.enqueued, client.mergedAsync)
	}

	withAllowNoRequiredChecks(t, true)
	allowed := &fakeQueue{pr: pr}
	if err := queuePullRequest(allowed, cfg, pr.Ref.Raw, false); err != nil {
		t.Fatalf("allowed queuePullRequest() error = %v", err)
	}
	if len(allowed.enqueued) != 1 {
		t.Fatalf("enqueued = %v, want one", allowed.enqueued)
	}
}

func TestQueueAllowFlagStillRefusesDraft(t *testing.T) {
	withAllowNoRequiredChecks(t, true)
	pr := readyPR()
	pr.Draft = true
	pr.Checks = []github.QueueCheck{{Name: "ci", Green: false, Conclusion: "FAILURE"}}
	cfg := queueConfig()
	cfg.Settings.RequiredChecks = nil
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want draft refusal")
		}
	})
	if !strings.Contains(output, "pull request is a draft") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "Enqueued") || strings.Contains(output, "Would enqueue") {
		t.Fatalf("draft was enqueued:\n%s", output)
	}
	if len(client.enqueued) != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueued = %v async = %v", client.enqueued, client.mergedAsync)
	}
}

func TestQueueNamesEachConfigGate(t *testing.T) {
	pr := readyPR()
	cfg := queueConfig()
	cfg.Settings.RequiredChecks = nil
	cfg.GitHub.Repos = []string{"other"}
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	for _, want := range []string{
		"required_checks is empty",
		"acme/widgets is not a configured repository",
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	if len(client.enqueued) != 0 {
		t.Fatalf("enqueued = %v", client.enqueued)
	}
}

func TestQueueReadinessShowsEffectiveConfig(t *testing.T) {
	cfg := queueConfig()
	cfg.Path = "/tmp/loaded.pkl"
	cfg.Settings.BlockMergeLabels = []string{"do-not-merge"}
	cfg.Settings.RequiredChecks = []string{"ci"}
	pr := readyPR()

	withMergeFlags(t, true, false, false)
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	for _, want := range []string{
		"config: /tmp/loaded.pkl",
		"block_merge_labels: do-not-merge",
		"required_checks: ci",
		"Would enqueue acme/widgets#12.",
	} {
		if !strings.Contains(output, want) {
			t.Errorf("dry run missing %q:\n%s", want, output)
		}
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}

	jsonOutput = true
	t.Cleanup(func() { jsonOutput = false })
	jsonClient := &fakeQueue{pr: pr}
	jsonOut := captureStdout(t, func() {
		if err := queuePullRequest(jsonClient, cfg, pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	assertEffectiveConfigJSON(t, jsonOut, "would-enqueue", "/tmp/loaded.pkl", []string{"do-not-merge"}, []string{"ci"})
	if jsonClient.enqueueCalls != 0 {
		t.Fatalf("json dry run enqueued %d", jsonClient.enqueueCalls)
	}

	cfg.Settings.RequiredChecks = nil
	jsonOutput = false
	refused := &fakeQueue{pr: pr}
	refusedOut := captureStdout(t, func() {
		if err := queuePullRequest(refused, cfg, pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	for _, want := range []string{
		"config: /tmp/loaded.pkl",
		"block_merge_labels: do-not-merge",
		"required_checks: (none)",
		"required_checks is empty",
	} {
		if !strings.Contains(refusedOut, want) {
			t.Errorf("refusal missing %q:\n%s", want, refusedOut)
		}
	}
	if len(refused.enqueued) != 0 {
		t.Fatalf("enqueued = %v", refused.enqueued)
	}

	jsonOutput = true
	jsonRefused := &fakeQueue{pr: pr}
	jsonRefusedOut := captureStdout(t, func() {
		if err := queuePullRequest(jsonRefused, cfg, pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	assertEffectiveConfigJSON(t, jsonRefusedOut, "refused", "/tmp/loaded.pkl", []string{"do-not-merge"}, []string{})
}

func assertEffectiveConfigJSON(t *testing.T, output, result, path string, labels, checks []string) {
	t.Helper()
	var got queueJSONResult
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("json.Unmarshal() error = %v\n%s", err, output)
	}
	if got.Result != result || got.Config != path {
		t.Fatalf("result = %+v", got)
	}
	if strings.Join(got.BlockMergeLabels, ",") != strings.Join(labels, ",") {
		t.Fatalf("blockMergeLabels = %#v", got.BlockMergeLabels)
	}
	if strings.Join(got.RequiredChecks, ",") != strings.Join(checks, ",") {
		t.Fatalf("requiredChecks = %#v", got.RequiredChecks)
	}
	if !strings.Contains(output, `"blockMergeLabels"`) || !strings.Contains(output, `"requiredChecks"`) || !strings.Contains(output, `"config"`) {
		t.Fatalf("json missing config fields:\n%s", output)
	}
}

func TestResolveMergeConfigPath(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("GH_MERGE_CONFIG", "")
	nudge := filepath.Join(home, ".config", "gh-nudge")
	merge := filepath.Join(home, ".config", "gh-merge")
	if err := os.MkdirAll(nudge, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(merge, 0o755); err != nil {
		t.Fatal(err)
	}
	nudgePkl := filepath.Join(nudge, "config.pkl")
	mergePkl := filepath.Join(merge, "config.pkl")
	if err := os.WriteFile(nudgePkl, []byte("pkl"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(mergePkl, []byte("pkl"), 0o600); err != nil {
		t.Fatal(err)
	}

	got, err := resolveMergeConfigPath("/flag.pkl")
	if err != nil || got != "/flag.pkl" {
		t.Fatalf("flag path = %q, %v", got, err)
	}
	t.Setenv("GH_MERGE_CONFIG", "/env.pkl")
	got, err = resolveMergeConfigPath("")
	if err != nil || got != "/env.pkl" {
		t.Fatalf("env path = %q, %v", got, err)
	}
	t.Setenv("GH_MERGE_CONFIG", "")
	got, err = resolveMergeConfigPath("")
	if err != nil || got != mergePkl {
		t.Fatalf("merge pkl = %q, want %q, err %v", got, mergePkl, err)
	}
	if err := os.Remove(mergePkl); err != nil {
		t.Fatal(err)
	}
	got, err = resolveMergeConfigPath("")
	if err != nil || got != nudgePkl {
		t.Fatalf("nudge pkl = %q, want %q, err %v", got, nudgePkl, err)
	}
	if err := os.Remove(nudgePkl); err != nil {
		t.Fatal(err)
	}
	wantYAML := filepath.Join(nudge, "config.yaml")
	got, err = resolveMergeConfigPath("")
	if err != nil || got != wantYAML {
		t.Fatalf("nudge yaml = %q, want %q, err %v", got, wantYAML, err)
	}
}

func TestParseGitHubRemote(t *testing.T) {
	tests := []struct {
		raw   string
		owner string
		repo  string
		ok    bool
	}{
		{raw: "https://github.com/acme/widgets.git", owner: "acme", repo: "widgets", ok: true},
		{raw: "https://github.com/acme/widgets", owner: "acme", repo: "widgets", ok: true},
		{raw: "git@github.com:acme/widgets.git", owner: "acme", repo: "widgets", ok: true},
		{raw: "ssh://git@github.com/acme/widgets.git", owner: "acme", repo: "widgets", ok: true},
		{raw: "https://example.com/acme/widgets.git", ok: false},
		{raw: "https://github.com/acme/widgets/pull/12", ok: false},
	}
	for _, tc := range tests {
		t.Run(tc.raw, func(t *testing.T) {
			owner, repo, ok := parseGitHubRemote(tc.raw)
			if ok != tc.ok || owner != tc.owner || repo != tc.repo {
				t.Fatalf("parseGitHubRemote() = %s/%s ok=%v", owner, repo, ok)
			}
		})
	}
}

func initGitRemote(t *testing.T, name, remote string) string {
	t.Helper()
	dir := t.TempDir()
	gitIn(t, dir, "init")
	gitIn(t, dir, "remote", "add", name, remote)
	return dir
}

func gitIn(t *testing.T, dir string, args ...string) {
	t.Helper()
	cmd := exec.Command("git", args...)
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %s: %v\n%s", strings.Join(args, " "), err, out)
	}
}

func withCommandStub(t *testing.T, fn func(string, ...string) (string, error)) {
	t.Helper()
	prev := runCommand
	runCommand = fn
	t.Cleanup(func() { runCommand = prev })
}
