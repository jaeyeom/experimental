package main

import (
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

type fakeQueue struct {
	pr          github.QueuePullRequest
	byRef       map[string]github.QueuePullRequest
	loadErr     error
	enqueueErr  error
	enqueued    []string
	downstack   []github.StackPR
	downCalls   int
	evidence    github.FailureEvidence
	landed      github.StackPR
	landedOK    bool
	mergedAsync []string
	loads       int
	snapshots   []github.QueuePullRequest
}

func (f *fakeQueue) LoadPullRequest(ref string) (github.QueuePullRequest, error) {
	f.loads++
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
	cfg := &config.Config{Settings: queueSettings()}

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

func TestQueueEmptyRequiredChecksSkipsCheckGate(t *testing.T) {
	pr := readyPR()
	pr.Checks = []github.QueueCheck{{Name: "ci", Green: false, Conclusion: "FAILURE"}}
	client := &fakeQueue{pr: pr}
	cfg := &config.Config{Settings: config.SettingsConfig{}}

	if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err != nil {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if len(client.enqueued) != 1 || client.enqueued[0] != "PR_NODE" {
		t.Errorf("enqueued = %v, want PR_NODE", client.enqueued)
	}
	if len(client.mergedAsync) != 0 {
		t.Errorf("mergedAsync = %v, want none", client.mergedAsync)
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
	cfg := &config.Config{Settings: settings}

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
			settings := config.SettingsConfig{RCAMarker: marker}
			if tc.limit != 0 {
				limit := tc.limit
				settings.MaxRequeues = &limit
			}
			cfg := &config.Config{Settings: settings}
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
			err := queuePullRequest(client, &config.Config{}, pr.Ref.Raw, false)
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
		if err := queuePullRequest(client, &config.Config{}, pr.Ref.Raw, true); err != nil {
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
	err := queuePullRequest(client, &config.Config{}, pr.Ref.Raw, true)
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
