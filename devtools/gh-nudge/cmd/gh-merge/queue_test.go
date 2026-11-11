package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
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
	above        []github.StackPR
	aboveErr     error
	native       bool
	nativeErr    error
	byBase       map[string][]github.BranchPull
	byHead       map[string][]github.BranchPull
	baseErr      error
	headErr      error
	downCalls    int
	evidence     github.FailureEvidence
	comments     []string
	commentCalls int
	commentRef   string
	dequeueCalls int
	dequeued     []string
	landed       github.StackPR
	landedOK     bool
	mergedAsync  []string
	loads        int
	snapshots    []github.QueuePullRequest
	loadErrs     []error
}

func (f *fakeQueue) LoadPullRequest(ref string) (github.QueuePullRequest, error) {
	f.loads++
	f.loadedRefs = append(f.loadedRefs, ref)
	if index := f.loads - 1; index < len(f.loadErrs) && f.loadErrs[index] != nil {
		return github.QueuePullRequest{}, f.loadErrs[index]
	}
	if f.loadErr != nil {
		return github.QueuePullRequest{}, f.loadErr
	}
	var pr github.QueuePullRequest
	switch {
	case len(f.snapshots) > 0:
		index := f.loads - 1
		if index >= len(f.snapshots) {
			index = len(f.snapshots) - 1
		}
		pr = f.snapshots[index]
	case f.byRef != nil:
		var ok bool
		pr, ok = f.byRef[ref]
		if !ok {
			return github.QueuePullRequest{}, errors.New("unknown pull request")
		}
	default:
		pr = f.pr
	}
	if pr.Ref.Raw == "" {
		pr.Ref.Raw = ref
	}
	if len(f.comments) > 0 {
		pr.Comments = append(append([]string{}, pr.Comments...), f.comments...)
	}
	return pr, nil
}

func (f *fakeQueue) DequeuePullRequest(nodeID string) error {
	f.dequeueCalls++
	f.dequeued = append(f.dequeued, nodeID)
	return nil
}

func (f *fakeQueue) CommentOnPullRequest(owner, repo string, number int, body string) error {
	f.commentCalls++
	f.commentRef = fmt.Sprintf("%s/%s#%d", owner, repo, number)
	f.comments = append(f.comments, body)
	return nil
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

func (f *fakeQueue) OpenAbove(_, _ string, _ int) ([]github.StackPR, error) {
	if f.aboveErr != nil {
		return nil, f.aboveErr
	}
	return f.above, nil
}

func (f *fakeQueue) InNativeStack(_, _ string, _ int) (bool, error) {
	if f.nativeErr != nil {
		return false, f.nativeErr
	}
	return f.native, nil
}

func (f *fakeQueue) OpenByBase(_, _, base string) ([]github.BranchPull, error) {
	if f.baseErr != nil {
		return nil, f.baseErr
	}
	if f.byBase == nil {
		return nil, nil
	}
	return f.byBase[base], nil
}

func (f *fakeQueue) OpenByHead(_, _, head string) ([]github.BranchPull, error) {
	if f.headErr != nil {
		return nil, f.headErr
	}
	if f.byHead == nil {
		return nil, nil
	}
	return f.byHead[head], nil
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

func TestQueueCircleCIPrintsFailedStepAndPattern(t *testing.T) {
	t.Setenv(circleTokenEnv, "token")
	logText := "ERROR: setup failed\n"
	for i := 0; i < 25; i++ {
		logText += fmt.Sprintf("line %d\n", i)
	}
	encoded, err := json.Marshal([]struct {
		Message string `json:"message"`
	}{{Message: logText}})
	if err != nil {
		t.Fatal(err)
	}
	var calls []string
	withCircleGet(t, func(_ context.Context, rawURL, token string) ([]byte, error) {
		if token != "token" {
			t.Fatalf("token = %q", token)
		}
		calls = append(calls, rawURL)
		switch {
		case strings.HasSuffix(rawURL, "/project/gh/acme/widgets/pipeline/42"):
			return []byte(`{"id":"pipe-1"}`), nil
		case strings.HasSuffix(rawURL, "/pipeline/pipe-1/workflow"):
			return []byte(`{"items":[{"id":"wf-ok","status":"success"},{"id":"wf-2","status":"failed"}]}`), nil
		case strings.HasSuffix(rawURL, "/workflow/wf-ok/job"):
			return []byte(`{"items":[{"job_number":1,"status":"success"}]}`), nil
		case strings.HasSuffix(rawURL, "/workflow/wf-2/job"):
			return []byte(`{"items":[{"job_number":7,"status":"success"},{"job_number":8,"status":"failed"}]}`), nil
		case strings.HasSuffix(rawURL, "/project/gh/acme/widgets/8"):
			return []byte(`{"steps":[{"actions":[{"status":"success","output_url":"https://circleci.com/output/ok"},{"status":"failed","output_url":"https://circleci.com/output/8"}]}]}`), nil
		case rawURL == "https://circleci.com/output/8":
			return encoded, nil
		default:
			t.Fatalf("unexpected circleci url %s", rawURL)
			return nil, nil
		}
	})

	pr := removedPR()
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:      "ci/circleci",
		TargetURL: "https://app.circleci.com/pipelines/github/acme/widgets/42",
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want RCA refusal")
		}
	})
	wantCalls := []string{
		"https://circleci.com/api/v2/project/gh/acme/widgets/pipeline/42",
		"https://circleci.com/api/v2/pipeline/pipe-1/workflow",
		"https://circleci.com/api/v2/workflow/wf-ok/job",
		"https://circleci.com/api/v2/workflow/wf-2/job",
		"https://circleci.com/api/v1.1/project/gh/acme/widgets/8",
		"https://circleci.com/output/8",
	}
	if strings.Join(calls, "\n") != strings.Join(wantCalls, "\n") {
		t.Fatalf("calls = %v", calls)
	}
	for _, want := range []string{"ci/circleci https://app.circleci.com/pipelines/github/acme/widgets/42", "line 24", "ERROR: setup failed"} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	if strings.Contains(output, "line 0\n") || strings.Contains(output, "Failing step log tail:") {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueCircleCIUsesConfiguredPattern(t *testing.T) {
	t.Setenv(circleTokenEnv, "token")
	logText := "ERROR: ignore\n"
	for i := 0; i < 25; i++ {
		logText += fmt.Sprintf("line %d\n", i)
	}
	logText = "FATAL: disk\n" + logText
	encoded, err := json.Marshal([]struct {
		Message string `json:"message"`
	}{{Message: logText}})
	if err != nil {
		t.Fatal(err)
	}
	withCircleGet(t, func(_ context.Context, rawURL, _ string) ([]byte, error) {
		switch {
		case strings.HasSuffix(rawURL, "/pipeline/42"):
			return []byte(`{"id":"pipe-1"}`), nil
		case strings.HasSuffix(rawURL, "/workflow"):
			return []byte(`{"items":[{"id":"wf-2","status":"failed"}]}`), nil
		case strings.HasSuffix(rawURL, "/job"):
			return []byte(`{"items":[{"job_number":8,"status":"failed"}]}`), nil
		case strings.HasSuffix(rawURL, "/widgets/8"):
			return []byte(`{"steps":[{"actions":[{"status":"failed","output_url":"https://circleci.com/output/8"}]}]}`), nil
		default:
			return encoded, nil
		}
	})
	pr := removedPR()
	cfg := queueConfig()
	cfg.Settings.FailureLogPattern = "^FATAL:"
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:      "ci/circleci",
		TargetURL: "https://app.circleci.com/pipelines/github/acme/widgets/42",
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil")
		}
	})
	if !strings.Contains(output, "FATAL: disk") || !strings.Contains(output, "line 24") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "ERROR: ignore") {
		t.Fatalf("pattern did not filter:\n%s", output)
	}
}

func TestQueueCircleCIJobNumberResolvesWorkflow(t *testing.T) {
	t.Setenv(circleTokenEnv, "token")
	const workflowID = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
	const jobLink = "https://circleci.com/gh/acme/widgets/4242"
	encoded, err := json.Marshal([]struct {
		Message string `json:"message"`
	}{{Message: "ERROR: setup failed\n"}})
	if err != nil {
		t.Fatal(err)
	}
	var calls []string
	withCircleGet(t, func(_ context.Context, rawURL, token string) ([]byte, error) {
		if token != "token" {
			t.Fatalf("token = %q", token)
		}
		calls = append(calls, rawURL)
		switch rawURL {
		case "https://circleci.com/api/v2/project/gh/acme/widgets/job/4242":
			return []byte(`{"latest_workflow":{"id":"` + workflowID + `"}}`), nil
		case "https://circleci.com/api/v2/workflow/" + workflowID + "/job":
			return []byte(`{"items":[{"job_number":8,"status":"failed"}]}`), nil
		case "https://circleci.com/api/v1.1/project/gh/acme/widgets/8":
			return []byte(`{"steps":[{"actions":[{"status":"failed","output_url":"https://circleci.com/output/8"}]}]}`), nil
		case "https://circleci.com/output/8":
			return encoded, nil
		default:
			t.Fatalf("unexpected circleci url %s", rawURL)
			return nil, nil
		}
	})
	pr := removedPR()
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:      "ci/circleci",
		TargetURL: jobLink,
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want RCA refusal")
		}
	})
	wantCalls := []string{
		"https://circleci.com/api/v2/project/gh/acme/widgets/job/4242",
		"https://circleci.com/api/v2/workflow/" + workflowID + "/job",
		"https://circleci.com/api/v1.1/project/gh/acme/widgets/8",
		"https://circleci.com/output/8",
	}
	if strings.Join(calls, "\n") != strings.Join(wantCalls, "\n") {
		t.Fatalf("calls = %v", calls)
	}
	for _, call := range calls {
		if strings.Contains(call, "/pipeline/") {
			t.Fatalf("job number treated as a pipeline: %s", call)
		}
	}
	if !strings.Contains(output, "ERROR: setup failed") {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueRemovalResolvesStatusTargetNotCancelledCheck(t *testing.T) {
	t.Setenv(circleTokenEnv, "token")
	const workflowID = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
	const otherWorkflow = "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
	const otherURL = "https://app.circleci.com/pipelines/github/acme/widgets/42/workflows/" + otherWorkflow
	const jobLink = "https://circleci.com/gh/acme/widgets/4242"
	encoded, err := json.Marshal([]struct {
		Message string `json:"message"`
	}{{Message: "ERROR: unit failed\n"}})
	if err != nil {
		t.Fatal(err)
	}
	var calls []string
	withCircleGet(t, func(_ context.Context, rawURL, token string) ([]byte, error) {
		if token != "token" {
			t.Fatalf("token = %q", token)
		}
		calls = append(calls, rawURL)
		switch rawURL {
		case "https://circleci.com/api/v2/project/gh/acme/widgets/job/4242":
			return []byte(`{"latest_workflow":{"id":"` + workflowID + `"}}`), nil
		case "https://circleci.com/api/v2/workflow/" + workflowID + "/job":
			return []byte(`{"items":[{"job_number":8,"status":"failed"}]}`), nil
		case "https://circleci.com/api/v1.1/project/gh/acme/widgets/8":
			return []byte(`{"steps":[{"actions":[{"status":"failed","output_url":"https://circleci.com/output/8"}]}]}`), nil
		case "https://circleci.com/output/8":
			return encoded, nil
		default:
			t.Fatalf("unexpected circleci url %s", rawURL)
			return nil, nil
		}
	})
	pr := removedPR()
	gh := github.NewClientWithExecutor(ghRoute{routes: map[string]string{
		"check-runs": `{
			"check_runs":[{
				"name":"build",
				"conclusion":"cancelled",
				"html_url":"https://github.com/acme/widgets/runs/9",
				"details_url":"` + otherURL + `"
			}]
		}`,
		"commits/deadbeef/status": `{
			"statuses":[{
				"context":"ci/circleci: unit",
				"state":"failure",
				"target_url":"` + jobLink + `"
			}]
		}`,
	}})
	client := &rankedQueue{fakeQueue: &fakeQueue{pr: pr}, gh: gh}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want RCA refusal")
		}
	})
	wantCalls := []string{
		"https://circleci.com/api/v2/project/gh/acme/widgets/job/4242",
		"https://circleci.com/api/v2/workflow/" + workflowID + "/job",
		"https://circleci.com/api/v1.1/project/gh/acme/widgets/8",
		"https://circleci.com/output/8",
	}
	if strings.Join(calls, "\n") != strings.Join(wantCalls, "\n") {
		t.Fatalf("calls = %v", calls)
	}
	for _, call := range calls {
		if strings.Contains(call, otherWorkflow) {
			t.Fatalf("requested the cancelled check run workflow: %s", call)
		}
	}
	if !strings.Contains(output, "ERROR: unit failed") || strings.Contains(output, "no failed job") || strings.Contains(output, otherWorkflow) {
		t.Fatalf("output = %s", output)
	}
}

type ghRoute struct {
	routes map[string]string
}

func (g ghRoute) Execute(_ string, args ...string) (string, error) {
	joined := strings.Join(args, " ")
	best := ""
	for key := range g.routes {
		if strings.Contains(joined, key) && len(key) > len(best) {
			best = key
		}
	}
	if best == "" {
		return "", fmt.Errorf("unexpected gh call: %s", joined)
	}
	return g.routes[best], nil
}

func (g ghRoute) ExecuteWithStdin(_, _ string, _ ...string) (string, error) {
	return "", errors.New("unexpected gh stdin")
}

type rankedQueue struct {
	*fakeQueue
	gh *github.Client
}

func (q *rankedQueue) FailureEvidence(owner, repo, sha string) (github.FailureEvidence, error) {
	evidence, err := q.gh.FailureEvidence(owner, repo, sha)
	if err != nil {
		return github.FailureEvidence{}, fmt.Errorf("failure evidence: %w", err)
	}
	return evidence, nil
}

func TestQueueCircleCIMissingToken(t *testing.T) {
	t.Setenv(circleTokenEnv, "")
	withCircleCLI(t, false)
	withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
		t.Fatal("circleci request without a token")
		return nil, nil
	})
	pr := removedPR()
	target := "https://app.circleci.com/pipelines/github/acme/widgets/42"
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:      "ci/circleci",
		TargetURL: target,
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil")
		}
	})
	for _, want := range []string{
		"ci/circleci",
		target,
		"log unavailable: no CircleCI token and circleci CLI is not on PATH",
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	if strings.TrimSpace(output) == "log unavailable: no CircleCI token" || strings.Contains(output, "Failing step log tail:") {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueCircleCLIPrintsTailAndTimestampPattern(t *testing.T) {
	t.Setenv(circleTokenEnv, "")
	withCircleCLI(t, true)
	withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
		t.Fatal("circleci http api was called")
		return nil, nil
	})
	const workflowID = "11111111-1111-1111-1111-111111111111"
	const jobID = "6bde3c42-8312-4297-be30-57e10dd84d18"
	logText := "(12:34:56) ERROR: setup failed\n"
	for i := 0; i < 25; i++ {
		logText += fmt.Sprintf("line %d\n", i)
	}
	outputBody, err := json.Marshal(map[string]any{
		"steps": []map[string]string{
			{"outcome": "succeeded", "output": "ok\n"},
			{"outcome": "failed", "output": logText},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name != "circleci" {
			t.Fatalf("command = %s %v", name, args)
		}
		switch {
		case len(args) >= 2 && args[0] == "workflow" && args[1] == "get":
			if !circleArgIs(args, workflowID) {
				t.Fatalf("args = %v", args)
			}
			return `{"jobs":[{"id":"ok-job","outcome":"succeeded"},{"id":"` + jobID + `","outcome":"failed"}]}`, nil
		case len(args) >= 2 && args[0] == "job" && args[1] == "get":
			if !circleArgIs(args, jobID) {
				t.Fatalf("args = %v", args)
			}
			return `{"status":"failed"}`, nil
		case len(args) >= 3 && args[0] == "job" && args[1] == "output" && args[2] == "list":
			if !circleArgIs(args, jobID) {
				t.Fatalf("args = %v", args)
			}
			return string(outputBody), nil
		default:
			t.Fatalf("args = %v", args)
			return "", nil
		}
	})
	pr := removedPR()
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:      "ci/circleci",
		TargetURL: "https://app.circleci.com/pipelines/github/acme/widgets/42/workflows/" + workflowID,
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil")
		}
	})
	for _, want := range []string{"line 24", "(12:34:56) ERROR: setup failed"} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	if strings.Contains(output, "line 0\n") {
		t.Fatalf("output included lines outside the tail:\n%s", output)
	}
}

func TestQueueCircleCLILegacyTargetUsesCheckRunWorkflow(t *testing.T) {
	t.Setenv(circleTokenEnv, "")
	withCircleCLI(t, true)
	withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
		t.Fatal("circleci http api was called")
		return nil, nil
	})
	const workflowID = "11111111-1111-1111-1111-111111111111"
	const jobID = "6bde3c42-8312-4297-be30-57e10dd84d18"
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name != "circleci" {
			t.Fatalf("command = %s %v", name, args)
		}
		for _, arg := range args {
			if arg == "4242" || strings.Contains(arg, "4242") {
				t.Fatalf("cli invoked with build number: %v", args)
			}
		}
		switch {
		case len(args) >= 2 && args[0] == "workflow" && args[1] == "get":
			if !circleArgIs(args, workflowID) {
				t.Fatalf("args = %v", args)
			}
			return `{"jobs":[{"id":"` + jobID + `","outcome":"failed"}]}`, nil
		case len(args) >= 3 && args[0] == "job":
			return `{"steps":[{"outcome":"failed","output":"(12:34:56) ERROR: setup failed\n"}]}`, nil
		default:
			t.Fatalf("args = %v", args)
			return "", nil
		}
	})
	pr := removedPR()
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:        "ci/circleci",
		TargetURL:   "https://circleci.com/gh/acme/widgets/4242",
		CheckRunURL: "https://app.circleci.com/pipelines/github/acme/widgets/42/workflows/" + workflowID,
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil")
		}
	})
	if !strings.Contains(output, "(12:34:56) ERROR: setup failed") {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueCircleCLILegacyTargetUsesWorkflowUUIDAndJobName(t *testing.T) {
	const workflowID = "11111111-1111-1111-1111-111111111111"
	const unitJob = "6bde3c42-8312-4297-be30-57e10dd84d18"
	const otherJob = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
	const buildLink = "https://circleci.com/gh/acme/widgets/4242"
	for _, path := range []string{"workflow", "workflows"} {
		t.Run(path, func(t *testing.T) {
			t.Setenv(circleTokenEnv, "")
			withCircleCLI(t, true)
			withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
				t.Fatal("circleci http api was called")
				return nil, nil
			})
			withCommandStub(t, func(name string, args ...string) (string, error) {
				if name != "circleci" {
					t.Fatalf("command = %s %v", name, args)
				}
				if len(args) > 0 && args[0] == "pipeline" {
					t.Fatalf("pipeline command: %v", args)
				}
				for _, arg := range args {
					if strings.Contains(arg, "4242") || arg == otherJob {
						t.Fatalf("cli args = %v", args)
					}
				}
				switch {
				case len(args) >= 2 && args[0] == "workflow" && args[1] == "get":
					if !circleArgIs(args, workflowID) {
						t.Fatalf("args = %v", args)
					}
					return `{"jobs":[{"id":"` + otherJob + `","name":"lint","outcome":"failed"},{"id":"` + unitJob + `","name":"unit","outcome":"failed"}]}`, nil
				case len(args) >= 2 && args[0] == "job" && args[1] == "get":
					if !circleArgIs(args, unitJob) {
						t.Fatalf("args = %v", args)
					}
					return `{"status":"failed"}`, nil
				case len(args) >= 3 && args[0] == "job" && args[1] == "output" && args[2] == "list":
					if !circleArgIs(args, unitJob) {
						t.Fatalf("args = %v", args)
					}
					return `{"steps":[{"outcome":"failed","output":"(12:34:56) ERROR: setup failed\n"}]}`, nil
				default:
					t.Fatalf("args = %v", args)
					return "", nil
				}
			})
			pr := removedPR()
			client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
				Name:        "ci/circleci: unit",
				TargetURL:   buildLink,
				CheckRunURL: "https://app.circleci.com/pipelines/github/acme/widgets/42/" + path + "/" + workflowID,
			}}
			output := captureStdout(t, func() {
				if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
					t.Fatal("queuePullRequest() error = nil")
				}
			})
			if !strings.Contains(output, "(12:34:56) ERROR: setup failed") {
				t.Fatalf("output = %s", output)
			}
		})
	}
}

func TestQueueCircleCLIMissingWorkflowPrintsNotFound(t *testing.T) {
	t.Setenv(circleTokenEnv, "")
	withCircleCLI(t, true)
	withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
		t.Fatal("circleci http api was called")
		return nil, nil
	})
	withCommandStub(t, func(name string, args ...string) (string, error) {
		t.Fatalf("command = %s %v", name, args)
		return "", nil
	})
	pr := removedPR()
	target := "https://circleci.com/gh/acme/widgets/4242"
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:      "ci/circleci: unit",
		TargetURL: target,
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil")
		}
	})
	for _, want := range []string{target, "workflow not found"} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	if strings.Contains(output, "unrecognized CircleCI pipeline URL") || strings.Contains(output, "pipeline workflows") {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueCircleTokenKeepsPipelineListing(t *testing.T) {
	t.Setenv(circleTokenEnv, "token")
	withCircleCLI(t, true)
	withCommandStub(t, func(name string, args ...string) (string, error) {
		t.Fatalf("command = %s %v", name, args)
		return "", nil
	})
	withCircleGet(t, func(_ context.Context, rawURL, token string) ([]byte, error) {
		if token != "token" {
			t.Fatalf("token = %q", token)
		}
		switch {
		case strings.HasSuffix(rawURL, "/project/gh/acme/widgets/pipeline/42"):
			return []byte(`{"id":"pipe-1"}`), nil
		case strings.HasSuffix(rawURL, "/pipeline/pipe-1/workflow"):
			return []byte(`{"items":[{"id":"wf-2","status":"failed"}]}`), nil
		case strings.HasSuffix(rawURL, "/workflow/wf-2/job"):
			return []byte(`{"items":[{"job_number":8,"status":"failed"}]}`), nil
		case strings.HasSuffix(rawURL, "/project/gh/acme/widgets/8"):
			return []byte(`{"steps":[{"actions":[{"status":"failed","output_url":"https://circleci.com/output/8"}]}]}`), nil
		case rawURL == "https://circleci.com/output/8":
			return []byte(`[{"message":"ERROR: setup failed\n"}]`), nil
		default:
			t.Fatalf("unexpected circleci url %s", rawURL)
			return nil, nil
		}
	})
	pr := removedPR()
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:      "ci/circleci",
		TargetURL: "https://app.circleci.com/pipelines/github/acme/widgets/42",
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil")
		}
	})
	if !strings.Contains(output, "ERROR: setup failed") {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueCirclePipelineCollectsFailedJobs(t *testing.T) {
	const pipeline = "https://app.circleci.com/pipelines/github/acme/widgets/42"
	tests := []struct {
		name      string
		check     string
		workflows string
		jobs      map[string]string
		wantJob   string
		wantLog   string
		absentLog string
	}{
		{
			name:      "later workflow",
			check:     "ci/circleci",
			workflows: `{"items":[{"id":"wf-empty","name":"setup","status":"failed"},{"id":"wf-real","name":"test","status":"failed"}]}`,
			jobs: map[string]string{
				"wf-empty": `{"items":[{"job_number":7,"name":"setup","status":"success"}]}`,
				"wf-real":  `{"items":[{"job_number":9,"name":"unit","status":"failed"}]}`,
			},
			wantJob:   "9",
			wantLog:   "ERROR: second workflow",
			absentLog: "no failed job",
		},
		{
			name:      "job name",
			check:     "ci/circleci: unit",
			workflows: `{"items":[{"id":"wf-a","name":"lint-wf","status":"failed"},{"id":"wf-b","name":"test-wf","status":"failed"}]}`,
			jobs: map[string]string{
				"wf-a": `{"items":[{"job_number":7,"name":"lint","status":"failed"}]}`,
				"wf-b": `{"items":[{"job_number":8,"name":"unit","status":"failed"}]}`,
			},
			wantJob:   "8",
			wantLog:   "ERROR: unit job",
			absentLog: "ERROR: lint job",
		},
		{
			name:      "workflow name",
			check:     "ci/circleci: build",
			workflows: `{"items":[{"id":"wf-test","name":"test","status":"failed"},{"id":"wf-build","name":"build","status":"failed"}]}`,
			jobs: map[string]string{
				"wf-test":  `{"items":[{"job_number":4,"name":"unit","status":"failed"}]}`,
				"wf-build": `{"items":[{"job_number":3,"name":"compile","status":"failed"}]}`,
			},
			wantJob:   "3",
			wantLog:   "ERROR: build workflow",
			absentLog: "ERROR: test workflow",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv(circleTokenEnv, "token")
			var fetched []string
			withCircleGet(t, func(_ context.Context, rawURL, token string) ([]byte, error) {
				if token != "token" {
					t.Fatalf("token = %q", token)
				}
				switch {
				case strings.HasSuffix(rawURL, "/project/gh/acme/widgets/pipeline/42"):
					return []byte(`{"id":"pipe-1"}`), nil
				case strings.HasSuffix(rawURL, "/pipeline/pipe-1/workflow"):
					return []byte(tc.workflows), nil
				case strings.Contains(rawURL, "/workflow/") && strings.HasSuffix(rawURL, "/job"):
					id := strings.TrimSuffix(strings.TrimPrefix(rawURL, "https://circleci.com/api/v2/workflow/"), "/job")
					body, ok := tc.jobs[id]
					if !ok {
						t.Fatalf("unexpected workflow jobs %s", rawURL)
					}
					return []byte(body), nil
				case strings.Contains(rawURL, "/project/gh/acme/widgets/"):
					job := strings.TrimPrefix(rawURL, "https://circleci.com/api/v1.1/project/gh/acme/widgets/")
					fetched = append(fetched, job)
					return []byte(`{"steps":[{"actions":[{"status":"failed","output_url":"https://circleci.com/output/` + job + `"}]}]}`), nil
				case strings.HasPrefix(rawURL, "https://circleci.com/output/"):
					job := strings.TrimPrefix(rawURL, "https://circleci.com/output/")
					return []byte(`[{"message":"` + tc.wantLog + ` for ` + job + `\n"}]`), nil
				default:
					t.Fatalf("unexpected circleci url %s", rawURL)
					return nil, nil
				}
			})
			pr := removedPR()
			client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
				Name:      tc.check,
				TargetURL: pipeline,
			}}
			output := captureStdout(t, func() {
				if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
					t.Fatal("queuePullRequest() error = nil")
				}
			})
			if strings.Join(fetched, ",") != tc.wantJob {
				t.Fatalf("fetched jobs = %v, want %s", fetched, tc.wantJob)
			}
			if !strings.Contains(output, tc.wantLog) || strings.Contains(output, tc.absentLog) {
				t.Fatalf("output = %s", output)
			}
		})
	}
}

func TestQueueCircleUnparsedTargetUsesWorkflowUUID(t *testing.T) {
	t.Setenv(circleTokenEnv, "token")
	withCircleCLI(t, true)
	withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
		t.Fatal("circleci http api was called")
		return nil, nil
	})
	const workflowID = "11111111-1111-1111-1111-111111111111"
	const jobID = "6bde3c42-8312-4297-be30-57e10dd84d18"
	target := "https://app.circleci.com/pipelines/github/acme/widgets/latest"
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name != "circleci" || (len(args) > 0 && args[0] == "pipeline") {
			t.Fatalf("command = %s %v", name, args)
		}
		for _, arg := range args {
			if strings.Contains(arg, "latest") {
				t.Fatalf("cli invoked with pipeline url: %v", args)
			}
		}
		switch {
		case len(args) >= 2 && args[0] == "workflow" && args[1] == "get":
			if !circleArgIs(args, workflowID) {
				t.Fatalf("args = %v", args)
			}
			return `{"jobs":[{"id":"` + jobID + `","name":"unit","outcome":"failed"}]}`, nil
		case len(args) >= 3 && args[0] == "job":
			return `{"steps":[{"outcome":"failed","output":"ERROR: setup failed\n"}]}`, nil
		default:
			t.Fatalf("args = %v", args)
			return "", nil
		}
	})
	pr := removedPR()
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:        "ci/circleci: unit",
		TargetURL:   target,
		CheckRunURL: "https://app.circleci.com/pipelines/github/acme/widgets/42/workflow/" + workflowID,
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil")
		}
	})
	if !strings.Contains(output, "ERROR: setup failed") || strings.Contains(output, "unrecognized CircleCI pipeline URL") {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueCircleLogModeSelectsAPIOrCLI(t *testing.T) {
	t.Run("cli does not call http", func(t *testing.T) {
		t.Setenv(circleTokenEnv, "token")
		withCircleCLI(t, true)
		withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
			t.Fatal("circleci http api was called")
			return nil, nil
		})
		withCommandStub(t, func(name string, args ...string) (string, error) {
			if name != "circleci" {
				t.Fatalf("command = %s", name)
			}
			if len(args) >= 2 && args[0] == "workflow" {
				return `{"jobs":[{"id":"job-1","outcome":"failed"}]}`, nil
			}
			return `{"steps":[{"outcome":"failed","output":"ERROR: setup failed\n"}]}`, nil
		})
		cfg := queueConfig()
		cfg.Settings.CircleCILog = config.CircleCILogCLI
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
			Name:      "ci/circleci",
			TargetURL: "https://app.circleci.com/pipelines/github/acme/widgets/42/workflows/11111111-1111-1111-1111-111111111111",
		}}
		output := captureStdout(t, func() {
			if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err == nil {
				t.Fatal("queuePullRequest() error = nil")
			}
		})
		if !strings.Contains(output, "ERROR: setup failed") {
			t.Fatalf("output = %s", output)
		}
	})
	t.Run("api does not run circleci", func(t *testing.T) {
		t.Setenv(circleTokenEnv, "")
		prev := circleCLILookPath
		circleCLILookPath = func(string) (string, error) {
			t.Fatal("circleci cli was looked up")
			return "", exec.ErrNotFound
		}
		t.Cleanup(func() { circleCLILookPath = prev })
		withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
			t.Fatal("circleci http api was called")
			return nil, nil
		})
		cfg := queueConfig()
		cfg.Settings.CircleCILog = config.CircleCILogAPI
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
			Name:      "ci/circleci",
			TargetURL: "https://circleci.com/gh/acme/widgets/42",
		}}
		output := captureStdout(t, func() {
			if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err == nil {
				t.Fatal("queuePullRequest() error = nil")
			}
		})
		if !strings.Contains(output, "log unavailable: no CircleCI token") {
			t.Fatalf("output = %s", output)
		}
		if strings.Contains(output, "circleci CLI") {
			t.Fatalf("api mode named the cli:\n%s", output)
		}
	})
}

func withCircleCLI(t *testing.T, present bool) {
	t.Helper()
	prev := circleCLILookPath
	circleCLILookPath = func(string) (string, error) {
		if !present {
			return "", exec.ErrNotFound
		}
		return "/usr/bin/circleci", nil
	}
	t.Cleanup(func() { circleCLILookPath = prev })
}

func circleArgIs(args []string, want string) bool {
	for _, arg := range args {
		if arg == want {
			return true
		}
	}
	return false
}

func TestQueueOtherTargetPrintsWhyLogWasNotFetched(t *testing.T) {
	pr := removedPR()
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:      "jenkins",
		TargetURL: "https://ci.example/build/1",
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil")
		}
	})
	if !strings.Contains(output, "jenkins https://ci.example/build/1") || !strings.Contains(output, "log unavailable: unsupported target") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "Failing step log tail:") {
		t.Fatalf("empty tail printed:\n%s", output)
	}
}

func removedPR() github.QueuePullRequest {
	pr := readyPR()
	pr.Events = []github.QueueEvent{{
		ID:        "EVT1",
		Kind:      github.QueueEventRemoved,
		CreatedAt: time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC),
		Reason:    "failed_checks",
		CommitSHA: "deadbeef",
	}}
	return pr
}

func withCircleGet(t *testing.T, fn func(context.Context, string, string) ([]byte, error)) {
	t.Helper()
	prev := circleGet
	circleGet = fn
	t.Cleanup(func() { circleGet = prev })
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

func TestQueueRetargetedStackWithNothingOpenBelow(t *testing.T) {
	defaultBranch := testDefaultBranch()
	pr := chainPR(12, defaultBranch, "topic-b")
	stackErr := errors.New("this pull request is part of a stack and must be merged sequentially using merge-async")
	const why = "no open pull requests below acme/widgets#12; enqueuing with merge-async"

	tests := []struct {
		name      string
		downstack []github.StackPR
	}{
		{name: "empty downstack"},
		{name: "merged pull request below", downstack: []github.StackPR{{Number: 11, State: "merged"}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			client := &fakeQueue{pr: pr, enqueueErr: stackErr, downstack: tc.downstack}
			output := captureStdout(t, func() {
				if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
					t.Fatalf("queuePullRequest() error = %v", err)
				}
			})
			if len(client.mergedAsync) != 1 || client.mergedAsync[0] != "acme/widgets#12@abc123" {
				t.Errorf("mergedAsync = %v, want acme/widgets#12@abc123", client.mergedAsync)
			}
			if !strings.Contains(output, why) {
				t.Errorf("output missing %q:\n%s", why, output)
			}
			if !strings.Contains(output, "Enqueued acme/widgets#12 with merge-async.") {
				t.Errorf("output missing merge-async result:\n%s", output)
			}
		})
	}

	t.Run("open pull request below still refuses", func(t *testing.T) {
		client := &fakeQueue{pr: pr, enqueueErr: stackErr, downstack: []github.StackPR{{Number: 11, State: "open"}}}
		output := captureStdout(t, func() {
			err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false)
			if err == nil || !strings.Contains(err.Error(), "--include-downstack") {
				t.Fatalf("queuePullRequest() error = %v, want --include-downstack refusal", err)
			}
		})
		if !strings.Contains(output, "- acme/widgets#11") {
			t.Errorf("output missing open pull request below:\n%s", output)
		}
		if strings.Contains(output, why) {
			t.Errorf("output has no-downstack line despite an open pull request below:\n%s", output)
		}
		if len(client.mergedAsync) != 0 {
			t.Errorf("mergedAsync = %v, want none", client.mergedAsync)
		}
	})

	t.Run("dry run does not refuse", func(t *testing.T) {
		withMergeFlags(t, true, false, false)
		client := &fakeQueue{pr: pr, enqueueErr: stackErr}
		output := captureStdout(t, func() {
			if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
		})
		if !strings.Contains(output, "Would enqueue acme/widgets#12.") {
			t.Errorf("output = %s", output)
		}
		if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
			t.Errorf("enqueueCalls = %d async = %v, want neither API", client.enqueueCalls, client.mergedAsync)
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

func TestWatchAndRCAPrintLoadingStepTail(t *testing.T) {
	const fetchLine = "E: Failed to fetch http://snapshot.example/pool/main/a/acme/acme_1.0_amd64.deb  500  Internal Server Error"
	evidence := func(t *testing.T) github.FailureEvidence {
		t.Helper()
		return loadingStepEvidence(t)
	}
	t.Run("watch", func(t *testing.T) {
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: evidence(t)}
		var err error
		output := captureStdout(t, func() {
			err = watchPullRequests(client, []string{pr.Ref.Raw}, watchOptions{
				timeout: time.Minute,
				cfg:     queueConfig(),
			})
		})
		assertWatchExit(t, err, 2)
		for _, want := range []string{fetchLine, "acme/widgets#12 failed_checks"} {
			if !strings.Contains(output, want) {
				t.Errorf("output missing %q:\n%s", want, output)
			}
		}
		if strings.Contains(output, "canceled-only-line") || strings.Contains(output, "load line 0\n") {
			t.Fatalf("tail was not the failing step:\n%s", output)
		}
	})
	t.Run("rca", func(t *testing.T) {
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: evidence(t)}
		path := writeRCAFile(t, "mirror returned 500\n")
		output := captureStdout(t, func() {
			if err := postRCA(client, rcaConfig(), []string{"acme/widgets#12", "--class", "infra", "--file", path}); err != nil {
				t.Fatalf("postRCA() error = %v", err)
			}
		})
		for _, want := range []string{fetchLine, "Commented on acme/widgets#12."} {
			if !strings.Contains(output, want) {
				t.Errorf("output missing %q:\n%s", want, output)
			}
		}
		if strings.Contains(output, "canceled-only-line") || strings.Contains(output, "load line 0\n") {
			t.Fatalf("tail was not the failing step:\n%s", output)
		}
		if strings.Contains(client.comments[0], "snapshot.example") || strings.Contains(client.comments[0], "canceled-only-line") {
			t.Fatalf("comment included the log: %q", client.comments[0])
		}
	})
}

func loadingStepEvidence(t *testing.T) github.FailureEvidence {
	t.Helper()
	t.Setenv(circleTokenEnv, "")
	withCircleCLI(t, true)
	withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
		t.Fatal("circleci http api was called")
		return nil, nil
	})
	const workflowID = "11111111-1111-1111-1111-111111111111"
	const jobID = "6bde3c42-8312-4297-be30-57e10dd84d18"
	var failed strings.Builder
	for i := 0; i < 25; i++ {
		fmt.Fprintf(&failed, "load line %d\n", i)
	}
	failed.WriteString("E: Failed to fetch http://snapshot.example/pool/main/a/acme/acme_1.0_amd64.deb  500  Internal Server Error\n")
	outputBody, err := json.Marshal(map[string]any{
		"steps": []map[string]any{
			{"name": "Spin up environment", "phase": "ended", "outcome": "succeeded", "output": "booted\n"},
			{"name": "Install packages", "phase": "started", "outcome": "", "exit_code": 100, "output": failed.String()},
			{"name": "Test", "phase": "ended", "outcome": "canceled", "output": strings.Repeat("canceled-only-line\n", 30)},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name != "circleci" {
			t.Fatalf("command = %s %v", name, args)
		}
		switch {
		case len(args) >= 2 && args[0] == "workflow" && args[1] == "get":
			if !circleArgIs(args, workflowID) {
				t.Fatalf("args = %v", args)
			}
			return `{"jobs":[{"id":"` + jobID + `","name":"unit","outcome":"failed"}]}`, nil
		case len(args) >= 2 && args[0] == "job" && args[1] == "get":
			if !circleArgIs(args, jobID) {
				t.Fatalf("args = %v", args)
			}
			return `{"status":"failed"}`, nil
		case len(args) >= 3 && args[0] == "job" && args[1] == "output" && args[2] == "list":
			if !circleArgIs(args, jobID) {
				t.Fatalf("args = %v", args)
			}
			return string(outputBody), nil
		default:
			t.Fatalf("args = %v", args)
			return "", nil
		}
	})
	return github.FailureEvidence{
		Name:        "ci/circleci: unit",
		TargetURL:   "https://circleci.com/gh/acme/widgets/4242",
		CheckRunURL: "https://app.circleci.com/pipelines/github/acme/widgets/42/workflow/" + workflowID,
	}
}

func TestNoFailedJobContinues(t *testing.T) {
	const workflowID = "11111111-1111-1111-1111-111111111111"
	t.Run("queue without rca", func(t *testing.T) {
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: noFailedJobEvidence(t, workflowID)}
		var err error
		output := captureStdout(t, func() {
			err = queuePullRequest(client, queueConfig(), pr.Ref.Raw, false)
		})
		if err == nil || err.Error() != "refusing to enqueue: no RCA comment for that removal" {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
		assertNoFailedJobNote(t, output, workflowID)
		if len(client.enqueued) != 0 {
			t.Fatalf("enqueued = %v", client.enqueued)
		}
	})
	t.Run("queue our-code", func(t *testing.T) {
		pr := removedPR()
		pr.HeadSHA = "deadbeef"
		pr.HeadCommittedAt = pr.Events[0].CreatedAt.Add(-time.Hour)
		pr.Comments = []string{"RCA EVT1 class=our-code"}
		client := &fakeQueue{pr: pr, evidence: noFailedJobEvidence(t, workflowID)}
		var err error
		output := captureStdout(t, func() {
			err = queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false)
		})
		const want = "refusing to enqueue: our-code removal and head deadbeef unchanged since 2026-01-02T00:00:00Z"
		if err == nil || err.Error() != want {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
		assertNoFailedJobNote(t, output, workflowID)
		if strings.Contains(output, "pushed since") || len(client.enqueued) != 0 {
			t.Fatalf("output = %s enqueued = %v", output, client.enqueued)
		}
	})
	t.Run("queue flake", func(t *testing.T) {
		pr := removedPR()
		pr.Comments = []string{"RCA EVT1 class=flake"}
		client := &fakeQueue{pr: pr, evidence: noFailedJobEvidence(t, workflowID)}
		output := captureStdout(t, func() {
			if err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
		})
		assertNoFailedJobNote(t, output, workflowID)
		if !strings.Contains(output, "Enqueued acme/widgets#12.") || len(client.enqueued) != 1 {
			t.Fatalf("output = %s enqueued = %v", output, client.enqueued)
		}
	})
	t.Run("queue over the requeue limit", func(t *testing.T) {
		pr := removedPR()
		base := pr.Events[0].CreatedAt
		pr.Events = []github.QueueEvent{
			{ID: "A0", Kind: github.QueueEventAdded, CreatedAt: base.Add(-7 * time.Minute)},
			{ID: "R1", Kind: github.QueueEventRemoved, CreatedAt: base.Add(-6 * time.Minute), Reason: "failed_checks"},
			{ID: "A1", Kind: github.QueueEventAdded, CreatedAt: base.Add(-5 * time.Minute)},
			{ID: "R2", Kind: github.QueueEventRemoved, CreatedAt: base.Add(-4 * time.Minute), Reason: "failed_checks"},
			{ID: "A2", Kind: github.QueueEventAdded, CreatedAt: base.Add(-3 * time.Minute)},
			{ID: "R3", Kind: github.QueueEventRemoved, CreatedAt: base.Add(-2 * time.Minute), Reason: "failed_checks"},
			{ID: "A3", Kind: github.QueueEventAdded, CreatedAt: base.Add(-1 * time.Minute)},
			{ID: "EVT1", Kind: github.QueueEventRemoved, CreatedAt: base, Reason: "failed_checks", CommitSHA: "deadbeef"},
		}
		pr.Comments = []string{"RCA EVT1 class=flake"}
		limit := 2
		cfg := rcaConfig()
		cfg.Settings.MaxRequeues = &limit
		client := &fakeQueue{pr: pr, evidence: noFailedJobEvidence(t, workflowID)}
		var err error
		output := captureStdout(t, func() {
			err = queuePullRequest(client, cfg, pr.Ref.Raw, false)
		})
		const want = "refusing to enqueue: 3 earlier requeues is greater than max_requeues 2"
		if err == nil || err.Error() != want {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
		assertNoFailedJobNote(t, output, workflowID)
		if len(client.enqueued) != 0 {
			t.Fatalf("enqueued = %v", client.enqueued)
		}
	})
	t.Run("queue json", func(t *testing.T) {
		jsonOutput = true
		t.Cleanup(func() { jsonOutput = false })
		pr := removedPR()
		pr.Comments = []string{"RCA EVT1 class=flake"}
		client := &fakeQueue{pr: pr, evidence: noFailedJobEvidence(t, workflowID)}
		output := captureStdout(t, func() {
			if err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
		})
		assertNoFailedJobNote(t, output, workflowID)
		if !strings.Contains(output, `"result": "enqueued"`) || len(client.enqueued) != 1 {
			t.Fatalf("output = %s enqueued = %v", output, client.enqueued)
		}
	})
	t.Run("watch", func(t *testing.T) {
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: noFailedJobEvidence(t, workflowID)}
		var err error
		output := captureStdout(t, func() {
			err = watchPullRequests(client, []string{pr.Ref.Raw}, watchOptions{
				timeout: time.Minute,
				cfg:     queueConfig(),
			})
		})
		assertWatchExit(t, err, 2)
		assertNoFailedJobNote(t, output, workflowID)
		if !strings.Contains(output, "acme/widgets#12 failed_checks") || strings.Contains(output, "circleci workflow get") {
			t.Fatalf("output = %s", output)
		}
	})
	t.Run("rca", func(t *testing.T) {
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: noFailedJobEvidence(t, workflowID)}
		path := writeRCAFile(t, "Checked the status webhook.\n")
		output := captureStdout(t, func() {
			if err := postRCA(client, rcaConfig(), []string{"acme/widgets#12", "--class", "infra", "--file", path}); err != nil {
				t.Fatalf("postRCA() error = %v", err)
			}
		})
		assertNoFailedJobNote(t, output, workflowID)
		if !strings.Contains(output, "Commented on acme/widgets#12.") {
			t.Fatalf("output = %s", output)
		}
		comment := client.comments[0]
		if !strings.Contains(comment, "Checked the status webhook.") || !strings.Contains(comment, "RCA EVT1 class=infra") {
			t.Fatalf("comment = %q", comment)
		}
		if strings.Contains(comment, "apt mirror") || strings.Contains(comment, "no failed job") {
			t.Fatalf("comment included the note: %q", comment)
		}
	})
	t.Run("cli error still stops", func(t *testing.T) {
		t.Setenv(circleTokenEnv, "")
		withCircleCLI(t, true)
		withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
			t.Fatal("circleci http api was called")
			return nil, nil
		})
		withCommandStub(t, func(name string, args ...string) (string, error) {
			if name == "circleci" && len(args) >= 2 && args[0] == "workflow" && args[1] == "get" {
				return "", errors.New("exit status 1")
			}
			t.Fatalf("command = %s %v", name, args)
			return "", nil
		})
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
			Name:        "webhook/build",
			TargetURL:   "https://circleci.com/gh/acme/widgets/4242",
			CheckRunURL: "https://app.circleci.com/pipelines/github/acme/widgets/42/workflow/" + workflowID,
			Statuses:    statusNotes(),
		}}
		var err error
		output := captureStdout(t, func() {
			err = queuePullRequest(client, queueConfig(), pr.Ref.Raw, false)
		})
		if err == nil || !strings.Contains(err.Error(), "circleci workflow get") || strings.Contains(err.Error(), "no failed job") {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
		if strings.Contains(output, "no failed job in") || len(client.enqueued) != 0 {
			t.Fatalf("output = %s enqueued = %v", output, client.enqueued)
		}
	})
	t.Run("api", func(t *testing.T) {
		t.Setenv(circleTokenEnv, "token")
		withCommandStub(t, func(name string, args ...string) (string, error) {
			t.Fatalf("command = %s %v", name, args)
			return "", nil
		})
		withCircleGet(t, func(_ context.Context, rawURL, token string) ([]byte, error) {
			if token != "token" {
				t.Fatalf("token = %q", token)
			}
			switch {
			case strings.HasSuffix(rawURL, "/project/gh/acme/widgets/pipeline/42"):
				return []byte(`{"id":"pipe-1"}`), nil
			case strings.HasSuffix(rawURL, "/pipeline/pipe-1/workflow"):
				return []byte(`{"items":[{"id":"` + workflowID + `","status":"failed"}]}`), nil
			case strings.HasSuffix(rawURL, "/workflow/"+workflowID+"/job"):
				return []byte(`{"items":[{"job_number":7,"status":"success"},{"job_number":8,"status":"success"}]}`), nil
			default:
				t.Fatalf("unexpected circleci url %s", rawURL)
				return nil, nil
			}
		})
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
			Name:      "webhook/build",
			TargetURL: "https://app.circleci.com/pipelines/github/acme/widgets/42",
			Statuses:  statusNotes(),
		}}
		var err error
		output := captureStdout(t, func() {
			err = queuePullRequest(client, queueConfig(), pr.Ref.Raw, false)
		})
		if err == nil || err.Error() != "refusing to enqueue: no RCA comment for that removal" {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
		assertNoFailedJobNote(t, output, workflowID)
		if len(client.enqueued) != 0 {
			t.Fatalf("enqueued = %v", client.enqueued)
		}
	})
	t.Run("api pipeline without a failed workflow still stops", func(t *testing.T) {
		t.Setenv(circleTokenEnv, "token")
		withCircleGet(t, func(_ context.Context, rawURL, _ string) ([]byte, error) {
			switch {
			case strings.HasSuffix(rawURL, "/pipeline/42"):
				return []byte(`{"id":"pipe-1"}`), nil
			case strings.HasSuffix(rawURL, "/workflow"):
				return []byte(`{"items":[{"id":"` + workflowID + `","status":"success"}]}`), nil
			case strings.Contains(rawURL, "/job"):
				return []byte(`{"items":[{"job_number":8,"status":"success"}]}`), nil
			default:
				t.Fatalf("unexpected circleci url %s", rawURL)
				return nil, nil
			}
		})
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
			Name:      "webhook/build",
			TargetURL: "https://app.circleci.com/pipelines/github/acme/widgets/42",
			Statuses:  statusNotes(),
		}}
		var err error
		output := captureStdout(t, func() {
			err = queuePullRequest(client, queueConfig(), pr.Ref.Raw, false)
		})
		if err == nil || !strings.Contains(err.Error(), "circleci pipeline has no failed workflow") {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
		if strings.Contains(output, "no failed job in") || strings.Contains(err.Error(), "no failed job") || len(client.enqueued) != 0 {
			t.Fatalf("err = %v output = %s enqueued = %v", err, output, client.enqueued)
		}
	})
}

func statusNotes() []github.CommitStatus {
	return []github.CommitStatus{
		{Context: "webhook/build", State: "error", Description: "apt mirror HTTP 500"},
		{Context: "ci/tests", State: "failure", Description: "unit tests failed"},
		{Context: "ci/empty", State: "error"},
		{Context: "ci/ok", State: "success", Description: "success-only-line"},
		{Context: "ci/pending", State: "pending", Description: "pending-only-line"},
	}
}

func noFailedJobEvidence(t *testing.T, workflowID string) github.FailureEvidence {
	t.Helper()
	t.Setenv(circleTokenEnv, "")
	withCircleCLI(t, true)
	withCircleGet(t, func(context.Context, string, string) ([]byte, error) {
		t.Fatal("circleci http api was called")
		return nil, nil
	})
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name != "circleci" {
			t.Fatalf("command = %s %v", name, args)
		}
		switch {
		case len(args) >= 2 && args[0] == "workflow" && args[1] == "get":
			if !circleArgIs(args, workflowID) {
				t.Fatalf("args = %v", args)
			}
			return `{"jobs":[{"id":"job-1","name":"unit","outcome":"success"},{"id":"job-2","name":"lint","outcome":"succeeded"}]}`, nil
		default:
			t.Fatalf("args = %v", args)
			return "", nil
		}
	})
	return github.FailureEvidence{
		Name:        "webhook/build",
		TargetURL:   "https://circleci.com/gh/acme/widgets/4242",
		CheckRunURL: "https://app.circleci.com/pipelines/github/acme/widgets/42/workflow/" + workflowID,
		Statuses:    statusNotes(),
	}
}

func assertNoFailedJobNote(t *testing.T, output, workflowID string) {
	t.Helper()
	for _, want := range []string{
		"no failed job in " + workflowID,
		"webhook/build error apt mirror HTTP 500",
		"ci/tests failure unit tests failed",
		"ci/empty error\n",
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	for _, absent := range []string{
		"success-only-line",
		"pending-only-line",
		"circleci workflow has no failed job",
		"Failing step log tail:",
		"ci/empty error ",
	} {
		if strings.Contains(output, absent) {
			t.Errorf("output contains %q:\n%s", absent, output)
		}
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

type scriptedWatch struct {
	fakeQueue
	steps []error
	pr    github.QueuePullRequest
	n     int
}

func (s *scriptedWatch) LoadPullRequest(ref string) (github.QueuePullRequest, error) {
	s.n++
	s.loads++
	s.loadedRefs = append(s.loadedRefs, ref)
	if len(s.steps) > 0 {
		idx := s.n - 1
		if idx >= len(s.steps) {
			idx = len(s.steps) - 1
		}
		if s.steps[idx] != nil {
			return github.QueuePullRequest{}, s.steps[idx]
		}
	}
	pr := s.pr
	if pr.Ref.Raw == "" {
		pr.Ref.Raw = ref
	}
	return pr, nil
}

func watchClock(start time.Time, timeout time.Duration) (*[]time.Duration, watchOptions) {
	now := start
	slept := []time.Duration{}
	return &slept, watchOptions{
		timeout:  timeout,
		interval: 15 * time.Second,
		now:      func() time.Time { return now },
		sleep: func(d time.Duration) {
			slept = append(slept, d)
			now = now.Add(d)
		},
	}
}

func TestWatchRetriesTransientGitHubError(t *testing.T) {
	merged := readyPR()
	merged.Merged = true
	start := time.Date(2026, 3, 1, 0, 0, 0, 0, time.UTC)
	rate := errors.New("load pull request acme/widgets#12: gh api graphql: HTTP 403: you have exceeded a secondary rate limit")
	http502 := errors.New("load pull request acme/widgets#12: gh api graphql: HTTP 502")
	retryAfter := errors.New("load pull request acme/widgets#12: gh api graphql: HTTP 403: secondary rate limit\nRetry-After: 30")
	permanent := errors.New("load pull request acme/widgets#12: could not resolve to a PullRequest with the number of 12")

	t.Run("secondary rate limit then merged", func(t *testing.T) {
		slept, opt := watchClock(start, time.Minute)
		client := &scriptedWatch{steps: []error{rate, nil}, pr: merged}
		output := captureStdout(t, func() {
			if err := watchPullRequests(client, []string{"acme/widgets#12"}, opt); err != nil {
				t.Fatalf("watchPullRequests() error = %v", err)
			}
		})
		if !strings.Contains(output, "acme/widgets#12 merged") {
			t.Fatalf("output = %q", output)
		}
		if len(*slept) != 1 || (*slept)[0] < 15*time.Second {
			t.Fatalf("slept = %v, want one backoff of at least 15s", *slept)
		}
		if client.n != 2 {
			t.Fatalf("loads = %d, want 2", client.n)
		}
	})

	t.Run("http 502 then merged", func(t *testing.T) {
		slept, opt := watchClock(start, time.Minute)
		client := &scriptedWatch{steps: []error{http502, nil}, pr: merged}
		output := captureStdout(t, func() {
			if err := watchPullRequests(client, []string{"acme/widgets#12"}, opt); err != nil {
				t.Fatalf("watchPullRequests() error = %v", err)
			}
		})
		if !strings.Contains(output, "acme/widgets#12 merged") {
			t.Fatalf("output = %q", output)
		}
		if len(*slept) != 1 || (*slept)[0] < 15*time.Second {
			t.Fatalf("slept = %v, want one backoff of at least 15s", *slept)
		}
	})

	t.Run("retry-after", func(t *testing.T) {
		slept, opt := watchClock(start, time.Minute)
		client := &scriptedWatch{steps: []error{retryAfter, nil}, pr: merged}
		if err := watchPullRequests(client, []string{"acme/widgets#12"}, opt); err != nil {
			t.Fatalf("watchPullRequests() error = %v", err)
		}
		if len(*slept) != 1 || (*slept)[0] < 30*time.Second {
			t.Fatalf("slept = %v, want at least 30s", *slept)
		}
	})

	t.Run("permanent error exits", func(t *testing.T) {
		slept, opt := watchClock(start, time.Minute)
		client := &scriptedWatch{steps: []error{permanent}, pr: merged}
		err := watchPullRequests(client, []string{"acme/widgets#12"}, opt)
		if err == nil || !strings.Contains(err.Error(), "could not resolve") {
			t.Fatalf("watchPullRequests() error = %v", err)
		}
		if len(*slept) != 0 || client.n != 1 {
			t.Fatalf("slept = %v loads = %d, want no wait", *slept, client.n)
		}
	})

	t.Run("connection failure then merged", func(t *testing.T) {
		failures := []error{
			errors.New("Get \"https://api.github.com/graphql\": dial tcp: lookup api.github.com: no such host"),
			errors.New("Get \"https://api.github.com/graphql\": dial tcp 127.0.0.1:443: connect: connection refused"),
			errors.New("Get \"https://api.github.com/graphql\": read tcp: connection reset by peer"),
			errors.New("Get \"https://api.github.com/graphql\": remote error: tls: handshake failure"),
			errors.New("Get \"https://api.github.com/graphql\": unexpected EOF"),
			errors.New("EOF"),
		}
		for _, failure := range failures {
			t.Run(failure.Error(), func(t *testing.T) {
				slept, opt := watchClock(start, time.Minute)
				client := &scriptedWatch{steps: []error{failure, nil}, pr: merged}
				output := captureStdout(t, func() {
					if err := watchPullRequests(client, []string{"acme/widgets#12"}, opt); err != nil {
						t.Fatalf("watchPullRequests() error = %v", err)
					}
				})
				if !strings.Contains(output, "acme/widgets#12 merged") {
					t.Fatalf("output = %q", output)
				}
				if len(*slept) != 1 || (*slept)[0] < 15*time.Second {
					t.Fatalf("slept = %v, want one backoff of at least 15s", *slept)
				}
				if client.n != 2 {
					t.Fatalf("loads = %d, want 2", client.n)
				}
			})
		}
	})

	t.Run("http status stays permanent", func(t *testing.T) {
		for _, failure := range []error{
			errors.New("gh api graphql: HTTP 401"),
			errors.New("gh api graphql: HTTP 404"),
		} {
			t.Run(failure.Error(), func(t *testing.T) {
				slept, opt := watchClock(start, time.Minute)
				client := &scriptedWatch{steps: []error{failure}, pr: merged}
				err := watchPullRequests(client, []string{"acme/widgets#12"}, opt)
				if err == nil || !strings.Contains(err.Error(), failure.Error()) {
					t.Fatalf("watchPullRequests() error = %v", err)
				}
				if len(*slept) != 0 || client.n != 1 {
					t.Fatalf("slept = %v loads = %d, want no poll", *slept, client.n)
				}
			})
		}
	})

	t.Run("deadline still times out", func(t *testing.T) {
		slept, opt := watchClock(start, 20*time.Second)
		client := &scriptedWatch{steps: []error{rate}, pr: merged}
		var err error
		output := captureStdout(t, func() {
			err = watchPullRequests(client, []string{"acme/widgets#12"}, opt)
		})
		var exit *watchExitError
		if !errors.As(err, &exit) || exit.code != 5 {
			t.Fatalf("watchPullRequests() error = %v, want timeout status", err)
		}
		if !strings.Contains(output, "acme/widgets#12 timeout") {
			t.Fatalf("output = %q", output)
		}
		if len(*slept) == 0 {
			t.Fatal("slept = none, want a backoff before the deadline")
		}
	})
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
	var err error
	output := captureStdout(t, func() {
		err = watchPullRequests(client, []string{
			"acme/widgets#1",
			"acme/widgets#2",
			"acme/widgets#3",
		}, watchOptions{timeout: 0, asJSON: true})
	})
	for _, want := range []string{`"result": "merged"`, `"result": "failed_checks"`, `"result": "timeout"`} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	var status *watchExitError
	if !errors.As(err, &status) || status.code != 2 {
		t.Fatalf("watchPullRequests() error = %v, want exit 2", err)
	}
	if strings.Contains(output, "ejected:") {
		t.Fatalf("output prefixed ejected:\n%s", output)
	}
}

func TestWatchRemovalReasonsAndTimeoutExitCodes(t *testing.T) {
	cases := []struct {
		name   string
		reason string
		code   int
	}{
		{name: "failed checks", reason: "failed_checks", code: 2},
		{name: "merge conflict", reason: "merge_conflict", code: 3},
		{name: "manual", reason: "manual", code: 4},
		{name: "other reason", reason: "blocked", code: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			pr := readyPR()
			pr.Merged = false
			pr.Events = []github.QueueEvent{{
				ID:        "R1",
				Kind:      github.QueueEventRemoved,
				CreatedAt: time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC),
				Reason:    tc.reason,
			}}
			client := &fakeQueue{pr: pr}
			var err error
			output := captureStdout(t, func() {
				err = watchPullRequests(client, []string{pr.Ref.Raw}, watchOptions{timeout: time.Minute})
			})
			assertWatchExit(t, err, tc.code)
			if !strings.Contains(output, "acme/widgets#12 "+tc.reason) {
				t.Fatalf("output = %q", output)
			}
			if strings.Contains(output, "ejected:") {
				t.Fatalf("output = %q", output)
			}
		})
	}
}

func TestWatchTimeoutExits5(t *testing.T) {
	pr := readyPR()
	pr.Merged = false
	client := &fakeQueue{pr: pr}
	var err error
	output := captureStdout(t, func() {
		err = watchPullRequests(client, []string{pr.Ref.Raw}, watchOptions{timeout: 0})
	})
	assertWatchExit(t, err, 5)
	if !strings.Contains(output, "acme/widgets#12 timeout") {
		t.Fatalf("output = %q", output)
	}
}

func TestWatchMergedJSONExitsZero(t *testing.T) {
	pr := readyPR()
	pr.Merged = true
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := watchPullRequests(client, []string{pr.Ref.Raw}, watchOptions{timeout: time.Minute, asJSON: true}); err != nil {
			t.Fatalf("watchPullRequests() error = %v", err)
		}
	})
	if !strings.Contains(output, `"result": "merged"`) {
		t.Fatalf("output = %s", output)
	}
}

func TestWatchFirstMergeThenTimeoutExits5(t *testing.T) {
	merged := readyPR()
	merged.Ref.Raw = "acme/widgets#1"
	merged.Merged = true
	pending := readyPR()
	pending.Ref.Raw = "acme/widgets#2"
	pending.Merged = false
	client := &fakeQueue{byRef: map[string]github.QueuePullRequest{
		"acme/widgets#1": merged,
		"acme/widgets#2": pending,
	}}
	var err error
	output := captureStdout(t, func() {
		err = watchPullRequests(client, []string{"acme/widgets#1", "acme/widgets#2"}, watchOptions{timeout: 0, asJSON: true})
	})
	assertWatchExit(t, err, 5)
	for _, want := range []string{`"result": "merged"`, `"result": "timeout"`} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
}

func TestWatchDoesNotReadStdin(t *testing.T) {
	reader, writer, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		reader.Close()
		writer.Close()
	})
	if _, err := writer.Write([]byte("leave-me")); err != nil {
		t.Fatal(err)
	}
	orig := os.Stdin
	os.Stdin = reader
	t.Cleanup(func() { os.Stdin = orig })
	pr := readyPR()
	pr.Merged = true
	client := &fakeQueue{pr: pr}
	if err := watchPullRequests(client, []string{pr.Ref.Raw}, watchOptions{timeout: 0}); err != nil {
		t.Fatal(err)
	}
	if err := reader.SetReadDeadline(time.Now().Add(2 * time.Second)); err != nil {
		t.Fatal(err)
	}
	buf := make([]byte, 8)
	n, err := reader.Read(buf)
	if err != nil || string(buf[:n]) != "leave-me" {
		t.Fatalf("stdin = %q err = %v", buf[:n], err)
	}
}

func TestParseWatchArgsReadsTimeout(t *testing.T) {
	refs, timeout, err := parseWatchArgs([]string{"--timeout", "5m", "acme/widgets#12"})
	if err != nil || timeout != 5*time.Minute || len(refs) != 1 || refs[0] != "acme/widgets#12" {
		t.Fatalf("refs=%v timeout=%s err=%v", refs, timeout, err)
	}
	refs, timeout, err = parseWatchArgs([]string{"--timeout=1s", "12"})
	if err != nil || timeout != time.Second || len(refs) != 1 {
		t.Fatalf("refs=%v timeout=%s err=%v", refs, timeout, err)
	}
	if _, timeout, err = parseWatchArgs([]string{"acme/widgets#12"}); err != nil || timeout != defaultWatchTimeout {
		t.Fatalf("timeout=%s err=%v", timeout, err)
	}
	if _, _, err = parseWatchArgs([]string{"--timeout", "later"}); !errors.Is(err, errWatchUsage) {
		t.Fatalf("err = %v, want usage", err)
	}
	if _, _, err = parseWatchArgs([]string{"--nope", "12"}); !errors.Is(err, errWatchUsage) {
		t.Fatalf("err = %v, want usage", err)
	}
}

func assertWatchExit(t *testing.T, err error, code int) {
	t.Helper()
	var status *watchExitError
	if !errors.As(err, &status) || status.code != code {
		t.Fatalf("error = %v, want exit %d", err, code)
	}
}

func TestEarlierRequeuesIgnoresTheFirstEnqueue(t *testing.T) {
	base := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	events := []github.QueueEvent{
		{Kind: github.QueueEventAdded, CreatedAt: base},
		{Kind: github.QueueEventRemoved, CreatedAt: base.Add(time.Minute)},
	}
	got, err := earlierRequeues("", nil, events)
	if err != nil || got != 0 {
		t.Errorf("earlierRequeues() = %d, %v, want 0", got, err)
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

func TestQueuePrintsStackedAboveWithoutEnqueueingIt(t *testing.T) {
	pr := readyPR()
	client := &fakeQueue{pr: pr, above: []github.StackPR{{Number: 14, State: "open"}}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if !strings.Contains(output, "stacked above: #14, retarget needed after merge") {
		t.Fatalf("output = %s", output)
	}
	if client.enqueueCalls != 1 || len(client.enqueued) != 1 || client.enqueued[0] != "PR_NODE" {
		t.Fatalf("enqueued = %v calls = %d", client.enqueued, client.enqueueCalls)
	}
}

func TestQueueDryRunPrintsStackedAbove(t *testing.T) {
	withMergeFlags(t, true, false, false)
	pr := readyPR()
	client := &fakeQueue{pr: pr, above: []github.StackPR{
		{Number: 14, State: "open"},
		{Number: 15, State: "open"},
	}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if !strings.Contains(output, "stacked above: #14, #15, retarget needed after merge") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "Enqueued") {
		t.Fatalf("dry run enqueued:\n%s", output)
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}
}

func TestQueueNoPullRequestAbove(t *testing.T) {
	pr := readyPR()
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if !strings.Contains(output, "stacked above: (none)") {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueJSONIncludesStackedAbove(t *testing.T) {
	jsonOutput = true
	t.Cleanup(func() { jsonOutput = false })
	pr := readyPR()
	client := &fakeQueue{pr: pr, above: []github.StackPR{{Number: 14, State: "open"}}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	var got queueJSONResult
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("json.Unmarshal() error = %v\n%s", err, output)
	}
	if len(got.StackedAbove) != 1 || got.StackedAbove[0] != 14 {
		t.Fatalf("stackedAbove = %v\n%s", got.StackedAbove, output)
	}
	if client.enqueueCalls != 1 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}
}

func TestQueueStackedAboveLookupErrorDoesNotEnqueue(t *testing.T) {
	pr := readyPR()
	client := &fakeQueue{pr: pr, aboveErr: errors.New("stack api down")}
	err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, true)
	if err == nil {
		t.Fatal("queuePullRequest() error = nil")
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 || client.downCalls != 0 {
		t.Fatalf("enqueueCalls = %d async = %v downCalls = %d", client.enqueueCalls, client.mergedAsync, client.downCalls)
	}
}

func testDefaultBranch() string {
	return "default-ref"
}

func chainPR(number int, base, head string) github.QueuePullRequest {
	pr := readyPR()
	pr.Ref.Number = number
	pr.Ref.Raw = fmt.Sprintf("acme/widgets#%d", number)
	pr.BaseRefName = base
	pr.HeadRefName = head
	pr.DefaultBranch = testDefaultBranch()
	return pr
}

func branchChainClient() *fakeQueue {
	defaultBranch := testDefaultBranch()
	return &fakeQueue{
		byRef: map[string]github.QueuePullRequest{
			"acme/widgets#10": chainPR(10, defaultBranch, "topic-a"),
			"acme/widgets#11": chainPR(11, "topic-a", "topic-b"),
			"acme/widgets#12": chainPR(12, "topic-b", "topic-c"),
		},
		byBase: map[string][]github.BranchPull{
			"topic-a": {{Number: 11, State: "open", BaseRefName: "topic-a", HeadRefName: "topic-b"}},
			"topic-b": {{Number: 12, State: "open", BaseRefName: "topic-b", HeadRefName: "topic-c"}},
		},
		byHead: map[string][]github.BranchPull{
			"topic-a": {{Number: 10, State: "open", BaseRefName: defaultBranch, HeadRefName: "topic-a"}},
			"topic-b": {{Number: 11, State: "open", BaseRefName: "topic-a", HeadRefName: "topic-b"}},
		},
	}
}

func TestQueueRefusesNonNativeBaseAndNamesPullBelow(t *testing.T) {
	defaultBranch := testDefaultBranch()
	client := branchChainClient()
	output := captureStdout(t, func() {
		err := queuePullRequest(client, queueConfig(), "acme/widgets#11", true)
		if err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	want := fmt.Sprintf("base is topic-a, not %s; retarget to %s first (it sits on #10)", defaultBranch, defaultBranch)
	if !strings.Contains(output, want) {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "Would enqueue") || strings.Contains(output, "Enqueued") {
		t.Fatalf("refused pull request was enqueued:\n%s", output)
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 || client.downCalls != 0 {
		t.Fatalf("enqueueCalls = %d async = %v downCalls = %d", client.enqueueCalls, client.mergedAsync, client.downCalls)
	}
}

func TestQueueDryRunDoesNotBypassNonNativeBase(t *testing.T) {
	withMergeFlags(t, true, false, false)
	defaultBranch := testDefaultBranch()
	client := branchChainClient()
	output := captureStdout(t, func() {
		err := queuePullRequest(client, queueConfig(), "acme/widgets#11", false)
		if err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	want := fmt.Sprintf("base is topic-a, not %s; retarget to %s first (it sits on #10)", defaultBranch, defaultBranch)
	if !strings.Contains(output, want) {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "Would enqueue") {
		t.Fatalf("dry run printed Would enqueue:\n%s", output)
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}
}

func TestQueueAllowNoRequiredChecksDoesNotBypassNonNativeBase(t *testing.T) {
	withAllowNoRequiredChecks(t, true)
	defaultBranch := testDefaultBranch()
	client := branchChainClient()
	output := captureStdout(t, func() {
		err := queuePullRequest(client, queueConfig(), "acme/widgets#11", false)
		if err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	want := fmt.Sprintf("base is topic-a, not %s; retarget to %s first (it sits on #10)", defaultBranch, defaultBranch)
	if !strings.Contains(output, want) {
		t.Fatalf("output = %s", output)
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}
}

func TestQueueBranchWalkPrintsSinglePullAbove(t *testing.T) {
	defaultBranch := testDefaultBranch()
	pr := chainPR(10, defaultBranch, "topic-a")
	client := &fakeQueue{
		pr: pr,
		byBase: map[string][]github.BranchPull{
			"topic-a": {{Number: 11, State: "open", BaseRefName: "topic-a", HeadRefName: "topic-b"}},
		},
	}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if !strings.Contains(output, "stacked above: #11, retarget needed after merge") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "#12") {
		t.Fatalf("output = %s", output)
	}
	if client.enqueueCalls != 1 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}
}

func TestQueueBranchWalkListsStackedAboveNearestFirst(t *testing.T) {
	client := branchChainClient()
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), "acme/widgets#10", false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if !strings.Contains(output, "stacked above: #11, #12, retarget needed after merge") {
		t.Fatalf("output = %s", output)
	}
	if !strings.Contains(output, "Enqueued acme/widgets#10.") {
		t.Fatalf("output = %s", output)
	}
	if client.enqueueCalls != 1 || len(client.mergedAsync) != 0 || client.downCalls != 0 {
		t.Fatalf("enqueueCalls = %d async = %v downCalls = %d", client.enqueueCalls, client.mergedAsync, client.downCalls)
	}
}

func TestQueueOfMiddleStillRefusesAndNamesBasePull(t *testing.T) {
	defaultBranch := testDefaultBranch()
	client := branchChainClient()
	output := captureStdout(t, func() {
		err := queuePullRequest(client, queueConfig(), "acme/widgets#11", false)
		if err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	want := fmt.Sprintf("base is topic-a, not %s; retarget to %s first (it sits on #10)", defaultBranch, defaultBranch)
	if !strings.Contains(output, want) {
		t.Fatalf("output = %s", output)
	}
	if !strings.Contains(output, "stacked above: #12, retarget needed after merge") {
		t.Fatalf("output = %s", output)
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}
}

func TestQueueNativeStackIsNotRefusedForBase(t *testing.T) {
	defaultBranch := testDefaultBranch()
	pr := chainPR(11, "topic-a", "topic-b")
	client := &fakeQueue{
		pr:     pr,
		native: true,
		above:  []github.StackPR{{Number: 14, State: "open"}},
	}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if strings.Contains(output, "base is ") {
		t.Fatalf("native stack was refused:\n%s", output)
	}
	if !strings.Contains(output, "stacked above: #14, retarget needed after merge") {
		t.Fatalf("output = %s", output)
	}
	if !strings.Contains(output, "Enqueued acme/widgets#11.") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, defaultBranch) && strings.Contains(output, "retarget to") {
		t.Fatalf("output names a retarget:\n%s", output)
	}
	if client.enqueueCalls != 1 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}
}

func TestQueueNonNativeBaseOmitsParentheticalWhenNothingBelow(t *testing.T) {
	defaultBranch := testDefaultBranch()
	pr := chainPR(11, "topic-a", "topic-b")
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false)
		if err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	want := fmt.Sprintf("base is topic-a, not %s; retarget to %s first", defaultBranch, defaultBranch)
	if !strings.Contains(output, want) {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "it sits on") {
		t.Fatalf("output names a pull request below:\n%s", output)
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
		t.Fatalf("enqueueCalls = %d async = %v", client.enqueueCalls, client.mergedAsync)
	}
}

func TestQueueBranchWalkSkipsClosedAndMergesNativeNumbers(t *testing.T) {
	defaultBranch := testDefaultBranch()
	pr := chainPR(10, defaultBranch, "topic-a")
	client := &fakeQueue{
		pr:    pr,
		above: []github.StackPR{{Number: 11, State: "open"}},
		byBase: map[string][]github.BranchPull{
			"topic-a": {
				{Number: 11, State: "open", HeadRefName: "topic-b", BaseRefName: "topic-a"},
				{Number: 9, State: "closed", HeadRefName: "topic-closed", BaseRefName: "topic-a"},
			},
			"topic-b": {{Number: 12, State: "open", HeadRefName: "topic-c", BaseRefName: "topic-b"}},
		},
	}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if !strings.Contains(output, "stacked above: #11, #12, retarget needed after merge") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "#9") {
		t.Fatalf("closed pull request listed:\n%s", output)
	}
}

func TestQueueBranchWalkErrorDoesNotEnqueue(t *testing.T) {
	pr := chainPR(11, "topic-a", "topic-b")
	client := &fakeQueue{pr: pr, baseErr: errors.New("pull api down")}
	err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, true)
	if err == nil {
		t.Fatal("queuePullRequest() error = nil")
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 || client.downCalls != 0 {
		t.Fatalf("enqueueCalls = %d async = %v downCalls = %d", client.enqueueCalls, client.mergedAsync, client.downCalls)
	}
}

func TestQueueNativeStackErrorDoesNotEnqueue(t *testing.T) {
	pr := readyPR()
	client := &fakeQueue{pr: pr, nativeErr: errors.New("stack api down")}
	err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, true)
	if err == nil {
		t.Fatal("queuePullRequest() error = nil")
	}
	if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 || client.downCalls != 0 {
		t.Fatalf("enqueueCalls = %d async = %v downCalls = %d", client.enqueueCalls, client.mergedAsync, client.downCalls)
	}
}

func TestQueueBranchWalkDoesNotReplaceDownstack(t *testing.T) {
	defaultBranch := testDefaultBranch()
	pr := chainPR(10, defaultBranch, "topic-a")
	client := &fakeQueue{
		pr:         pr,
		enqueueErr: errors.New("this pull request is part of a stack and must be merged sequentially using merge-async"),
		downstack:  []github.StackPR{{Number: 9, State: "open"}},
		byBase: map[string][]github.BranchPull{
			"topic-a": {{Number: 11, State: "open", HeadRefName: "topic-b", BaseRefName: "topic-a"}},
		},
	}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, true); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if !strings.Contains(output, "stacked above: #11, retarget needed after merge") {
		t.Fatalf("output = %s", output)
	}
	if !strings.Contains(output, "- acme/widgets#9") {
		t.Fatalf("output = %s", output)
	}
	if strings.Contains(output, "- acme/widgets#11") {
		t.Fatalf("walked pull request was added to the downstack:\n%s", output)
	}
	if len(client.mergedAsync) != 1 || !strings.Contains(client.mergedAsync[0], "acme/widgets#10@") {
		t.Fatalf("async = %v", client.mergedAsync)
	}
	if client.downCalls != 1 {
		t.Fatalf("downCalls = %d", client.downCalls)
	}
}

func TestQueueHelpNamesRepositoryDefaultBranch(t *testing.T) {
	help := queueCommandHelp()
	if !strings.Contains(help, "repository's default branch") {
		t.Fatalf("help = %q", help)
	}
	if baseRefusalFormat != "base is %s, not %s; retarget to %s first" {
		t.Fatalf("format = %q", baseRefusalFormat)
	}
	if !strings.Contains(baseRefusalDoc, "repository's default branch") {
		t.Fatalf("doc = %q", baseRefusalDoc)
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
	for _, part := range []string{"--timeout", "30m", "stdin", "terminal"} {
		if !strings.Contains(watchCommandHelp(), part) {
			t.Errorf("watch help missing %s: %q", part, watchCommandHelp())
		}
	}
	if !strings.Contains(timeoutHelp(), "30m") || !strings.Contains(timeoutHelp(), "duration") {
		t.Errorf("timeout help = %q", timeoutHelp())
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
		"settings from: global",
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
	assertEffectiveConfigJSON(t, jsonOut, "would-enqueue", "/tmp/loaded.pkl", "global", []string{"do-not-merge"}, []string{"ci"})
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
		"settings from: global",
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
	assertEffectiveConfigJSON(t, jsonRefusedOut, "refused", "/tmp/loaded.pkl", "global", []string{"do-not-merge"}, []string{})
}

func assertEffectiveConfigJSON(t *testing.T, output, result, path, from string, labels, checks []string) {
	t.Helper()
	var got queueJSONResult
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("json.Unmarshal() error = %v\n%s", err, output)
	}
	if got.Result != result || got.Config != path || got.SettingsFrom != from {
		t.Fatalf("result = %+v", got)
	}
	if strings.Join(got.BlockMergeLabels, ",") != strings.Join(labels, ",") {
		t.Fatalf("blockMergeLabels = %#v", got.BlockMergeLabels)
	}
	if strings.Join(got.RequiredChecks, ",") != strings.Join(checks, ",") {
		t.Fatalf("requiredChecks = %#v", got.RequiredChecks)
	}
	if !strings.Contains(output, `"blockMergeLabels"`) || !strings.Contains(output, `"requiredChecks"`) || !strings.Contains(output, `"config"`) || !strings.Contains(output, `"settingsFrom"`) {
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

func TestRCAPostsMarkerForLatestRemoval(t *testing.T) {
	path := writeRCAFile(t, "Investigated the reset.\n")
	pr := readyPR()
	base := time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC)
	pr.Events = []github.QueueEvent{
		{ID: "OLD", Kind: github.QueueEventRemoved, CreatedAt: base, Reason: "failed_checks"},
		{ID: "NEW", Kind: github.QueueEventRemoved, CreatedAt: base.Add(time.Hour), Reason: "failed_checks"},
	}
	client := &fakeQueue{pr: pr}
	cfg := rcaConfig()
	output := captureStdout(t, func() {
		err := postRCA(client, cfg, []string{"acme/widgets#12", "--class", "flake", "--file", path})
		if err != nil {
			t.Fatalf("postRCA() error = %v", err)
		}
	})
	if client.commentCalls != 1 || client.commentRef != "acme/widgets#12" {
		t.Fatalf("comments = %d ref = %s", client.commentCalls, client.commentRef)
	}
	body := client.comments[0]
	if !strings.Contains(body, "Investigated the reset.") || !strings.Contains(body, "RCA NEW class=flake") {
		t.Fatalf("comment = %q", body)
	}
	if strings.Contains(body, "RCA OLD") {
		t.Fatalf("comment used an older removal: %q", body)
	}
	if !strings.Contains(output, "Commented on acme/widgets#12.") {
		t.Fatalf("output = %q", output)
	}

	queueOut := captureStdout(t, func() {
		if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if client.enqueueCalls != 1 {
		t.Fatalf("enqueueCalls = %d\n%s", client.enqueueCalls, queueOut)
	}
	if strings.Contains(queueOut, "no RCA comment") {
		t.Fatalf("queue refused for a missing RCA:\n%s", queueOut)
	}
}

func TestRCAOurCodeAndRequeueLimitStillRefuse(t *testing.T) {
	path := writeRCAFile(t, "We broke it.\n")
	base := time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC)
	pr := readyPR()
	pr.Events = []github.QueueEvent{
		{ID: "OLD", Kind: github.QueueEventRemoved, CreatedAt: base, Reason: "failed_checks"},
		{ID: "ADD", Kind: github.QueueEventAdded, CreatedAt: base.Add(time.Minute)},
		{ID: "NEW", Kind: github.QueueEventRemoved, CreatedAt: base.Add(time.Hour), Reason: "failed_checks", CommitSHA: pr.HeadSHA},
	}
	cfg := rcaConfig()
	limit := 0
	cfg.Settings.MaxRequeues = &limit
	client := &fakeQueue{pr: pr}
	if err := postRCA(client, cfg, []string{"acme/widgets#12", "--class", "our-code", "--file", path}); err != nil {
		t.Fatalf("postRCA() error = %v", err)
	}
	if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err == nil || !strings.Contains(err.Error(), "our-code") {
		t.Fatalf("queue error = %v, want our-code", err)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}

	client = &fakeQueue{pr: pr}
	if err := postRCA(client, cfg, []string{"acme/widgets#12", "--class", "flake", "--file", path}); err != nil {
		t.Fatalf("postRCA() error = %v", err)
	}
	if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err == nil || !strings.Contains(err.Error(), "max_requeues") {
		t.Fatalf("queue error = %v, want requeue limit", err)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}
}

func ourCodeRemoval(id, sha string, at time.Time) github.QueueEvent {
	return github.QueueEvent{
		ID: id, Kind: github.QueueEventRemoved, CreatedAt: at,
		Reason: "failed_checks", CommitSHA: sha,
	}
}

func TestOurCodeSameHeadRefuses(t *testing.T) {
	pr := readyPR()
	at := time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)
	pr.Events = []github.QueueEvent{ourCodeRemoval("EVT", pr.HeadSHA, at)}
	pr.HeadCommittedAt = at.Add(-time.Hour)
	pr.Comments = []string{"RCA EVT class=our-code"}
	client := &fakeQueue{pr: pr}
	err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false)
	const want = "refusing to enqueue: our-code removal and head abc123 unchanged since 2026-01-02T03:04:05Z"
	if err == nil || err.Error() != want {
		t.Fatalf("queue error = %v, want %q", err, want)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}
}

func TestOurCodeNullBeforeCommitForcePushEnqueues(t *testing.T) {
	pr := readyPR()
	at := time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)
	pr.Events = []github.QueueEvent{ourCodeRemoval("EVT", "", at)}
	pr.Comments = []string{"RCA EVT class=our-code"}
	pr.ForcePushedAt = []time.Time{at.Add(time.Minute)}
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	want := "our-code ejection at 2026-01-02T03:04:05Z; head abc123 pushed since, proceeding"
	if !strings.Contains(output, want) {
		t.Fatalf("output = %q, want %q", output, want)
	}
	if client.enqueueCalls != 1 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}
}

func TestOurCodeNullBeforeCommitLaterHeadDateEnqueues(t *testing.T) {
	pr := readyPR()
	at := time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)
	pr.Events = []github.QueueEvent{ourCodeRemoval("EVT", "", at)}
	pr.Comments = []string{"RCA EVT class=our-code"}
	pr.HeadCommittedAt = at.Add(time.Minute)
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	want := "our-code ejection at 2026-01-02T03:04:05Z; head abc123 pushed since, proceeding"
	if !strings.Contains(output, want) {
		t.Fatalf("output = %q, want %q", output, want)
	}
	if client.enqueueCalls != 1 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}
}

func TestOurCodeNullBeforeCommitUndecidedDoesNotRefuseOurCode(t *testing.T) {
	pr := readyPR()
	at := time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)
	pr.Events = []github.QueueEvent{ourCodeRemoval("EVT", "", at)}
	pr.Comments = []string{"RCA EVT class=our-code"}
	client := &fakeQueue{pr: pr}
	err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false)
	if err == nil || !strings.Contains(err.Error(), "head change could not be decided") {
		t.Fatalf("queue error = %v, want head change could not be decided", err)
	}
	if strings.Contains(err.Error(), "RCA class is our-code") {
		t.Fatalf("queue error = %v, used the our-code refusal", err)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}
}

func TestOurCodeLatestRemovalHeadChange(t *testing.T) {
	t1 := time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)
	t2 := t1.Add(time.Hour)
	twoRemovals := func() github.QueuePullRequest {
		pr := readyPR()
		pr.Events = []github.QueueEvent{
			ourCodeRemoval("R1", "mergegroup1", t1),
			{ID: "A1", Kind: github.QueueEventAdded, CreatedAt: t1.Add(time.Minute)},
			ourCodeRemoval("R2", "mergegroup2", t2),
		}
		pr.Comments = []string{"RCA R1 class=our-code", "RCA R2 class=our-code"}
		pr.HeadCommittedAt = t1.Add(-48 * time.Hour)
		return pr
	}
	const unchanged = "refusing to enqueue: our-code removal and head abc123 unchanged since 2026-01-02T04:04:05Z"
	const undecided = "head change could not be decided"
	tests := []struct {
		name    string
		edit    func(*github.QueuePullRequest)
		wantErr string
	}{
		{name: "two removals on the same head", edit: func(*github.QueuePullRequest) {}, wantErr: unchanged},
		{name: "force push after the last removal", edit: func(pr *github.QueuePullRequest) {
			pr.ForcePushedAt = []time.Time{t2.Add(time.Minute)}
		}},
		{name: "new commit after the last removal", edit: func(pr *github.QueuePullRequest) {
			pr.HeadCommittedAt = t2.Add(time.Minute)
		}},
		{name: "push between removals only", edit: func(pr *github.QueuePullRequest) {
			pr.ForcePushedAt = []time.Time{t1.Add(30 * time.Minute)}
			pr.HeadCommittedAt = t1.Add(29 * time.Minute)
			pr.HeadPushedAt = t1.Add(30 * time.Minute)
		}, wantErr: unchanged},
		{name: "contradictory head dates", edit: func(pr *github.QueuePullRequest) {
			pr.HeadCommittedAt = t2.Add(time.Minute)
			pr.HeadPushedAt = t2.Add(-time.Minute)
		}, wantErr: undecided},
		{name: "null beforeCommit and no date evidence", edit: func(pr *github.QueuePullRequest) {
			for i := range pr.Events {
				pr.Events[i].CommitSHA = ""
			}
			pr.HeadCommittedAt = time.Time{}
		}, wantErr: undecided},
	}
	for _, tt := range tests {
		for _, dry := range []bool{false, true} {
			t.Run(fmt.Sprintf("%s dry=%v", tt.name, dry), func(t *testing.T) {
				withMergeFlags(t, dry, false, false)
				pr := twoRemovals()
				tt.edit(&pr)
				client := &fakeQueue{pr: pr}
				var err error
				output := captureStdout(t, func() {
					err = queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false)
				})
				if tt.wantErr != "" {
					if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
						t.Fatalf("queue error = %v, want %q", err, tt.wantErr)
					}
					if strings.Contains(output, "pushed since") || client.enqueueCalls != 0 {
						t.Fatalf("enqueueCalls = %d output:\n%s", client.enqueueCalls, output)
					}
					return
				}
				if err != nil {
					t.Fatalf("queuePullRequest() error = %v", err)
				}
				want := "our-code ejection at 2026-01-02T04:04:05Z; head abc123 pushed since, proceeding"
				if !strings.Contains(output, want) {
					t.Fatalf("output = %q, want %q", output, want)
				}
				wantCalls := 1
				if dry {
					wantCalls = 0
				}
				if client.enqueueCalls != wantCalls {
					t.Fatalf("enqueueCalls = %d, want %d", client.enqueueCalls, wantCalls)
				}
			})
		}
	}
}

func TestOurCodeMissingCommentStillRefuses(t *testing.T) {
	pr := readyPR()
	pr.Events = []github.QueueEvent{
		ourCodeRemoval("EVT", "deadbeef", time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)),
	}
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false)
		if err == nil || !strings.Contains(err.Error(), "no RCA comment") {
			t.Fatalf("queue error = %v, want no RCA comment", err)
		}
	})
	if strings.Contains(output, "pushed since") {
		t.Fatalf("missing RCA printed the proceed line:\n%s", output)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}
}

func TestOurCodeRequeueIsOutsideMaxRequeues(t *testing.T) {
	base := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	added := func(id string, at time.Time) github.QueueEvent {
		return github.QueueEvent{ID: id, Kind: github.QueueEventAdded, CreatedAt: at}
	}
	removed := func(id string, at time.Time) github.QueueEvent {
		return github.QueueEvent{ID: id, Kind: github.QueueEventRemoved, CreatedAt: at, Reason: "failed_checks"}
	}
	events := []github.QueueEvent{
		added("A0", base),
		ourCodeRemoval("OUR", "deadbeef", base.Add(time.Minute)),
		added("A-OUR", base.Add(2*time.Minute)),
		removed("R1", base.Add(3*time.Minute)),
		added("A1", base.Add(4*time.Minute)),
		removed("R2", base.Add(5*time.Minute)),
		added("A2", base.Add(6*time.Minute)),
		removed("EVT", base.Add(7*time.Minute)),
	}
	comments := []string{
		"RCA OUR class=our-code",
		"RCA EVT class=flake",
	}
	limit := 2
	cfg := rcaConfig()
	cfg.Settings.MaxRequeues = &limit

	pr := readyPR()
	pr.HeadSHA = "abc123"
	pr.Events = events
	pr.Comments = comments
	client := &fakeQueue{pr: pr}
	if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err != nil {
		t.Fatalf("queue at the flake cap error = %v", err)
	}
	if client.enqueueCalls != 1 {
		t.Fatalf("enqueueCalls = %d, want the flake requeue allowed", client.enqueueCalls)
	}

	events = append(events,
		added("A3", base.Add(8*time.Minute)),
		removed("NEXT", base.Add(9*time.Minute)),
	)
	pr.Events = events
	comments = append(comments, "RCA NEXT class=flake")
	pr.Comments = comments
	client = &fakeQueue{pr: pr}
	err := queuePullRequest(client, cfg, pr.Ref.Raw, false)
	if err == nil || !strings.Contains(err.Error(), "3 earlier requeues is greater than max_requeues 2") {
		t.Fatalf("queue error = %v, want the flake count", err)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}
}

func TestRCARefusesWithoutPosting(t *testing.T) {
	path := writeRCAFile(t, "note\n")
	pr := readyPR()
	pr.Events = []github.QueueEvent{{
		ID: "NEW", Kind: github.QueueEventRemoved, CreatedAt: time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC), Reason: "failed_checks",
	}}
	tests := []struct {
		name string
		args []string
		cfg  *config.Config
		pr   github.QueuePullRequest
	}{
		{
			name: "unknown class",
			args: []string{"acme/widgets#12", "--class", "human", "--file", path},
			cfg:  rcaConfig(),
			pr:   pr,
		},
		{
			name: "missing file",
			args: []string{"acme/widgets#12", "--class", "flake", "--file", path + "-missing"},
			cfg:  rcaConfig(),
			pr:   pr,
		},
		{
			name: "empty marker",
			args: []string{"acme/widgets#12", "--class", "infra", "--file", path},
			cfg:  queueConfig(),
			pr:   pr,
		},
		{
			name: "no removal",
			args: []string{"acme/widgets#12", "--class", "flake", "--file", path},
			cfg:  rcaConfig(),
			pr:   readyPR(),
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			client := &fakeQueue{pr: tc.pr}
			if err := postRCA(client, tc.cfg, tc.args); err == nil {
				t.Fatal("postRCA() error = nil")
			}
			if client.commentCalls != 0 {
				t.Fatalf("commentCalls = %d", client.commentCalls)
			}
		})
	}
}

func TestRCARefusesEmptyFileWithoutPosting(t *testing.T) {
	pr := readyPR()
	pr.Events = []github.QueueEvent{{
		ID: "NEW", Kind: github.QueueEventRemoved, CreatedAt: time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC), Reason: "failed_checks",
	}}
	tests := []struct {
		name string
		body string
	}{
		{name: "empty", body: ""},
		{name: "whitespace", body: " \t\n\n  \t"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			path := writeRCAFile(t, tc.body)
			client := &fakeQueue{pr: pr}
			err := postRCA(client, rcaConfig(), []string{"acme/widgets#12", "--class", "flake", "--file", path})
			if err == nil {
				t.Fatal("postRCA() error = nil, want empty file")
			}
			if client.commentCalls != 0 {
				t.Fatalf("commentCalls = %d, want 0", client.commentCalls)
			}
		})
	}

	path := writeRCAFile(t, "Investigated the reset.\n")
	client := &fakeQueue{pr: pr}
	if err := postRCA(client, rcaConfig(), []string{"acme/widgets#12", "--class", "flake", "--file", path}); err != nil {
		t.Fatalf("postRCA() error = %v", err)
	}
	if client.commentCalls != 1 {
		t.Fatalf("commentCalls = %d, want 1", client.commentCalls)
	}
	body := client.comments[0]
	if !strings.Contains(body, "Investigated the reset.") || !strings.Contains(body, "RCA NEW class=flake") {
		t.Fatalf("comment = %q", body)
	}
}

func TestRCABareNumberUsesCheckoutRemote(t *testing.T) {
	dir := initGitRemote(t, "origin", "https://github.com/acme/widgets.git")
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name == "gh" {
			t.Errorf("gh was called: %v", args)
			return "", errors.New("gh should not be called")
		}
		return defaultRunCommand(name, args...)
	})
	t.Chdir(dir)
	path := writeRCAFile(t, "note\n")
	pr := readyPR()
	pr.Events = []github.QueueEvent{{
		ID: "NEW", Kind: github.QueueEventRemoved, CreatedAt: time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC),
	}}
	client := &fakeQueue{pr: pr}
	if err := postRCA(client, rcaConfig(), []string{"12", "--class", "infra", "--file", path}); err != nil {
		t.Fatalf("postRCA() error = %v", err)
	}
	if len(client.loadedRefs) != 1 || client.loadedRefs[0] != "acme/widgets#12" {
		t.Fatalf("loaded = %v", client.loadedRefs)
	}
	if client.commentCalls != 1 || !strings.Contains(client.comments[0], "RCA NEW class=infra") {
		t.Fatalf("comment = %v", client.comments)
	}
}

func TestDequeueRemovesQueuedPullRequest(t *testing.T) {
	withMergeFlags(t, false, false, false)
	withDequeueClock(t)
	pr := readyPR()
	pr.InMergeQueue = true
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := dequeueCommand(client, []string{"acme/widgets#12"}); err != nil {
			t.Fatalf("dequeueCommand() error = %v", err)
		}
	})
	if client.dequeueCalls != 1 || len(client.dequeued) != 1 || client.dequeued[0] != "PR_NODE" {
		t.Fatalf("dequeued = %v calls = %d", client.dequeued, client.dequeueCalls)
	}
	if !strings.Contains(output, "Dequeued acme/widgets#12.") {
		t.Fatalf("output = %q", output)
	}
}

func TestDequeueDryRunDoesNotMutate(t *testing.T) {
	withMergeFlags(t, false, false, false)
	pr := readyPR()
	pr.InMergeQueue = true
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := dequeueCommand(client, []string{"-dry-run", "acme/widgets#12"}); err != nil {
			t.Fatalf("dequeueCommand() error = %v", err)
		}
	})
	if client.dequeueCalls != 0 {
		t.Fatalf("dequeueCalls = %d", client.dequeueCalls)
	}
	if !strings.Contains(output, "Would dequeue acme/widgets#12.") || strings.Contains(output, "Dequeued") {
		t.Fatalf("output = %q", output)
	}
	help := dryRunHelp()
	for _, want := range []string{"merge", "enqueue", "dequeue"} {
		if !strings.Contains(help, want) {
			t.Errorf("dry-run help %q missing %s", help, want)
		}
	}
}

func TestDequeueRefusesWhenNotQueued(t *testing.T) {
	withMergeFlags(t, false, false, false)
	client := &fakeQueue{pr: readyPR()}
	err := dequeueCommand(client, []string{"acme/widgets#12"})
	if err == nil || !strings.Contains(err.Error(), "not in the merge queue") {
		t.Fatalf("error = %v", err)
	}
	if client.dequeueCalls != 0 {
		t.Fatalf("dequeueCalls = %d", client.dequeueCalls)
	}
}

func TestDequeueBareNumberUsesCheckoutRemote(t *testing.T) {
	withMergeFlags(t, false, false, false)
	dir := initGitRemote(t, "origin", "https://github.com/acme/widgets.git")
	withCommandStub(t, func(name string, args ...string) (string, error) {
		if name == "gh" {
			t.Errorf("gh was called: %v", args)
			return "", errors.New("gh should not be called")
		}
		return defaultRunCommand(name, args...)
	})
	t.Chdir(dir)
	withDequeueClock(t)
	pr := readyPR()
	pr.InMergeQueue = true
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := dequeueCommand(client, []string{"12"}); err != nil {
			t.Fatalf("dequeueCommand() error = %v", err)
		}
	})
	for _, ref := range client.loadedRefs {
		if ref != "acme/widgets#12" {
			t.Fatalf("loaded = %v", client.loadedRefs)
		}
	}
	if client.dequeueCalls != 1 || !strings.Contains(output, "Dequeued acme/widgets#12.") {
		t.Fatalf("calls = %d output = %q", client.dequeueCalls, output)
	}
}

func rcaConfig() *config.Config {
	cfg := queueConfig()
	cfg.Settings.RCAMarker = "RCA {event_id} class={class}"
	return cfg
}

func writeRCAFile(t *testing.T, body string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "body.md")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func stringListPtr(items ...string) *[]string {
	copied := append([]string(nil), items...)
	return &copied
}

func emptyStringList() *[]string {
	items := []string{}
	return &items
}

func TestQueueRepoEntryOverridesRequiredChecksAndInheritsLabels(t *testing.T) {
	pr := readyPR()
	pr.Checks = []github.QueueCheck{{Name: "lint", Green: true, Conclusion: "SUCCESS"}}
	cfg := queueConfig()
	cfg.Path = "/tmp/loaded.pkl"
	cfg.Settings.BlockMergeLabels = []string{"do-not-merge"}
	cfg.Settings.RequiredChecks = []string{"ci"}
	cfg.Settings.FlakeSignatures = []string{"global-flake"}
	cfg.RepoSettings = map[string]config.RepoSettings{
		"acme/widgets": {RequiredChecks: stringListPtr("lint")},
	}
	withMergeFlags(t, true, false, false)
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, cfg, pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	for _, want := range []string{
		"settings from: acme/widgets",
		"required_checks: lint",
		"block_merge_labels: do-not-merge",
		"Would enqueue acme/widgets#12.",
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	if strings.Contains(output, "required check \"ci\"") {
		t.Fatalf("queue used the global required check:\n%s", output)
	}
	jsonOutput = true
	t.Cleanup(func() { jsonOutput = false })
	jsonOut := captureStdout(t, func() {
		if err := queuePullRequest(&fakeQueue{pr: pr}, cfg, pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	assertEffectiveConfigJSON(t, jsonOut, "would-enqueue", "/tmp/loaded.pkl", "acme/widgets", []string{"do-not-merge"}, []string{"lint"})
}

func TestQueueExplicitEmptyBlockLabelsOverrideGlobal(t *testing.T) {
	pr := readyPR()
	pr.Labels = []string{"do-not-merge"}
	cfg := queueConfig()
	cfg.Path = "/tmp/loaded.pkl"
	cfg.Settings.BlockMergeLabels = []string{"do-not-merge"}
	cfg.RepoSettings = map[string]config.RepoSettings{
		"acme/widgets": {BlockMergeLabels: emptyStringList()},
	}
	withMergeFlags(t, true, false, false)
	output := captureStdout(t, func() {
		if err := queuePullRequest(&fakeQueue{pr: pr}, cfg, pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	for _, want := range []string{
		"settings from: acme/widgets",
		"block_merge_labels: (none)",
		"Would enqueue acme/widgets#12.",
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	if strings.Contains(output, "blocks merge") {
		t.Fatalf("empty block list inherited the global label:\n%s", output)
	}
}

func TestQueueEmptyEntryRequiredChecksRefuses(t *testing.T) {
	pr := readyPR()
	cfg := queueConfig()
	cfg.Path = "/tmp/loaded.pkl"
	cfg.Settings.RequiredChecks = []string{"ci"}
	cfg.RepoSettings = map[string]config.RepoSettings{
		"acme/widgets": {RequiredChecks: emptyStringList()},
	}
	output := captureStdout(t, func() {
		if err := queuePullRequest(&fakeQueue{pr: pr}, cfg, pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	for _, want := range []string{
		"settings from: acme/widgets",
		"required_checks is empty",
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}

	withAllowNoRequiredChecks(t, true)
	withMergeFlags(t, true, false, false)
	allowed := captureStdout(t, func() {
		if err := queuePullRequest(&fakeQueue{pr: pr}, cfg, pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if !strings.Contains(allowed, "Would enqueue acme/widgets#12.") {
		t.Fatalf("allow flag did not bypass an empty entry list:\n%s", allowed)
	}

	pr.Draft = true
	draft := captureStdout(t, func() {
		if err := queuePullRequest(&fakeQueue{pr: pr}, cfg, pr.Ref.Raw, false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want draft refusal")
		}
	})
	if !strings.Contains(draft, "pull request is a draft") {
		t.Fatalf("allow flag skipped another gate:\n%s", draft)
	}
}

func TestWatchJSONIncludesConfigAndSettingsFrom(t *testing.T) {
	widgets := readyPR()
	widgets.Merged = true
	other := readyPR()
	other.Ref = github.PullRef{Owner: "acme", Repo: "other", Number: 3, Raw: "acme/other#3"}
	other.Merged = true
	client := &fakeQueue{byRef: map[string]github.QueuePullRequest{
		widgets.Ref.Raw: widgets,
		other.Ref.Raw:   other,
	}}
	cfg := queueConfig()
	cfg.Path = "/tmp/loaded.pkl"
	cfg.RepoSettings = map[string]config.RepoSettings{
		"acme/widgets": {RequiredChecks: stringListPtr("lint")},
	}
	output := captureStdout(t, func() {
		if err := watchPullRequests(client, []string{widgets.Ref.Raw, other.Ref.Raw}, watchOptions{
			timeout: time.Minute,
			asJSON:  true,
			cfg:     cfg,
		}); err != nil {
			t.Fatalf("watchPullRequests() error = %v", err)
		}
	})
	for _, want := range []string{
		`"config": "/tmp/loaded.pkl"`,
		`"settingsFrom": "acme/widgets"`,
		`"settingsFrom": "global"`,
		`"result": "merged"`,
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	text := captureStdout(t, func() {
		if err := watchPullRequests(client, []string{widgets.Ref.Raw}, watchOptions{timeout: time.Minute, cfg: cfg}); err != nil {
			t.Fatalf("watchPullRequests() error = %v", err)
		}
	})
	if text != "acme/widgets#12 merged\n" {
		t.Fatalf("text watch = %q", text)
	}
}

func TestRCAJSONUsesRepoMarker(t *testing.T) {
	path := writeRCAFile(t, "Investigated the reset.\n")
	pr := readyPR()
	pr.Events = []github.QueueEvent{{
		ID:        "NEW",
		Kind:      github.QueueEventRemoved,
		CreatedAt: time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC),
		Reason:    "failed_checks",
	}}
	marker := "ENTRY {event_id} class={class}"
	cfg := queueConfig()
	cfg.Path = "/tmp/loaded.pkl"
	cfg.RepoSettings = map[string]config.RepoSettings{
		"acme/widgets": {RCAMarker: &marker},
	}
	client := &fakeQueue{pr: pr}
	jsonOutput = true
	t.Cleanup(func() { jsonOutput = false })
	output := captureStdout(t, func() {
		err := postRCA(client, cfg, []string{"acme/widgets#12", "--class", "flake", "--file", path})
		if err != nil {
			t.Fatalf("postRCA() error = %v", err)
		}
	})
	for _, want := range []string{
		`"config": "/tmp/loaded.pkl"`,
		`"settingsFrom": "acme/widgets"`,
		`"pullRequest": "acme/widgets#12"`,
	} {
		if !strings.Contains(output, want) {
			t.Errorf("output missing %q:\n%s", want, output)
		}
	}
	if strings.Contains(output, "Commented on") {
		t.Fatalf("json rca printed text: %s", output)
	}
	if !strings.Contains(client.comments[0], "ENTRY NEW class=flake") {
		t.Fatalf("comment = %q", client.comments[0])
	}

	jsonOutput = false
	blank := ""
	cfg.Settings.RCAMarker = "RCA {event_id} class={class}"
	cfg.RepoSettings["acme/widgets"] = config.RepoSettings{RCAMarker: &blank}
	err := postRCA(&fakeQueue{pr: pr}, cfg, []string{"acme/widgets#12", "--class", "flake", "--file", path})
	if err == nil || err.Error() != "rca_marker is empty" {
		t.Fatalf("postRCA() error = %v", err)
	}
}

func pendingWidgets() github.QueuePullRequest {
	pr := readyPR()
	pr.Checks = []github.QueueCheck{{Name: "ci", Green: false, Conclusion: ""}}
	return pr
}

func withWhenGreenWait(t *testing.T, timeout time.Duration) *[]time.Duration {
	t.Helper()
	prevFlag := whenGreen
	prevTimeout := whenGreenTimeout
	prevNow := whenGreenNow
	prevSleep := whenGreenSleep
	whenGreen = true
	whenGreenTimeout = timeout
	var slept []time.Duration
	now := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	whenGreenNow = func() time.Time { return now }
	whenGreenSleep = func(d time.Duration) {
		slept = append(slept, d)
		now = now.Add(d)
	}
	t.Cleanup(func() {
		whenGreen = prevFlag
		whenGreenTimeout = prevTimeout
		whenGreenNow = prevNow
		whenGreenSleep = prevSleep
	})
	return &slept
}

func TestWhenGreenTimeoutDefaultsToTwoHours(t *testing.T) {
	if defaultWhenGreenTimeout != 2*time.Hour {
		t.Fatalf("defaultWhenGreenTimeout = %s", defaultWhenGreenTimeout)
	}
	if !strings.Contains(whenGreenHelp(), "2h") || !strings.Contains(queueCommandHelp(), "--when-green") {
		t.Fatalf("help = %q / %q", whenGreenHelp(), queueCommandHelp())
	}
	if !strings.Contains(timeoutHelp(), "2h") || !strings.Contains(timeoutHelp(), "30m") {
		t.Fatalf("timeout help = %q", timeoutHelp())
	}
}

func TestCheckConclusionKind(t *testing.T) {
	pending := []string{"", "PENDING", "EXPECTED", "QUEUED", "IN_PROGRESS", "WAITING", "NEUTRAL", "SKIPPED"}
	for _, conclusion := range pending {
		if got := checkConclusionKind(conclusion); got != "pending" {
			t.Errorf("checkConclusionKind(%q) = %s, want pending", conclusion, got)
		}
	}
	for _, conclusion := range []string{"FAILURE", "CANCELLED", "TIMED_OUT", "ACTION_REQUIRED", "failure"} {
		if got := checkConclusionKind(conclusion); got != "failure" {
			t.Errorf("checkConclusionKind(%q) = %s, want failure", conclusion, got)
		}
	}
	if got := checkConclusionKind("success"); got != "success" {
		t.Fatalf("checkConclusionKind(success) = %s", got)
	}
}

func TestNextWhenGreenDelayBacksOff(t *testing.T) {
	delay := whenGreenInitialDelay
	var got []time.Duration
	for range 4 {
		got = append(got, delay)
		delay = nextWhenGreenDelay(delay)
	}
	want := []time.Duration{15 * time.Second, 30 * time.Second, time.Minute, time.Minute}
	if len(got) != len(want) {
		t.Fatalf("delays = %v", got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("delays = %v, want %v", got, want)
		}
	}
}

func TestQueueWhenGreenEnqueuesOnceChecksPass(t *testing.T) {
	slept := withWhenGreenWait(t, time.Hour)
	first := pendingWidgets()
	second := first
	second.Checks = []github.QueueCheck{{Name: "ci", Green: true, Conclusion: "SUCCESS"}}
	client := &fakeQueue{snapshots: []github.QueuePullRequest{first, second}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), first.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if client.enqueueCalls != 1 || len(client.enqueued) != 1 || client.enqueued[0] != first.NodeID {
		t.Fatalf("enqueued = %v calls = %d", client.enqueued, client.enqueueCalls)
	}
	if client.loads != 2 {
		t.Fatalf("loads = %d, want 2", client.loads)
	}
	if !strings.Contains(output, "Enqueued acme/widgets#12.") {
		t.Fatalf("output = %s", output)
	}
	if len(*slept) != 1 || (*slept)[0] != whenGreenInitialDelay {
		t.Fatalf("sleeps = %v", *slept)
	}
}

func TestQueueWhenGreenAbortsWithoutEnqueue(t *testing.T) {
	first := pendingWidgets()
	tests := []struct {
		name   string
		mutate func(*github.QueuePullRequest)
		want   string
	}{
		{
			name: "failure",
			mutate: func(pr *github.QueuePullRequest) {
				pr.Checks = []github.QueueCheck{{Name: "ci", Green: false, Conclusion: "FAILURE"}}
			},
			want: `required check "ci" failed (FAILURE)`,
		},
		{
			name: "head SHA",
			mutate: func(pr *github.QueuePullRequest) {
				pr.HeadSHA = "def456"
			},
			want: "head SHA changed",
		},
		{
			name: "approval",
			mutate: func(pr *github.QueuePullRequest) {
				pr.ReviewDecision = "CHANGES_REQUESTED"
			},
			want: "pull request is not approved",
		},
		{
			name: "unresolved thread",
			mutate: func(pr *github.QueuePullRequest) {
				pr.UnresolvedThreads = 1
			},
			want: "unresolved review thread",
		},
		{
			name: "draft appears",
			mutate: func(pr *github.QueuePullRequest) {
				pr.Draft = true
			},
			want: "pull request is a draft",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			withWhenGreenWait(t, time.Hour)
			second := first
			tc.mutate(&second)
			client := &fakeQueue{snapshots: []github.QueuePullRequest{first, second}}
			var err error
			output := captureStdout(t, func() {
				err = queuePullRequest(client, queueConfig(), first.Ref.Raw, false)
			})
			if err == nil {
				t.Fatal("queuePullRequest() error = nil, want abort")
			}
			if client.enqueueCalls != 0 || len(client.mergedAsync) != 0 {
				t.Fatalf("enqueued = %v async = %v", client.enqueued, client.mergedAsync)
			}
			if !strings.Contains(output, tc.want) || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("output = %s\nerr = %v", output, err)
			}
		})
	}
}

func TestQueueWhenGreenJSONReportsAbortReason(t *testing.T) {
	withWhenGreenWait(t, time.Hour)
	jsonOutput = true
	t.Cleanup(func() { jsonOutput = false })
	first := pendingWidgets()
	second := first
	second.HeadSHA = "def456"
	client := &fakeQueue{snapshots: []github.QueuePullRequest{first, second}}
	output := captureStdout(t, func() {
		err := queuePullRequest(client, queueConfig(), first.Ref.Raw, false)
		if err == nil || !strings.Contains(err.Error(), "head SHA changed") {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	var got queueJSONResult
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("json.Unmarshal() error = %v\n%s", err, output)
	}
	if got.Result != "head SHA changed" || got.PullRequest != "acme/widgets#12" {
		t.Fatalf("result = %+v", got)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}
}

func TestQueueWhenGreenDryRunDoesNotPoll(t *testing.T) {
	slept := withWhenGreenWait(t, time.Hour)
	withMergeFlags(t, true, false, false)
	t.Run("would-wait", func(t *testing.T) {
		client := &fakeQueue{pr: pendingWidgets()}
		output := captureStdout(t, func() {
			if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
		})
		if client.loads != 1 || len(*slept) != 0 || client.enqueueCalls != 0 {
			t.Fatalf("loads = %d sleeps = %v enqueued = %v", client.loads, *slept, client.enqueued)
		}
		if !strings.Contains(output, "Would wait for acme/widgets#12.") {
			t.Fatalf("output = %s", output)
		}
	})
	t.Run("would-enqueue", func(t *testing.T) {
		client := &fakeQueue{pr: readyPR()}
		output := captureStdout(t, func() {
			if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
		})
		if client.loads != 1 || client.enqueueCalls != 0 {
			t.Fatalf("loads = %d enqueued = %v", client.loads, client.enqueued)
		}
		if !strings.Contains(output, "Would enqueue acme/widgets#12.") || strings.Contains(output, "Would wait") {
			t.Fatalf("output = %s", output)
		}
	})
	t.Run("json would-wait", func(t *testing.T) {
		jsonOutput = true
		t.Cleanup(func() { jsonOutput = false })
		client := &fakeQueue{pr: pendingWidgets()}
		output := captureStdout(t, func() {
			if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
		})
		var got queueJSONResult
		if err := json.Unmarshal([]byte(output), &got); err != nil {
			t.Fatalf("json.Unmarshal() error = %v\n%s", err, output)
		}
		if got.Result != "would-wait" {
			t.Fatalf("result = %+v", got)
		}
		if client.loads != 1 || client.enqueueCalls != 0 {
			t.Fatalf("loads = %d enqueued = %v", client.loads, client.enqueued)
		}
	})
}

func TestQueuePendingRefusesImmediatelyWithoutWhenGreen(t *testing.T) {
	if whenGreen {
		t.Fatal("whenGreen leaked from another test")
	}
	client := &fakeQueue{pr: pendingWidgets()}
	output := captureStdout(t, func() {
		err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false)
		if err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	if client.loads != 1 || client.enqueueCalls != 0 {
		t.Fatalf("loads = %d enqueued = %v", client.loads, client.enqueued)
	}
	if !strings.Contains(output, `required check "ci" is not green (not green)`) {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueWhenGreenRefusesOtherProblemsImmediately(t *testing.T) {
	tests := []struct {
		name string
		pr   github.QueuePullRequest
		want string
	}{
		{name: "draft", pr: func() github.QueuePullRequest {
			pr := pendingWidgets()
			pr.Draft = true
			return pr
		}(), want: "pull request is a draft"},
		{name: "failure", pr: func() github.QueuePullRequest {
			pr := readyPR()
			pr.Checks = []github.QueueCheck{{Name: "ci", Green: false, Conclusion: "FAILURE"}}
			return pr
		}(), want: `required check "ci" is not green (FAILURE)`},
		{name: "missing and not approved", pr: func() github.QueuePullRequest {
			pr := readyPR()
			pr.Checks = nil
			pr.ReviewDecision = "REVIEW_REQUIRED"
			return pr
		}(), want: `required check "ci" is missing`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			slept := withWhenGreenWait(t, time.Hour)
			client := &fakeQueue{pr: tc.pr}
			output := captureStdout(t, func() {
				err := queuePullRequest(client, queueConfig(), tc.pr.Ref.Raw, false)
				if err == nil {
					t.Fatal("queuePullRequest() error = nil, want refusal")
				}
			})
			if client.loads != 1 || len(*slept) != 0 || client.enqueueCalls != 0 {
				t.Fatalf("loads = %d sleeps = %v enqueued = %v", client.loads, *slept, client.enqueued)
			}
			if !strings.Contains(output, tc.want) {
				t.Fatalf("output = %s", output)
			}
		})
	}
}

func TestQueueWhenGreenRefusesRCABeforeWaiting(t *testing.T) {
	slept := withWhenGreenWait(t, time.Hour)
	pr := pendingWidgets()
	pr.Events = []github.QueueEvent{{
		ID:        "EVT1",
		Kind:      github.QueueEventRemoved,
		CreatedAt: time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC),
		Reason:    "failed_checks",
	}}
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		err := queuePullRequest(client, queueConfig(), pr.Ref.Raw, false)
		if err == nil || !strings.Contains(err.Error(), "no RCA comment") {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if client.loads != 1 || len(*slept) != 0 || client.enqueueCalls != 0 {
		t.Fatalf("loads = %d sleeps = %v enqueued = %v", client.loads, *slept, client.enqueued)
	}
	if strings.Contains(output, "Would wait") || strings.Contains(output, "Enqueued") {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueWhenGreenRetriesTransientLoad(t *testing.T) {
	withWhenGreenWait(t, time.Hour)
	first := pendingWidgets()
	success := first
	success.Checks = []github.QueueCheck{{Name: "ci", Green: true, Conclusion: "SUCCESS"}}
	client := &fakeQueue{
		snapshots: []github.QueuePullRequest{first, first, success},
		loadErrs:  []error{nil, errors.New("http 502"), nil},
	}
	if err := queuePullRequest(client, queueConfig(), first.Ref.Raw, false); err != nil {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if client.loads != 3 || client.enqueueCalls != 1 {
		t.Fatalf("loads = %d enqueued = %v", client.loads, client.enqueued)
	}
}

func TestQueueWhenGreenHardLoadErrorDoesNotEnqueue(t *testing.T) {
	withWhenGreenWait(t, time.Hour)
	first := pendingWidgets()
	client := &fakeQueue{
		snapshots: []github.QueuePullRequest{first, first},
		loadErrs:  []error{nil, errors.New("boom")},
	}
	err := queuePullRequest(client, queueConfig(), first.Ref.Raw, false)
	if err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if client.loads != 2 || client.enqueueCalls != 0 {
		t.Fatalf("loads = %d enqueued = %v", client.loads, client.enqueued)
	}
}

func TestQueueWhenGreenTimesOut(t *testing.T) {
	slept := withWhenGreenWait(t, 20*time.Second)
	client := &fakeQueue{pr: pendingWidgets()}
	var err error
	output := captureStdout(t, func() {
		err = queuePullRequest(client, queueConfig(), "acme/widgets#12", false)
	})
	if err == nil || !strings.Contains(err.Error(), "timeout") {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueued = %v", client.enqueued)
	}
	if !strings.Contains(output, "Not enqueueing acme/widgets#12: timeout") {
		t.Fatalf("output = %s", output)
	}
	if len(*slept) != 2 || (*slept)[0] != 15*time.Second || (*slept)[1] != 5*time.Second {
		t.Fatalf("sleeps = %v", *slept)
	}
}

func TestQueueWhenGreenBackoffWhileStillPending(t *testing.T) {
	slept := withWhenGreenWait(t, time.Hour)
	first := pendingWidgets()
	second := first
	second.Checks = []github.QueueCheck{{Name: "ci", Green: false, Conclusion: "IN_PROGRESS"}}
	third := first
	third.Checks = []github.QueueCheck{{Name: "ci", Green: true, Conclusion: "SUCCESS"}}
	client := &fakeQueue{snapshots: []github.QueuePullRequest{first, second, third}}
	if err := queuePullRequest(client, queueConfig(), first.Ref.Raw, false); err != nil {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if client.enqueueCalls != 1 || client.loads != 3 {
		t.Fatalf("loads = %d enqueued = %v", client.loads, client.enqueued)
	}
	if len(*slept) != 2 || (*slept)[0] != 15*time.Second || (*slept)[1] != 30*time.Second {
		t.Fatalf("sleeps = %v", *slept)
	}
}

func gateLineOf(t *testing.T, output string) string {
	t.Helper()
	var found []string
	for _, line := range strings.Split(output, "\n") {
		if strings.HasPrefix(line, "latest queue event:") {
			found = append(found, line)
		}
	}
	if len(found) != 1 {
		t.Fatalf("gate lines = %q, want exactly one in:\n%s", found, output)
	}
	return found[0]
}

func TestQueueGateLine(t *testing.T) {
	at := time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)
	settings := rcaConfig().Settings
	tests := []struct {
		name     string
		events   []github.QueueEvent
		comments []string
		want     string
	}{
		{name: "no events", want: "latest queue event: none"},
		{
			name:   "added",
			events: []github.QueueEvent{{ID: "A1", Kind: github.QueueEventAdded, CreatedAt: at}},
			want:   "latest queue event: added A1 at 2026-01-02T03:04:05Z",
		},
		{
			name:   "removed without RCA",
			events: []github.QueueEvent{ourCodeRemoval("EVT", "", at)},
			want:   "latest queue event: removed EVT at 2026-01-02T03:04:05Z; RCA: none",
		},
		{
			name:     "removed with flake RCA",
			events:   []github.QueueEvent{ourCodeRemoval("EVT", "", at)},
			comments: []string{"RCA EVT class=flake"},
			want:     "latest queue event: removed EVT at 2026-01-02T03:04:05Z; RCA: flake (matched)",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pr := readyPR()
			pr.Events = tt.events
			pr.Comments = tt.comments
			if got := gateLine(pr, settings); got != tt.want {
				t.Fatalf("gateLine() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestQueueDryRunAndRealRunShareThePreflight(t *testing.T) {
	at := time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)
	removal := func(pr *github.QueuePullRequest, comment string) {
		pr.Events = []github.QueueEvent{ourCodeRemoval("EVT", "", at)}
		if comment != "" {
			pr.Comments = []string{comment}
		}
	}
	tests := []struct {
		name    string
		edit    func(*github.QueuePullRequest)
		wantErr string
	}{
		{name: "problems", edit: func(pr *github.QueuePullRequest) { pr.Draft = true }, wantErr: "refusing to enqueue acme/widgets#12"},
		{name: "missing RCA", edit: func(pr *github.QueuePullRequest) { removal(pr, "") }, wantErr: "no RCA comment for that removal"},
		{name: "our-code without a new head", edit: func(pr *github.QueuePullRequest) {
			removal(pr, "RCA EVT class=our-code")
			pr.HeadCommittedAt = at.Add(-time.Hour)
		}, wantErr: "unchanged since"},
		{name: "requeue limit", edit: func(pr *github.QueuePullRequest) {
			pr.Events = []github.QueueEvent{
				{ID: "A0", Kind: github.QueueEventAdded, CreatedAt: at.Add(-3 * time.Minute)},
				ourCodeRemoval("R1", "", at.Add(-2*time.Minute)),
				{ID: "A1", Kind: github.QueueEventAdded, CreatedAt: at.Add(-time.Minute)},
				ourCodeRemoval("EVT", "", at),
			}
			pr.Comments = []string{"RCA EVT class=flake"}
		}, wantErr: "greater than max_requeues"},
		{name: "no refusal", edit: func(pr *github.QueuePullRequest) { removal(pr, "RCA EVT class=flake") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var errs, gates [2]string
			for i, dry := range []bool{true, false} {
				withMergeFlags(t, dry, false, false)
				pr := readyPR()
				tt.edit(&pr)
				cfg := rcaConfig()
				limit := 0
				cfg.Settings.MaxRequeues = &limit
				client := &fakeQueue{pr: pr}
				var err error
				output := captureStdout(t, func() {
					err = queuePullRequest(client, cfg, pr.Ref.Raw, false)
				})
				if err != nil {
					errs[i] = err.Error()
				}
				gates[i] = gateLineOf(t, output)
				wantCalls := 0
				if !dry && tt.wantErr == "" {
					wantCalls = 1
				}
				if client.enqueueCalls != wantCalls {
					t.Fatalf("dry=%v enqueueCalls = %d, want %d", dry, client.enqueueCalls, wantCalls)
				}
			}
			if errs[0] != errs[1] {
				t.Fatalf("dry run error %q, real run error %q", errs[0], errs[1])
			}
			if !strings.Contains(errs[0], tt.wantErr) || (tt.wantErr == "" && errs[0] != "") {
				t.Fatalf("error = %q, want %q", errs[0], tt.wantErr)
			}
			if gates[0] != gates[1] {
				t.Fatalf("dry run gate %q, real run gate %q", gates[0], gates[1])
			}
		})
	}
}

func TestQueueJSONIncludesGate(t *testing.T) {
	withMergeFlags(t, true, false, false)
	jsonOutput = true
	t.Cleanup(func() { jsonOutput = false })
	pr := readyPR()
	pr.Events = []github.QueueEvent{ourCodeRemoval("EVT", "", time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC))}
	pr.Comments = []string{"RCA EVT class=infra"}
	client := &fakeQueue{pr: pr}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	// Removal evidence is printed before the JSON document.
	_, doc, ok := strings.Cut(output, "{")
	if !ok {
		t.Fatalf("no JSON in %q", output)
	}
	var got queueJSONResult
	if err := json.Unmarshal([]byte("{"+doc), &got); err != nil {
		t.Fatalf("decode %q: %v", output, err)
	}
	want := "latest queue event: removed EVT at 2026-01-02T03:04:05Z; RCA: infra (matched)"
	if got.Gate != want {
		t.Fatalf("gate = %q, want %q", got.Gate, want)
	}
}

func withDequeueClock(t *testing.T) *[]time.Duration {
	t.Helper()
	prevNow, prevSleep := dequeueNow, dequeueSleep
	now := time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC)
	var slept []time.Duration
	dequeueNow = func() time.Time { return now }
	dequeueSleep = func(d time.Duration) {
		slept = append(slept, d)
		now = now.Add(d)
	}
	t.Cleanup(func() { dequeueNow, dequeueSleep = prevNow, prevSleep })
	return &slept
}

func queuedPR() github.QueuePullRequest {
	pr := readyPR()
	pr.InMergeQueue = true
	pr.Events = []github.QueueEvent{{ID: "A1", Kind: github.QueueEventAdded, CreatedAt: time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)}}
	return pr
}

func TestDequeueWaitsForRemovalBeforeQueueDryRun(t *testing.T) {
	withMergeFlags(t, false, false, false)
	slept := withDequeueClock(t)
	before := queuedPR()
	lagging := before
	lagging.InMergeQueue = false
	removed := lagging
	removed.Events = append(append([]github.QueueEvent{}, before.Events...),
		ourCodeRemoval("EVT", "", time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC)))
	client := &fakeQueue{snapshots: []github.QueuePullRequest{before, lagging, removed}}
	output := captureStdout(t, func() {
		if err := dequeueCommand(client, []string{"acme/widgets#12"}); err != nil {
			t.Fatalf("dequeueCommand() error = %v", err)
		}
	})
	if client.loads != 3 || len(*slept) != 2 {
		t.Fatalf("loads = %d sleeps = %v", client.loads, *slept)
	}
	if !strings.Contains(output, "Dequeued acme/widgets#12.") || strings.Contains(output, "Warning") {
		t.Fatalf("output = %q", output)
	}

	withMergeFlags(t, true, false, false)
	var err error
	captureStdout(t, func() {
		err = queuePullRequest(client, rcaConfig(), "acme/widgets#12", false)
	})
	if err == nil || !strings.Contains(err.Error(), "no RCA comment for that removal") {
		t.Fatalf("queue -dry-run error = %v, want no RCA comment for that removal", err)
	}
}

func TestDequeueWarnsWhenRemovalNeverAppears(t *testing.T) {
	withMergeFlags(t, false, false, false)
	slept := withDequeueClock(t)
	client := &fakeQueue{pr: queuedPR()}
	output := captureStdout(t, func() {
		if err := dequeueCommand(client, []string{"acme/widgets#12"}); err != nil {
			t.Fatalf("dequeueCommand() error = %v", err)
		}
	})
	var total time.Duration
	for _, d := range *slept {
		total += d
	}
	if total != dequeueSettleTimeout {
		t.Fatalf("slept %v in total (%v), want %v", total, *slept, dequeueSettleTimeout)
	}
	for _, want := range []string{
		"Dequeued acme/widgets#12.",
		"Warning: the timeline for acme/widgets#12 does not show the removal yet; queue checks may be stale",
	} {
		if !strings.Contains(output, want) {
			t.Fatalf("output = %q, want %q", output, want)
		}
	}
}

func withMergeableWait(t *testing.T) *[]time.Duration {
	t.Helper()
	prevNow, prevSleep := mergeableNow, mergeableSleep
	var slept []time.Duration
	now := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	mergeableNow = func() time.Time { return now }
	mergeableSleep = func(d time.Duration) {
		slept = append(slept, d)
		now = now.Add(d)
	}
	t.Cleanup(func() { mergeableNow, mergeableSleep = prevNow, prevSleep })
	return &slept
}

func mergeableSnapshots(states ...string) []github.QueuePullRequest {
	out := make([]github.QueuePullRequest, len(states))
	for i, state := range states {
		out[i] = readyPR()
		out[i].Mergeable = state
	}
	return out
}

func TestQueueDryRunWaitsForUnknownMergeable(t *testing.T) {
	slept := withMergeableWait(t)
	withMergeFlags(t, true, false, false)
	client := &fakeQueue{snapshots: mergeableSnapshots("UNKNOWN", "UNKNOWN", "MERGEABLE")}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if !strings.Contains(output, "Would enqueue acme/widgets#12.") {
		t.Fatalf("output = %s", output)
	}
	if client.loads != 3 || client.enqueueCalls != 0 {
		t.Fatalf("loads = %d enqueued = %v", client.loads, client.enqueued)
	}
	if len(*slept) != 2 || (*slept)[0] != 2*time.Second || (*slept)[1] != 4*time.Second {
		t.Fatalf("sleeps = %v", *slept)
	}
}

func TestQueueRefusesConflictAfterUnknownMergeable(t *testing.T) {
	for _, dry := range []bool{true, false} {
		t.Run(fmt.Sprintf("dry=%v", dry), func(t *testing.T) {
			withMergeableWait(t)
			withMergeFlags(t, dry, false, false)
			client := &fakeQueue{snapshots: mergeableSnapshots("UNKNOWN", "CONFLICTING")}
			output := captureStdout(t, func() {
				if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err == nil {
					t.Fatal("queuePullRequest() error = nil, want refusal")
				}
			})
			if !strings.Contains(output, "- pull request has merge conflicts") {
				t.Fatalf("output = %s", output)
			}
			if client.enqueueCalls != 0 || strings.Contains(output, "Would enqueue") {
				t.Fatalf("enqueued = %v output = %s", client.enqueued, output)
			}
		})
	}
}

func TestQueueRefusesStillUnknownMergeable(t *testing.T) {
	slept := withMergeableWait(t)
	withMergeFlags(t, true, false, false)
	client := &fakeQueue{snapshots: mergeableSnapshots("UNKNOWN")}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	if !strings.Contains(output, "- mergeable state still UNKNOWN after 60s; GitHub has not computed it yet, retry later") {
		t.Fatalf("output = %s", output)
	}
	var total time.Duration
	for _, d := range *slept {
		total += d
	}
	if total > 60*time.Second || total < 50*time.Second {
		t.Fatalf("total sleep = %s, sleeps = %v", total, *slept)
	}
	for i := 1; i < len(*slept)-1; i++ {
		if (*slept)[i] < (*slept)[i-1] {
			t.Fatalf("sleeps shrink before the last one: %v", *slept)
		}
	}
}

func TestQueueRefusesConflictWithoutWaiting(t *testing.T) {
	slept := withMergeableWait(t)
	client := &fakeQueue{snapshots: mergeableSnapshots("CONFLICTING")}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	if !strings.Contains(output, "- pull request has merge conflicts") {
		t.Fatalf("output = %s", output)
	}
	if len(*slept) != 0 || client.loads != 1 || client.enqueueCalls != 0 {
		t.Fatalf("sleeps = %v loads = %d enqueued = %v", *slept, client.loads, client.enqueued)
	}
}

func TestQueueWhenGreenTreatsStillUnknownMergeableAsPending(t *testing.T) {
	withMergeableWait(t)
	withWhenGreenWait(t, time.Hour)
	// One load, six re-reads within the 60s preflight budget, then one
	// when-green reload that sees MERGEABLE.
	states := []string{"UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "MERGEABLE"}
	client := &fakeQueue{snapshots: mergeableSnapshots(states...)}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if client.enqueueCalls != 1 || client.loads != len(states) {
		t.Fatalf("loads = %d enqueued = %v output = %s", client.loads, client.enqueued, output)
	}
}

func TestQueueWhenGreenAbortsOnConflictWhileWaiting(t *testing.T) {
	withMergeableWait(t)
	withWhenGreenWait(t, time.Hour)
	snapshots := mergeableSnapshots("MERGEABLE", "CONFLICTING")
	snapshots[0].Checks = []github.QueueCheck{{Name: "ci", Green: false, Conclusion: ""}}
	client := &fakeQueue{snapshots: snapshots}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want abort")
		}
	})
	if !strings.Contains(output, "Not enqueueing acme/widgets#12: pull request has merge conflicts") || client.enqueueCalls != 0 {
		t.Fatalf("enqueued = %v output = %s", client.enqueued, output)
	}
}

func missingWidgets() github.QueuePullRequest {
	pr := readyPR()
	pr.Checks = nil
	return pr
}

func TestQueueWhenGreenWaitsForMissingCheck(t *testing.T) {
	slept := withWhenGreenWait(t, time.Hour)
	first := missingWidgets()
	second := pendingWidgets()
	third := readyPR()
	client := &fakeQueue{snapshots: []github.QueuePullRequest{first, second, third}}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), first.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if client.enqueueCalls != 1 || client.loads != 3 {
		t.Fatalf("loads = %d enqueued = %v", client.loads, client.enqueued)
	}
	if len(*slept) != 2 {
		t.Fatalf("sleeps = %v", *slept)
	}
	if !strings.Contains(output, "Enqueued acme/widgets#12.") {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueWhenGreenKeepsWaitingWhenCheckDisappears(t *testing.T) {
	withWhenGreenWait(t, time.Hour)
	first := pendingWidgets()
	client := &fakeQueue{snapshots: []github.QueuePullRequest{first, missingWidgets(), readyPR()}}
	if err := queuePullRequest(client, queueConfig(), first.Ref.Raw, false); err != nil {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if client.enqueueCalls != 1 || client.loads != 3 {
		t.Fatalf("loads = %d enqueued = %v", client.loads, client.enqueued)
	}
}

func TestQueueWhenGreenTimesOutOnMissingCheck(t *testing.T) {
	withWhenGreenWait(t, 20*time.Second)
	client := &fakeQueue{pr: missingWidgets()}
	var err error
	output := captureStdout(t, func() {
		err = queuePullRequest(client, queueConfig(), "acme/widgets#12", false)
	})
	want := `required check "ci" is missing`
	if err == nil || !strings.Contains(err.Error(), "timeout") || !strings.Contains(err.Error(), want) {
		t.Fatalf("queuePullRequest() error = %v", err)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueued = %v", client.enqueued)
	}
	if !strings.Contains(output, "Not enqueueing acme/widgets#12: timeout") || !strings.Contains(output, want) {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueWhenGreenJSONTimeoutOnMissingCheck(t *testing.T) {
	withWhenGreenWait(t, 20*time.Second)
	jsonOutput = true
	t.Cleanup(func() { jsonOutput = false })
	client := &fakeQueue{pr: missingWidgets()}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want timeout")
		}
	})
	var got queueJSONResult
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("json.Unmarshal() error = %v\n%s", err, output)
	}
	if got.Result != "timeout" || !slices.Contains(got.Readiness, `required check "ci" is missing`) {
		t.Fatalf("result = %+v", got)
	}
}

func TestQueueMissingCheckRefusesImmediatelyWithoutWhenGreen(t *testing.T) {
	if whenGreen {
		t.Fatal("whenGreen leaked from another test")
	}
	client := &fakeQueue{pr: missingWidgets()}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err == nil {
			t.Fatal("queuePullRequest() error = nil, want refusal")
		}
	})
	if client.loads != 1 || client.enqueueCalls != 0 {
		t.Fatalf("loads = %d enqueued = %v", client.loads, client.enqueued)
	}
	if !strings.Contains(output, `required check "ci" is missing`) {
		t.Fatalf("output = %s", output)
	}
}

func TestQueueWhenGreenDryRunWouldWaitForMissingCheck(t *testing.T) {
	slept := withWhenGreenWait(t, time.Hour)
	withMergeFlags(t, true, false, false)
	client := &fakeQueue{pr: missingWidgets()}
	output := captureStdout(t, func() {
		if err := queuePullRequest(client, queueConfig(), "acme/widgets#12", false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if client.loads != 1 || len(*slept) != 0 || client.enqueueCalls != 0 {
		t.Fatalf("loads = %d sleeps = %v enqueued = %v", client.loads, *slept, client.enqueued)
	}
	if !strings.Contains(output, "Would wait for acme/widgets#12.") || !strings.Contains(output, `required check "ci" is missing`) {
		t.Fatalf("output = %s", output)
	}
}

func TestPendingOnly(t *testing.T) {
	required := []string{"ci"}
	tests := []struct {
		name   string
		checks []github.QueueCheck
		extra  []string
		want   bool
	}{
		{name: "missing", want: true},
		{name: "pending", checks: []github.QueueCheck{{Name: "ci", Conclusion: "IN_PROGRESS"}}, want: true},
		{name: "failure", checks: []github.QueueCheck{{Name: "ci", Conclusion: "FAILURE"}}, want: false},
		{name: "success", checks: []github.QueueCheck{{Name: "ci", Green: true, Conclusion: "SUCCESS"}}, want: false},
		{name: "missing and not approved", extra: []string{"pull request is not approved"}, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			problems := append(requiredCheckProblems(tc.checks, required), tc.extra...)
			if got := pendingOnly(tc.checks, required, problems); got != tc.want {
				t.Fatalf("pendingOnly() = %v, want %v (problems %q)", got, tc.want, problems)
			}
		})
	}
}
