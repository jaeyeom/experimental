package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

func (f *fakeQueue) EjectionEvidence(_, _, _ string, _ time.Time) (github.FailureEvidence, error) {
	return f.evidence, nil
}

// circleAPIFixture serves pipeline 42 with workflow wf-1 ("build-test").
// Job 8 is the failed job; jobs lists every job in the workflow.
type circleAPIFixture struct {
	jobs   string
	build  string
	output string
	tests  string
}

func withEvidenceCircleAPI(t *testing.T, fx circleAPIFixture) *[]string {
	t.Helper()
	t.Setenv(circleTokenEnv, "token")
	encoded, err := json.Marshal([]struct {
		Message string `json:"message"`
	}{{Message: fx.output}})
	if err != nil {
		t.Fatal(err)
	}
	var calls []string
	withCircleGet(t, func(_ context.Context, rawURL, _ string) ([]byte, error) {
		calls = append(calls, rawURL)
		switch rawURL {
		case "https://circleci.com/api/v2/project/gh/acme/widgets/pipeline/42":
			return []byte(`{"id":"pipe-1"}`), nil
		case "https://circleci.com/api/v2/pipeline/pipe-1/workflow":
			return []byte(`{"items":[{"id":"wf-1","name":"build-test","status":"failed"}]}`), nil
		case "https://circleci.com/api/v2/workflow/wf-1/job":
			return []byte(`{"items":` + fx.jobs + `}`), nil
		case "https://circleci.com/api/v1.1/project/gh/acme/widgets/8":
			return []byte(fx.build), nil
		case "https://circleci.com/output/8":
			return encoded, nil
		case "https://circleci.com/api/v2/project/gh/acme/widgets/8/tests":
			if fx.tests == "" {
				return nil, fmt.Errorf("circleci status 404")
			}
			return []byte(fx.tests), nil
		default:
			t.Fatalf("unexpected circleci url %s", rawURL)
			return nil, nil
		}
	})
	return &calls
}

const evidenceBuild8 = `"build_url":"https://circleci.com/gh/acme/widgets/8","workflows":{"workflow_id":"wf-1","workflow_name":"build-test","job_name":"test-amd64"}`

func evidenceTestFailure() circleAPIFixture {
	return circleAPIFixture{
		jobs:   `[{"job_number":8,"name":"test-amd64","status":"failed","stopped_at":"2026-01-01T23:58:00Z"}]`,
		build:  `{"status":"failed",` + evidenceBuild8 + `,"steps":[{"actions":[{"status":"failed","exit_code":1,"output_url":"https://circleci.com/output/8"}]}]}`,
		output: "compiling\n\x1b[31mERROR: TestParse failed\x1b[0m\nconnection reset by peer\nexit 1\n",
		tests:  `{"items":[{"classname":"pkg/parse","name":"TestParse","result":"failure"},{"classname":"pkg/parse","name":"TestOK","result":"success"}]}`,
	}
}

func evidenceInfraFailure() circleAPIFixture {
	return circleAPIFixture{
		jobs:  `[{"job_number":8,"name":"test-amd64","status":"infrastructure_fail","stopped_at":"2026-01-01T23:58:00Z"}]`,
		build: `{"status":"infrastructure_fail",` + evidenceBuild8 + `,"messages":[{"type":"error","message":"stopped responding"}],"steps":[{"actions":[{"status":"canceled","output_url":"https://circleci.com/output/8"}]}]}`,
	}
}

func evidenceSiblingFailure() circleAPIFixture {
	fx := evidenceTestFailure()
	fx.jobs = `[
		{"job_number":7,"name":"test-arm64","status":"success","stopped_at":"2026-01-01T23:57:00Z"},
		{"job_number":8,"name":"test-amd64","status":"failed","stopped_at":"2026-01-01T23:58:00Z"},
		{"job_number":9,"name":"deploy","status":"canceled","stopped_at":"2026-01-02T00:00:30Z"}
	]`
	return fx
}

func evidencePR() github.QueuePullRequest {
	pr := removedPR()
	pr.Events = append([]github.QueueEvent{{
		ID:        "OLD",
		Kind:      github.QueueEventRemoved,
		CreatedAt: time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC),
		Reason:    "merge_conflict",
		CommitSHA: "cafe",
	}}, pr.Events...)
	return pr
}

func runEvidence(t *testing.T, client *fakeQueue, args ...string) string {
	t.Helper()
	cfg := queueConfig()
	cfg.Settings.FlakeSignatures = []string{"connection reset"}
	return captureStdout(t, func() {
		if err := evidenceCommand(client, cfg, args); err != nil {
			t.Fatalf("evidenceCommand() error = %v", err)
		}
	})
}

func assertGolden(t *testing.T, name, got string) {
	t.Helper()
	assertGoldenFile(t, filepath.Join("testdata", "evidence", name), got)
}

func assertGoldenFile(t *testing.T, path, got string) {
	t.Helper()
	want, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got != string(want) {
		t.Fatalf("%s mismatch\ngot:\n%s\nwant:\n%s", path, got, want)
	}
}

func TestEvidenceJSONGolden(t *testing.T) {
	tests := []struct {
		name    string
		fixture circleAPIFixture
	}{
		{name: "test_failure.json", fixture: evidenceTestFailure()},
		{name: "infra_failure.json", fixture: evidenceInfraFailure()},
		{name: "passing_sibling.json", fixture: evidenceSiblingFailure()},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			withEvidenceCircleAPI(t, tc.fixture)
			pr := evidencePR()
			client := &fakeQueue{pr: pr, evidence: circleEvidence()}
			output := runEvidence(t, client, "acme/widgets#12", "--json")
			assertGolden(t, tc.name, output)
		})
	}
}

func TestEvidenceTextMatchesQueueAndStripsANSI(t *testing.T) {
	withEvidenceCircleAPI(t, evidenceTestFailure())
	pr := removedPR()
	client := &fakeQueue{pr: pr, evidence: circleEvidence()}
	output := runEvidence(t, client, "acme/widgets#12")
	want := "Removal reason: failed_checks\n" +
		"ci/circleci " + infraTargetURL + "\n" +
		"compiling\nERROR: TestParse failed\nconnection reset by peer\nexit 1\n" +
		"ERROR: TestParse failed\n" +
		"Flake hint: pattern \"connection reset\" matched \"connection reset\" (not a verdict)\n"
	if output != want {
		t.Fatalf("output:\n%q\nwant:\n%q", output, want)
	}
}

func TestEvidenceSelectsEventByID(t *testing.T) {
	pr := evidencePR()
	client := &fakeQueue{pr: pr}
	output := runEvidence(t, client, "acme/widgets#12", "--event", "OLD", "--json")
	var got evidenceJSON
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("decode: %v\n%s", err, output)
	}
	if got.EventID != "OLD" || got.Reason != "merge_conflict" || got.Commit != "cafe" {
		t.Fatalf("got = %+v", got)
	}
	err := evidenceCommand(client, queueConfig(), []string{"acme/widgets#12", "--event", "MISSING"})
	if err == nil || !strings.Contains(err.Error(), "MISSING") {
		t.Fatalf("error = %v, want missing event", err)
	}
}

func TestEvidenceUsage(t *testing.T) {
	for _, args := range [][]string{nil, {"--event"}, {"a#1", "b#2"}, {"a#1", "--bogus"}} {
		if err := evidenceCommand(&fakeQueue{}, queueConfig(), args); err != errEvidenceUsage {
			t.Errorf("args %v: error = %v, want usage", args, err)
		}
	}
}

func TestEvidenceExcerptIsBounded(t *testing.T) {
	var lines []string
	for i := 0; i < 2*maxExcerptLines; i++ {
		lines = append(lines, fmt.Sprintf("ERROR: case %d", i))
	}
	got := boundExcerpt(lines)
	if len(got.Lines) != maxExcerptLines || !got.Truncated {
		t.Fatalf("lines = %d truncated = %v", len(got.Lines), got.Truncated)
	}

	long := "ERROR: " + strings.Repeat("x", 2*maxExcerptBytes)
	got = boundExcerpt([]string{long, "ERROR: next"})
	size := 0
	for _, line := range got.Lines {
		size += len(line) + 1
	}
	if size > maxExcerptBytes || !got.Truncated || len(got.Lines) != 1 {
		t.Fatalf("size = %d lines = %d truncated = %v", size, len(got.Lines), got.Truncated)
	}

	got = boundExcerpt([]string{"ERROR: one"})
	if got.Truncated || len(got.Lines) != 1 {
		t.Fatalf("small excerpt = %+v", got)
	}
}

func TestStripANSI(t *testing.T) {
	in := "\x1b[1;31mERROR:\x1b[0m bad \x1b]8;;https://x\x07link\x1b]8;;\x07 \x1b(Bdone"
	if got := stripANSI(in); got != "ERROR: bad link done" {
		t.Fatalf("stripANSI() = %q", got)
	}
}

func TestEvidenceJSONFromActionsJob(t *testing.T) {
	pr := removedPR()
	client := &fakeQueue{pr: pr, evidence: github.FailureEvidence{
		Name:       "test (amd64)",
		TargetURL:  "https://github.com/acme/widgets/actions/runs/9/job/8",
		JobURL:     "https://github.com/acme/widgets/actions/runs/9/job/8",
		Log:        "Run tests\n\x1b[31mERROR: unit failed\x1b[0m\n",
		JobID:      "8",
		JobName:    "test (amd64)",
		Workflow:   "CI",
		WorkflowID: "9",
		Siblings: []github.JobOutcome{
			{ID: "7", Name: "test (arm64)", Outcome: "success", URL: "https://github.com/acme/widgets/actions/runs/9/job/7"},
			{ID: "6", Name: "lint", Outcome: "cancelled", CompletedAt: time.Date(2026, 1, 2, 0, 1, 0, 0, time.UTC)},
		},
	}}
	output := runEvidence(t, client, "acme/widgets#12", "--json")
	var got evidenceJSON
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("decode: %v\n%s", err, output)
	}
	if got.Job != (evidenceJob{ID: "8", Name: "test (amd64)", URL: "https://github.com/acme/widgets/actions/runs/9/job/8"}) {
		t.Errorf("job = %+v", got.Job)
	}
	if got.Workflow != (evidenceWorkflow{ID: "9", Name: "CI"}) {
		t.Errorf("workflow = %+v", got.Workflow)
	}
	if len(got.Siblings) != 1 || got.Siblings[0].Name != "test (arm64)" {
		t.Errorf("siblings = %+v", got.Siblings)
	}
	if strings.Join(got.Excerpt.Lines, "\n") != "ERROR: unit failed" {
		t.Errorf("excerpt = %+v", got.Excerpt)
	}

	text := runEvidence(t, client, "acme/widgets#12")
	if strings.Contains(text, "\x1b") || !strings.Contains(text, "ERROR: unit failed") {
		t.Errorf("text = %q", text)
	}
}
