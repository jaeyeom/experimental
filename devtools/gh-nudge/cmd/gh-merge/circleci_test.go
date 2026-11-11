package main

import (
	"context"
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

const (
	infraJobURL    = "https://circleci.com/gh/acme/widgets/8"
	infraTargetURL = "https://app.circleci.com/pipelines/github/acme/widgets/42"
	infraWorkflow  = "11111111-1111-1111-1111-111111111111"
	infraJobID     = "6bde3c42-8312-4297-be30-57e10dd84d18"
)

// withCircleJobAPI fakes the CircleCI API for pipeline 42 whose only failed
// job is number 8 with the given status. build is the v1.1 job body; step
// output is served at https://circleci.com/output/8.
func withCircleJobAPI(t *testing.T, status, build, stepOutput string) *[]string {
	t.Helper()
	t.Setenv(circleTokenEnv, "token")
	encoded, err := json.Marshal([]struct {
		Message string `json:"message"`
	}{{Message: stepOutput}})
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
			return []byte(`{"items":[{"id":"wf-1","status":"failed"}]}`), nil
		case "https://circleci.com/api/v2/workflow/wf-1/job":
			return []byte(`{"items":[{"job_number":7,"status":"success"},{"job_number":8,"status":"` + status + `"}]}`), nil
		case "https://circleci.com/api/v1.1/project/gh/acme/widgets/8":
			return []byte(build), nil
		case "https://circleci.com/output/8":
			return encoded, nil
		default:
			t.Fatalf("unexpected circleci url %s", rawURL)
			return nil, nil
		}
	})
	return &calls
}

func circleEvidence() github.FailureEvidence {
	return github.FailureEvidence{Name: "ci/circleci", TargetURL: infraTargetURL}
}

func runFailureEvidence(t *testing.T, evidence github.FailureEvidence) (string, failureReport) {
	t.Helper()
	var report failureReport
	output := captureStdout(t, func() {
		var err error
		report, err = printFailureEvidence(queueConfig(), evidence)
		if err != nil {
			t.Fatalf("printFailureEvidence() error = %v", err)
		}
	})
	return output, report
}

func TestCircleFailedJobGolden(t *testing.T) {
	withCircleJobAPI(t, "failed",
		`{"status":"failed","build_url":"`+infraJobURL+`","steps":[{"actions":[{"status":"success","output_url":"https://circleci.com/output/ok"},{"status":"failed","exit_code":1,"output_url":"https://circleci.com/output/8"}]}]}`,
		"compiling\nERROR: unit failed\nexit 1\n")
	output, report := runFailureEvidence(t, circleEvidence())
	const golden = "ci/circleci " + infraTargetURL + "\n" +
		"compiling\nERROR: unit failed\nexit 1\n" +
		"ERROR: unit failed\n"
	if output != golden {
		t.Fatalf("output:\n%s\nwant:\n%s", output, golden)
	}
	if report.Suggested != "" || report.Text != "compiling\nERROR: unit failed\nexit 1" {
		t.Fatalf("report = %+v", report)
	}
}

func TestCircleInfraFailureWithoutFailedStep(t *testing.T) {
	calls := withCircleJobAPI(t, "infrastructure_fail",
		`{"status":"infrastructure_fail","build_url":"`+infraJobURL+`","messages":[{"type":"error","message":"stopped responding"}],"steps":[{"actions":[{"status":"success","output_url":"https://circleci.com/output/ok"},{"status":"canceled","output_url":"https://circleci.com/output/8"}]}]}`,
		"noise line\n")
	output, report := runFailureEvidence(t, circleEvidence())
	want := "ci/circleci " + infraTargetURL + "\n" +
		"infrastructure failure: stopped responding\n" +
		"Job: " + infraJobURL + "\n" +
		"Suggested RCA class: infra\n"
	if output != want {
		t.Fatalf("output:\n%s\nwant:\n%s", output, want)
	}
	for _, call := range *calls {
		if strings.Contains(call, "/output/") {
			t.Fatalf("fetched a step log without a failed step: %v", *calls)
		}
	}
	if report.Suggested != "infra" || !strings.Contains(report.Text, "infrastructure failure: stopped responding") {
		t.Fatalf("report = %+v", report)
	}
}

func TestCircleInfraFailureWithFailedStepPrintsTail(t *testing.T) {
	withCircleJobAPI(t, "infrastructure_fail",
		`{"status":"infrastructure_fail","build_url":"`+infraJobURL+`","messages":[{"type":"error","message":"stopped responding"}],"steps":[{"actions":[{"status":"success","exit_code":0,"output_url":"https://circleci.com/output/ok"},{"status":"running","exit_code":137,"output_url":"https://circleci.com/output/8"}]}]}`,
		"step output\nKilled\n")
	output, report := runFailureEvidence(t, circleEvidence())
	want := "ci/circleci " + infraTargetURL + "\n" +
		"infrastructure failure: stopped responding\n" +
		"Job: " + infraJobURL + "\n" +
		"Suggested RCA class: infra\n" +
		"step output\nKilled\n"
	if output != want {
		t.Fatalf("output:\n%s\nwant:\n%s", output, want)
	}
	if report.Suggested != "infra" || !strings.Contains(report.Text, "Killed") {
		t.Fatalf("report = %+v", report)
	}
}

func TestCircleInfraFailureNoReason(t *testing.T) {
	for _, status := range []string{"infrastructure_fail", "Infrastructure-Fail", "INFRASTRUCTURE_FAIL"} {
		t.Run(status, func(t *testing.T) {
			withCircleJobAPI(t, status,
				`{"status":"`+status+`","steps":[]}`, "")
			output, report := runFailureEvidence(t, circleEvidence())
			for _, want := range []string{
				"infrastructure failure: no reason given\n",
				"Job: " + infraJobURL + "\n",
				"Suggested RCA class: infra\n",
			} {
				if !strings.Contains(output, want) {
					t.Errorf("output missing %q:\n%s", want, output)
				}
			}
			if report.Suggested != "infra" {
				t.Fatalf("report = %+v", report)
			}
		})
	}
}

// withCircleInfraCLI fakes the circleci CLI for a workflow whose job reports
// infrastructure_fail with a "stopped responding" warning.
func withCircleInfraCLI(t *testing.T, steps string) {
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
			return `{"jobs":[{"id":"` + infraJobID + `","name":"unit","outcome":"infrastructure_fail"}]}`, nil
		case len(args) >= 2 && args[0] == "job" && args[1] == "get":
			if !circleArgIs(args, infraJobID) {
				t.Fatalf("args = %v", args)
			}
			return `{"id":"` + infraJobID + `","status":"infrastructure_fail","warnings":[{"type":"infra","message":"stopped responding"}]}`, nil
		case len(args) >= 3 && args[0] == "job" && args[1] == "output" && args[2] == "list":
			return steps, nil
		default:
			t.Fatalf("args = %v", args)
			return "", nil
		}
	})
}

func TestCircleCLIInfraFailure(t *testing.T) {
	withCircleInfraCLI(t, `{"steps":[{"outcome":"succeeded","output":"booted\n"},{"outcome":"canceled","output":"noise line\n"}]}`)
	target := infraTargetURL + "/workflows/" + infraWorkflow
	output, report := runFailureEvidence(t, github.FailureEvidence{Name: "ci/circleci: unit", TargetURL: target})
	want := "ci/circleci: unit " + target + "\n" +
		"infrastructure failure: stopped responding\n" +
		"Job: " + target + "\n" +
		"Suggested RCA class: infra\n"
	if output != want {
		t.Fatalf("output:\n%s\nwant:\n%s", output, want)
	}
	if report.Suggested != "infra" {
		t.Fatalf("report = %+v", report)
	}
}

func TestWatchAndRCAPrintInfraSuggestion(t *testing.T) {
	evidence := func(t *testing.T) github.FailureEvidence {
		t.Helper()
		withCircleInfraCLI(t, `{"steps":[{"outcome":"canceled","output":"noise line\n"}]}`)
		return github.FailureEvidence{Name: "ci/circleci: unit", TargetURL: infraTargetURL + "/workflows/" + infraWorkflow}
	}
	wants := []string{"infrastructure failure: stopped responding", "Suggested RCA class: infra"}
	t.Run("watch", func(t *testing.T) {
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: evidence(t)}
		var err error
		output := captureStdout(t, func() {
			err = watchPullRequests(client, []string{pr.Ref.Raw}, watchOptions{timeout: time.Minute, cfg: queueConfig()})
		})
		assertWatchExit(t, err, 2)
		for _, want := range wants {
			if !strings.Contains(output, want) {
				t.Errorf("output missing %q:\n%s", want, output)
			}
		}
		if strings.Contains(output, "noise line") {
			t.Fatalf("printed log noise:\n%s", output)
		}
	})
	t.Run("rca", func(t *testing.T) {
		pr := removedPR()
		client := &fakeQueue{pr: pr, evidence: evidence(t)}
		path := writeRCAFile(t, "runner died\n")
		output := captureStdout(t, func() {
			if err := postRCA(client, rcaConfig(), []string{"acme/widgets#12", "--class", "infra", "--file", path}); err != nil {
				t.Fatalf("postRCA() error = %v", err)
			}
		})
		for _, want := range wants {
			if !strings.Contains(output, want) {
				t.Errorf("output missing %q:\n%s", want, output)
			}
		}
	})
}
