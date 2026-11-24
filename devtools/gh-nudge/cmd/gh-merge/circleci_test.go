package main

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

func TestMain(m *testing.M) {
	home, err := os.MkdirTemp("", "gh-merge-test-home-*")
	if err != nil {
		panic(err)
	}
	_ = os.Setenv("HOME", home)
	code := m.Run()
	_ = os.RemoveAll(home)
	os.Exit(code)
}

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

func writeFakeScript(t *testing.T, dir, name, body string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body), 0o700); err != nil { //nolint:gosec // G306: test helper executable script
		t.Fatal(err)
	}
	return path
}

func writeTokenFile(t *testing.T, path, content string, perm os.FileMode) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), perm); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, perm); err != nil {
		t.Fatal(err)
	}
}

func TestCircleTokenFile0600IsolatedToCircleChild(t *testing.T) {
	t.Setenv(circleTokenEnv, "")
	dir := t.TempDir()
	const fixture = "cc-test-file-value-0600"
	tokenPath := filepath.Join(dir, "circleci-token")
	writeTokenFile(t, tokenPath, fixture+"\n", 0o600)

	binDir := filepath.Join(dir, "bin")
	if err := os.MkdirAll(binDir, 0o700); err != nil {
		t.Fatal(err)
	}
	circleEnvOut := filepath.Join(dir, "circle-env.txt")
	ghEnvOut := filepath.Join(dir, "gh-env.txt")
	writeFakeScript(t, binDir, "circleci", `
printf '%s' "$CIRCLE_TOKEN" > `+circleEnvOut+`
case "$1 $2" in
  "workflow get")
    printf '{"jobs":[{"id":"`+infraJobID+`","name":"unit","outcome":"failed"}]}'
    ;;
  "job get")
    printf '{"id":"`+infraJobID+`","status":"failed"}'
    ;;
  "job output")
    printf '%s' '{"steps":[{"outcome":"failed","output":"ERROR: unit failed\n"}]}'
    ;;
  *)
    exit 1
    ;;
esac
`)
	writeFakeScript(t, binDir, "gh", `
printf '%s' "${CIRCLE_TOKEN-UNSET}" > `+ghEnvOut+`
printf 'acme/widgets\n'
`)
	t.Setenv("PATH", binDir+string(os.PathListSeparator)+os.Getenv("PATH"))

	cfg := queueConfig()
	cfg.Settings.CircleCILog = config.CircleCILogCLI
	cfg.Settings.CircleCITokenFile = tokenPath

	target := infraTargetURL + "/workflows/" + infraWorkflow
	var err error
	output := captureStdout(t, func() {
		_, err = printFailureEvidence(cfg, github.FailureEvidence{Name: "ci/circleci: unit", TargetURL: target})
	})
	if err != nil {
		t.Fatalf("printFailureEvidence() error = %v", err)
	}
	if !strings.Contains(output, "ERROR: unit failed") {
		t.Fatalf("output missing failure line:\n%s", output)
	}
	if strings.Contains(output, fixture) {
		t.Fatalf("output leaked token:\n%s", output)
	}

	gotCircleEnv, err := os.ReadFile(circleEnvOut)
	if err != nil {
		t.Fatalf("read circle env capture: %v", err)
	}
	if string(gotCircleEnv) != fixture {
		t.Fatalf("circleci child CIRCLE_TOKEN = %q, want %q", string(gotCircleEnv), fixture)
	}
	if got := os.Getenv(circleTokenEnv); got != "" {
		t.Fatalf("gh-merge own CIRCLE_TOKEN = %q, want empty", got)
	}

	if _, err := runCommand("gh", "repo", "view"); err != nil {
		t.Fatalf("runCommand(gh) error = %v", err)
	}
	gotGHEnv, err := os.ReadFile(ghEnvOut)
	if err != nil {
		t.Fatalf("read gh env capture: %v", err)
	}
	if string(gotGHEnv) != "" && string(gotGHEnv) != "UNSET" {
		t.Fatalf("gh child received CIRCLE_TOKEN = %q, want unset/empty", string(gotGHEnv))
	}
}

func TestCircleTokenFile0644FailsWithoutLeakingToken(t *testing.T) {
	t.Setenv(circleTokenEnv, "")
	dir := t.TempDir()
	const fixture = "cc-test-0644-never-print"
	tokenPath := filepath.Join(dir, "circleci-token")
	writeTokenFile(t, tokenPath, fixture+"\n", 0o644)

	cfg := queueConfig()
	cfg.Settings.CircleCITokenFile = tokenPath

	var err error
	output := captureStdout(t, func() {
		_, err = printFailureEvidence(cfg, circleEvidence())
	})
	if err == nil {
		t.Fatal("printFailureEvidence() error = nil, want mode error")
	}
	msg := err.Error()
	if !strings.Contains(msg, tokenPath) {
		t.Errorf("error missing path %q: %s", tokenPath, msg)
	}
	if !strings.Contains(msg, "0644") || !strings.Contains(msg, "0600") {
		t.Errorf("error missing mode details: %s", msg)
	}
	if strings.Contains(msg, fixture) || strings.Contains(output, fixture) {
		t.Fatalf("token leaked in output=%q err=%q", output, msg)
	}
}

func TestCircleTokenFileEmptyFails(t *testing.T) {
	for _, tc := range []struct {
		name    string
		content string
	}{
		{name: "empty", content: ""},
		{name: "whitespace only", content: "  \n\t\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv(circleTokenEnv, "")
			tokenPath := filepath.Join(t.TempDir(), "circleci-token")
			writeTokenFile(t, tokenPath, tc.content, 0o600)

			cfg := queueConfig()
			cfg.Settings.CircleCITokenFile = tokenPath
			_, err := resolveCircleCIToken(cfg)
			if err == nil {
				t.Fatal("resolveCircleCIToken() error = nil, want empty error")
			}
			if !strings.Contains(err.Error(), tokenPath) || !strings.Contains(err.Error(), "empty") {
				t.Fatalf("error = %q, want path and empty reason", err.Error())
			}
		})
	}
}

func TestCircleTokenFileSymlinkTo0600Works(t *testing.T) {
	t.Setenv(circleTokenEnv, "")
	dir := t.TempDir()
	const fixture = "cc-symlink-value"
	targetPath := filepath.Join(dir, "real-token")
	writeTokenFile(t, targetPath, "  "+fixture+" \n", 0o600)
	linkPath := filepath.Join(dir, "link-token")
	if err := os.Symlink(targetPath, linkPath); err != nil {
		t.Fatal(err)
	}

	cfg := queueConfig()
	cfg.Settings.CircleCITokenFile = linkPath
	got, err := resolveCircleCIToken(cfg)
	if err != nil {
		t.Fatalf("resolveCircleCIToken() error = %v", err)
	}
	if got != fixture {
		t.Fatalf("token = %q, want %q", got, fixture)
	}

	// Symlink to a 0644 target must fail.
	if err := os.Chmod(targetPath, 0o644); err != nil {
		t.Fatal(err)
	}
	if _, err := resolveCircleCIToken(cfg); err == nil || !strings.Contains(err.Error(), "0644") {
		t.Fatalf("resolveCircleCIToken(0644 target) error = %v, want mode error", err)
	}
}

func TestCircleTokenFileMissingDefaultFallsBack(t *testing.T) {
	t.Setenv(circleTokenEnv, "")
	t.Setenv("HOME", t.TempDir())

	// Missing default file is not an error and falls back to CLI behavior.
	withCircleInfraCLI(t, `{"steps":[{"outcome":"canceled","output":"noise line\n"}]}`)
	cfg := queueConfig()
	cfg.Settings.CircleCITokenFile = config.DefaultCircleCITokenFile
	target := infraTargetURL + "/workflows/" + infraWorkflow
	var err error
	output := captureStdout(t, func() {
		_, err = printFailureEvidence(cfg, github.FailureEvidence{Name: "ci/circleci: unit", TargetURL: target})
	})
	if err != nil {
		t.Fatalf("printFailureEvidence() error = %v, want fallback without error", err)
	}
	if !strings.Contains(output, "infrastructure failure: stopped responding") {
		t.Fatalf("output = %s", output)
	}

	// Missing explicitly configured non-default file fails loudly.
	missingPath := filepath.Join(t.TempDir(), "does-not-exist")
	cfg.Settings.CircleCITokenFile = missingPath
	if _, err := resolveCircleCIToken(cfg); err == nil || !strings.Contains(err.Error(), missingPath) {
		t.Fatalf("resolveCircleCIToken(missing configured) error = %v, want path error", err)
	}
}

func TestCircleTokenFileTildeDefaultAndEdgeCases(t *testing.T) {
	t.Setenv(circleTokenEnv, "")
	home := t.TempDir()
	t.Setenv("HOME", home)

	// Default path ~/.config/secrets/circleci-token with mode 0400 works.
	defaultPath := filepath.Join(home, ".config", "secrets", "circleci-token")
	writeTokenFile(t, defaultPath, "cc-default-token\n", 0o400)
	cfg := queueConfig()
	got, err := resolveCircleCIToken(cfg)
	if err != nil {
		t.Fatalf("resolveCircleCIToken(default) error = %v", err)
	}
	if got != "cc-default-token" {
		t.Fatalf("token = %q, want cc-default-token", got)
	}

	// Directory at configured path fails regular-file check.
	dirPath := filepath.Join(home, "not-a-file")
	if err := os.MkdirAll(dirPath, 0o600); err != nil {
		t.Fatal(err)
	}
	cfg.Settings.CircleCITokenFile = "~/not-a-file"
	if _, err := resolveCircleCIToken(cfg); err == nil || !strings.Contains(err.Error(), "not a regular file") {
		t.Fatalf("resolveCircleCIToken(dir) error = %v, want regular-file error", err)
	}
}

func TestCircleTokenEnvOverridesFile(t *testing.T) {
	dir := t.TempDir()
	tokenPath := filepath.Join(dir, "circleci-token")
	// Even a bad-mode file is ignored when CIRCLE_TOKEN is set in the environment.
	writeTokenFile(t, tokenPath, "from-file\n", 0o644)
	t.Setenv(circleTokenEnv, "from-env")

	cfg := queueConfig()
	cfg.Settings.CircleCITokenFile = tokenPath
	got, err := resolveCircleCIToken(cfg)
	if err != nil {
		t.Fatalf("resolveCircleCIToken() error = %v", err)
	}
	if got != "from-env" {
		t.Fatalf("token = %q, want from-env", got)
	}
}

func TestCircleChildFailureRedactsTokenInStderr(t *testing.T) {
	t.Setenv(circleTokenEnv, "")
	dir := t.TempDir()
	const fixture = "cc-test-stderr-redact-value"
	tokenPath := filepath.Join(dir, "circleci-token")
	writeTokenFile(t, tokenPath, fixture+"\n", 0o600)

	binDir := filepath.Join(dir, "bin")
	if err := os.MkdirAll(binDir, 0o700); err != nil {
		t.Fatal(err)
	}
	writeFakeScript(t, binDir, "circleci", `
printf 'Error: rejected token %s from config\n' "$CIRCLE_TOKEN" >&2
exit 1
`)
	t.Setenv("PATH", binDir+string(os.PathListSeparator)+os.Getenv("PATH"))

	cfg := queueConfig()
	cfg.Settings.CircleCILog = config.CircleCILogCLI
	cfg.Settings.CircleCITokenFile = tokenPath

	target := infraTargetURL + "/workflows/" + infraWorkflow
	var err error
	output := captureStdout(t, func() {
		_, err = printFailureEvidence(cfg, github.FailureEvidence{Name: "ci/circleci: unit", TargetURL: target})
	})
	if err == nil {
		t.Fatal("printFailureEvidence() error = nil, want child failure")
	}
	msg := err.Error()
	if strings.Contains(msg, fixture) || strings.Contains(output, fixture) {
		t.Fatalf("token leaked in err=%q output=%q", msg, output)
	}
	if !strings.Contains(msg, "[REDACTED]") {
		t.Fatalf("error missing [REDACTED]: %s", msg)
	}
	if !strings.Contains(msg, "circleci workflow get "+infraWorkflow+" --json") {
		t.Fatalf("error missing command and args: %s", msg)
	}
}
