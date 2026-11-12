package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"os"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

type fakeMergeClient struct {
	prs    []models.PullRequest
	merged []string
}

func (f *fakeMergeClient) GetMergeablePullRequests() ([]models.PullRequest, error) {
	return f.prs, nil
}

func (f *fakeMergeClient) MergePullRequest(prURL string, _ bool) error {
	f.merged = append(f.merged, prURL)
	return nil
}

func mergeFixture(title, url string, labels ...string) models.PullRequest {
	pr := models.PullRequest{
		Title:       title,
		URL:         url,
		HeadRefName: "feature",
		Mergeable:   "MERGEABLE",
	}
	for _, name := range labels {
		pr.Labels = append(pr.Labels, models.Label{Name: name})
	}
	return pr
}

func withAllowNoRequiredChecks(t *testing.T, allow bool) {
	t.Helper()
	prev := allowNoRequiredChecks
	allowNoRequiredChecks = allow
	t.Cleanup(func() { allowNoRequiredChecks = prev })
}

func withMergeFlags(t *testing.T, dry, del, frc bool) {
	t.Helper()
	prevDry, prevDel, prevForce := dryRun, deleteBranch, force
	dryRun, deleteBranch, force = dry, del, frc
	t.Cleanup(func() {
		dryRun, deleteBranch, force = prevDry, prevDel, prevForce
	})
}

func captureStdout(t *testing.T, fn func()) string {
	t.Helper()
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatalf("pipe: %v", err)
	}
	orig := os.Stdout
	os.Stdout = w
	t.Cleanup(func() { os.Stdout = orig })

	fn()
	if err := w.Close(); err != nil {
		t.Fatalf("close pipe: %v", err)
	}
	var buf bytes.Buffer
	if _, err := io.Copy(&buf, r); err != nil {
		t.Fatalf("read stdout: %v", err)
	}
	return buf.String()
}

func TestMergePRsSkipsBlockLabel(t *testing.T) {
	withMergeFlags(t, false, false, false)
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Hold this", "https://github.com/example/repo/pull/1", "do-not-merge"),
			mergeFixture("Ship this", "https://github.com/example/repo/pull/2", "ready"),
		},
	}
	cfg := &config.Config{Settings: config.SettingsConfig{BlockMergeLabels: []string{"do-not-merge"}}}

	output := captureStdout(t, func() {
		if err := mergePRs(client, cfg); err != nil {
			t.Fatalf("mergePRs() error = %v", err)
		}
	})

	if len(client.merged) != 1 || client.merged[0] != "https://github.com/example/repo/pull/2" {
		t.Errorf("merged = %v, want only pull/2", client.merged)
	}
	if !strings.Contains(output, `Skipping: label "do-not-merge" blocks merge`) {
		t.Errorf("output missing block skip line:\n%s", output)
	}
	if !strings.Contains(output, "Skipped: 1 due to a block label.") {
		t.Errorf("output missing block skip summary:\n%s", output)
	}
}

func TestMergePRsBlockLabelIgnoresForce(t *testing.T) {
	withMergeFlags(t, false, false, true)
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Hold this", "https://github.com/example/repo/pull/1", "do-not-merge"),
		},
	}
	client.prs[0].StatusCheckRollup = []models.StatusCheck{{
		Name:       "ci",
		Status:     "COMPLETED",
		Conclusion: "FAILURE",
	}}
	cfg := &config.Config{Settings: config.SettingsConfig{BlockMergeLabels: []string{"do-not-merge"}}}

	output := captureStdout(t, func() {
		if err := mergePRs(client, cfg); err != nil {
			t.Fatalf("mergePRs() error = %v", err)
		}
	})

	if len(client.merged) != 0 {
		t.Errorf("merged = %v, want none", client.merged)
	}
	if !strings.Contains(output, `Skipping: label "do-not-merge" blocks merge`) {
		t.Errorf("output missing block skip line:\n%s", output)
	}
	if strings.Contains(output, "CI checks failed") {
		t.Errorf("block label should be reported before the CI failure:\n%s", output)
	}
}

func TestMergePRsEmptyBlockListMergesLabeledPR(t *testing.T) {
	withMergeFlags(t, false, false, false)
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Labeled", "https://github.com/example/repo/pull/1", "do-not-merge"),
		},
	}
	cfg := &config.Config{}

	if err := mergePRs(client, cfg); err != nil {
		t.Fatalf("mergePRs() error = %v", err)
	}
	if len(client.merged) != 1 || client.merged[0] != "https://github.com/example/repo/pull/1" {
		t.Errorf("merged = %v, want pull/1", client.merged)
	}
}

func TestMergePRsBlockLabelIsCaseSensitive(t *testing.T) {
	withMergeFlags(t, false, false, false)
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Different case", "https://github.com/example/repo/pull/1", "Do-Not-Merge"),
		},
	}
	cfg := &config.Config{Settings: config.SettingsConfig{BlockMergeLabels: []string{"do-not-merge"}}}

	if err := mergePRs(client, cfg); err != nil {
		t.Fatalf("mergePRs() error = %v", err)
	}
	if len(client.merged) != 1 {
		t.Errorf("merged = %v, want the pull request merged", client.merged)
	}
}

func TestMergePRsDryRunDoesNotMergeBlockedPR(t *testing.T) {
	withMergeFlags(t, true, false, false)
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Hold this", "https://github.com/example/repo/pull/1", "do-not-merge"),
			mergeFixture("Ship this", "https://github.com/example/repo/pull/2"),
		},
	}
	cfg := &config.Config{Settings: config.SettingsConfig{BlockMergeLabels: []string{"do-not-merge"}}}

	output := captureStdout(t, func() {
		if err := mergePRs(client, cfg); err != nil {
			t.Fatalf("mergePRs() error = %v", err)
		}
	})

	if len(client.merged) != 0 {
		t.Errorf("merged = %v, want none in dry run", client.merged)
	}
	holdAt := strings.Index(output, "Hold this")
	shipAt := strings.Index(output, "Ship this")
	if holdAt < 0 || shipAt < 0 || shipAt < holdAt {
		t.Fatalf("output missing both pull requests:\n%s", output)
	}
	if strings.Contains(output[holdAt:shipAt], "Would merge") {
		t.Errorf("dry run should not offer to merge the blocked pull request:\n%s", output[holdAt:shipAt])
	}
	if !strings.Contains(output[shipAt:], "Would merge PR and keep branch") {
		t.Errorf("dry run should still preview the unblocked pull request:\n%s", output)
	}
}

func TestListMergeablePRsShowsBlockLabel(t *testing.T) {
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Hold this", "https://github.com/example/repo/pull/1", "do-not-merge"),
		},
	}
	cfg := &config.Config{Settings: config.SettingsConfig{BlockMergeLabels: []string{"do-not-merge", "hold"}}}

	output := captureStdout(t, func() {
		if err := listMergeablePRs(client, cfg); err != nil {
			t.Fatalf("listMergeablePRs() error = %v", err)
		}
	})

	if !strings.Contains(output, "Hold this") {
		t.Errorf("list should still show the blocked pull request:\n%s", output)
	}
	if !strings.Contains(output, `Blocked: label "do-not-merge" prevents merge`) {
		t.Errorf("output missing blocked line:\n%s", output)
	}
}

func TestMergeEmptyRepoBlockLabelsOverrideGlobal(t *testing.T) {
	withMergeFlags(t, false, false, false)
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Hold this", "https://github.com/acme/widgets/pull/1", "do-not-merge"),
		},
	}
	empty := []string{}
	cfg := &config.Config{
		Settings: config.SettingsConfig{BlockMergeLabels: []string{"do-not-merge"}},
		RepoSettings: map[string]config.RepoSettings{
			"acme/widgets": {BlockMergeLabels: &empty},
		},
	}
	if err := mergePRs(client, cfg); err != nil {
		t.Fatalf("mergePRs() error = %v", err)
	}
	if len(client.merged) != 1 {
		t.Fatalf("merged = %v, want the pull request", client.merged)
	}
}

func TestListSuccessStatusContextIsNotPending(t *testing.T) {
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Ship this", "https://github.com/acme/widgets/pull/1"),
		},
	}
	client.prs[0].StatusCheckRollup = []models.StatusCheck{
		{Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS"},
		{Context: "ci/circleci", State: "SUCCESS"},
	}
	cfg := &config.Config{Settings: config.SettingsConfig{RequiredChecks: []string{"ci", "ci/circleci"}}}
	output := captureStdout(t, func() {
		if err := listMergeablePRs(client, cfg); err != nil {
			t.Fatalf("listMergeablePRs() error = %v", err)
		}
	})
	if strings.Contains(output, "checks still running") {
		t.Fatalf("list treated the successful status context as pending:\n%s", output)
	}
	if !strings.Contains(output, "CI: PASS (2/2 checks passed)") {
		t.Fatalf("output missing pass line:\n%s", output)
	}
}

func TestListUsesRepoBlockLabels(t *testing.T) {
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Hold this", "https://github.com/acme/widgets/pull/1", "do-not-merge"),
		},
	}
	labels := []string{"do-not-merge"}
	cfg := &config.Config{
		RepoSettings: map[string]config.RepoSettings{
			"acme/widgets": {BlockMergeLabels: &labels},
		},
	}
	output := captureStdout(t, func() {
		if err := listMergeablePRs(client, cfg); err != nil {
			t.Fatalf("listMergeablePRs() error = %v", err)
		}
	})
	if !strings.Contains(output, `Blocked: label "do-not-merge" prevents merge`) {
		t.Fatalf("list ignored the repository entry:\n%s", output)
	}
}

func captureLog(t *testing.T) *bytes.Buffer {
	t.Helper()
	var buf bytes.Buffer
	prev := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(&buf, nil)))
	t.Cleanup(func() { slog.SetDefault(prev) })
	return &buf
}

func TestListLogsEffectiveRepoBlockLabels(t *testing.T) {
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Hold this", "https://github.com/acme/widgets/pull/1", "do-not-merge"),
		},
	}
	labels := []string{"do-not-merge"}
	checks := []string{"lint", "test"}
	cfg := &config.Config{
		RepoSettings: map[string]config.RepoSettings{
			"acme/widgets": {BlockMergeLabels: &labels, RequiredChecks: &checks},
		},
	}
	logs := captureLog(t)
	output := captureStdout(t, func() {
		if err := listMergeablePRs(client, cfg); err != nil {
			t.Fatalf("listMergeablePRs() error = %v", err)
		}
	})
	logText := logs.String()
	if strings.Contains(logText, "block_merge_labels=[]") {
		t.Fatalf("startup logged the empty global list as the gate:\n%s", logText)
	}
	want := "acme/widgets: block_merge_labels=[do-not-merge] required_checks=2"
	if !strings.Contains(logText, want) {
		t.Fatalf("log = %q, want substring %q", logText, want)
	}
	if !strings.Contains(output, `Blocked: label "do-not-merge" prevents merge`) {
		t.Fatalf("output missing blocked line:\n%s", output)
	}
}

func TestListKeepsGlobalStartupLogWhenNoRepoSettings(t *testing.T) {
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Ship this", "https://github.com/acme/widgets/pull/1"),
		},
	}
	cfg := &config.Config{Settings: config.SettingsConfig{
		BlockMergeLabels: []string{"do-not-merge", "hold"},
	}}
	logs := captureLog(t)
	if err := listMergeablePRs(client, cfg); err != nil {
		t.Fatalf("listMergeablePRs() error = %v", err)
	}
	logText := logs.String()
	if !strings.Contains(logText, "block_merge_labels") ||
		!strings.Contains(logText, "do-not-merge") ||
		!strings.Contains(logText, "hold") {
		t.Fatalf("startup log missing the global list:\n%s", logText)
	}
	if strings.Contains(logText, "required_checks=") {
		t.Fatalf("empty repo_settings should keep the global line:\n%s", logText)
	}
}

func TestListLogsGlobalValuesForRepoWithoutEntry(t *testing.T) {
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Hold this", "https://github.com/acme/widgets/pull/1", "do-not-merge"),
			mergeFixture("Other", "https://github.com/acme/other/pull/2"),
			mergeFixture("Again", "https://github.com/acme/widgets/pull/3", "do-not-merge"),
		},
	}
	labels := []string{"do-not-merge"}
	cfg := &config.Config{
		Settings: config.SettingsConfig{
			BlockMergeLabels: []string{"hold"},
			RequiredChecks:   []string{"ci", "lint"},
		},
		RepoSettings: map[string]config.RepoSettings{
			"acme/widgets": {BlockMergeLabels: &labels},
		},
	}
	logs := captureLog(t)
	if err := listMergeablePRs(client, cfg); err != nil {
		t.Fatalf("listMergeablePRs() error = %v", err)
	}
	logText := logs.String()
	wantWidgets := "acme/widgets: block_merge_labels=[do-not-merge] required_checks=2"
	wantOther := "acme/other: block_merge_labels=[hold] required_checks=2"
	if !strings.Contains(logText, wantWidgets) || !strings.Contains(logText, wantOther) {
		t.Fatalf("log = %q, want %q and %q", logText, wantWidgets, wantOther)
	}
	if strings.Count(logText, "acme/widgets: block_merge_labels=") != 1 {
		t.Fatalf("widgets logged more than once:\n%s", logText)
	}
}

func TestMergeLogsEffectiveRepoBlockLabels(t *testing.T) {
	withMergeFlags(t, false, false, false)
	client := &fakeMergeClient{
		prs: []models.PullRequest{
			mergeFixture("Hold this", "https://github.com/acme/widgets/pull/1", "do-not-merge"),
		},
	}
	labels := []string{"do-not-merge"}
	checks := []string{"lint", "test"}
	cfg := &config.Config{
		RepoSettings: map[string]config.RepoSettings{
			"acme/widgets": {BlockMergeLabels: &labels, RequiredChecks: &checks},
		},
	}
	logs := captureLog(t)
	output := captureStdout(t, func() {
		if err := mergePRs(client, cfg); err != nil {
			t.Fatalf("mergePRs() error = %v", err)
		}
	})
	logText := logs.String()
	if strings.Contains(logText, "block_merge_labels=[]") {
		t.Fatalf("startup logged the empty global list as the gate:\n%s", logText)
	}
	want := "acme/widgets: block_merge_labels=[do-not-merge] required_checks=2"
	if !strings.Contains(logText, want) {
		t.Fatalf("log = %q, want substring %q", logText, want)
	}
	if !strings.Contains(output, `Skipping: label "do-not-merge" blocks merge`) {
		t.Fatalf("merge ignored the repository entry:\n%s", output)
	}
	if len(client.merged) != 0 {
		t.Fatalf("merged = %v, want none", client.merged)
	}
}

func TestListGHFailurePrintsStderr(t *testing.T) {
	exec := &ghAttempts{calls: []ghAttempt{
		{stdout: "[]", stderr: "gh: api error", err: errors.New("exit status 1")},
		{stdout: "[]", stderr: "gh: api error", err: errors.New("exit status 1")},
	}}
	client := github.NewClientWithExecutor(exec)
	var cmdErr error
	stdout, stderr := captureOutputs(t, func() {
		cmdErr = listMergeablePRs(client, &config.Config{})
	})
	if cmdErr == nil {
		t.Fatal("expected non-zero list failure")
	}
	if strings.Contains(stdout, "No PRs with no review requests found.") {
		t.Fatalf("printed the empty-result line:\n%s", stdout)
	}
	if !strings.Contains(stderr+cmdErr.Error(), "gh: api error") {
		t.Fatalf("output missing gh stderr\nstdout: %s\nstderr: %s\nerr: %v", stdout, stderr, cmdErr)
	}
	if exec.n != 2 {
		t.Fatalf("gh calls = %d, want 2", exec.n)
	}
}

func TestListGHTruncatedJSON(t *testing.T) {
	exec := &ghAttempts{calls: []ghAttempt{{stdout: `[{"url":`}}}
	client := github.NewClientWithExecutor(exec)
	var cmdErr error
	stdout, _ := captureOutputs(t, func() {
		cmdErr = listMergeablePRs(client, &config.Config{})
	})
	if cmdErr == nil {
		t.Fatal("expected non-zero list failure")
	}
	if strings.Contains(stdout, "No PRs with no review requests found.") {
		t.Fatalf("printed the empty-result line:\n%s", stdout)
	}
	if !strings.Contains(cmdErr.Error(), "unexpected end of JSON input") {
		t.Fatalf("error = %v, want unexpected end of JSON input", cmdErr)
	}
	if exec.n != 1 {
		t.Fatalf("gh calls = %d, want 1 (truncated JSON is not retried)", exec.n)
	}
}

func TestListGHEmptyArray(t *testing.T) {
	exec := &ghAttempts{calls: []ghAttempt{{stdout: "[]", stderr: "warning: not a pull request list"}}}
	client := github.NewClientWithExecutor(exec)
	var cmdErr error
	stdout, _ := captureOutputs(t, func() {
		cmdErr = listMergeablePRs(client, &config.Config{})
	})
	if cmdErr != nil {
		t.Fatalf("listMergeablePRs() error = %v", cmdErr)
	}
	if !strings.Contains(stdout, "No PRs with no review requests found.") {
		t.Fatalf("output missing empty-result line:\n%s", stdout)
	}
	if exec.n != 1 {
		t.Fatalf("gh calls = %d, want 1", exec.n)
	}
}

func captureOutputs(t *testing.T, fn func()) (string, string) {
	t.Helper()
	outR, outW, err := os.Pipe()
	if err != nil {
		t.Fatalf("stdout pipe: %v", err)
	}
	errR, errW, err := os.Pipe()
	if err != nil {
		t.Fatalf("stderr pipe: %v", err)
	}
	origOut, origErr := os.Stdout, os.Stderr
	os.Stdout, os.Stderr = outW, errW
	t.Cleanup(func() {
		os.Stdout, os.Stderr = origOut, origErr
	})
	fn()
	if err := outW.Close(); err != nil {
		t.Fatalf("close stdout: %v", err)
	}
	if err := errW.Close(); err != nil {
		t.Fatalf("close stderr: %v", err)
	}
	var stdout, stderr bytes.Buffer
	if _, err := io.Copy(&stdout, outR); err != nil {
		t.Fatalf("read stdout: %v", err)
	}
	if _, err := io.Copy(&stderr, errR); err != nil {
		t.Fatalf("read stderr: %v", err)
	}
	return stdout.String(), stderr.String()
}

type ghAttempt struct {
	stdout string
	stderr string
	err    error
}

type ghAttempts struct {
	calls []ghAttempt
	n     int
}

func (g *ghAttempts) Execute(cmd string, args ...string) (string, error) {
	stdout, stderr, err := g.ExecuteSeparated(cmd, args...)
	combined := stdout
	if stderr != "" {
		if combined != "" {
			combined += "\n"
		}
		combined += stderr
	}
	return combined, err
}

func (g *ghAttempts) ExecuteWithStdin(_ string, cmd string, args ...string) (string, error) {
	return g.Execute(cmd, args...)
}

func TestListCIIgnoresOptionalFailure(t *testing.T) {
	output := listCIOutput(t, []string{"ci"}, nil, models.StatusCheck{
		Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS",
	}, models.StatusCheck{Context: "optional", State: "FAILURE"})
	if !strings.Contains(output, "CI: PASS") || strings.Contains(output, "CI: FAIL") {
		t.Fatalf("list verdict:\n%s", output)
	}
	if !strings.Contains(output, "other: optional") {
		t.Fatalf("output missing other line:\n%s", output)
	}
}

func TestListCIRequiredFailureStillListsOptional(t *testing.T) {
	output := listCIOutput(t, []string{"ci"}, nil, models.StatusCheck{
		Name: "ci", Status: "COMPLETED", Conclusion: "FAILURE",
	}, models.StatusCheck{Context: "optional", State: "FAILURE"})
	if !strings.Contains(output, "CI: FAIL") {
		t.Fatalf("list verdict:\n%s", output)
	}
	if !strings.Contains(output, "other: optional") {
		t.Fatalf("output missing other line:\n%s", output)
	}
}

func TestListCIRequiredPendingIgnoresOptionalFailure(t *testing.T) {
	output := listCIOutput(t, []string{"ci"}, nil, models.StatusCheck{
		Name: "ci", Status: "IN_PROGRESS",
	}, models.StatusCheck{Context: "optional", State: "FAILURE"})
	if !strings.Contains(output, "CI: PENDING") || strings.Contains(output, "CI: FAIL") {
		t.Fatalf("list verdict:\n%s", output)
	}
	if !strings.Contains(output, "other: optional") {
		t.Fatalf("output missing other line:\n%s", output)
	}
}

func TestListCIEmptyRequiredIsNone(t *testing.T) {
	output := listCIOutput(t, nil, nil, models.StatusCheck{Context: "optional", State: "FAILURE"})
	if !strings.Contains(output, "CI: NONE") || strings.Contains(output, "CI: FAIL") {
		t.Fatalf("list verdict:\n%s", output)
	}
	if !strings.Contains(output, "other: optional") {
		t.Fatalf("output missing other line:\n%s", output)
	}
}

func TestListCIUsesRepoRequiredChecks(t *testing.T) {
	lint := []string{"lint"}
	output := listCIOutput(t, []string{"ci"}, map[string]config.RepoSettings{
		"acme/widgets": {RequiredChecks: &lint},
	}, models.StatusCheck{
		Name: "ci", Status: "COMPLETED", Conclusion: "FAILURE",
	}, models.StatusCheck{
		Name: "lint", Status: "COMPLETED", Conclusion: "SUCCESS",
	}, models.StatusCheck{Context: "optional", State: "FAILURE"})
	if !strings.Contains(output, "CI: PASS") || strings.Contains(output, "CI: FAIL") {
		t.Fatalf("list used the global required list:\n%s", output)
	}
	if !strings.Contains(output, "other: ci, optional") {
		t.Fatalf("output = %q", output)
	}
}

func TestListCIOptionalPendingDoesNotChangeVerdict(t *testing.T) {
	output := listCIOutput(t, []string{"ci"}, nil, models.StatusCheck{
		Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS",
	}, models.StatusCheck{Context: "optional", State: "PENDING"})
	if !strings.Contains(output, "CI: PASS") || strings.Contains(output, "CI: PENDING") {
		t.Fatalf("list verdict:\n%s", output)
	}
	if strings.Contains(output, "other:") {
		t.Fatalf("pending optional was listed as a failure:\n%s", output)
	}
}

func TestListCIMissingRequiredIsFail(t *testing.T) {
	output := listCIOutput(t, []string{"ci", "lint"}, nil, models.StatusCheck{
		Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS",
	})
	if !strings.Contains(output, "CI: FAIL") {
		t.Fatalf("missing required check was not a failure:\n%s", output)
	}
	if strings.Contains(output, "other:") {
		t.Fatalf("missing required check was printed as other:\n%s", output)
	}
}

func TestMergeStillSkipsOptionalFailure(t *testing.T) {
	withMergeFlags(t, false, false, false)
	pr := mergeFixture("Ship this", "https://github.com/acme/widgets/pull/1")
	pr.StatusCheckRollup = []models.StatusCheck{
		{Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS"},
		{Context: "optional", State: "FAILURE"},
	}
	client := &fakeMergeClient{prs: []models.PullRequest{pr}}
	cfg := &config.Config{Settings: config.SettingsConfig{RequiredChecks: []string{"ci"}}}
	output := captureStdout(t, func() {
		if err := mergePRs(client, cfg); err != nil {
			t.Fatalf("mergePRs() error = %v", err)
		}
	})
	if len(client.merged) != 0 {
		t.Fatalf("merged = %v, want none", client.merged)
	}
	if !strings.Contains(output, "Skipping: CI checks failed") {
		t.Fatalf("merge ignored ChecksStatus:\n%s", output)
	}
}

func listCIOutput(t *testing.T, required []string, repos map[string]config.RepoSettings, checks ...models.StatusCheck) string {
	t.Helper()
	pr := mergeFixture("Ship this", "https://github.com/acme/widgets/pull/1")
	pr.StatusCheckRollup = checks
	client := &fakeMergeClient{prs: []models.PullRequest{pr}}
	cfg := &config.Config{
		Settings:     config.SettingsConfig{RequiredChecks: required},
		RepoSettings: repos,
	}
	return captureStdout(t, func() {
		if err := listMergeablePRs(client, cfg); err != nil {
			t.Fatalf("listMergeablePRs() error = %v", err)
		}
	})
}

func TestListVerboseSkipReasons(t *testing.T) {
	withVerbose(t, true)
	draft := mergeFixture("Draft this", "https://github.com/acme/widgets/pull/1")
	draft.IsDraft = true
	review := mergeFixture("Review this", "https://github.com/acme/widgets/pull/2")
	review.ReviewRequests = []models.ReviewRequest{{Login: "sam"}}
	conflict := mergeFixture("Conflict this", "https://github.com/acme/widgets/pull/3")
	conflict.Mergeable = "CONFLICTING"
	output := listSkipped(t, &config.Config{}, draft, review, conflict)
	for _, url := range []string{draft.URL, review.URL, conflict.URL} {
		if strings.Contains(output, "URL: "+url) {
			t.Fatalf("ready list included %s:\n%s", url, output)
		}
	}
	if !strings.Contains(output, "skipped: "+draft.URL+" draft") {
		t.Fatalf("output missing draft reason:\n%s", output)
	}
	if !strings.Contains(output, "skipped: "+review.URL+" review requested") {
		t.Fatalf("output missing review reason:\n%s", output)
	}
	if !strings.Contains(output, "skipped: "+conflict.URL+" not mergeable") {
		t.Fatalf("output missing mergeable reason:\n%s", output)
	}
}

func TestListQuietOmitsSkippedLines(t *testing.T) {
	withVerbose(t, false)
	draft := mergeFixture("Draft this", "https://github.com/acme/widgets/pull/1")
	draft.IsDraft = true
	output := listSkipped(t, &config.Config{}, draft)
	if strings.Contains(output, "skipped:") {
		t.Fatalf("quiet list printed a skipped line:\n%s", output)
	}
	if strings.Contains(output, draft.URL) {
		t.Fatalf("quiet list showed the draft:\n%s", output)
	}
}

func TestListVerboseBlockLabelStaysReady(t *testing.T) {
	withVerbose(t, true)
	pr := mergeFixture("Hold this", "https://github.com/acme/widgets/pull/4", "do-not-merge")
	cfg := &config.Config{Settings: config.SettingsConfig{BlockMergeLabels: []string{"do-not-merge"}}}
	output := listSkipped(t, cfg, pr)
	if !strings.Contains(output, `Blocked: label "do-not-merge" prevents merge`) {
		t.Fatalf("ready list dropped the blocked pull request:\n%s", output)
	}
	if !strings.Contains(output, "skipped: "+pr.URL+" blocking label do-not-merge") {
		t.Fatalf("output missing blocking label reason:\n%s", output)
	}
}

func TestListVerboseJoinsEveryReason(t *testing.T) {
	withVerbose(t, true)
	pr := mergeFixture("Hold this", "https://github.com/acme/widgets/pull/9", "do-not-merge")
	pr.IsDraft = true
	pr.Mergeable = "CONFLICTING"
	pr.ReviewRequests = []models.ReviewRequest{{Login: "sam"}}
	cfg := &config.Config{Settings: config.SettingsConfig{BlockMergeLabels: []string{"do-not-merge"}}}
	output := listSkipped(t, cfg, pr)
	want := "skipped: " + pr.URL + " draft, review requested, not mergeable, blocking label do-not-merge"
	if !strings.Contains(output, want) {
		t.Fatalf("output missing %q:\n%s", want, output)
	}
}

func TestListUnknownThenMergeableIsReady(t *testing.T) {
	withVerbose(t, true)
	pr := mergeFixture("Ship this", "https://github.com/acme/widgets/pull/5")
	pr.Mergeable = "UNKNOWN"
	script := &refreshScript{
		prs:  []models.PullRequest{pr},
		next: map[string][]string{pr.URL: {"MERGEABLE"}},
	}
	output := captureStdout(t, func() {
		if err := listFetchedPullRequests(script, &config.Config{}, script.sleep); err != nil {
			t.Fatalf("listFetchedPullRequests() error = %v", err)
		}
	})
	if !strings.Contains(output, "URL: "+pr.URL) {
		t.Fatalf("resolved pull request was not ready:\n%s", output)
	}
	if strings.Contains(output, "skipped:") || strings.Contains(output, "unknown") {
		t.Fatalf("resolved pull request was skipped:\n%s", output)
	}
	if script.refreshes != 1 || len(script.sleeps) != 1 || script.sleeps[0] != time.Second {
		t.Fatalf("refreshes=%d sleeps=%v", script.refreshes, script.sleeps)
	}
}

func TestListUnknownStaysSkipped(t *testing.T) {
	withVerbose(t, true)
	pr := mergeFixture("Wait this", "https://github.com/acme/widgets/pull/6")
	pr.Mergeable = "UNKNOWN"
	script := &refreshScript{
		prs:  []models.PullRequest{pr},
		next: map[string][]string{pr.URL: {"UNKNOWN", "UNKNOWN"}},
	}
	output := captureStdout(t, func() {
		if err := listFetchedPullRequests(script, &config.Config{}, script.sleep); err != nil {
			t.Fatalf("listFetchedPullRequests() error = %v", err)
		}
	})
	if strings.Contains(output, "URL: "+pr.URL) || strings.Contains(output, "Found ") {
		t.Fatalf("unknown pull request was ready:\n%s", output)
	}
	if !strings.Contains(output, "skipped: "+pr.URL+" unknown") {
		t.Fatalf("output missing unknown reason:\n%s", output)
	}
	if script.refreshes != 2 || len(script.sleeps) != 2 {
		t.Fatalf("refreshes=%d sleeps=%v", script.refreshes, script.sleeps)
	}
}

func listSkipped(t *testing.T, cfg *config.Config, prs ...models.PullRequest) string {
	t.Helper()
	script := &refreshScript{prs: prs}
	return captureStdout(t, func() {
		if err := listFetchedPullRequests(script, cfg, script.sleep); err != nil {
			t.Fatalf("listFetchedPullRequests() error = %v", err)
		}
	})
}

func withVerbose(t *testing.T, on bool) {
	t.Helper()
	prev := verbose
	verbose = on
	t.Cleanup(func() { verbose = prev })
}

type refreshScript struct {
	prs       []models.PullRequest
	next      map[string][]string
	sleeps    []time.Duration
	refreshes int
}

func (s *refreshScript) GetAuthoredPullRequests() ([]models.PullRequest, error) {
	return s.prs, nil
}

func (s *refreshScript) GetMergeablePullRequests() ([]models.PullRequest, error) {
	return nil, errors.New("list should fetch authored pull requests")
}

func (s *refreshScript) MergePullRequest(string, bool) error { return nil }

func (s *refreshScript) MergeableState(url string) (string, error) {
	seq := s.next[url]
	if len(seq) == 0 {
		return "", errors.New("unexpected mergeable refresh for " + url)
	}
	s.next[url] = seq[1:]
	s.refreshes++
	return seq[0], nil
}

func (s *refreshScript) sleep(d time.Duration) {
	s.sleeps = append(s.sleeps, d)
}

func (g *ghAttempts) ExecuteSeparated(string, ...string) (string, string, error) {
	if g.n >= len(g.calls) {
		return "", "", errors.New("unexpected gh call")
	}
	call := g.calls[g.n]
	g.n++
	return call.stdout, call.stderr, call.err
}

// sweepExec answers gh pr list with the cheap list payload and
// gh pr view --json statusCheckRollup from checks. It records every call.
type sweepExec struct {
	t      *testing.T
	calls  [][]string
	list   []models.PullRequest
	checks map[string][]models.StatusCheck
}

func (s *sweepExec) Execute(cmd string, args ...string) (string, error) {
	stdout, _, err := s.ExecuteSeparated(cmd, args...)
	return stdout, err
}

func (s *sweepExec) ExecuteWithStdin(_ string, cmd string, args ...string) (string, error) {
	return s.Execute(cmd, args...)
}

func (s *sweepExec) ExecuteSeparated(_ string, args ...string) (string, string, error) {
	s.calls = append(s.calls, append([]string(nil), args...))
	var body any
	switch {
	case len(args) >= 2 && args[0] == "pr" && args[1] == "list":
		list := make([]models.PullRequest, len(s.list))
		for i, pr := range s.list {
			pr.StatusCheckRollup = nil
			pr.Files = nil
			list[i] = pr
		}
		body = list
	case len(args) == 5 && args[0] == "pr" && args[1] == "view" && args[4] == "statusCheckRollup":
		body = map[string][]models.StatusCheck{"statusCheckRollup": s.checks[args[2]]}
	default:
		return "", "", errors.New("unexpected gh call: " + strings.Join(args, " "))
	}
	out, err := json.Marshal(body)
	if err != nil {
		s.t.Fatalf("marshal fake gh output: %v", err)
	}
	return string(out), "", nil
}

func (s *sweepExec) callsWith(sub, field string) [][]string {
	var got [][]string
	for _, args := range s.calls {
		if len(args) >= 2 && args[1] == sub && slices.Contains(args, field) {
			got = append(got, args)
		}
	}
	return got
}

// sweepFixture is 10 open pull requests. Only pull/1 and pull/2 are
// non-draft, mergeable, and have no review requests.
func sweepFixture() []models.PullRequest {
	pass := []models.StatusCheck{{Name: "ci", Status: "COMPLETED", Conclusion: "SUCCESS"}}
	fail := []models.StatusCheck{{Name: "ci", Status: "COMPLETED", Conclusion: "FAILURE"}}
	review := []models.ReviewRequest{{Type: "User", Login: "alice"}}
	prs := make([]models.PullRequest, 10)
	for i := range prs {
		n := i + 1
		prs[i] = mergeFixture(fmt.Sprintf("PR %d", n), fmt.Sprintf("https://github.com/acme/widgets/pull/%d", n))
		prs[i].StatusCheckRollup = pass
		prs[i].Files = []models.File{{Path: fmt.Sprintf("f%d.go", n)}}
	}
	prs[1].StatusCheckRollup = fail
	prs[2].IsDraft = true
	prs[3].IsDraft = true
	prs[4].ReviewRequests = review
	prs[5].ReviewRequests = review
	prs[6].Mergeable = "CONFLICTING"
	prs[7].Mergeable = "CONFLICTING"
	prs[8].IsDraft, prs[8].Mergeable = true, "CONFLICTING"
	prs[9].ReviewRequests, prs[9].Mergeable = review, "CONFLICTING"
	return prs
}

func newSweepExec(t *testing.T) *sweepExec {
	t.Helper()
	exec := &sweepExec{t: t, list: sweepFixture(), checks: map[string][]models.StatusCheck{}}
	for _, pr := range exec.list {
		exec.checks[pr.URL] = pr.StatusCheckRollup
	}
	return exec
}

func assertCheapSweep(t *testing.T, exec *sweepExec) {
	t.Helper()
	lists := exec.callsWith("list", "--json")
	if len(lists) != 1 {
		t.Fatalf("pr list calls = %d, want 1: %v", len(lists), exec.calls)
	}
	fields := strings.Split(lists[0][len(lists[0])-1], ",")
	if slices.Contains(fields, "files") || slices.Contains(fields, "statusCheckRollup") {
		t.Fatalf("pr list --json %v requests files or statusCheckRollup", fields)
	}
	var checked []string
	for _, args := range exec.callsWith("view", "statusCheckRollup") {
		checked = append(checked, args[2])
	}
	want := []string{"https://github.com/acme/widgets/pull/1", "https://github.com/acme/widgets/pull/2"}
	if !slices.Equal(checked, want) {
		t.Fatalf("status checks fetched for %v, want %v", checked, want)
	}
	for _, args := range exec.calls {
		if slices.Contains(args, "files") {
			t.Fatalf("files fetched: %v", args)
		}
	}
	if len(exec.calls) != 3 {
		t.Fatalf("gh calls = %d, want 3: %v", len(exec.calls), exec.calls)
	}
}

func TestListSweepFetchesChecksOnlyForCandidates(t *testing.T) {
	cfg := &config.Config{Settings: config.SettingsConfig{RequiredChecks: []string{"ci"}}}
	exec := newSweepExec(t)
	client := github.NewClientWithExecutor(exec)
	got := captureStdout(t, func() {
		if err := listFetchedPullRequests(client, cfg, func(time.Duration) {}); err != nil {
			t.Fatalf("listFetchedPullRequests() error = %v", err)
		}
	})
	assertCheapSweep(t, exec)

	// The old single-call path had checks on every listed pull request.
	old := &refreshScript{prs: sweepFixture()}
	want := captureStdout(t, func() {
		if err := listFetchedPullRequests(old, cfg, old.sleep); err != nil {
			t.Fatalf("old listFetchedPullRequests() error = %v", err)
		}
	})
	if got != want {
		t.Fatalf("list output changed:\ngot:\n%s\nwant:\n%s", got, want)
	}
	if !strings.Contains(got, "CI: PASS") || !strings.Contains(got, "CI: FAIL") {
		t.Fatalf("list output lacks CI status:\n%s", got)
	}
}

func TestMergeSweepFetchesChecksOnlyForCandidates(t *testing.T) {
	withMergeFlags(t, true, false, false)
	cfg := &config.Config{}
	exec := newSweepExec(t)
	client := github.NewClientWithExecutor(exec)
	got := captureStdout(t, func() {
		if err := mergePRs(client, cfg); err != nil {
			t.Fatalf("mergePRs() error = %v", err)
		}
	})
	assertCheapSweep(t, exec)

	// The old single-call path returned the filtered pull requests with checks.
	var candidates []models.PullRequest
	for _, pr := range sweepFixture() {
		if listReady(&pr) {
			candidates = append(candidates, pr)
		}
	}
	want := captureStdout(t, func() {
		if err := mergePRs(&fakeMergeClient{prs: candidates}, cfg); err != nil {
			t.Fatalf("old mergePRs() error = %v", err)
		}
	})
	if got != want {
		t.Fatalf("merge output changed:\ngot:\n%s\nwant:\n%s", got, want)
	}
	if !strings.Contains(got, "Skipping: CI checks failed") {
		t.Fatalf("merge did not see the failing check:\n%s", got)
	}
}

func TestMergeStatusCheckErrorFailsCommand(t *testing.T) {
	withMergeFlags(t, true, false, false)
	client := github.NewClientWithExecutor(&failingChecks{newSweepExec(t)})
	err := mergePRs(client, &config.Config{})
	if err == nil || !strings.Contains(err.Error(), "status checks") {
		t.Fatalf("mergePRs() error = %v, want status checks error", err)
	}
}

// failingChecks fails every status check read.
type failingChecks struct{ *sweepExec }

func (f *failingChecks) ExecuteSeparated(cmd string, args ...string) (string, string, error) {
	if slices.Contains(args, "statusCheckRollup") {
		return "", "gh: boom", errors.New("exit status 1")
	}
	return f.sweepExec.ExecuteSeparated(cmd, args...)
}
