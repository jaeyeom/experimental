package main

import (
	"bytes"
	"io"
	"log/slog"
	"os"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
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
