package approve

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

const sampleGeneratedYAML = `
- pattern: "**/*.pb.go"
  regenerate: "make proto"
  bazel_generated: true
  source: "proto/"
- pattern: "**/zz_generated.*.go"
  owner: "codegen"
`

func runGeneratedConfig(t *testing.T, fake *fakeClient, cfg config.ApproveConfig, viewed config.ViewedConfig, opts Options, generatedLists map[string]string) runResult {
	t.Helper()
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client:         fake,
		Config:         cfg,
		Viewed:         viewed,
		GeneratedLists: generatedLists,
		Out:            &out,
		Log:            &logBuf,
		Now:            fixedNow,
	}
	err := runner.Run(opts)
	var entries []LogEntry
	for _, line := range strings.Split(strings.TrimSpace(logBuf.String()), "\n") {
		if line == "" {
			continue
		}
		var entry LogEntry
		if jsonErr := json.Unmarshal([]byte(line), &entry); jsonErr != nil {
			t.Fatalf("log line %q: %v", line, jsonErr)
		}
		entries = append(entries, entry)
	}
	return runResult{
		err:  err,
		out:  out.String(),
		log:  entries,
		fake: fake,
	}
}

func generatedPR(n int, baseSHA string, files ...models.File) github.ApprovalPullRequest {
	pr := prNumber(n)
	pr.BaseRef = "main"
	pr.BaseSHA = baseSHA
	pr.NodeID = fmt.Sprintf("PR_node_%d", n)
	pr.Files = files
	pr.ChangedFiles = len(files)
	return pr
}

func TestGeneratedListReadFromBaseSHAAndCachedPerBaseSHA(t *testing.T) {
	pr1 := generatedPR(1, "sha-base-1", models.File{Path: "api/service.pb.go"})
	pr2 := generatedPR(2, "sha-base-1", models.File{Path: "pkg/zz_generated.deepcopy.go"})
	pr3 := generatedPR(3, "sha-base-2", models.File{Path: "api/other.pb.go"})

	fake := newFake(pr1, pr2, pr3)
	fake.repoFiles = map[string]string{
		"sha-base-1:tools/generated_files.yaml": sampleGeneratedYAML,
		"sha-base-2:tools/generated_files.yaml": "- pattern: \"**/*.pb.go\"\n",
	}
	cfg := config.ApproveConfig{
		Body: "LGTM",
		Rules: []config.ApproveRule{{
			Name:          "generated",
			PathsAllMatch: []string{config.GeneratedListRef},
		}},
	}
	res := runGeneratedConfig(t, fake, cfg, config.ViewedConfig{}, Options{Explain: true}, map[string]string{
		"acme/widgets": "tools/generated_files.yaml",
	})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if fake.repoFileReads != 2 {
		t.Fatalf("repoFileReads = %d, want 2 (once per base SHA)", fake.repoFileReads)
	}
	revs := slices.Clone(fake.repoFileRevs)
	slices.Sort(revs)
	if strings.Join(revs, ",") != "sha-base-1,sha-base-2" {
		t.Fatalf("repoFileRevs = %v, want [sha-base-1 sha-base-2]", fake.repoFileRevs)
	}
	for i, entry := range res.log {
		if entry.Action != ActionWouldApprove {
			t.Fatalf("pr %d action = %q, want would_approve (reason=%q)", i+1, entry.Action, entry.Reason)
		}
	}
	if !strings.Contains(res.out, `api/service.pb.go: @generated "**/*.pb.go"`) {
		t.Fatalf("explain missing matched entry:\n%s", res.out)
	}
	if !strings.Contains(res.out, `pkg/zz_generated.deepcopy.go: @generated "**/zz_generated.*.go"`) {
		t.Fatalf("explain missing second matched entry:\n%s", res.out)
	}
}

func TestGeneratedListUnionInApproveFields(t *testing.T) {
	t.Run("paths_all_match union with inline glob", func(t *testing.T) {
		pr := generatedPR(1, "sha1",
			models.File{Path: "api/a.pb.go"},
			models.File{Path: "yarn.lock"},
		)
		fake := newFake(pr)
		fake.repoFiles = map[string]string{"sha1:gen.yaml": sampleGeneratedYAML}
		cfg := config.ApproveConfig{
			Body: "LGTM",
			Rules: []config.ApproveRule{{
				Name:          "gen-or-lock",
				PathsAllMatch: []string{config.GeneratedListRef, "**/*.lock"},
			}},
		}
		res := runGeneratedConfig(t, fake, cfg, config.ViewedConfig{}, Options{Explain: true}, map[string]string{
			"acme/widgets": "gen.yaml",
		})
		if res.err != nil || res.log[0].Action != ActionWouldApprove {
			t.Fatalf("err=%v log=%+v", res.err, res.log[0])
		}
		if !strings.Contains(res.out, `api/a.pb.go: @generated "**/*.pb.go"`) {
			t.Fatalf("explain output = \n%s", res.out)
		}
	})

	t.Run("paths_any_match union with inline glob", func(t *testing.T) {
		pr := generatedPR(1, "sha1",
			models.File{Path: "api/a.pb.go"},
			models.File{Path: "main.go"},
		)
		fake := newFake(pr)
		fake.repoFiles = map[string]string{"sha1:gen.yaml": sampleGeneratedYAML}
		cfg := config.ApproveConfig{
			Body: "LGTM",
			Rules: []config.ApproveRule{{
				Name:          "any-gen",
				PathsAnyMatch: []string{"docs/**", config.GeneratedListRef},
			}},
		}
		res := runGeneratedConfig(t, fake, cfg, config.ViewedConfig{}, Options{Explain: true}, map[string]string{
			"acme/widgets": "gen.yaml",
		})
		if res.err != nil || res.log[0].Action != ActionWouldApprove {
			t.Fatalf("err=%v log=%+v", res.err, res.log[0])
		}
		if !strings.Contains(res.out, `api/a.pb.go: @generated "**/*.pb.go"`) {
			t.Fatalf("explain output = \n%s", res.out)
		}
	})

	t.Run("paths_none_match union with inline glob", func(t *testing.T) {
		pr := generatedPR(1, "sha1",
			models.File{Path: "api/a.pb.go"},
		)
		fake := newFake(pr)
		fake.repoFiles = map[string]string{"sha1:gen.yaml": sampleGeneratedYAML}
		cfg := config.ApproveConfig{
			Body: "LGTM",
			Rules: []config.ApproveRule{{
				Name:           "no-gen",
				PathsNoneMatch: []string{".github/**", config.GeneratedListRef},
			}},
		}
		res := runGeneratedConfig(t, fake, cfg, config.ViewedConfig{}, Options{Explain: true}, map[string]string{
			"acme/widgets": "gen.yaml",
		})
		if res.err != nil || res.log[0].Action != ActionManual {
			t.Fatalf("err=%v log=%+v", res.err, res.log[0])
		}
		if !strings.Contains(res.out, `api/a.pb.go: @generated "**/*.pb.go"`) {
			t.Fatalf("explain output = \n%s", res.out)
		}
	})

	t.Run("file_checks key @generated union with inline", func(t *testing.T) {
		pr := generatedPR(1, "sha1",
			fcFile("api/a.pb.go", 0, 5),
			fcFile("other/README.md", 0, 2),
		)
		fake := newFake(pr)
		fake.repoFiles = map[string]string{"sha1:gen.yaml": sampleGeneratedYAML}
		cfg := config.ApproveConfig{
			Body: "LGTM",
			Rules: []config.ApproveRule{{
				Name: "fc-gen",
				FileChecks: map[string]config.FileCheck{
					config.GeneratedListRef: {Builtin: config.FileCheckOnlyDeletions},
					"other/**":              {Builtin: config.FileCheckOnlyDeletions},
				},
			}},
		}
		res := runGeneratedConfig(t, fake, cfg, config.ViewedConfig{}, Options{Explain: true}, map[string]string{
			"acme/widgets": "gen.yaml",
		})
		if res.err != nil || res.log[0].Action != ActionWouldApprove {
			t.Fatalf("err=%v log=%+v\nout=%s", res.err, res.log[0], res.out)
		}
		if !strings.Contains(res.out, `@generated "**/*.pb.go"`) {
			t.Fatalf("explain output = \n%s", res.out)
		}
	})
}

func TestGeneratedListInViewedRules(t *testing.T) {
	pr := generatedPR(1, "sha1",
		viewedFile("api/a.pb.go", models.ViewerUnviewed),
		viewedFile("yarn.lock", models.ViewerUnviewed),
		viewedFile("pkg/zz_generated.deepcopy.go", models.ViewerUnviewed),
		viewedFile("main.go", models.ViewerUnviewed),
	)
	fake := newFake(pr)
	fake.repoFiles = map[string]string{"sha1:gen.yaml": sampleGeneratedYAML}
	viewed := config.ViewedConfig{
		Enabled:        true,
		MaxMarksPerRun: config.DefaultMaxMarksPerRun,
		Rules: []config.ViewedRule{
			{
				Name:           "gen",
				PathsMatch:     []string{config.GeneratedListRef, "**/*.lock"},
				PathsNoneMatch: []string{"**/zz_generated.*.go"},
			},
		},
	}
	res := runGeneratedConfig(t, fake, config.ApproveConfig{Body: "LGTM"}, viewed, Options{Apply: true, Explain: true}, map[string]string{
		"acme/widgets": "gen.yaml",
	})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionMarkViewed || strings.Join(res.log[0].Paths, ",") != "api/a.pb.go,yarn.lock" {
		t.Fatalf("entry = %+v", res.log[0])
	}
	if !strings.Contains(res.out, `api/a.pb.go: @generated "**/*.pb.go"`) {
		t.Fatalf("explain output missing @generated entry:\n%s", res.out)
	}

	// Also verify @generated in paths_none_match excludes generated files.
	pr2 := generatedPR(2, "sha1",
		viewedFile("api/a.pb.go", models.ViewerUnviewed),
		viewedFile("api/hand.go", models.ViewerUnviewed),
	)
	fake2 := newFake(pr2)
	fake2.repoFiles = map[string]string{"sha1:gen.yaml": sampleGeneratedYAML}
	viewedExclude := config.ViewedConfig{
		Enabled:        true,
		MaxMarksPerRun: config.DefaultMaxMarksPerRun,
		Rules: []config.ViewedRule{{
			Name:           "all-api-except-gen",
			PathsMatch:     []string{"api/**"},
			PathsNoneMatch: []string{config.GeneratedListRef},
		}},
	}
	res2 := runGeneratedConfig(t, fake2, config.ApproveConfig{Body: "LGTM"}, viewedExclude, Options{Apply: true, Explain: true}, map[string]string{
		"acme/widgets": "gen.yaml",
	})
	if res2.err != nil {
		t.Fatal(res2.err)
	}
	if strings.Join(res2.log[0].Paths, ",") != "api/hand.go" {
		t.Fatalf("paths = %v, want [api/hand.go]", res2.log[0].Paths)
	}
	if !strings.Contains(res2.out, `excluded by rule all-api-except-gen paths_none_match @generated "**/*.pb.go"`) {
		t.Fatalf("explain output = \n%s", res2.out)
	}
}

func TestParseGeneratedList(t *testing.T) {
	patterns, err := parseGeneratedList(sampleGeneratedYAML)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(patterns, ",") != "**/*.pb.go,**/zz_generated.*.go" {
		t.Fatalf("patterns = %v", patterns)
	}

	if _, err := parseGeneratedList("- regenerate: make proto\n"); err == nil {
		t.Fatal("expected error for entry without pattern")
	}
	if _, err := parseGeneratedList("- pattern: \"a/[\"\n"); err == nil {
		t.Fatal("expected error for invalid glob")
	}
	if _, err := parseGeneratedList("not: [valid: yaml"); err == nil {
		t.Fatal("expected error for invalid yaml")
	}
}

func TestGeneratedListFailClosed(t *testing.T) {
	tests := []struct {
		name      string
		repoFiles map[string]string
		fileErr   error
		want      string
	}{
		{
			name:      "missing file",
			repoFiles: map[string]string{},
			want:      "generated list tools/generated_files.yaml: not found at sha1",
		},
		{
			name:    "unreadable file",
			fileErr: errors.New("api timeout"),
			want:    "generated list tools/generated_files.yaml: api timeout",
		},
		{
			name:      "invalid yaml",
			repoFiles: map[string]string{"sha1:tools/generated_files.yaml": "- regenerate: make proto\n"},
			want:      "generated list tools/generated_files.yaml: entry 1: missing pattern",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			pr := generatedPR(1, "sha1", viewedFile("api/a.pb.go", models.ViewerUnviewed))
			fake := newFake(pr)
			fake.repoFiles = tc.repoFiles
			fake.repoFileErr = tc.fileErr

			cfg := config.ApproveConfig{
				Body: "LGTM",
				Rules: []config.ApproveRule{
					{Name: "all", PathsAllMatch: []string{config.GeneratedListRef}},
					{Name: "any", PathsAnyMatch: []string{config.GeneratedListRef}},
					{Name: "none", PathsNoneMatch: []string{config.GeneratedListRef}},
					{Name: "fc", FileChecks: map[string]config.FileCheck{config.GeneratedListRef: {Builtin: config.FileCheckOnlyDeletions}}},
				},
			}
			viewed := config.ViewedConfig{
				Enabled:        true,
				MaxMarksPerRun: config.DefaultMaxMarksPerRun,
				Rules: []config.ViewedRule{{
					Name:       "gen",
					PathsMatch: []string{config.GeneratedListRef},
				}},
			}
			res := runGeneratedConfig(t, fake, cfg, viewed, Options{Apply: true, Explain: true}, map[string]string{
				"acme/widgets": "tools/generated_files.yaml",
			})
			if res.err == nil {
				t.Fatalf("expected error from failed viewed rule, got nil")
			}
			if res.log[0].Action != ActionError {
				t.Fatalf("action = %q, want error", res.log[0].Action)
			}
			if len(fake.approvals) != 0 {
				t.Fatalf("unexpected approvals: %+v", fake.approvals)
			}
			if len(fake.marks) != 0 {
				t.Fatalf("viewed rule marked files on failure: %+v", fake.marks)
			}
			for _, ruleRes := range res.log[0].Rules {
				if ruleRes.Matched {
					t.Fatalf("rule %q unexpectedly matched", ruleRes.Rule)
				}
				first, ok := ruleRes.FirstFailure()
				if !ok || !strings.Contains(first.Saw, tc.want) {
					t.Fatalf("rule %q failure = %+v, want %q", ruleRes.Rule, first, tc.want)
				}
			}
			if !strings.Contains(res.out, tc.want) {
				t.Fatalf("explain output missing %q:\n%s", tc.want, res.out)
			}
		})
	}
}
