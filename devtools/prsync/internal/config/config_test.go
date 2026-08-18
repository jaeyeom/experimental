package config

import (
	"errors"
	"maps"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"
	"time"
)

func TestParse(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		input   string
		want    map[string]string
		wantErr bool
		errLine int
		errKey  string
	}{
		{
			name:  "empty",
			input: "",
			want:  map[string]string{},
		},
		{
			name:  "blank lines and comments",
			input: "# header\n\n  \n  # indented comment\nfoo=bar\n",
			want:  map[string]string{"foo": "bar"},
		},
		{
			name:  "trim key and value",
			input: "  foo  =  bar  \n",
			want:  map[string]string{"foo": "bar"},
		},
		{
			name:  "value may contain hash",
			input: "prompt=hello # still value\n",
			want:  map[string]string{"prompt": "hello # still value"},
		},
		{
			name:  "double quotes stripped",
			input: `foo="bar baz"` + "\n",
			want:  map[string]string{"foo": "bar baz"},
		},
		{
			name:  "double quote escapes",
			input: `foo="bar\"baz\\qux"` + "\n",
			want:  map[string]string{"foo": `bar"baz\qux`},
		},
		{
			name:  "single quotes literal",
			input: `foo='bar\'baz'` + "\n",
			want:  map[string]string{"foo": `bar\'baz`},
		},
		{
			name:  "unmatched quotes kept",
			input: `foo="bar` + "\n",
			want:  map[string]string{"foo": `"bar`},
		},
		{
			name:  "duplicate last wins",
			input: "foo=1\nfoo=2\n",
			want:  map[string]string{"foo": "2"},
		},
		{
			name:  "empty value",
			input: "foo=\n",
			want:  map[string]string{"foo": ""},
		},
		{
			name:    "missing equals",
			input:   "just-a-key\n",
			wantErr: true,
			errLine: 1,
		},
		{
			name:    "empty key",
			input:   "=value\n",
			wantErr: true,
			errLine: 1,
		},
		{
			name:    "invalid key",
			input:   "foo-bar=1\n",
			wantErr: true,
			errLine: 1,
			errKey:  "foo-bar",
		},
		{
			name:    "error names line number after comments",
			input:   "# ok\n\nbad line\n",
			wantErr: true,
			errLine: 3,
		},
		{
			name:  "underscore and digit keys",
			input: "A1_b=ok\n_hidden=yes\n",
			want:  map[string]string{"A1_b": "ok", "_hidden": "yes"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, err := Parse(strings.NewReader(tc.input))
			if tc.wantErr {
				if err == nil {
					t.Fatal("expected error")
				}
				var keyErr *KeyError
				if !errors.As(err, &keyErr) {
					t.Fatalf("error type %T: %v", err, err)
				}
				if tc.errLine != 0 && keyErr.Line != tc.errLine {
					t.Fatalf("Line = %d, want %d", keyErr.Line, tc.errLine)
				}
				if tc.errKey != "" && keyErr.Key != tc.errKey {
					t.Fatalf("Key = %q, want %q", keyErr.Key, tc.errKey)
				}
				return
			}
			if err != nil {
				t.Fatalf("Parse: %v", err)
			}
			if !maps.Equal(got, tc.want) {
				t.Fatalf("Parse = %#v, want %#v", got, tc.want)
			}
		})
	}
}

func TestLoadSearchOrder(t *testing.T) {
	tests := []struct {
		name        string
		setup       func(t *testing.T) (explicit string)
		wantAuthor  string
		wantSource  func(home, cwd, bazelWD string) string
		wantErr     bool
		wantDefault bool
	}{
		{
			name: "explicit path wins",
			setup: func(t *testing.T) string {
				t.Helper()
				home, cwd := isolateConfigEnv(t)
				mustWrite(t, filepath.Join(home, ".config", "prsync", "prsync.config"), "author=from-home\n")
				mustWrite(t, filepath.Join(cwd, "prsync.config"), "author=from-cwd\n")
				explicit := filepath.Join(t.TempDir(), "custom.config")
				mustWrite(t, explicit, "author=from-explicit\n")
				return explicit
			},
			wantAuthor: "from-explicit",
			wantSource: func(_, _, _ string) string { return "" }, // filled below via explicit
		},
		{
			name: "explicit missing is error",
			setup: func(t *testing.T) string {
				t.Helper()
				isolateConfigEnv(t)
				return filepath.Join(t.TempDir(), "missing.config")
			},
			wantErr: true,
		},
		{
			name: "cwd file before home",
			setup: func(t *testing.T) string {
				t.Helper()
				home, cwd := isolateConfigEnv(t)
				mustWrite(t, filepath.Join(home, ".config", "prsync", "prsync.config"), "author=from-home\n")
				mustWrite(t, filepath.Join(cwd, "prsync.config"), "author=from-cwd\n")
				return ""
			},
			wantAuthor: "from-cwd",
			wantSource: func(_, cwd, _ string) string {
				return filepath.Join(cwd, "prsync.config")
			},
		},
		{
			name: "home file when cwd missing",
			setup: func(t *testing.T) string {
				t.Helper()
				home, _ := isolateConfigEnv(t)
				mustWrite(t, filepath.Join(home, ".config", "prsync", "prsync.config"), "author=from-home\n")
				return ""
			},
			wantAuthor: "from-home",
			wantSource: func(home, _, _ string) string {
				return filepath.Join(home, ".config", "prsync", "prsync.config")
			},
		},
		{
			name: "BUILD_WORKING_DIRECTORY replaces cwd",
			setup: func(t *testing.T) string {
				t.Helper()
				home, cwd := isolateConfigEnv(t)
				mustWrite(t, filepath.Join(cwd, "prsync.config"), "author=from-cwd\n")
				bazelWD := t.TempDir()
				mustWrite(t, filepath.Join(bazelWD, "prsync.config"), "author=from-bazel-wd\n")
				mustWrite(t, filepath.Join(home, ".config", "prsync", "prsync.config"), "author=from-home\n")
				t.Setenv("BUILD_WORKING_DIRECTORY", bazelWD)
				return ""
			},
			wantAuthor: "from-bazel-wd",
			wantSource: func(_, _, bazelWD string) string {
				return filepath.Join(bazelWD, "prsync.config")
			},
		},
		{
			name: "missing files use defaults",
			setup: func(t *testing.T) string {
				t.Helper()
				isolateConfigEnv(t)
				return ""
			},
			wantDefault: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			explicit := tc.setup(t)
			home := os.Getenv("HOME")
			cwd, err := os.Getwd()
			if err != nil {
				t.Fatal(err)
			}
			bazelWD := os.Getenv("BUILD_WORKING_DIRECTORY")

			got, err := Load(explicit)
			if tc.wantErr {
				if err == nil {
					t.Fatal("expected error")
				}
				var keyErr *KeyError
				if !errors.As(err, &keyErr) {
					t.Fatalf("error type %T: %v", err, err)
				}
				return
			}
			if err != nil {
				t.Fatalf("Load: %v", err)
			}
			if tc.wantDefault {
				want := Defaults()
				if got.SourcePath != "" {
					t.Fatalf("SourcePath = %q, want empty", got.SourcePath)
				}
				if got.Author != want.Author || got.GHBin != want.GHBin || !slices.Equal(got.WaitUntil, want.WaitUntil) {
					t.Fatalf("defaults mismatch: %#v", got)
				}
				return
			}
			if got.Author != tc.wantAuthor {
				t.Fatalf("Author = %q, want %q (source %q)", got.Author, tc.wantAuthor, got.SourcePath)
			}
			if explicit != "" {
				if got.SourcePath != explicit {
					t.Fatalf("SourcePath = %q, want %q", got.SourcePath, explicit)
				}
				return
			}
			if tc.wantSource != nil {
				wantPath := tc.wantSource(home, cwd, bazelWD)
				if got.SourcePath != wantPath {
					t.Fatalf("SourcePath = %q, want %q", got.SourcePath, wantPath)
				}
			}
		})
	}
}

func TestLoadAppliesKnownKeys(t *testing.T) {
	dir := t.TempDir()
	promptFile := filepath.Join(dir, "prompt.txt")
	rebaseFile := filepath.Join(dir, "rebase.txt")
	mustWrite(t, promptFile, "from-file")
	mustWrite(t, rebaseFile, "rebase-from-file")
	cfgPath := filepath.Join(dir, "prsync.config")
	mustWrite(t, cfgPath, strings.Join([]string{
		"repos=acme/widgets other/repo",
		"author=alice",
		"title_id_pattern=TICKET-[0-9]+",
		"tab_label_template=wip/{id}",
		"herdr_bin=/bin/herdr",
		"gh_bin=/bin/gh",
		"concurrency_wait_on=managed",
		"gate_poll_ms=5",
		"gate_timeout_ms=50",
		"dispatch_prompt_template=@" + promptFile,
		"rebase_prompt_template=@" + rebaseFile,
		"dispatch_include_drafts=1",
		"dispatch_wait_until=idle done",
		"dispatch_timeout_ms=100",
		"state_file=" + filepath.Join(dir, "state.json"),
		"dry_run=false",
		"unknown_future_key=ignored",
	}, "\n")+"\n")

	got, err := Load(cfgPath)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if !slices.Equal(got.Repos, []string{"acme/widgets", "other/repo"}) {
		t.Fatalf("Repos = %#v", got.Repos)
	}
	if got.Author != "alice" {
		t.Fatalf("Author = %q", got.Author)
	}
	if got.TitleIDPattern == nil || got.TitleIDPattern.String() != `TICKET-[0-9]+` {
		t.Fatalf("TitleIDPattern = %v", got.TitleIDPattern)
	}
	if got.TabLabelTemplate != "wip/{id}" {
		t.Fatalf("TabLabelTemplate = %q", got.TabLabelTemplate)
	}
	if got.HerdrBin != "/bin/herdr" || got.GHBin != "/bin/gh" {
		t.Fatalf("bins herdr=%q gh=%q", got.HerdrBin, got.GHBin)
	}
	if got.ConcurrencyWaitOn != "managed" {
		t.Fatalf("ConcurrencyWaitOn = %q", got.ConcurrencyWaitOn)
	}
	if got.GatePoll != 5*time.Millisecond || got.GateTimeout != 50*time.Millisecond {
		t.Fatalf("gate durations poll=%s timeout=%s", got.GatePoll, got.GateTimeout)
	}
	if got.PromptTemplate != "from-file" {
		t.Fatalf("PromptTemplate = %q", got.PromptTemplate)
	}
	if got.RebasePromptTemplate != "rebase-from-file" {
		t.Fatalf("RebasePromptTemplate = %q", got.RebasePromptTemplate)
	}
	if !got.IncludeDrafts {
		t.Fatal("IncludeDrafts = false")
	}
	if !slices.Equal(got.WaitUntil, []string{"idle", "done"}) {
		t.Fatalf("WaitUntil = %#v", got.WaitUntil)
	}
	if got.DispatchTimeout != 100*time.Millisecond {
		t.Fatalf("DispatchTimeout = %s", got.DispatchTimeout)
	}
	if got.StateFile != filepath.Join(dir, "state.json") {
		t.Fatalf("StateFile = %q", got.StateFile)
	}
	if got.DryRun {
		t.Fatal("DryRun = true")
	}
}

func TestLoadMillisAreMilliseconds(t *testing.T) {
	path := filepath.Join(t.TempDir(), "prsync.config")
	mustWrite(t, path, "gate_poll_ms=2000\n")
	got, err := Load(path)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if got.GatePoll != 2*time.Second {
		t.Fatalf("GatePoll = %s, want 2s (not 2000ns)", got.GatePoll)
	}
}

func TestLoadExpandsTilde(t *testing.T) {
	home, _ := isolateConfigEnv(t)
	path := filepath.Join(t.TempDir(), "prsync.config")
	mustWrite(t, path, "state_file=~/custom/state.json\ngh_bin=~/bin/gh\nherdr_bin=~/bin/herdr\n")
	got, err := Load(path)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if got.StateFile != filepath.Join(home, "custom", "state.json") {
		t.Fatalf("StateFile = %q", got.StateFile)
	}
	if got.GHBin != filepath.Join(home, "bin", "gh") {
		t.Fatalf("GHBin = %q", got.GHBin)
	}
	if got.HerdrBin != filepath.Join(home, "bin", "herdr") {
		t.Fatalf("HerdrBin = %q", got.HerdrBin)
	}
}

func TestLoadValidationErrors(t *testing.T) {
	t.Parallel()

	overflow := "9223372036854776" // > MaxInt64 / time.Millisecond
	tests := []struct {
		name string
		body string
		key  string
	}{
		{name: "bad repo", body: "repos=not-a-repo\n", key: "repos"},
		{name: "bad regex", body: "title_id_pattern=[A-Z+\n", key: "title_id_pattern"},
		{name: "empty template", body: "tab_label_template=\n", key: "tab_label_template"},
		{name: "empty herdr", body: "herdr_bin=\n", key: "herdr_bin"},
		{name: "empty gh", body: "gh_bin=\n", key: "gh_bin"},
		{name: "bad enum", body: "concurrency_wait_on=sometimes\n", key: "concurrency_wait_on"},
		{name: "zero poll", body: "gate_poll_ms=0\n", key: "gate_poll_ms"},
		{name: "negative timeout", body: "gate_timeout_ms=-1\n", key: "gate_timeout_ms"},
		{name: "non integer ms", body: "dispatch_timeout_ms=2s\n", key: "dispatch_timeout_ms"},
		{name: "overflow ms", body: "gate_poll_ms=" + overflow + "\n", key: "gate_poll_ms"},
		{name: "bad bool", body: "dry_run=yes\n", key: "dry_run"},
		{name: "empty wait until", body: "dispatch_wait_until=\n", key: "dispatch_wait_until"},
		{name: "missing prompt file", body: "dispatch_prompt_template=@/no/such/prompt\n", key: "dispatch_prompt_template"},
		{name: "missing rebase prompt file", body: "rebase_prompt_template=@/no/such/rebase\n", key: "rebase_prompt_template"},
		{name: "empty state file", body: "state_file=\n", key: "state_file"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			path := filepath.Join(t.TempDir(), "prsync.config")
			mustWrite(t, path, tc.body)
			_, err := Load(path)
			if err == nil {
				t.Fatal("expected error")
			}
			var keyErr *KeyError
			if !errors.As(err, &keyErr) {
				t.Fatalf("error type %T: %v", err, err)
			}
			if keyErr.Key != tc.key {
				t.Fatalf("Key = %q, want %q (err=%v)", keyErr.Key, tc.key, err)
			}
		})
	}
}

func TestKeyErrorMessage(t *testing.T) {
	t.Parallel()
	err := &KeyError{Key: "title_id_pattern", Reason: "error parsing regexp: missing closing ]: `[A-Z+`"}
	got := err.Error()
	want := "key title_id_pattern: error parsing regexp: missing closing ]: `[A-Z+`"
	if got != want {
		t.Fatalf("Error() = %q, want %q", got, want)
	}
}

func TestDefaults(t *testing.T) {
	t.Parallel()
	got := Defaults()
	if got.TitleIDPattern == nil || got.TitleIDPattern.String() != `[A-Z]+-[0-9]+` {
		t.Fatalf("TitleIDPattern = %v", got.TitleIDPattern)
	}
	if got.TabLabelTemplate != "{id}" {
		t.Fatalf("TabLabelTemplate = %q", got.TabLabelTemplate)
	}
	if got.HerdrBin != "herdr" || got.GHBin != "gh" {
		t.Fatalf("bins herdr=%q gh=%q", got.HerdrBin, got.GHBin)
	}
	if got.ConcurrencyWaitOn != "any" {
		t.Fatalf("ConcurrencyWaitOn = %q", got.ConcurrencyWaitOn)
	}
	if got.GatePoll != 2000*time.Millisecond {
		t.Fatalf("GatePoll = %s", got.GatePoll)
	}
	if got.GateTimeout != 1800000*time.Millisecond {
		t.Fatalf("GateTimeout = %s", got.GateTimeout)
	}
	if got.DispatchTimeout != 1800000*time.Millisecond {
		t.Fatalf("DispatchTimeout = %s", got.DispatchTimeout)
	}
	if !slices.Equal(got.WaitUntil, []string{"idle", "done"}) {
		t.Fatalf("WaitUntil = %#v", got.WaitUntil)
	}
	if !got.DryRun {
		t.Fatal("DryRun default is not true")
	}
	if got.IncludeDrafts {
		t.Fatal("IncludeDrafts default is not false")
	}
	if got.StateFile != "~/.config/prsync/state.json" {
		t.Fatalf("StateFile = %q", got.StateFile)
	}
	if !regexp.MustCompile(`Address the unresolved review comments`).MatchString(got.PromptTemplate) {
		t.Fatalf("PromptTemplate missing built-in text: %q", got.PromptTemplate)
	}
	if !regexp.MustCompile(`(?s)Check out \{head\}.*Address the unresolved review comments`).MatchString(got.PromptTemplate) {
		t.Fatalf("PromptTemplate missing branch-switch preamble before comment body: %q", got.PromptTemplate)
	}
	if !regexp.MustCompile(`stash or commit`).MatchString(got.PromptTemplate) {
		t.Fatalf("PromptTemplate missing stash/commit: %q", got.PromptTemplate)
	}
	if !regexp.MustCompile(`gh pr checkout \{number\}`).MatchString(got.PromptTemplate) {
		t.Fatalf("PromptTemplate missing gh pr checkout: %q", got.PromptTemplate)
	}
	if !regexp.MustCompile(`origin/\{base\}`).MatchString(got.PromptTemplate) {
		t.Fatalf("PromptTemplate missing origin/{base}: %q", got.PromptTemplate)
	}
	if !regexp.MustCompile(`on \{head\} at its latest tip`).MatchString(got.PromptTemplate) {
		t.Fatalf("PromptTemplate missing latest-tip guard: %q", got.PromptTemplate)
	}
	if !regexp.MustCompile(`(?i)mechanical`).MatchString(got.PromptTemplate) {
		t.Fatalf("PromptTemplate missing mechanical-triage: %q", got.PromptTemplate)
	}
	if !regexp.MustCompile(`(?i)ask the user`).MatchString(got.PromptTemplate) {
		t.Fatalf("PromptTemplate missing ask-the-user: %q", got.PromptTemplate)
	}
	if !regexp.MustCompile(`(?i)recommend`).MatchString(got.PromptTemplate) {
		t.Fatalf("PromptTemplate missing recommended option: %q", got.PromptTemplate)
	}
	if regexp.MustCompile(`make the change \(or reply if you`).MatchString(got.PromptTemplate) {
		t.Fatalf("PromptTemplate still instructs autonomous resolve: %q", got.PromptTemplate)
	}
	if !regexp.MustCompile(`Check out \{head\}`).MatchString(got.RebasePromptTemplate) {
		t.Fatalf("RebasePromptTemplate missing checkout: %q", got.RebasePromptTemplate)
	}
	if !regexp.MustCompile(`origin/\{base\}`).MatchString(got.RebasePromptTemplate) {
		t.Fatalf("RebasePromptTemplate missing origin/{base}: %q", got.RebasePromptTemplate)
	}
	if !regexp.MustCompile(`--force-with-lease`).MatchString(got.RebasePromptTemplate) {
		t.Fatalf("RebasePromptTemplate missing --force-with-lease: %q", got.RebasePromptTemplate)
	}
	if !regexp.MustCompile(`(?i)do not create a new worktree`).MatchString(got.RebasePromptTemplate) {
		t.Fatalf("RebasePromptTemplate missing no-worktree: %q", got.RebasePromptTemplate)
	}
	if got.SourcePath != "" {
		t.Fatalf("SourcePath = %q", got.SourcePath)
	}
}

func isolateConfigEnv(t *testing.T) (home, cwd string) {
	t.Helper()
	home = t.TempDir()
	cwd = t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("BUILD_WORKING_DIRECTORY", "")
	t.Chdir(cwd)
	return home, cwd
}

func mustWrite(t *testing.T, path, body string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
}
