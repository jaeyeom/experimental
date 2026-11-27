package devtools_test

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"testing"
	"time"
)

type versionMode int

const (
	modeVersionJSON versionMode = iota
	modeVersionFlag
)

type versionSpec struct {
	mode versionMode
}

// commandVersionSpecs lists every devtools/*/cmd/* binary and how it reports
// its version. New binaries must be added here (or in versionSkipReasons with
// an explicit reason).
var commandVersionSpecs = map[string]versionSpec{
	"agent-pool":                             {mode: modeVersionJSON},
	"agent-poold":                            {mode: modeVersionJSON},
	"agentbus":                               {mode: modeVersionJSON},
	"agentbusd":                              {mode: modeVersionFlag},
	"agents-md-size":                         {mode: modeVersionFlag},
	"authwatch":                              {mode: modeVersionJSON},
	"box-renderer":                           {mode: modeVersionFlag},
	"claude-usage":                           {mode: modeVersionJSON},
	"contacts":                               {mode: modeVersionJSON},
	"decide":                                 {mode: modeVersionJSON},
	"devcheck":                               {mode: modeVersionJSON},
	"diskwatch":                              {mode: modeVersionJSON},
	"docsync":                                {mode: modeVersionJSON},
	"factory":                                {mode: modeVersionJSON},
	"fork-keeper":                            {mode: modeVersionJSON},
	"genfiles":                               {mode: modeVersionFlag},
	"gh-approve":                             {mode: modeVersionJSON},
	"gh-ask":                                 {mode: modeVersionJSON},
	"gh-codeowners":                          {mode: modeVersionJSON},
	"gh-merge":                               {mode: modeVersionJSON},
	"gh-nudge":                               {mode: modeVersionJSON},
	"gh-outbox":                              {mode: modeVersionJSON},
	"gh-pr-review":                           {mode: modeVersionJSON},
	"gh-slack":                               {mode: modeVersionJSON},
	"gh-storage":                             {mode: modeVersionJSON},
	"gh-throttle":                            {mode: modeVersionJSON},
	"healthsnap":                             {mode: modeVersionJSON},
	"install-check":                          {mode: modeVersionJSON},
	"memstore":                               {mode: modeVersionJSON},
	"migration-immutability":                 {mode: modeVersionFlag},
	"org-lint":                               {mode: modeVersionFlag},
	"org-lint-go":                            {mode: modeVersionFlag},
	"org-lint-parity":                        {mode: modeVersionFlag},
	"orgctl":                                 {mode: modeVersionJSON},
	"pkgvar":                                 {mode: modeVersionFlag},
	"prsync":                                 {mode: modeVersionJSON},
	"quotapace":                              {mode: modeVersionJSON},
	"rename-jira-keys":                       {mode: modeVersionFlag},
	"repo-sync":                              {mode: modeVersionJSON},
	"review-and-push-loop":                   {mode: modeVersionJSON},
	"secretwatch":                            {mode: modeVersionJSON},
	"slackwatch":                             {mode: modeVersionJSON},
	"unnecessary-interface-assertion-linter": {mode: modeVersionFlag},
}

// versionSkipReasons documents any devtools/*/cmd/* binary intentionally
// excluded from version reporting checks, with a required non-empty reason.
var versionSkipReasons = map[string]string{}

func discoverDevtoolsCommands(t *testing.T) []string {
	t.Helper()
	devtoolsDir := findDevtoolsDir(t)
	matches, err := filepath.Glob(filepath.Join(devtoolsDir, "*", "cmd", "*", "main.go"))
	if err != nil {
		t.Fatalf("glob devtools commands: %v", err)
	}
	if len(matches) == 0 {
		t.Fatalf("no devtools/*/cmd/*/main.go files found under %s", devtoolsDir)
	}
	commands := make([]string, 0, len(matches))
	for _, match := range matches {
		commands = append(commands, filepath.Base(filepath.Dir(match)))
	}
	sort.Strings(commands)
	return commands
}

func findDevtoolsDir(t *testing.T) string {
	t.Helper()
	return filepath.Join(findWorkspaceRoot(t), "devtools")
}

func findWorkspaceRoot(t *testing.T) string {
	t.Helper()
	for _, key := range []string{"RUNFILES_DIR", "TEST_SRCDIR"} {
		if root := os.Getenv(key); root != "" {
			ws := os.Getenv("TEST_WORKSPACE")
			if ws == "" {
				ws = "_main"
			}
			candidate := filepath.Join(root, ws)
			if info, err := os.Stat(filepath.Join(candidate, "devtools")); err == nil && info.IsDir() {
				return candidate
			}
		}
	}
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	return filepath.Dir(wd)
}

func checkVersionCoverage(commands []string, specs map[string]versionSpec, skipReasons map[string]string) []string {
	var problems []string
	cmdSet := make(map[string]bool, len(commands))
	sortedCommands := append([]string(nil), commands...)
	sort.Strings(sortedCommands)

	for _, cmd := range sortedCommands {
		cmdSet[cmd] = true
		_, hasSpec := specs[cmd]
		reason, skipped := skipReasons[cmd]
		if hasSpec && skipped {
			problems = append(problems, fmt.Sprintf("devtools command %q appears in both commandVersionSpecs and versionSkipReasons", cmd))
			continue
		}
		if hasSpec {
			continue
		}
		if !skipped {
			problems = append(problems, fmt.Sprintf("devtools command %q is missing from commandVersionSpecs and versionSkipReasons", cmd))
			continue
		}
		if strings.TrimSpace(reason) == "" {
			problems = append(problems, fmt.Sprintf("devtools command %q has an empty skip reason in versionSkipReasons", cmd))
		}
	}

	specKeys := make([]string, 0, len(specs))
	for cmd := range specs {
		specKeys = append(specKeys, cmd)
	}
	sort.Strings(specKeys)
	for _, cmd := range specKeys {
		if !cmdSet[cmd] {
			problems = append(problems, fmt.Sprintf("stale commandVersionSpecs entry %q: no matching devtools/*/cmd/* command exists", cmd))
		}
	}

	skipKeys := make([]string, 0, len(skipReasons))
	for cmd := range skipReasons {
		skipKeys = append(skipKeys, cmd)
	}
	sort.Strings(skipKeys)
	for _, cmd := range skipKeys {
		if !cmdSet[cmd] {
			problems = append(problems, fmt.Sprintf("stale versionSkipReasons entry %q: no matching devtools/*/cmd/* command exists", cmd))
		}
	}

	return problems
}

func verifyBinaryVersion(ctx context.Context, name, binPath, sandboxRoot string, spec versionSpec) error {
	args := []string{"--version"}
	if spec.mode == modeVersionJSON {
		args = []string{"version", "--json"}
	}

	runCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()

	// #nosec G204 -- binPath is resolved from Bazel runfiles or controlled test fixtures.
	cmd := exec.CommandContext(runCtx, binPath, args...)
	cmd.Dir = sandboxRoot
	cmd.Env = []string{
		"HOME=" + sandboxRoot,
		"XDG_CONFIG_HOME=" + filepath.Join(sandboxRoot, "config"),
		"XDG_STATE_HOME=" + filepath.Join(sandboxRoot, "state"),
		"XDG_DATA_HOME=" + filepath.Join(sandboxRoot, "data"),
		"XDG_CACHE_HOME=" + filepath.Join(sandboxRoot, "cache"),
		"XDG_RUNTIME_DIR=" + filepath.Join(sandboxRoot, "run"),
		"PATH=" + filepath.Join(sandboxRoot, "emptybin"),
	}

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("%s %s failed: %w (stderr=%q)", name, strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}

	out := strings.TrimSpace(stdout.String())
	if out == "" {
		return fmt.Errorf("%s %s printed empty output", name, strings.Join(args, " "))
	}

	switch spec.mode {
	case modeVersionJSON:
		var payload struct {
			Tool    string `json:"tool"`
			Version string `json:"version"`
		}
		if err := json.Unmarshal([]byte(out), &payload); err != nil {
			return fmt.Errorf("%s version --json invalid JSON %q: %w", name, out, err)
		}
		if payload.Tool != name {
			return fmt.Errorf("%s version --json tool = %q, want %q", name, payload.Tool, name)
		}
		if strings.TrimSpace(payload.Version) == "" {
			return fmt.Errorf("%s version --json returned empty version in %q", name, out)
		}
	case modeVersionFlag:
		if !strings.HasPrefix(out, name+" ") {
			return fmt.Errorf("%s --version output %q does not start with %q", name, out, name+" ")
		}
		rest := strings.TrimSpace(strings.TrimPrefix(out, name))
		if rest == "" || !strings.Contains(rest, "(") || !strings.Contains(rest, ")") {
			return fmt.Errorf("%s --version output %q missing version and VCS revision", name, out)
		}
	default:
		return fmt.Errorf("unknown versionMode %d for %s", spec.mode, name)
	}
	return nil
}

func parseBinaryPathsEnv(wsRoot, raw string) (map[string]string, error) {
	paths := make(map[string]string)
	for _, entry := range strings.Fields(raw) {
		name, rel, ok := strings.Cut(entry, "=")
		if !ok || name == "" || rel == "" {
			return nil, fmt.Errorf("malformed DEVTOOLS_VERSION_BINS entry %q", entry)
		}
		if !filepath.IsAbs(rel) {
			rel = filepath.Join(wsRoot, rel)
		}
		paths[name] = rel
	}
	return paths, nil
}

func TestVersionCoverageRegistry(t *testing.T) {
	commands := discoverDevtoolsCommands(t)
	for _, problem := range checkVersionCoverage(commands, commandVersionSpecs, versionSkipReasons) {
		t.Error(problem)
	}
}

func TestAllDevtoolsBinariesReportVersion(t *testing.T) {
	raw := strings.TrimSpace(os.Getenv("DEVTOOLS_VERSION_BINS"))
	if raw == "" {
		if os.Getenv("TEST_SRCDIR") != "" {
			t.Fatal("DEVTOOLS_VERSION_BINS is empty inside Bazel test environment")
		}
		t.Skip("pre-built binaries are provided by Bazel data dependencies; run bazel test //devtools:version_coverage_test")
	}

	bins, err := parseBinaryPathsEnv(findWorkspaceRoot(t), raw)
	if err != nil {
		t.Fatal(err)
	}

	commands := discoverDevtoolsCommands(t)
	if problems := checkVersionCoverage(commands, commandVersionSpecs, versionSkipReasons); len(problems) > 0 {
		t.Fatalf("coverage problems:\n%s", strings.Join(problems, "\n"))
	}

	for _, cmd := range commands {
		if _, skipped := versionSkipReasons[cmd]; skipped {
			continue
		}
		spec := commandVersionSpecs[cmd]
		binPath, ok := bins[cmd]
		if !ok {
			t.Errorf("no built binary path for %q in DEVTOOLS_VERSION_BINS", cmd)
			continue
		}
		t.Run(cmd, func(t *testing.T) {
			t.Parallel()
			sandbox := t.TempDir()
			for _, sub := range []string{"config", "state", "data", "cache", "run", "emptybin"} {
				if err := os.MkdirAll(filepath.Join(sandbox, sub), 0o750); err != nil {
					t.Fatal(err)
				}
			}
			if err := verifyBinaryVersion(context.Background(), cmd, binPath, sandbox, spec); err != nil {
				t.Fatal(err)
			}
		})
	}
}

func TestCheckVersionCoverage(t *testing.T) {
	tests := []struct {
		name        string
		commands    []string
		specs       map[string]versionSpec
		skipReasons map[string]string
		want        []string
	}{
		{
			name:        "all covered or skipped with reason",
			commands:    []string{"alpha", "beta"},
			specs:       map[string]versionSpec{"alpha": {mode: modeVersionJSON}},
			skipReasons: map[string]string{"beta": "repo-internal helper"},
			want:        nil,
		},
		{
			name:        "missing binary fails",
			commands:    []string{"alpha", "untracked"},
			specs:       map[string]versionSpec{"alpha": {mode: modeVersionJSON}},
			skipReasons: map[string]string{},
			want:        []string{`devtools command "untracked" is missing from commandVersionSpecs and versionSkipReasons`},
		},
		{
			name:        "empty skip reason fails",
			commands:    []string{"beta"},
			specs:       map[string]versionSpec{},
			skipReasons: map[string]string{"beta": "   "},
			want:        []string{`devtools command "beta" has an empty skip reason in versionSkipReasons`},
		},
		{
			name:        "duplicate spec and skip fails",
			commands:    []string{"alpha"},
			specs:       map[string]versionSpec{"alpha": {mode: modeVersionFlag}},
			skipReasons: map[string]string{"alpha": "duplicated"},
			want:        []string{`devtools command "alpha" appears in both commandVersionSpecs and versionSkipReasons`},
		},
		{
			name:        "stale entries fail",
			commands:    []string{"alpha"},
			specs:       map[string]versionSpec{"alpha": {mode: modeVersionFlag}, "gone1": {mode: modeVersionFlag}},
			skipReasons: map[string]string{"gone2": "old tool"},
			want: []string{
				`stale commandVersionSpecs entry "gone1": no matching devtools/*/cmd/* command exists`,
				`stale versionSkipReasons entry "gone2": no matching devtools/*/cmd/* command exists`,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := checkVersionCoverage(tt.commands, tt.specs, tt.skipReasons)
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("checkVersionCoverage() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestVerifyBinaryVersionFailures(t *testing.T) {
	writeFakeBin := func(t *testing.T, script string) string {
		t.Helper()
		path := filepath.Join(t.TempDir(), "fakebin")
		if err := os.WriteFile(path, []byte("#!/bin/sh\n"+script), 0o755); err != nil { //nolint:gosec // test executable
			t.Fatal(err)
		}
		return path
	}

	sandbox := t.TempDir()
	for _, sub := range []string{"config", "state", "data", "cache", "run", "emptybin"} {
		if err := os.MkdirAll(filepath.Join(sandbox, sub), 0o750); err != nil {
			t.Fatal(err)
		}
	}

	t.Run("fails on non-zero exit", func(t *testing.T) {
		bin := writeFakeBin(t, "echo 'unexpected argument' >&2\nexit 2\n")
		err := verifyBinaryVersion(context.Background(), "fake", bin, sandbox, versionSpec{mode: modeVersionFlag})
		if err == nil {
			t.Fatal("expected error for non-zero exit")
		}
	})

	t.Run("fails on empty output", func(t *testing.T) {
		bin := writeFakeBin(t, "exit 0\n")
		err := verifyBinaryVersion(context.Background(), "fake", bin, sandbox, versionSpec{mode: modeVersionFlag})
		if err == nil || !strings.Contains(err.Error(), "empty output") {
			t.Fatalf("err = %v, want empty output error", err)
		}
	})

	t.Run("fails on unusable flag output", func(t *testing.T) {
		bin := writeFakeBin(t, "echo 'something unrelated'\n")
		err := verifyBinaryVersion(context.Background(), "fake", bin, sandbox, versionSpec{mode: modeVersionFlag})
		if err == nil {
			t.Fatal("expected error for unusable --version output")
		}
	})

	t.Run("fails on empty JSON version", func(t *testing.T) {
		bin := writeFakeBin(t, "printf '{\"tool\":\"fake\",\"version\":\"\"}\\n'\n")
		err := verifyBinaryVersion(context.Background(), "fake", bin, sandbox, versionSpec{mode: modeVersionJSON})
		if err == nil || !strings.Contains(err.Error(), "empty version") {
			t.Fatalf("err = %v, want empty version error", err)
		}
	})

	t.Run("fails on invalid JSON", func(t *testing.T) {
		bin := writeFakeBin(t, "echo 'not json'\n")
		err := verifyBinaryVersion(context.Background(), "fake", bin, sandbox, versionSpec{mode: modeVersionJSON})
		if err == nil {
			t.Fatal("expected error for invalid JSON")
		}
	})

	t.Run("passes on valid flag output", func(t *testing.T) {
		bin := writeFakeBin(t, "echo 'fake v0.1.0 (abc1234)'\n")
		if err := verifyBinaryVersion(context.Background(), "fake", bin, sandbox, versionSpec{mode: modeVersionFlag}); err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})

	t.Run("passes on valid JSON output", func(t *testing.T) {
		bin := writeFakeBin(t, "printf '{\"tool\":\"fake\",\"version\":\"v0.1.0\"}\\n'\n")
		if err := verifyBinaryVersion(context.Background(), "fake", bin, sandbox, versionSpec{mode: modeVersionJSON}); err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})
}
