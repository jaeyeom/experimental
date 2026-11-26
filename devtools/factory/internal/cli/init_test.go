package cli

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func setupInitEnv(t *testing.T, wd, home, xdgConfig string, xdgSet bool) config.Env {
	t.Helper()
	env := config.OSEnv()
	env.Getwd = func() (string, error) { return wd, nil }
	env.HomeDir = func() (string, error) { return home, nil }
	env.LookupEnv = func(key string) (string, bool) {
		if key == "XDG_CONFIG_HOME" {
			return xdgConfig, xdgSet
		}
		return "", false
	}
	prev := processEnv
	processEnv = func() config.Env { return env }
	t.Cleanup(func() { processEnv = prev })
	return env
}

func useInitMock(t *testing.T) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	prev := newCommandExec
	newCommandExec = func() executor.Executor { return mock }
	t.Cleanup(func() { newCommandExec = prev })
	return mock
}

func TestInitGitHubRemotesAndLoad(t *testing.T) {
	tests := []struct {
		name       string
		remoteURL  string
		underHome  bool
		symRefOut  string
		symRefFail bool
		baseFlag   string
		wantRepo   string
		wantName   string
		wantBase   string
	}{
		{
			name:      "ssh scp remote under home",
			remoteURL: "git@github.com:jaeyeom/agentic-factory.git",
			underHome: true,
			symRefOut: "refs/remotes/origin/main\n",
			wantRepo:  "jaeyeom/agentic-factory",
			wantName:  "agentic-factory",
			wantBase:  "main",
		},
		{
			name:      "ssh url remote",
			remoteURL: "ssh://git@github.com/org/my-repo.git",
			underHome: true,
			symRefOut: "refs/remotes/origin/trunk\n",
			wantRepo:  "org/my-repo",
			wantName:  "my-repo",
			wantBase:  "trunk",
		},
		{
			name:       "https remote with .git and symbolic-ref fallback to main",
			remoteURL:  "https://github.com/octocat/hello-world.git",
			underHome:  false,
			symRefFail: true,
			wantRepo:   "octocat/hello-world",
			wantName:   "hello-world",
			wantBase:   "main",
		},
		{
			name:      "https remote without .git and explicit --base",
			remoteURL: "https://github.com/octocat/hello-world",
			underHome: true,
			baseFlag:  "develop",
			wantRepo:  "octocat/hello-world",
			wantName:  "hello-world",
			wantBase:  "develop",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			home := t.TempDir()
			var repoRoot string
			if tc.underHome {
				repoRoot = filepath.Join(home, "src", tc.wantName)
				if err := os.MkdirAll(repoRoot, 0o750); err != nil {
					t.Fatal(err)
				}
			} else {
				repoRoot = t.TempDir()
			}
			env := setupInitEnv(t, repoRoot, home, "", false)
			mock := useInitMock(t)

			mock.ExpectCommandWithArgs("git", "-C", repoRoot, "rev-parse", "--show-toplevel").
				WillSucceed(repoRoot+"\n", 0).Once().Build()
			if tc.baseFlag == "" {
				if tc.symRefFail {
					mock.ExpectCommandWithArgs("git", "-C", repoRoot, "symbolic-ref", "refs/remotes/origin/HEAD").
						WillFail("fatal: ref refs/remotes/origin/HEAD is not a symbolic ref\n", 128).Once().Build()
				} else {
					mock.ExpectCommandWithArgs("git", "-C", repoRoot, "symbolic-ref", "refs/remotes/origin/HEAD").
						WillSucceed(tc.symRefOut, 0).Once().Build()
				}
			}
			mock.ExpectCommandWithArgs("git", "-C", repoRoot, "remote", "get-url", "origin").
				WillSucceed(tc.remoteURL+"\n", 0).Once().Build()

			args := []string{"init"}
			if tc.baseFlag != "" {
				args = append(args, "--base", tc.baseFlag)
			}
			stdout, stderr, code := runCmd(t, args...)
			if code != ExitOK {
				t.Fatalf("exit = %d, stdout = %s, stderr = %s", code, stdout, stderr)
			}
			if stderr != "" {
				t.Fatalf("stderr = %q, want empty", stderr)
			}
			if err := mock.AssertExpectationsMet(); err != nil {
				t.Fatal(err)
			}

			cfgPath := filepath.Join(repoRoot, "factory.toml")
			if !strings.Contains(stdout, cfgPath) {
				t.Fatalf("stdout = %q, want path %q", stdout, cfgPath)
			}
			raw, err := os.ReadFile(cfgPath) //nolint:gosec // G304: path is the temp config this test just wrote.
			if err != nil {
				t.Fatalf("read %s: %v", cfgPath, err)
			}
			text := string(raw)
			for _, want := range []string{
				"# [defaults]",
				"# [worker]",
				"# [lease]",
				"# [output]",
				"# [claim]",
				`require_labels = ["agent-ready"]`,
				`start_prompt   = "/gh-issue-resolver {url}"`,
			} {
				if !strings.Contains(text, want) {
					t.Errorf("generated config missing %q:\n%s", want, text)
				}
			}
			if strings.Contains(text, "claim_label") {
				t.Errorf("generated GitHub config should omit legacy claim_label:\n%s", text)
			}
			if tc.underHome && !strings.Contains(text, `cwd            = "~/src/`+tc.wantName+`"`) {
				t.Errorf("expected ~/ shorthand in cwd:\n%s", text)
			}

			cfg, err := config.Load(cfgPath, env)
			if err != nil {
				t.Fatalf("config.Load(%s): %v\n%s", cfgPath, err, text)
			}
			if len(cfg.Sources) != 1 {
				t.Fatalf("sources = %d, want 1", len(cfg.Sources))
			}
			src := cfg.Sources[0]
			if src.Kind != "github" || src.Name != tc.wantName || src.Repo != tc.wantRepo || src.Base != tc.wantBase {
				t.Fatalf("source = %+v, want kind=github name=%s repo=%s base=%s", src, tc.wantName, tc.wantRepo, tc.wantBase)
			}
			if src.Cwd != repoRoot {
				t.Fatalf("expanded cwd = %q, want %q", src.Cwd, repoRoot)
			}
		})
	}
}

func TestInitExistingFileRefusesWithoutForceAndOverwritesWithForce(t *testing.T) {
	home := t.TempDir()
	repoRoot := filepath.Join(home, "repo")
	if err := os.MkdirAll(repoRoot, 0o750); err != nil {
		t.Fatal(err)
	}
	cfgPath := filepath.Join(repoRoot, "factory.toml")
	if err := os.WriteFile(cfgPath, []byte("# existing\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	env := setupInitEnv(t, repoRoot, home, "", false)

	mock := useInitMock(t)
	mock.ExpectCommandWithArgs("git", "-C", repoRoot, "rev-parse", "--show-toplevel").
		WillSucceed(repoRoot+"\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", repoRoot, "symbolic-ref", "refs/remotes/origin/HEAD").
		WillSucceed("refs/remotes/origin/main\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", repoRoot, "remote", "get-url", "origin").
		WillSucceed("git@github.com:org/repo.git\n", 0).Once().Build()

	_, stderr, code := runCmd(t, "init")
	if code != ExitError {
		t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitError, stderr)
	}
	if !strings.Contains(stderr, "already exists") || !strings.Contains(stderr, "--force") {
		t.Fatalf("stderr = %q, want already exists and --force hint", stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	got, err := os.ReadFile(cfgPath) //nolint:gosec // G304: path is the temp config this test just wrote.
	if err != nil {
		t.Fatal(err)
	}
	if string(got) != "# existing\n" {
		t.Fatalf("file was modified without --force: %q", string(got))
	}

	mock2 := useInitMock(t)
	mock2.ExpectCommandWithArgs("git", "-C", repoRoot, "rev-parse", "--show-toplevel").
		WillSucceed(repoRoot+"\n", 0).Once().Build()
	mock2.ExpectCommandWithArgs("git", "-C", repoRoot, "symbolic-ref", "refs/remotes/origin/HEAD").
		WillSucceed("refs/remotes/origin/main\n", 0).Once().Build()
	mock2.ExpectCommandWithArgs("git", "-C", repoRoot, "remote", "get-url", "origin").
		WillSucceed("git@github.com:org/repo.git\n", 0).Once().Build()

	stdout, stderr, code := runCmd(t, "init", "--force")
	if code != ExitOK {
		t.Fatalf("exit = %d, stdout = %s, stderr = %s", code, stdout, stderr)
	}
	if err := mock2.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	if _, err := config.Load(cfgPath, env); err != nil {
		t.Fatalf("config.Load after --force: %v", err)
	}
}

func TestInitGlobalCreateAppendAndDuplicateRefusal(t *testing.T) {
	home := t.TempDir()
	xdg := t.TempDir()
	repoA := filepath.Join(home, "src", "alpha")
	repoB := filepath.Join(home, "src", "beta")
	repoSub := filepath.Join(repoA, "subrepo")
	for _, d := range []string{repoA, repoB, repoSub} {
		if err := os.MkdirAll(d, 0o750); err != nil {
			t.Fatal(err)
		}
	}
	globalPath := filepath.Join(xdg, "factory", "factory.toml")
	env := setupInitEnv(t, repoA, home, xdg, true)

	// 1. Create global config when it does not exist.
	mock := useInitMock(t)
	mock.ExpectCommandWithArgs("git", "-C", repoA, "rev-parse", "--show-toplevel").
		WillSucceed(repoA+"\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", repoA, "symbolic-ref", "refs/remotes/origin/HEAD").
		WillSucceed("refs/remotes/origin/main\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", repoA, "remote", "get-url", "origin").
		WillSucceed("git@github.com:org/alpha.git\n", 0).Once().Build()

	stdout, stderr, code := runCmd(t, "init", "--global")
	if code != ExitOK {
		t.Fatalf("create global exit = %d, stdout = %s, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	cfg, err := config.Load(globalPath, env)
	if err != nil {
		t.Fatalf("config.Load(global create): %v", err)
	}
	if len(cfg.Sources) != 1 || cfg.Sources[0].Name != "alpha" {
		t.Fatalf("sources = %+v", cfg.Sources)
	}

	// 2. Append a second repo to existing global config.
	setupInitEnv(t, repoB, home, xdg, true)
	mock2 := useInitMock(t)
	mock2.ExpectCommandWithArgs("git", "-C", repoB, "rev-parse", "--show-toplevel").
		WillSucceed(repoB+"\n", 0).Once().Build()
	mock2.ExpectCommandWithArgs("git", "-C", repoB, "symbolic-ref", "refs/remotes/origin/HEAD").
		WillSucceed("refs/remotes/origin/main\n", 0).Once().Build()
	mock2.ExpectCommandWithArgs("git", "-C", repoB, "remote", "get-url", "origin").
		WillSucceed("https://github.com/org/beta.git\n", 0).Once().Build()

	stdout, stderr, code = runCmd(t, "init", "--global")
	if code != ExitOK {
		t.Fatalf("append global exit = %d, stdout = %s, stderr = %s", code, stdout, stderr)
	}
	if !strings.Contains(stdout, "appended github:beta") {
		t.Fatalf("stdout = %q, want appended github:beta", stdout)
	}
	if err := mock2.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	cfg, err = config.Load(globalPath, env)
	if err != nil {
		t.Fatalf("config.Load(global append): %v", err)
	}
	if len(cfg.Sources) != 2 || cfg.Sources[1].Name != "beta" {
		t.Fatalf("sources = %+v", cfg.Sources)
	}

	// 3. Refuse duplicate source name.
	repoDupName := filepath.Join(home, "src", "other-dir")
	if err := os.MkdirAll(repoDupName, 0o750); err != nil {
		t.Fatal(err)
	}
	setupInitEnv(t, repoDupName, home, xdg, true)
	mock3 := useInitMock(t)
	mock3.ExpectCommandWithArgs("git", "-C", repoDupName, "rev-parse", "--show-toplevel").
		WillSucceed(repoDupName+"\n", 0).Once().Build()
	mock3.ExpectCommandWithArgs("git", "-C", repoDupName, "symbolic-ref", "refs/remotes/origin/HEAD").
		WillSucceed("refs/remotes/origin/main\n", 0).Once().Build()
	mock3.ExpectCommandWithArgs("git", "-C", repoDupName, "remote", "get-url", "origin").
		WillSucceed("git@github.com:other/alpha.git\n", 0).Once().Build()

	_, stderr, code = runCmd(t, "init", "--global")
	if code != ExitError {
		t.Fatalf("dup name exit = %d, want %d, stderr = %s", code, ExitError, stderr)
	}
	if !strings.Contains(stderr, `already has a source named "alpha"`) {
		t.Fatalf("stderr = %q, want duplicate name error", stderr)
	}
	if err := mock3.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}

	// 4. Refuse repo covered by an existing source cwd.
	setupInitEnv(t, repoSub, home, xdg, true)
	mock4 := useInitMock(t)
	mock4.ExpectCommandWithArgs("git", "-C", repoSub, "rev-parse", "--show-toplevel").
		WillSucceed(repoSub+"\n", 0).Once().Build()
	mock4.ExpectCommandWithArgs("git", "-C", repoSub, "symbolic-ref", "refs/remotes/origin/HEAD").
		WillSucceed("refs/remotes/origin/main\n", 0).Once().Build()
	mock4.ExpectCommandWithArgs("git", "-C", repoSub, "remote", "get-url", "origin").
		WillSucceed("git@github.com:org/subrepo.git\n", 0).Once().Build()

	_, stderr, code = runCmd(t, "init", "--global")
	if code != ExitError {
		t.Fatalf("covered cwd exit = %d, want %d, stderr = %s", code, ExitError, stderr)
	}
	if !strings.Contains(stderr, "already has source github:alpha covering") {
		t.Fatalf("stderr = %q, want covering source error", stderr)
	}
	if err := mock4.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestInitDryRunWritesNothing(t *testing.T) {
	home := t.TempDir()
	repoRoot := filepath.Join(home, "src", "demo")
	if err := os.MkdirAll(repoRoot, 0o750); err != nil {
		t.Fatal(err)
	}
	setupInitEnv(t, repoRoot, home, "", false)
	mock := useInitMock(t)
	mock.ExpectCommandWithArgs("git", "-C", repoRoot, "rev-parse", "--show-toplevel").
		WillSucceed(repoRoot+"\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", repoRoot, "symbolic-ref", "refs/remotes/origin/HEAD").
		WillSucceed("refs/remotes/origin/main\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", repoRoot, "remote", "get-url", "origin").
		WillSucceed("git@github.com:org/demo.git\n", 0).Once().Build()

	stdout, stderr, code := runCmd(t, "init", "--dry-run")
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	cfgPath := filepath.Join(repoRoot, "factory.toml")
	if !strings.Contains(stdout, cfgPath) || !strings.Contains(stdout, `repo           = "org/demo"`) {
		t.Fatalf("dry-run stdout missing path or TOML:\n%s", stdout)
	}
	if _, err := os.Stat(cfgPath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("dry-run created %s", cfgPath)
	}
}

func TestInitKindJiraScaffoldsPlaceholders(t *testing.T) {
	home := t.TempDir()
	repoRoot := filepath.Join(home, "work", "monorepo")
	if err := os.MkdirAll(repoRoot, 0o750); err != nil {
		t.Fatal(err)
	}
	setupInitEnv(t, home, home, "", false)
	mock := useInitMock(t)
	mock.ExpectCommandWithArgs("git", "-C", repoRoot, "rev-parse", "--show-toplevel").
		WillSucceed(repoRoot+"\n", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", repoRoot, "symbolic-ref", "refs/remotes/origin/HEAD").
		WillSucceed("refs/remotes/origin/main\n", 0).Once().Build()

	stdout, stderr, code := runCmd(t, "init", "--cwd", repoRoot, "--kind", "jira")
	if code != ExitOK {
		t.Fatalf("exit = %d, stdout = %s, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	raw, err := os.ReadFile(filepath.Join(repoRoot, "factory.toml")) //nolint:gosec // G304: path is the temp config this test just wrote.
	if err != nil {
		t.Fatal(err)
	}
	text := string(raw)
	for _, want := range []string{
		`name             = "monorepo"`,
		`kind             = "jira"`,
		`site             = "https://example.atlassian.net"`,
		`email            = "you@example.com"`,
		`token_file       = "~/.config/factory/jira-token"`,
		`jql              = "project = PRJ"`,
		`claim_status     = "In Progress"`,
		`cwd              = "~/work/monorepo"`,
		`base             = "main"`,
		`start_prompt     = "/implement-jira-ticket {id}"`,
	} {
		if !strings.Contains(text, want) {
			t.Errorf("jira config missing %q:\n%s", want, text)
		}
	}
}

func TestInitErrors(t *testing.T) {
	t.Run("outside git repository", func(t *testing.T) {
		dir := t.TempDir()
		setupInitEnv(t, dir, dir, "", false)
		mock := useInitMock(t)
		mock.ExpectCommandWithArgs("git", "-C", dir, "rev-parse", "--show-toplevel").
			WillFail("fatal: not a git repository (or any of the parent directories): .git\n", 128).Once().Build()

		_, stderr, code := runCmd(t, "init")
		if code != ExitError {
			t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitError, stderr)
		}
		if !strings.Contains(stderr, "not a git repository") {
			t.Fatalf("stderr = %q, want not a git repository", stderr)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("non-github remote without --kind jira", func(t *testing.T) {
		dir := t.TempDir()
		setupInitEnv(t, dir, dir, "", false)
		mock := useInitMock(t)
		mock.ExpectCommandWithArgs("git", "-C", dir, "rev-parse", "--show-toplevel").
			WillSucceed(dir+"\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("git", "-C", dir, "symbolic-ref", "refs/remotes/origin/HEAD").
			WillSucceed("refs/remotes/origin/main\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("git", "-C", dir, "remote", "get-url", "origin").
			WillSucceed("git@gitlab.com:acme/widget.git\n", 0).Once().Build()

		_, stderr, code := runCmd(t, "init")
		if code != ExitError {
			t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitError, stderr)
		}
		if !strings.Contains(stderr, "non-GitHub remote") || !strings.Contains(stderr, "--kind jira") {
			t.Fatalf("stderr = %q, want non-GitHub remote and --kind jira hint", stderr)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("unexpected --kind", func(t *testing.T) {
		_, stderr, code := runCmd(t, "init", "--kind", "gitlab")
		if code != ExitUsage {
			t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitUsage, stderr)
		}
		if !strings.Contains(stderr, "expected github or jira") {
			t.Fatalf("stderr = %q", stderr)
		}
	})

	t.Run("global with force", func(t *testing.T) {
		_, stderr, code := runCmd(t, "init", "--global", "--force")
		if code != ExitUsage {
			t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitUsage, stderr)
		}
	})

	t.Run("unexpected symbolic-ref output", func(t *testing.T) {
		dir := t.TempDir()
		setupInitEnv(t, dir, dir, "", false)
		mock := useInitMock(t)
		mock.ExpectCommandWithArgs("git", "-C", dir, "rev-parse", "--show-toplevel").
			WillSucceed(dir+"\n", 0).Once().Build()
		mock.ExpectCommandWithArgs("git", "-C", dir, "symbolic-ref", "refs/remotes/origin/HEAD").
			WillSucceed("refs/heads/main\n", 0).Once().Build()

		_, stderr, code := runCmd(t, "init")
		if code != ExitError {
			t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitError, stderr)
		}
		if !strings.Contains(stderr, "expected refs/remotes/origin/<branch>") {
			t.Fatalf("stderr = %q", stderr)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("positional arguments rejected", func(t *testing.T) {
		_, stderr, code := runCmd(t, "init", "extra")
		if code != ExitUsage {
			t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitUsage, stderr)
		}
	})
}
