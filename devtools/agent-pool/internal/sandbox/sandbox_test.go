package sandbox

import (
	"bufio"
	"context"
	"errors"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"
)

func TestCpltPrepareLinux(t *testing.T) {
	t.Parallel()

	isolatedHome := t.TempDir()
	realHome := t.TempDir()
	projectDir := t.TempDir()
	claudeDir := filepath.Join(isolatedHome, ".claude")

	runner := &CpltRunner{}
	spec := Spec{
		Agent:             "claude",
		ProjectDir:        projectDir,
		IsolatedHome:      isolatedHome,
		RealHome:          realHome,
		ClaudeConfigDir:   claudeDir,
		GitHubAuth:        GitHubAuthCpltGH,
		ExtraAllowedHosts: []string{"api.example.com", "pkg.example.org"},
		AgentArgs:         []string{"-p", "fix the tests"},
		TargetOS:          "linux",
	}

	inv, cleanup, err := runner.Prepare(spec)
	if err != nil {
		t.Fatalf("Prepare() error = %v", err)
	}
	if cleanup == nil {
		t.Fatal("Prepare() returned nil cleanup function")
	}

	if inv.Binary != "cplt" {
		t.Errorf("Binary = %q, want %q", inv.Binary, "cplt")
	}
	if inv.Dir != projectDir {
		t.Errorf("Dir = %q, want %q", inv.Dir, projectDir)
	}

	configPath := filepath.Join(isolatedHome, ".config", "cplt", "config.toml")
	info, err := os.Stat(configPath)
	if err != nil {
		t.Fatalf("stat config.toml: %v", err)
	}
	if perm := info.Mode().Perm(); perm != 0o600 {
		t.Errorf("config.toml mode = %04o, want 0600", perm)
	}

	rawConfig, err := os.ReadFile(configPath)
	if err != nil {
		t.Fatalf("read config.toml: %v", err)
	}
	cfgText := string(rawConfig)

	for _, want := range []string{
		"use_bubblewrap = true",
		"refuse_unenforced = true",
		"netns = true",
		"forced = true",
		"allow_api_write = false",
		"prevent_push = true",
		"inject_github_token = true",
	} {
		if !strings.Contains(cfgText, want) {
			t.Errorf("config.toml missing %q;\ngot:\n%s", want, cfgText)
		}
	}
	if strings.Contains(cfgText, "keychain_substitute") {
		t.Errorf("linux config.toml should not contain keychain_substitute;\ngot:\n%s", cfgText)
	}

	for _, flag := range []string{
		"--use-bubblewrap",
		"--proxy-forced",
		"--default-allowlist",
		"--inject-github-token",
		"--yes",
		"--quiet",
		"--no-audit",
	} {
		if !slices.Contains(inv.Args, flag) {
			t.Errorf("Args missing %q; got %v", flag, inv.Args)
		}
	}

	assertArgPair(t, inv.Args, "--agent", "claude")
	assertArgPair(t, inv.Args, "--preset", "strict")
	assertArgPair(t, inv.Args, "--project-dir", projectDir)
	assertArgPair(t, inv.Args, "--deny-env", "GH_CONFIG_DIR")
	assertArgPair(t, inv.Args, "--pass-env", "CLAUDE_CONFIG_DIR")

	for _, rel := range []string{
		".claude",
		".claude.json",
		".ssh",
		".gitconfig",
		filepath.Join(".config", "gh"),
		".aws",
		".gnupg",
	} {
		assertArgPair(t, inv.Args, "--deny-path", filepath.Join(realHome, rel))
	}
	if hasArgPair(inv.Args, "--deny-path", filepath.Join(realHome, "Library", "Keychains")) {
		t.Errorf("linux Args should not deny Library/Keychains; got %v", inv.Args)
	}

	domainsFile, ok := findArgValue(inv.Args, "--allowed-domains")
	if !ok {
		t.Fatalf("Args missing --allowed-domains; got %v", inv.Args)
	}
	if !strings.HasPrefix(domainsFile, filepath.Join(isolatedHome, ".cache")+string(filepath.Separator)) {
		t.Errorf("allowed-domains file %q not under %q", domainsFile, filepath.Join(isolatedHome, ".cache"))
	}
	domainsContent, err := os.ReadFile(domainsFile)
	if err != nil {
		t.Fatalf("read allowed-domains file: %v", err)
	}
	if got := strings.TrimSpace(string(domainsContent)); got != "api.example.com\npkg.example.org" {
		t.Errorf("allowed-domains content = %q, want %q", got, "api.example.com\npkg.example.org")
	}

	sepIdx := slices.Index(inv.Args, "--")
	if sepIdx < 0 {
		t.Fatalf("Args missing '--' separator; got %v", inv.Args)
	}
	if got := inv.Args[sepIdx+1:]; !slices.Equal(got, spec.AgentArgs) {
		t.Errorf("agent args after '--' = %v, want %v", got, spec.AgentArgs)
	}

	if err := cleanup(); err != nil {
		t.Fatalf("cleanup() error = %v", err)
	}
	if _, err := os.Stat(domainsFile); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("allowed-domains file %q still exists after cleanup (err=%v)", domainsFile, err)
	}
}

func TestCpltPrepareDarwin(t *testing.T) {
	t.Parallel()

	isolatedHome := t.TempDir()
	realHome := t.TempDir()
	projectDir := t.TempDir()

	runner := &CpltRunner{CpltBinary: "/usr/local/bin/cplt"}
	syntheticVal := strings.Repeat("a", 16)
	spec := Spec{
		Agent:            "claude",
		ProjectDir:       projectDir,
		IsolatedHome:     isolatedHome,
		RealHome:         realHome,
		ClaudeConfigDir:  filepath.Join(isolatedHome, ".claude"),
		ClaudeOAuthToken: syntheticVal,
		GitHubAuth:       GitHubAuthNone,
		AgentArgs:        []string{"--print"},
		TargetOS:         "darwin",
	}

	inv, cleanup, err := runner.Prepare(spec)
	if err != nil {
		t.Fatalf("Prepare() error = %v", err)
	}
	defer func() {
		if err := cleanup(); err != nil {
			t.Errorf("cleanup() error = %v", err)
		}
	}()

	if inv.Binary != "/usr/local/bin/cplt" {
		t.Errorf("Binary = %q, want %q", inv.Binary, "/usr/local/bin/cplt")
	}

	rawConfig, err := os.ReadFile(filepath.Join(isolatedHome, ".config", "cplt", "config.toml"))
	if err != nil {
		t.Fatalf("read config.toml: %v", err)
	}
	cfgText := string(rawConfig)

	if !strings.Contains(cfgText, "keychain_substitute = true") {
		t.Errorf("darwin config.toml missing keychain_substitute = true;\ngot:\n%s", cfgText)
	}
	if !strings.Contains(cfgText, "inject_github_token = false") {
		t.Errorf("darwin config.toml missing inject_github_token = false;\ngot:\n%s", cfgText)
	}
	for _, forbidden := range []string{"use_bubblewrap", "refuse_unenforced", "netns"} {
		if strings.Contains(cfgText, forbidden) {
			t.Errorf("darwin config.toml should not contain %q;\ngot:\n%s", forbidden, cfgText)
		}
	}

	if slices.Contains(inv.Args, "--use-bubblewrap") {
		t.Errorf("darwin Args should not include --use-bubblewrap; got %v", inv.Args)
	}
	if !slices.Contains(inv.Args, "--no-inject-github-token") {
		t.Errorf("darwin Args missing --no-inject-github-token; got %v", inv.Args)
	}
	assertArgPair(t, inv.Args, "--deny-path", filepath.Join(realHome, "Library", "Keychains"))
	assertArgPair(t, inv.Args, "--pass-env", "CLAUDE_CODE_OAUTH_TOKEN")
	assertArgPair(t, inv.Args, "--pass-env", "CLAUDE_CONFIG_DIR")

	envMap := envSliceToMap(inv.Env)
	if got := envMap["CLAUDE_CODE_OAUTH_TOKEN"]; got != syntheticVal {
		t.Errorf("CLAUDE_CODE_OAUTH_TOKEN = %q, want %q", got, syntheticVal)
	}
}

func TestCpltPrepareSanitizesEnv(t *testing.T) {
	t.Setenv("AWS_SECRET_ACCESS_KEY", "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY")
	t.Setenv("SSH_AUTH_SOCK", "/tmp/ssh-agent.sock")
	t.Setenv("ANTHROPIC_API_KEY", "sk-ant-secret")

	isolatedHome := t.TempDir()
	realHome := t.TempDir()
	projectDir := t.TempDir()
	claudeDir := filepath.Join(isolatedHome, ".claude")

	runner := &CpltRunner{}
	inv, cleanup, err := runner.Prepare(Spec{
		Agent:           "claude",
		ProjectDir:      projectDir,
		IsolatedHome:    isolatedHome,
		RealHome:        realHome,
		ClaudeConfigDir: claudeDir,
		GitHubAuth:      GitHubAuthCpltGH,
		TargetOS:        "linux",
	})
	if err != nil {
		t.Fatalf("Prepare() error = %v", err)
	}
	defer func() {
		if err := cleanup(); err != nil {
			t.Errorf("cleanup() error = %v", err)
		}
	}()

	envMap := envSliceToMap(inv.Env)
	for _, secret := range []string{"AWS_SECRET_ACCESS_KEY", "SSH_AUTH_SOCK", "ANTHROPIC_API_KEY"} {
		if _, ok := envMap[secret]; ok {
			t.Errorf("sanitized env leaked %s", secret)
		}
	}

	wantEnv := map[string]string{
		"HOME":              isolatedHome,
		"XDG_CONFIG_HOME":   filepath.Join(isolatedHome, ".config"),
		"XDG_CACHE_HOME":    filepath.Join(isolatedHome, ".cache"),
		"XDG_DATA_HOME":     filepath.Join(isolatedHome, ".local", "share"),
		"CLAUDE_CONFIG_DIR": claudeDir,
		"GH_CONFIG_DIR":     filepath.Join(realHome, ".config", "gh"),
	}
	for k, want := range wantEnv {
		if got := envMap[k]; got != want {
			t.Errorf("env[%q] = %q, want %q", k, got, want)
		}
	}
}

func TestProcessGroupTerminate(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Unix process-group test")
	}

	tests := []struct {
		name         string
		script       string
		minWait      time.Duration
		maxTotalWait time.Duration
	}{
		{
			name:         "SIGTERM terminates child and grandchild",
			script:       `sleep 30 & echo $!; wait`,
			minWait:      0,
			maxTotalWait: 2 * time.Second,
		},
		{
			name:         "SIGKILL escalation when SIGTERM ignored",
			script:       `trap '' TERM; (trap '' TERM; exec sleep 30) & echo $!; wait`,
			minWait:      150 * time.Millisecond,
			maxTotalWait: 2 * time.Second,
		},
	}

	runner := &CpltRunner{}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			proc, err := runner.Start(context.Background(), Invocation{
				Binary: "/bin/sh",
				Args:   []string{"-c", tc.script},
			})
			if err != nil {
				t.Fatalf("Start() error = %v", err)
			}
			defer func() {
				_ = proc.Stdout.Close()
				_ = proc.Stderr.Close()
			}()

			line, err := bufio.NewReader(proc.Stdout).ReadString('\n')
			if err != nil {
				t.Fatalf("read grandchild pid: %v", err)
			}
			grandchildPID, err := strconv.Atoi(strings.TrimSpace(line))
			if err != nil {
				t.Fatalf("parse grandchild pid %q: %v", line, err)
			}

			start := time.Now()
			if err := proc.Terminate(200 * time.Millisecond); err != nil {
				t.Fatalf("Terminate() error = %v", err)
			}
			elapsed := time.Since(start)

			if elapsed < tc.minWait {
				t.Errorf("Terminate() returned after %v, want >= %v", elapsed, tc.minWait)
			}
			if elapsed > tc.maxTotalWait {
				t.Errorf("Terminate() took %v, want <= %v", elapsed, tc.maxTotalWait)
			}

			_ = proc.Wait()

			// Poll briefly for kernel process table cleanup after SIGKILL.
			deadline := time.Now().Add(500 * time.Millisecond)
			for processAlive(grandchildPID) && time.Now().Before(deadline) {
				time.Sleep(10 * time.Millisecond)
			}
			if processAlive(grandchildPID) {
				t.Errorf("grandchild pid %d is still alive after Terminate()", grandchildPID)
			}
		})
	}
}

func TestReadIsolatedOAuthToken(t *testing.T) {
	t.Parallel()

	t.Run("reads valid credentials fixture", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		fixture := `{"claudeAiOauth":{"accessToken":"sk-ant-oat01-fixture-token"}}`
		if err := os.WriteFile(filepath.Join(dir, ".credentials.json"), []byte(fixture), 0o600); err != nil {
			t.Fatalf("write fixture: %v", err)
		}
		got, err := ReadIsolatedOAuthToken(dir)
		if err != nil {
			t.Fatalf("ReadIsolatedOAuthToken() error = %v", err)
		}
		if got != "sk-ant-oat01-fixture-token" {
			t.Errorf("ReadIsolatedOAuthToken() = %q, want %q", got, "sk-ant-oat01-fixture-token")
		}
	})

	t.Run("missing file returns empty without error", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		got, err := ReadIsolatedOAuthToken(dir)
		if err != nil {
			t.Fatalf("ReadIsolatedOAuthToken() error = %v, want nil", err)
		}
		if got != "" {
			t.Errorf("ReadIsolatedOAuthToken() = %q, want empty", got)
		}
	})

	t.Run("invalid json returns error", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		if err := os.WriteFile(filepath.Join(dir, ".credentials.json"), []byte("{not-json"), 0o600); err != nil {
			t.Fatalf("write invalid fixture: %v", err)
		}
		if _, err := ReadIsolatedOAuthToken(dir); err == nil {
			t.Fatal("ReadIsolatedOAuthToken() error = nil, want non-nil")
		}
	})
}

func TestLaunchCleansUpOnStartError(t *testing.T) {
	t.Parallel()

	isolatedHome := t.TempDir()
	runner := &CpltRunner{CpltBinary: filepath.Join(t.TempDir(), "nonexistent-cplt")}
	_, _, err := Launch(context.Background(), runner, Spec{
		Agent:             "claude",
		ProjectDir:        t.TempDir(),
		IsolatedHome:      isolatedHome,
		ExtraAllowedHosts: []string{"api.example.com"},
		TargetOS:          "linux",
	})
	if err == nil {
		t.Fatal("Launch() error = nil, want start error")
	}
	entries, readErr := os.ReadDir(filepath.Join(isolatedHome, ".cache"))
	if readErr != nil {
		t.Fatalf("read cache dir: %v", readErr)
	}
	if len(entries) != 0 {
		t.Errorf("expected allowed-domains temp file to be cleaned up, found %d entries", len(entries))
	}
}

func assertArgPair(t *testing.T, args []string, flag, val string) {
	t.Helper()
	if !hasArgPair(args, flag, val) {
		t.Errorf("Args missing %q %q; got %v", flag, val, args)
	}
}

func hasArgPair(args []string, flag, val string) bool {
	for i := 0; i+1 < len(args); i++ {
		if args[i] == flag && args[i+1] == val {
			return true
		}
	}
	return false
}

func findArgValue(args []string, flag string) (string, bool) {
	for i := 0; i+1 < len(args); i++ {
		if args[i] == flag {
			return args[i+1], true
		}
	}
	return "", false
}

func envSliceToMap(env []string) map[string]string {
	m := make(map[string]string, len(env))
	for _, kv := range env {
		if k, v, ok := strings.Cut(kv, "="); ok {
			m[k] = v
		}
	}
	return m
}
