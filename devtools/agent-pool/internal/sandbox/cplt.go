package sandbox

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/pelletier/go-toml/v2"
)

// CpltRunner prepares and runs cplt-sandboxed agent processes.
type CpltRunner struct {
	// CpltBinary is the cplt executable name or path. Defaults to "cplt".
	CpltBinary string
}

type cpltConfig struct {
	Sandbox  cpltSandboxConfig  `toml:"sandbox"`
	Proxy    cpltProxyConfig    `toml:"proxy"`
	GHGuard  cpltGHGuardConfig  `toml:"gh_guard"`
	GitGuard cpltGitGuardConfig `toml:"git_guard"`
}

type cpltSandboxConfig struct {
	Agent                 string `toml:"agent"`
	Preset                string `toml:"preset"`
	Yes                   bool   `toml:"yes"`
	Quiet                 bool   `toml:"quiet"`
	Audit                 bool   `toml:"audit"`
	Brief                 bool   `toml:"brief"`
	AllowEnvFiles         bool   `toml:"allow_env_files"`
	AllowLocalhostAny     bool   `toml:"allow_localhost_any"`
	AllowLifecycleScripts bool   `toml:"allow_lifecycle_scripts"`
	AllowGPGSigning       bool   `toml:"allow_gpg_signing"`
	AllowTmpExec          bool   `toml:"allow_tmp_exec"`
	AllowDocker           bool   `toml:"allow_docker"`
	AllowBrowser          bool   `toml:"allow_browser"`
	DenyClipboard         bool   `toml:"deny_clipboard"`
	UseBubblewrap         *bool  `toml:"use_bubblewrap,omitempty"`
	RefuseUnenforced      *bool  `toml:"refuse_unenforced,omitempty"`
	KeychainSubstitute    *bool  `toml:"keychain_substitute,omitempty"`
}

type cpltProxyConfig struct {
	Enabled           bool  `toml:"enabled"`
	Forced            bool  `toml:"forced"`
	DefaultAllowlist  bool  `toml:"default_allowlist"`
	InjectGitHubToken bool  `toml:"inject_github_token"`
	Netns             *bool `toml:"netns,omitempty"`
}

type cpltGHGuardConfig struct {
	Enabled        bool `toml:"enabled"`
	ScopeCheck     bool `toml:"scope_check"`
	BlockAuthToken bool `toml:"block_auth_token"`
	AllowAPIWrite  bool `toml:"allow_api_write"`
	AllowPRMerge   bool `toml:"allow_pr_merge"`
}

type cpltGitGuardConfig struct {
	Enabled          bool `toml:"enabled"`
	PreventPush      bool `toml:"prevent_push"`
	PreventForcePush bool `toml:"prevent_force_push"`
}

// Prepare writes an isolated cplt config.toml and returns a direct-exec
// Invocation with sanitized environment and a cleanup function.
func (r *CpltRunner) Prepare(spec Spec) (Invocation, func() error, error) {
	norm, err := normalizeSpec(spec)
	if err != nil {
		return Invocation{}, nil, err
	}

	if err := writeCpltConfig(norm); err != nil {
		return Invocation{}, nil, err
	}

	domainsFile, err := writeAllowedDomainsFile(norm.IsolatedHome, norm.ExtraAllowedHosts)
	if err != nil {
		return Invocation{}, nil, err
	}

	cleanup := func() error {
		if domainsFile == "" {
			return nil
		}
		if err := os.Remove(domainsFile); err != nil && !errors.Is(err, os.ErrNotExist) {
			return fmt.Errorf("remove allowed-domains file: %w", err)
		}
		return nil
	}

	binary := r.CpltBinary
	if binary == "" {
		binary = "cplt"
	}

	inv := Invocation{
		Binary: binary,
		Args:   buildCpltArgs(norm, domainsFile),
		Env:    buildSanitizedEnv(norm),
		Dir:    norm.ProjectDir,
	}
	return inv, cleanup, nil
}

// Start launches the prepared Invocation in its own process group.
func (r *CpltRunner) Start(ctx context.Context, inv Invocation) (*Process, error) {
	return startProcess(ctx, inv)
}

func normalizeSpec(spec Spec) (Spec, error) {
	if spec.Agent == "" {
		return Spec{}, errors.New("sandbox spec agent is required")
	}
	if spec.ProjectDir == "" {
		return Spec{}, errors.New("sandbox spec project dir is required")
	}
	if spec.IsolatedHome == "" {
		return Spec{}, errors.New("sandbox spec isolated home is required")
	}
	if spec.TargetOS == "" {
		spec.TargetOS = runtime.GOOS
	}
	if spec.ClaudeConfigDir == "" {
		spec.ClaudeConfigDir = filepath.Join(spec.IsolatedHome, ".claude")
	}
	if spec.GitHubAuth == "" {
		spec.GitHubAuth = GitHubAuthNone
	}
	if spec.GitHubAuth != GitHubAuthCpltGH && spec.GitHubAuth != GitHubAuthNone {
		return Spec{}, fmt.Errorf("unsupported github auth mode %q", spec.GitHubAuth)
	}
	return spec, nil
}

func buildCpltConfig(spec Spec) cpltConfig {
	trueVal := true
	cfg := cpltConfig{
		Sandbox: cpltSandboxConfig{
			Agent:                 spec.Agent,
			Preset:                "strict",
			Yes:                   true,
			Quiet:                 true,
			Audit:                 false,
			Brief:                 false,
			AllowEnvFiles:         false,
			AllowLocalhostAny:     false,
			AllowLifecycleScripts: false,
			AllowGPGSigning:       false,
			AllowTmpExec:          false,
			AllowDocker:           false,
			AllowBrowser:          false,
			DenyClipboard:         true,
		},
		Proxy: cpltProxyConfig{
			Enabled:           true,
			Forced:            true,
			DefaultAllowlist:  true,
			InjectGitHubToken: spec.GitHubAuth == GitHubAuthCpltGH,
		},
		GHGuard: cpltGHGuardConfig{
			Enabled:        true,
			ScopeCheck:     true,
			BlockAuthToken: true,
			AllowAPIWrite:  false,
			AllowPRMerge:   false,
		},
		GitGuard: cpltGitGuardConfig{
			Enabled:          true,
			PreventPush:      true,
			PreventForcePush: true,
		},
	}

	switch spec.TargetOS {
	case "linux":
		cfg.Sandbox.UseBubblewrap = &trueVal
		cfg.Sandbox.RefuseUnenforced = &trueVal
		cfg.Proxy.Netns = &trueVal
	case "darwin":
		cfg.Sandbox.KeychainSubstitute = &trueVal
	}
	return cfg
}

func writeCpltConfig(spec Spec) error {
	cfgDir := filepath.Join(spec.IsolatedHome, ".config", "cplt")
	if err := os.MkdirAll(cfgDir, 0o700); err != nil {
		return fmt.Errorf("create cplt config dir: %w", err)
	}

	raw, err := toml.Marshal(buildCpltConfig(spec))
	if err != nil {
		return fmt.Errorf("marshal cplt config: %w", err)
	}

	cfgPath := filepath.Join(cfgDir, "config.toml")
	if err := os.WriteFile(cfgPath, raw, 0o600); err != nil {
		return fmt.Errorf("write cplt config: %w", err)
	}
	return nil
}

func writeAllowedDomainsFile(isolatedHome string, hosts []string) (string, error) {
	if len(hosts) == 0 {
		return "", nil
	}

	cacheDir := filepath.Join(isolatedHome, ".cache")
	if err := os.MkdirAll(cacheDir, 0o700); err != nil {
		return "", fmt.Errorf("create cplt cache dir: %w", err)
	}

	f, err := os.CreateTemp(cacheDir, "allowed-domains-*.txt")
	if err != nil {
		return "", fmt.Errorf("create allowed-domains temp file: %w", err)
	}
	path := f.Name()

	content := strings.Join(hosts, "\n") + "\n"
	if err := f.Chmod(0o600); err != nil {
		_ = f.Close()
		_ = os.Remove(path)
		return "", fmt.Errorf("chmod allowed-domains file: %w", err)
	}
	if _, err := f.WriteString(content); err != nil {
		_ = f.Close()
		_ = os.Remove(path)
		return "", fmt.Errorf("write allowed-domains file: %w", err)
	}
	if err := f.Close(); err != nil {
		_ = os.Remove(path)
		return "", fmt.Errorf("close allowed-domains file: %w", err)
	}
	return path, nil
}

func buildCpltArgs(spec Spec, domainsFile string) []string {
	args := []string{
		"--agent", spec.Agent,
		"--preset", "strict",
		"--project-dir", spec.ProjectDir,
		"--proxy-forced",
		"--default-allowlist",
	}

	if spec.GitHubAuth == GitHubAuthCpltGH {
		args = append(args, "--inject-github-token", "--deny-env", "GH_CONFIG_DIR")
	} else {
		args = append(args, "--no-inject-github-token")
	}

	if spec.TargetOS == "linux" {
		args = append(args, "--use-bubblewrap")
	}

	args = appendDenyPaths(args, spec.RealHome, spec.IsolatedHome, spec.TargetOS)

	if spec.ClaudeOAuthToken != "" {
		args = append(args, "--pass-env", "CLAUDE_CODE_OAUTH_TOKEN")
	}
	args = append(args, "--pass-env", "CLAUDE_CONFIG_DIR", "--yes", "--quiet", "--no-audit")

	if domainsFile != "" {
		args = append(args, "--allowed-domains", domainsFile)
	}

	args = append(args, "--")
	args = append(args, spec.AgentArgs...)
	return args
}

func appendDenyPaths(args []string, realHome, isolatedHome, targetOS string) []string {
	if realHome == "" || realHome == isolatedHome {
		return args
	}

	denyRel := []string{
		".claude",
		".claude.json",
		".ssh",
		".gitconfig",
		filepath.Join(".config", "gh"),
		".aws",
		".gnupg",
	}
	if targetOS == "darwin" {
		denyRel = append(denyRel, filepath.Join("Library", "Keychains"))
	}
	for _, rel := range denyRel {
		args = append(args, "--deny-path", filepath.Join(realHome, rel))
	}
	return args
}

func buildSanitizedEnv(spec Spec) []string {
	env := make([]string, 0, 12)
	for _, key := range []string{"PATH", "TERM", "LANG", "LC_ALL", "TZ"} {
		if val, ok := os.LookupEnv(key); ok {
			env = append(env, key+"="+val)
		}
	}

	env = append(env,
		"HOME="+spec.IsolatedHome,
		"XDG_CONFIG_HOME="+filepath.Join(spec.IsolatedHome, ".config"),
		"XDG_CACHE_HOME="+filepath.Join(spec.IsolatedHome, ".cache"),
		"XDG_DATA_HOME="+filepath.Join(spec.IsolatedHome, ".local", "share"),
		"CLAUDE_CONFIG_DIR="+spec.ClaudeConfigDir,
	)

	if spec.ClaudeOAuthToken != "" {
		env = append(env, "CLAUDE_CODE_OAUTH_TOKEN="+spec.ClaudeOAuthToken)
	}
	if spec.GitHubAuth == GitHubAuthCpltGH && spec.RealHome != "" {
		env = append(env, "GH_CONFIG_DIR="+filepath.Join(spec.RealHome, ".config", "gh"))
	}
	return env
}
