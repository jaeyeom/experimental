package main

import (
	"context"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/sandbox"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/worker"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

const doctorHTTPTimeout = 5 * time.Second

func doctorCheckCplt(deps cliDeps) toolcli.DoctorCheck {
	return func(ctx context.Context, _ bool) toolcli.CheckResult {
		p, err := deps.lookPath("cplt")
		if err != nil {
			return toolcli.CheckResult{
				Name:   "cplt",
				Status: toolcli.StatusWarn,
				Detail: "cplt not found on PATH (required for consumer workers)",
				Hint:   "install cplt and ensure it is on PATH",
			}
		}
		out, err := deps.runCmd(ctx, p, []string{"doctor"}, nil)
		if err != nil {
			return toolcli.CheckResult{
				Name:   "cplt",
				Status: toolcli.StatusWarn,
				Detail: fmt.Sprintf("cplt doctor returned error: %s", firstLine(string(out))),
				Hint:   "run `cplt doctor` to diagnose sandbox prerequisites",
			}
		}
		return toolcli.CheckResult{
			Name:   "cplt",
			Status: toolcli.StatusOK,
			Detail: fmt.Sprintf("%s (cplt doctor ok)", p),
		}
	}
}

func doctorCheckPlatformSandbox(deps cliDeps) toolcli.DoctorCheck {
	return func(ctx context.Context, _ bool) toolcli.CheckResult {
		switch deps.goos {
		case "linux":
			p, err := deps.lookPath("bwrap")
			if err != nil {
				return toolcli.CheckResult{
					Name:   "platform-sandbox",
					Status: toolcli.StatusWarn,
					Detail: "bwrap (bubblewrap) not found on PATH",
					Hint:   "install bubblewrap for Linux namespace/netns isolation",
				}
			}
			return toolcli.CheckResult{
				Name:   "platform-sandbox",
				Status: toolcli.StatusOK,
				Detail: fmt.Sprintf("linux bubblewrap available (%s)", p),
			}
		case "darwin":
			return checkDarwinKeychainSubstitute(ctx, deps)
		default:
			return toolcli.CheckResult{
				Name:   "platform-sandbox",
				Status: toolcli.StatusWarn,
				Detail: fmt.Sprintf("unsupported platform %s for consumer sandbox", deps.goos),
			}
		}
	}
}

func checkDarwinKeychainSubstitute(ctx context.Context, deps cliDeps) toolcli.CheckResult {
	p, err := deps.lookPath("cplt")
	if err != nil {
		return toolcli.CheckResult{
			Name:   "platform-sandbox",
			Status: toolcli.StatusWarn,
			Detail: "macOS keychain_substitute cannot be verified without cplt",
			Hint:   "install cplt to enable macOS sandbox-exec + keychain_substitute",
		}
	}
	if _, err := deps.runCmd(ctx, p, []string{"--version"}, nil); err != nil {
		return toolcli.CheckResult{
			Name:   "platform-sandbox",
			Status: toolcli.StatusWarn,
			Detail: "macOS keychain_substitute readiness could not be verified",
			Hint:   "verify cplt version supports keychain_substitute on macOS",
		}
	}
	return toolcli.CheckResult{
		Name:   "platform-sandbox",
		Status: toolcli.StatusOK,
		Detail: "macOS keychain_substitute ready via cplt",
	}
}

func doctorCheckClaude(deps cliDeps) toolcli.DoctorCheck {
	return func(_ context.Context, _ bool) toolcli.CheckResult {
		p, err := deps.lookPath("claude")
		if err != nil {
			return toolcli.CheckResult{
				Name:   "claude",
				Status: toolcli.StatusWarn,
				Detail: "claude CLI not found on PATH (required for consumer workers)",
				Hint:   "install Claude Code CLI and ensure `claude` is on PATH",
			}
		}
		return toolcli.CheckResult{
			Name:   "claude",
			Status: toolcli.StatusOK,
			Detail: p,
		}
	}
}

func doctorCheckClaudeUsage(deps cliDeps) toolcli.DoctorCheck {
	return func(ctx context.Context, _ bool) toolcli.CheckResult {
		p, err := deps.lookPath("quotapace")
		if err != nil {
			return toolcli.CheckResult{
				Name:   "quotapace",
				Status: toolcli.StatusWarn,
				Detail: "quotapace not found on PATH",
				Hint:   "install quotapace via `make install`",
			}
		}

		cfg := loadWorkerConfigBestEffort()
		claudeDir := filepath.Join(cfg.Worker.IsolatedHome, ".claude")
		tok, err := sandbox.ReadIsolatedOAuthToken(claudeDir)
		if err != nil {
			return toolcli.CheckResult{
				Name:   "quotapace",
				Status: toolcli.StatusWarn,
				Detail: fmt.Sprintf("isolated auth unreadable in %s: %v", claudeDir, err),
				Hint:   "run `agent-pool init-auth` to initialize isolated credentials",
			}
		}
		if tok == "" {
			return toolcli.CheckResult{
				Name:   "quotapace",
				Status: toolcli.StatusWarn,
				Detail: fmt.Sprintf("%s present; isolated auth not initialized (%s)", p, claudeDir),
				Hint:   "run `agent-pool init-auth` before starting the worker daemon",
			}
		}

		env := append(os.Environ(), "CLAUDE_CONFIG_DIR="+claudeDir)
		if _, err := deps.runCmd(ctx, p, []string{"collect", "claude", "--json"}, env); err != nil {
			return toolcli.CheckResult{
				Name:   "quotapace",
				Status: toolcli.StatusWarn,
				Detail: fmt.Sprintf("isolated quotapace collect claude check failed for %s", claudeDir),
				Hint:   "re-run `agent-pool init-auth` to refresh isolated OAuth login",
			}
		}
		return toolcli.CheckResult{
			Name:   "quotapace",
			Status: toolcli.StatusOK,
			Detail: fmt.Sprintf("%s (isolated auth valid in %s)", p, claudeDir),
		}
	}
}

func doctorCheckGitHubAuth(deps cliDeps) toolcli.DoctorCheck {
	return func(ctx context.Context, _ bool) toolcli.CheckResult {
		cfg := loadWorkerConfigBestEffort()
		if cfg.Worker.GitHubAuth != sandbox.GitHubAuthCpltGH {
			return toolcli.CheckResult{
				Name:   "gh-auth",
				Status: toolcli.StatusOK,
				Detail: fmt.Sprintf("skipped (github_auth = %q)", cfg.Worker.GitHubAuth),
			}
		}
		p, err := deps.lookPath("gh")
		if err != nil {
			return toolcli.CheckResult{
				Name:   "gh-auth",
				Status: toolcli.StatusWarn,
				Detail: "gh CLI not found on PATH (needed when github_auth = \"cplt-gh\")",
				Hint:   "install GitHub CLI (`gh`) and run `gh auth login`, or set github_auth = \"none\"",
			}
		}
		if _, err := deps.runCmd(ctx, p, []string{"auth", "status"}, nil); err != nil {
			return toolcli.CheckResult{
				Name:   "gh-auth",
				Status: toolcli.StatusWarn,
				Detail: "gh is not authenticated",
				Hint:   "run `gh auth login` or set github_auth = \"none\" in worker.toml",
			}
		}
		return toolcli.CheckResult{
			Name:   "gh-auth",
			Status: toolcli.StatusOK,
			Detail: fmt.Sprintf("%s authenticated for cplt-gh", p),
		}
	}
}

func doctorCheckBrokerHealthz(deps cliDeps) toolcli.DoctorCheck {
	return func(ctx context.Context, offline bool) toolcli.CheckResult {
		if offline {
			return toolcli.CheckResult{
				Name:   "broker-healthz",
				Status: toolcli.StatusWarn,
				Detail: "skipping broker reachability check (--offline)",
			}
		}
		brokerURL := resolveDoctorBrokerURL()
		if brokerURL == "" {
			return toolcli.CheckResult{
				Name:   "broker-healthz",
				Status: toolcli.StatusWarn,
				Detail: "no broker URL configured (set AGENT_POOL_BROKER_URL, client.toml, or worker.toml)",
			}
		}

		client := deps.httpClient
		if client == nil {
			client = &http.Client{Timeout: doctorHTTPTimeout}
		}
		healthURL := strings.TrimRight(brokerURL, "/") + "/healthz"
		req, err := http.NewRequestWithContext(ctx, http.MethodGet, healthURL, nil)
		if err != nil {
			return toolcli.CheckResult{
				Name:   "broker-healthz",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("invalid broker URL %q: %v", brokerURL, err),
			}
		}
		resp, err := client.Do(req)
		if err != nil {
			return toolcli.CheckResult{
				Name:   "broker-healthz",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("broker unreachable at %s: %v", healthURL, err),
				Hint:   "verify agent-poold is running and reachable",
			}
		}
		defer resp.Body.Close()
		if resp.StatusCode != http.StatusOK {
			return toolcli.CheckResult{
				Name:   "broker-healthz",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("broker %s returned HTTP %d", healthURL, resp.StatusCode),
			}
		}
		return toolcli.CheckResult{
			Name:   "broker-healthz",
			Status: toolcli.StatusOK,
			Detail: fmt.Sprintf("%s reachable", healthURL),
		}
	}
}

func loadWorkerConfigBestEffort() *worker.Config {
	cfg, err := worker.LoadConfig(worker.DefaultConfigPath())
	if err != nil {
		return worker.DefaultConfig()
	}
	return cfg
}

func resolveDoctorBrokerURL() string {
	if u := strings.TrimSpace(os.Getenv("AGENT_POOL_BROKER_URL")); u != "" {
		return u
	}
	if cfg, err := loadClientConfig(defaultClientConfigPath()); err == nil && cfg.BrokerURL != "" {
		return cfg.BrokerURL
	}
	if wcfg, err := worker.LoadConfig(worker.DefaultConfigPath()); err == nil && wcfg.Worker.BrokerURL != "" {
		return wcfg.Worker.BrokerURL
	}
	return ""
}

func firstLine(s string) string {
	line, _, _ := strings.Cut(strings.TrimSpace(s), "\n")
	return strings.TrimSpace(line)
}
