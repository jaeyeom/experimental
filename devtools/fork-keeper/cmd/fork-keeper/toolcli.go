package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"time"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	forkkeeper "github.com/jaeyeom/experimental/devtools/fork-keeper"
)

type cmdRunner func(ctx context.Context, bin string, args []string, env []string) ([]byte, error)

func defaultRunCmd(ctx context.Context, bin string, args []string, env []string) ([]byte, error) {
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	// bin is resolved via LookPath for a configured reviewer CLI and args are
	// fixed status subcommands; no shell is invoked.
	//nolint:gosec // Known reviewer CLI auth status subcommand.
	// nosemgrep: dangerous-exec-command
	cmd := exec.CommandContext(ctx, bin, args...)
	cmd.Env = env
	out, err := cmd.Output()
	if err != nil {
		return out, fmt.Errorf("run %s: %w", bin, err)
	}
	return out, nil
}

func newForkKeeperTool() toolcli.Tool {
	return newForkKeeperToolWith(os.Environ(), exec.LookPath)
}

func newForkKeeperToolWith(env []string, lookPath func(string) (string, error)) toolcli.Tool {
	var configFlag string
	resolveCfgPath := func() (string, bool) {
		if configFlag != "" {
			return configFlag, true
		}
		if p, ok := lookupEnvValue(env, "FORK_KEEPER_CONFIG"); ok && p != "" {
			return p, true
		}
		p := forkkeeper.DefaultConfigPath()
		return p, p != ""
	}
	var tool toolcli.Tool
	tool = toolcli.Tool{
		Name:        "fork-keeper",
		ModulePath:  "github.com/jaeyeom/experimental",
		MainPackage: "github.com/jaeyeom/experimental/devtools/fork-keeper/cmd/fork-keeper",
		SkillText:   skillText,
		LookPath:    lookPath,
		RegisterDoctorFlags: func(fsFlag *flag.FlagSet) func(context.Context, io.Writer, io.Writer) (int, bool) {
			fsFlag.StringVar(&configFlag, "config", "", "path to forks.json")
			return nil
		},
		ConfigPath: resolveCfgPath,
		CheckConfig: func() error {
			path, ok := resolveCfgPath()
			if !ok {
				return errors.New("fork-keeper config path unavailable")
			}
			_, err := forkkeeper.LoadConfig(path)
			if err != nil {
				if configFlag == "" && errors.Is(err, fs.ErrNotExist) {
					return nil
				}
				return fmt.Errorf("load fork-keeper config: %w", err)
			}
			return nil
		},
		RequiredCommands: []string{"git"},
		ExtraDoctorCheckGroups: []toolcli.DoctorCheckGroup{
			func(ctx context.Context, _ bool) []toolcli.CheckResult {
				lp := tool.LookPath
				if lp == nil {
					lp = exec.LookPath
				}
				return extraDoctorChecks(ctx, env, lp, defaultRunCmd, resolveCfgPath)
			},
		},
	}
	return tool
}

func extraDoctorChecks(ctx context.Context, env []string, lookPath func(string) (string, error), runCmd cmdRunner, resolveCfgPath func() (string, bool)) []toolcli.CheckResult {
	var cfg forkkeeper.Config
	hasCfg := false
	if path, ok := resolveCfgPath(); ok {
		if loaded, err := forkkeeper.LoadConfig(path); err == nil {
			cfg = loaded
			hasCfg = true
		}
	}
	return []toolcli.CheckResult{
		checkGHBin(cfg, env, lookPath),
		checkSandbox(env, lookPath),
		checkReviewer(ctx, cfg, hasCfg, env, lookPath, runCmd),
	}
}

func checkGHBin(cfg forkkeeper.Config, env []string, lookPath func(string) (string, error)) toolcli.CheckResult {
	bin, err := cfg.ResolveGHBin("", env)
	if err != nil {
		return toolcli.CheckResult{
			Name:   "gh-bin",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "set gh_bin in forks.json or GH_BIN to a valid gh binary",
		}
	}
	resolved, err := lookPath(bin)
	if err != nil {
		return toolcli.CheckResult{
			Name:   "gh-bin",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("%s: %v", bin, err),
			Hint:   "install gh or set gh_bin / GH_BIN",
		}
	}
	return toolcli.CheckResult{
		Name:   "gh-bin",
		Status: toolcli.StatusOK,
		Detail: resolved,
	}
}

func checkSandbox(env []string, lookPath func(string) (string, error)) toolcli.CheckResult {
	mode := "auto"
	if v, ok := lookupEnvValue(env, "FORK_KEEPER_SANDBOX"); ok && strings.TrimSpace(v) != "" {
		mode = strings.TrimSpace(v)
	}
	resolved, err := forkkeeper.ResolveSandboxMode(mode, runtime.GOOS, lookPath)
	if err != nil {
		if mode == "auto" {
			return toolcli.CheckResult{
				Name:   "sandbox",
				Status: toolcli.StatusWarn,
				Detail: err.Error(),
				Hint:   "install the platform sandbox binary or set FORK_KEEPER_SANDBOX=none for unsandboxed reviewers",
			}
		}
		return toolcli.CheckResult{
			Name:   "sandbox",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "set FORK_KEEPER_SANDBOX to auto, seatbelt, bwrap, or none",
		}
	}
	return toolcli.CheckResult{
		Name:   "sandbox",
		Status: toolcli.StatusOK,
		Detail: resolved,
	}
}

func checkReviewer(ctx context.Context, cfg forkkeeper.Config, hasCfg bool, env []string, lookPath func(string) (string, error), runCmd cmdRunner) toolcli.CheckResult {
	names, err := collectConfiguredReviewerBins(cfg, hasCfg, env)
	if err != nil {
		return toolcli.CheckResult{
			Name:   "reviewer",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "fix reviewer_cmd / agent or FORK_KEEPER_REVIEWER_CMD",
		}
	}
	if len(names) == 0 {
		return toolcli.CheckResult{
			Name:   "reviewer",
			Status: toolcli.StatusOK,
			Detail: "built-in standalone rebase (needs-review); no external reviewer configured",
		}
	}

	resolved := make(map[string]string, len(names))
	var missing []string
	for _, b := range names {
		p, err := lookPath(b)
		if err != nil {
			missing = append(missing, b)
			continue
		}
		resolved[b] = p
	}
	if len(missing) > 0 {
		return toolcli.CheckResult{
			Name:   "reviewer",
			Status: toolcli.StatusFail,
			Detail: "missing configured reviewer executable(s): " + strings.Join(missing, ", "),
			Hint:   "install the configured reviewer command or clear agent/reviewer_cmd for built-in standalone mode",
		}
	}

	details := make([]string, 0, len(names))
	var hints []string
	seenHint := make(map[string]bool)
	hasAuthWarn := false
	for _, b := range names {
		p := resolved[b]
		checked, loggedIn, hint := checkReviewerAuth(ctx, b, p, env, runCmd)
		switch {
		case checked && !loggedIn:
			hasAuthWarn = true
			details = append(details, fmt.Sprintf("%s (%s, not logged in)", b, p))
			if hint != "" && !seenHint[hint] {
				seenHint[hint] = true
				hints = append(hints, hint)
			}
		case checked && loggedIn:
			details = append(details, fmt.Sprintf("%s (%s, logged in)", b, p))
		default:
			details = append(details, fmt.Sprintf("%s (%s, available)", b, p))
		}
	}
	if hasAuthWarn {
		return toolcli.CheckResult{
			Name:   "reviewer",
			Status: toolcli.StatusWarn,
			Detail: "configured: " + strings.Join(details, "; "),
			Hint:   strings.Join(hints, "; "),
		}
	}
	return toolcli.CheckResult{
		Name:   "reviewer",
		Status: toolcli.StatusOK,
		Detail: "configured: " + strings.Join(details, "; "),
	}
}

func checkReviewerAuth(ctx context.Context, binName, resolvedPath string, env []string, runCmd cmdRunner) (checked bool, loggedIn bool, hint string) {
	if runCmd == nil {
		return false, false, ""
	}
	base := filepath.Base(binName)
	if base == "." || base == string(filepath.Separator) {
		base = filepath.Base(resolvedPath)
	}
	switch base {
	case "codex":
		_, err := runCmd(ctx, resolvedPath, []string{"login", "status"}, env)
		if err != nil {
			return true, false, "run `codex login` to authenticate"
		}
		return true, true, ""
	case "claude":
		out, err := runCmd(ctx, resolvedPath, []string{"auth", "status"}, env)
		if err != nil {
			return true, false, "run `claude auth login` to authenticate"
		}
		var parsed struct {
			LoggedIn *bool `json:"loggedIn"`
		}
		if len(out) > 0 && json.Unmarshal(out, &parsed) == nil && parsed.LoggedIn != nil && !*parsed.LoggedIn {
			return true, false, "run `claude auth login` to authenticate"
		}
		return true, true, ""
	default:
		return false, false, ""
	}
}

func collectConfiguredReviewerBins(cfg forkkeeper.Config, hasCfg bool, env []string) ([]string, error) {
	forks := cfg.Forks
	if !hasCfg || len(forks) == 0 {
		forks = []forkkeeper.Fork{{}}
	}
	var globalCmd []string
	if hasCfg {
		globalCmd = cfg.ReviewerCmd
	}
	bins := make(map[string]bool)
	if len(globalCmd) > 0 {
		bins[globalCmd[0]] = true
	}
	for _, f := range forks {
		cmd, err := f.EffectiveReviewerCommand(nil, globalCmd, env)
		if err != nil {
			return nil, fmt.Errorf("resolve reviewer command: %w", err)
		}
		if len(cmd) > 0 {
			bins[cmd[0]] = true
		}
	}
	names := make([]string, 0, len(bins))
	for b := range bins {
		names = append(names, b)
	}
	sort.Strings(names)
	return names, nil
}

func lookupEnvValue(env []string, key string) (string, bool) {
	var val string
	var found bool
	for _, entry := range env {
		if k, v, ok := strings.Cut(entry, "="); ok && k == key {
			val = v
			found = true
		}
	}
	return val, found
}
