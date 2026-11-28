package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"runtime"
	"sort"
	"strings"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	forkkeeper "github.com/jaeyeom/experimental/devtools/fork-keeper"
)

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
			func(_ context.Context, _ bool) []toolcli.CheckResult {
				lp := tool.LookPath
				if lp == nil {
					lp = exec.LookPath
				}
				return extraDoctorChecks(env, lp, resolveCfgPath)
			},
		},
	}
	return tool
}

func extraDoctorChecks(env []string, lookPath func(string) (string, error), resolveCfgPath func() (string, bool)) []toolcli.CheckResult {
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
		checkReviewer(cfg, hasCfg, env, lookPath),
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

func checkReviewer(cfg forkkeeper.Config, hasCfg bool, env []string, lookPath func(string) (string, error)) toolcli.CheckResult {
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

	var missing []string
	for _, b := range names {
		if _, err := lookPath(b); err != nil {
			missing = append(missing, b)
		}
	}
	if len(missing) > 0 {
		return toolcli.CheckResult{
			Name:   "reviewer",
			Status: toolcli.StatusFail,
			Detail: "missing configured reviewer executable(s): " + strings.Join(missing, ", "),
			Hint:   "install the configured reviewer command or clear agent/reviewer_cmd for built-in standalone mode",
		}
	}
	return toolcli.CheckResult{
		Name:   "reviewer",
		Status: toolcli.StatusOK,
		Detail: "configured: " + strings.Join(names, ", "),
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
