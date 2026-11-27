package main

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"sort"
	"strings"

	"github.com/jaeyeom/experimental/devtools/healthsnap"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newHealthsnapTool() toolcli.Tool {
	return newHealthsnapToolWith(os.Environ(), exec.LookPath)
}

func newHealthsnapToolWith(env []string, lookPath func(string) (string, error)) toolcli.Tool {
	return toolcli.Tool{
		Name:        "healthsnap",
		MainPackage: "github.com/jaeyeom/experimental/devtools/healthsnap/cmd/healthsnap",
		SkillText:   skillText,
		LookPath:    lookPath,
		ConfigPath: func() (string, bool) {
			paths, err := healthsnap.LocatePaths(env)
			if err != nil {
				return "", false
			}
			return paths.ConfigFile, paths.ConfigFile != ""
		},
		CheckConfig: func() error {
			paths, err := healthsnap.LocatePaths(env)
			if err != nil {
				return fmt.Errorf("locate healthsnap paths: %w", err)
			}
			if _, err := healthsnap.LoadConfig(context.Background(), paths.ConfigFile, true); err != nil {
				return fmt.Errorf("load healthsnap config: %w", err)
			}
			return nil
		},
		ExtraDoctorCheckGroups: []toolcli.DoctorCheckGroup{
			func(ctx context.Context, _ bool) []toolcli.CheckResult {
				return extraDoctorChecks(ctx, env, lookPath)
			},
		},
	}
}

func extraDoctorChecks(ctx context.Context, env []string, lookPath func(string) (string, error)) []toolcli.CheckResult {
	paths, err := healthsnap.LocatePaths(env)
	if err != nil {
		return []toolcli.CheckResult{{
			Name:   "state-dir",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
		}}
	}
	cfg, err := healthsnap.LoadConfig(ctx, paths.ConfigFile, true)
	stateDir := paths.StateDir
	if err == nil && cfg.StateDir != "" {
		stateDir = healthsnap.ExpandHome(cfg.StateDir, env)
	}
	checks := []toolcli.CheckResult{checkStateDir(stateDir)}
	if err != nil {
		return checks
	}
	if _, statErr := os.Stat(paths.ConfigFile); errors.Is(statErr, fs.ErrNotExist) {
		return checks
	}
	checks = append(checks, checkConfiguredBinaries(cfg, lookPath))
	if r, ok := checkJudgmentRouting(cfg); ok {
		checks = append(checks, r)
	}
	return checks
}

func checkStateDir(stateDir string) toolcli.CheckResult {
	if err := os.MkdirAll(stateDir, 0o700); err != nil {
		return toolcli.CheckResult{
			Name:   "state-dir",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("%s: %v", stateDir, err),
		}
	}
	return toolcli.CheckResult{
		Name:   "state-dir",
		Status: toolcli.StatusOK,
		Detail: stateDir,
	}
}

func checkConfiguredBinaries(cfg healthsnap.Config, lookPath func(string) (string, error)) toolcli.CheckResult {
	needed := map[string]bool{}
	for _, c := range cfg.Checks {
		if c.Kind == healthsnap.KindCommand && len(c.Argv) > 0 {
			needed[c.Argv[0]] = true
		}
	}
	if len(cfg.Notify.Command) > 0 {
		needed[cfg.Notify.Command[0]] = true
	} else if cfg.Notify.Recipient != "" || cfg.Notify.JudgmentRecipient != "" {
		needed["agentbus"] = true
	}
	if len(cfg.Notify.JudgmentCommand) > 0 {
		needed[cfg.Notify.JudgmentCommand[0]] = true
	}
	if len(needed) == 0 {
		return toolcli.CheckResult{
			Name:   "check-commands",
			Status: toolcli.StatusOK,
			Detail: "no external check commands configured",
		}
	}

	var names, missing []string
	for name := range needed {
		names = append(names, name)
		if _, err := lookPath(name); err != nil {
			missing = append(missing, name)
		}
	}
	sort.Strings(names)
	sort.Strings(missing)
	if len(missing) > 0 {
		return toolcli.CheckResult{
			Name:   "check-commands",
			Status: toolcli.StatusFail,
			Detail: "missing on PATH: " + strings.Join(missing, ", "),
			Hint:   "install missing tools or update argv in config.pkl",
		}
	}
	return toolcli.CheckResult{
		Name:   "check-commands",
		Status: toolcli.StatusOK,
		Detail: strings.Join(names, ", "),
	}
}

func checkJudgmentRouting(cfg healthsnap.Config) (toolcli.CheckResult, bool) {
	hasJudgment := false
	for _, c := range cfg.Checks {
		if c.Judgment {
			hasJudgment = true
			break
		}
		for _, r := range c.Rules {
			if r.Judgment {
				hasJudgment = true
				break
			}
		}
		if hasJudgment {
			break
		}
	}
	if !hasJudgment {
		return toolcli.CheckResult{}, false
	}
	if cfg.Notify.JudgmentRecipient == "" && len(cfg.Notify.JudgmentCommand) == 0 {
		return toolcli.CheckResult{
			Name:   "judgment-routing",
			Status: toolcli.StatusWarn,
			Detail: "judgment rules configured without notify.judgmentRecipient or notify.judgmentCommand",
			Hint:   "set notify.judgmentRecipient in config.pkl so judgment alerts go to a separate recipient",
		}, true
	}
	return toolcli.CheckResult{
		Name:   "judgment-routing",
		Status: toolcli.StatusOK,
		Detail: cfg.Notify.JudgmentRecipient,
	}, true
}
