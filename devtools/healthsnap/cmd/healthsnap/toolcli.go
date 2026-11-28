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

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/healthsnap"
)

func newHealthsnapTool() toolcli.Tool {
	return newHealthsnapToolWith(os.Environ(), exec.LookPath)
}

func newHealthsnapToolWith(env []string, lookPath func(string) (string, error)) toolcli.Tool {
	return toolcli.Tool{
		Name:        "healthsnap",
		ModulePath:  "github.com/jaeyeom/experimental",
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
	needed := collectNeededBinaries(cfg)
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

func collectNeededBinaries(cfg healthsnap.Config) map[string]bool {
	needed := map[string]bool{}
	for _, c := range cfg.Checks {
		if c.Kind == healthsnap.KindCommand && len(c.Argv) > 0 {
			needed[c.Argv[0]] = true
		}
		collectRuleNotifyBinaries(needed, c.Rules, cfg.Notify)
	}
	if len(cfg.Notify.Command) > 0 {
		needed[cfg.Notify.Command[0]] = true
	} else if cfg.Notify.Recipient != "" || cfg.Notify.JudgmentRecipient != "" {
		needed["agentbus"] = true
	}
	if len(cfg.Notify.JudgmentCommand) > 0 {
		needed[cfg.Notify.JudgmentCommand[0]] = true
	}
	return needed
}

func collectRuleNotifyBinaries(needed map[string]bool, rules []healthsnap.RuleConfig, notifyCfg healthsnap.NotifyConfig) {
	for _, r := range rules {
		if r.State == healthsnap.SeverityInfo {
			continue
		}
		if len(r.Command) > 0 {
			needed[r.Command[0]] = true
			continue
		}
		if r.Recipient != "" && len(notifyCfg.Command) == 0 && (!r.Judgment || len(notifyCfg.JudgmentCommand) == 0) {
			needed["agentbus"] = true
		}
	}
}

func checkJudgmentRouting(cfg healthsnap.Config) (toolcli.CheckResult, bool) {
	hasJudgment := false
	needsGlobalJudgment := false
	for _, c := range cfg.Checks {
		if c.Judgment {
			hasJudgment = true
			needsGlobalJudgment = true
		}
		for _, r := range c.Rules {
			if r.Judgment && r.State != healthsnap.SeverityInfo {
				hasJudgment = true
				if r.Recipient == "" && len(r.Command) == 0 {
					needsGlobalJudgment = true
				}
			}
		}
	}
	if !hasJudgment {
		return toolcli.CheckResult{}, false
	}
	if needsGlobalJudgment && cfg.Notify.JudgmentRecipient == "" && len(cfg.Notify.JudgmentCommand) == 0 {
		return toolcli.CheckResult{
			Name:   "judgment-routing",
			Status: toolcli.StatusWarn,
			Detail: "judgment rules configured without notify.judgmentRecipient or notify.judgmentCommand",
			Hint:   "set notify.judgmentRecipient in config.pkl so judgment alerts go to a separate recipient",
		}, true
	}
	detail := cfg.Notify.JudgmentRecipient
	if detail == "" && !needsGlobalJudgment {
		detail = "per-rule routing"
	}
	return toolcli.CheckResult{
		Name:   "judgment-routing",
		Status: toolcli.StatusOK,
		Detail: detail,
	}, true
}
