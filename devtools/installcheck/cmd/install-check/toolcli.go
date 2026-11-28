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
	"strings"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/installcheck"
)

func newInstallCheckTool() toolcli.Tool {
	return newInstallCheckToolWith(os.Environ(), exec.LookPath)
}

func newInstallCheckToolWith(env []string, lookPath func(string) (string, error)) toolcli.Tool {
	var configFlag string
	resolveCfgPath := func() (string, bool) {
		if configFlag != "" {
			return configFlag, true
		}
		paths, err := installcheck.LocatePaths(env)
		if err != nil {
			return "", false
		}
		return paths.ConfigFile, paths.ConfigFile != ""
	}
	var tool toolcli.Tool
	tool = toolcli.Tool{
		Name:             "install-check",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/installcheck/cmd/install-check",
		SkillText:        skillText,
		RequiredCommands: []string{"git"},
		LookPath:         lookPath,
		RegisterDoctorFlags: func(fsFlag *flag.FlagSet) func(context.Context, io.Writer, io.Writer) (int, bool) {
			fsFlag.StringVar(&configFlag, "config", "", "config file")
			return nil
		},
		ConfigPath: resolveCfgPath,
		CheckConfig: func() error {
			path, ok := resolveCfgPath()
			if !ok {
				return errors.New("locate install-check paths: config path unavailable")
			}
			_, err := loadConfig(path)
			if err != nil {
				if configFlag == "" && errors.Is(err, fs.ErrNotExist) {
					return nil
				}
				return fmt.Errorf("load install-check config: %w", err)
			}
			return nil
		},
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
	var cfg installcheck.Config
	hasCfg := false
	if path, ok := resolveCfgPath(); ok {
		if loaded, err := loadConfig(path); err == nil {
			cfg = loaded
			hasCfg = true
		}
	}
	checks := []toolcli.CheckResult{checkGHBin(cfg, env, lookPath)}
	if hasCfg {
		checks = append(checks, checkNotifierCommand(cfg, env, lookPath))
	}
	return checks
}

func checkGHBin(cfg installcheck.Config, env []string, lookPath func(string) (string, error)) toolcli.CheckResult {
	bin, err := cfg.ResolveGHBin(env)
	if err != nil {
		return toolcli.CheckResult{
			Name:   "gh-bin",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "set gh_bin in config.toml or GH_BIN to a valid gh binary",
		}
	}
	resolved, err := resolveExecutable(bin, lookPath)
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

func checkNotifierCommand(cfg installcheck.Config, env []string, lookPath func(string) (string, error)) toolcli.CheckResult {
	mode := cfg.EffectiveNotifyMode()
	switch mode {
	case installcheck.NotifyModeStdout:
		return toolcli.CheckResult{
			Name:   "notifier",
			Status: toolcli.StatusOK,
			Detail: "stdout (no external command required)",
		}
	case installcheck.NotifyModeCommand:
		bin := cfg.NotifyCommand[0]
		if strings.HasPrefix(bin, "~/") {
			if home, ok := lookupEnvValue(env, "HOME"); ok && home != "" {
				bin = home + bin[1:]
			}
		}
		resolved, err := resolveExecutable(bin, lookPath)
		if err != nil {
			return toolcli.CheckResult{
				Name:   "notifier",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("command %s: %v", bin, err),
				Hint:   "ensure notify_command[0] is installed and executable",
			}
		}
		return toolcli.CheckResult{
			Name:   "notifier",
			Status: toolcli.StatusOK,
			Detail: "command: " + resolved,
		}
	default:
		resolved, err := resolveExecutable("agentbus", lookPath)
		if err != nil {
			return toolcli.CheckResult{
				Name:   "notifier",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("agentbus: %v", err),
				Hint:   "install agentbus or switch to notify_command / notify_mode = \"stdout\"",
			}
		}
		return toolcli.CheckResult{
			Name:   "notifier",
			Status: toolcli.StatusOK,
			Detail: "agentbus: " + resolved,
		}
	}
}

func resolveExecutable(bin string, lookPath func(string) (string, error)) (string, error) {
	p, err := lookPath(bin)
	if err != nil {
		return "", err
	}
	return p, nil
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
