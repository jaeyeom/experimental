package main

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

func newSlackwatchTool(configPath string) toolcli.Tool {
	cfg, err := slackwatch.LoadConfig(configPath)
	if err != nil {
		cfg = slackwatch.Config{}
	}
	renderedSkill, renderErr := renderSkillString(cfg)
	if renderErr != nil {
		renderedSkill = skillTemplateText
	}
	return toolcli.Tool{
		Name:          "slackwatch",
		MainPackage:   "github.com/jaeyeom/experimental/devtools/slackwatch/cmd/slackwatch",
		DaemonService: "slackwatch serve",
		SkillText:     renderedSkill,
		ConfigPath: func() (string, bool) {
			p := slackwatch.ConfigPath(configPath)
			return p, p != ""
		},
		CheckConfig: func() error {
			if _, err := slackwatch.LoadConfig(configPath); err != nil {
				return fmt.Errorf("load slackwatch config: %w", err)
			}
			return nil
		},
		ExtraDoctorChecks: []toolcli.DoctorCheck{
			func(_ context.Context, _ bool) toolcli.CheckResult {
				return checkSlackwatchTokenFile(configPath)
			},
		},
	}
}

func checkSlackwatchTokenFile(configPath string) toolcli.CheckResult {
	cfg, err := slackwatch.LoadConfig(configPath)
	if err != nil {
		return toolcli.CheckResult{
			Name:   "token-file",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "fix slackwatch config.json",
		}
	}
	if cfg.TokenFile == "" {
		return toolcli.CheckResult{
			Name:   "token-file",
			Status: toolcli.StatusWarn,
			Detail: "tokenFile is not configured",
			Hint:   "set tokenFile in slackwatch config.json",
		}
	}
	if err := slackwatch.CheckTokenFile(cfg.TokenFile); err != nil {
		return toolcli.CheckResult{
			Name:   "token-file",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "ensure tokenFile exists with mode 0600",
		}
	}
	return toolcli.CheckResult{
		Name:   "token-file",
		Status: toolcli.StatusOK,
		Detail: cfg.TokenFile,
	}
}
