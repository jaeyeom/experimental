package main

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strings"
	"time"

	"github.com/chorong-sh/chorong/pkg/toolcli"
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
	var extraSkills []toolcli.Skill
	if triageSkill, ok, err := renderTriageString(cfg); err == nil && ok {
		extraSkills = append(extraSkills, toolcli.Skill{
			Name: "slackwatch-triage",
			Text: triageSkill,
		})
	}
	return toolcli.Tool{
		Name:          "slackwatch",
		MainPackage:   "github.com/jaeyeom/experimental/devtools/slackwatch/cmd/slackwatch",
		DaemonService: "slackwatch serve",
		SkillText:     renderedSkill,
		ExtraSkills:   extraSkills,
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
			func(_ context.Context, _ bool) toolcli.CheckResult {
				return checkSlackwatchDaemon(configPath)
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

func checkSlackwatchDaemon(configPath string) toolcli.CheckResult {
	cfg, err := slackwatch.LoadConfig(configPath)
	if err != nil || cfg.DBPath == "" {
		return toolcli.CheckResult{
			Name:   "daemon-binary",
			Status: toolcli.StatusOK,
			Detail: "daemon not running",
		}
	}
	if _, err := os.Stat(cfg.DBPath); errors.Is(err, fs.ErrNotExist) {
		return toolcli.CheckResult{
			Name:   "daemon-binary",
			Status: toolcli.StatusOK,
			Detail: "daemon not running",
		}
	}
	store, err := slackwatch.OpenStore(cfg.DBPath)
	if err != nil {
		return toolcli.CheckResult{
			Name:   "daemon-binary",
			Status: toolcli.StatusWarn,
			Detail: fmt.Sprintf("open slackwatch db: %v", err),
		}
	}
	defer store.Close()
	st, err := store.ReadStatus(time.Now())
	if err != nil || st.PID == 0 || st.State == slackwatch.ConnDown || st.State == slackwatch.ConnStale {
		return toolcli.CheckResult{
			Name:   "daemon-binary",
			Status: toolcli.StatusOK,
			Detail: "daemon not running",
		}
	}
	if target, err := os.Readlink(fmt.Sprintf("/proc/%d/exe", st.PID)); err == nil && strings.HasSuffix(target, " (deleted)") {
		return toolcli.CheckResult{
			Name:   "daemon-binary",
			Status: toolcli.StatusWarn,
			Detail: "slackwatch binary updated; restart needed",
			Hint:   "restart slackwatch serve",
		}
	}
	if cur := slackwatch.CurrentBinaryInfo(); st.BinaryInfo != "" && cur != "" && st.BinaryInfo != cur {
		return toolcli.CheckResult{
			Name:   "daemon-binary",
			Status: toolcli.StatusWarn,
			Detail: "slackwatch binary updated; restart needed",
			Hint:   "restart slackwatch serve",
		}
	}
	return toolcli.CheckResult{
		Name:   "daemon-binary",
		Status: toolcli.StatusOK,
		Detail: fmt.Sprintf("daemon pid %d matches current binary", st.PID),
	}
}
