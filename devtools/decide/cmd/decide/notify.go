package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// fileConfig is the decide config file. An absent file runs no command.
// OptionsOnly makes relay asks reject free text unless --free-text is set.
// The example notifier line is not a default.
type fileConfig struct {
	NotifyTemplate string `json:"notifyTemplate"`
	NotifyPane     string `json:"notifyPane"`
	OptionsOnly    bool   `json:"optionsOnly"`
}

func configPath() (string, bool) {
	dir := os.Getenv("XDG_CONFIG_HOME")
	if dir == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", false
		}
		dir = filepath.Join(home, ".config")
	}
	return filepath.Join(dir, "decide", "config.json"), true
}

func loadConfig() (fileConfig, error) {
	path, ok := configPath()
	if !ok {
		return fileConfig{}, nil
	}
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return fileConfig{}, nil
	}
	if err != nil {
		return fileConfig{}, fmt.Errorf("read decide config: %w", err)
	}
	var cfg fileConfig
	if err := json.Unmarshal(data, &cfg); err != nil {
		return fileConfig{}, fmt.Errorf("parse decide config: %w", err)
	}
	return cfg, nil
}

// notifyAnswer runs the configured template after a relay answer is saved.
// An empty template does nothing. The item notify pane overrides the config pane.
func notifyAnswer(item *decide.Item, stderr io.Writer) error {
	cfg, err := loadConfig()
	if err != nil {
		return err
	}
	if strings.TrimSpace(cfg.NotifyTemplate) == "" {
		return nil
	}
	pane := cfg.NotifyPane
	if strings.TrimSpace(item.NotifyPane) != "" {
		pane = item.NotifyPane
	}
	answer := ""
	if item.Answer != nil {
		answer = item.Answer.Value
	}
	line := strings.NewReplacer(
		"{id}", item.ID,
		"{answer}", answer,
		"{owner}", item.Owner,
		"{pane}", pane,
	).Replace(cfg.NotifyTemplate)
	// The template is local operator config. Placeholders are that operator's ledger fields.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command("sh", "-c", line) //nolint:gosec // G204: the notify template is operator config
	cmd.Stdout = stderr
	cmd.Stderr = stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("notify: %w", err)
	}
	return nil
}
