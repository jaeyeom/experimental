package decide

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
)

// Config is the decide config file. An absent file runs no command.
// OptionsOnly makes relay asks reject free text unless --free-text is set.
// NotifyTemplate is a shell command. It reads "$DECIDE_ANSWER", "$DECIDE_ID",
// "$DECIDE_OWNER", and "$DECIDE_PANE". Those values are not written into the
// command string. An empty template runs nothing. The example is not a default.
// SendTemplate is the compose argv template. Empty means
// `herdr agent prompt {pane}` plus the note as its own argument.
// The note is not interpolated into sh -c.
type Config struct {
	NotifyTemplate string         `json:"notifyTemplate"`
	NotifyPane     string         `json:"notifyPane"`
	OptionsOnly    bool           `json:"optionsOnly"`
	SendTemplate   string         `json:"sendTemplate,omitempty"`
	Targets        []ConfigTarget `json:"targets,omitempty"`
}

// ConfigTarget is one compose destination. The pane id comes from config.
type ConfigTarget struct {
	Name string `json:"name"`
	Pane string `json:"pane"`
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

// LoadConfig reads the decide config file. A missing file is an empty config.
func LoadConfig() (Config, error) {
	path, ok := configPath()
	if !ok {
		return Config{}, nil
	}
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return Config{}, nil
	}
	if err != nil {
		return Config{}, fmt.Errorf("read decide config: %w", err)
	}
	var cfg Config
	if err := json.Unmarshal(data, &cfg); err != nil {
		return Config{}, fmt.Errorf("parse decide config: %w", err)
	}
	return cfg, nil
}

// Notify runs the configured template after a relay answer is saved.
// An empty template does nothing. The item notify pane overrides the config pane.
// "$DECIDE_ID", "$DECIDE_ANSWER", "$DECIDE_OWNER", and "$DECIDE_PANE" hold the
// fields. The answer is not substituted into the command string.
func Notify(item *Item, out io.Writer) error {
	cfg, err := LoadConfig()
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
	// The template is operator config. The answer stays in the environment.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command("sh", "-c", cfg.NotifyTemplate) //nolint:gosec // G204: the notify template is operator config; fields are environment variables
	cmd.Env = append([]string{
		"DECIDE_ID=" + item.ID,
		"DECIDE_ANSWER=" + answer,
		"DECIDE_OWNER=" + item.Owner,
		"DECIDE_PANE=" + pane,
	}, os.Environ()...)
	cmd.Stdout = out
	cmd.Stderr = out
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("notify: %w", err)
	}
	return nil
}
