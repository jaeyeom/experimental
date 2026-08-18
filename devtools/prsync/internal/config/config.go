// Package config loads the shell-sourceable prsync KEY=VALUE file.
package config

import (
	"errors"
	"fmt"
	"io/fs"
	"math"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"
)

const defaultPromptTemplate = `Address the unresolved review comments on PR #{number} ({url}).

Unaddressed threads:
{comments}

Triage each thread before acting:
- Mechanical / unambiguous (typo, nil check, rename, obvious bug): make the
  change, resolve the thread, and continue.
- Design decision, tradeoff, or disagreement: pause and ask the user how to
  proceed, with a recommended option. Do not edit, resolve, or push for that
  thread until they answer.

Push after the mechanical threads are done (and after the user answers any
questions). Do not touch other PRs.`

const defaultRebasePromptTemplate = `Rebase PR #{number} ({url}) onto origin/{base} in this working directory. Do not create a new worktree.

1. Check out {head}.
2. Fetch origin.
3. Rebase onto origin/{base}.
4. Resolve any conflicts.
5. Push with --force-with-lease.

Do not touch other PRs.`

var repoPattern = regexp.MustCompile(`^[^/\s]+/[^/\s]+$`)

// Config is the resolved prsync configuration.
type Config struct {
	Repos                []string
	Author               string
	TitleIDPattern       *regexp.Regexp
	TabLabelTemplate     string
	HerdrBin             string
	GHBin                string
	ConcurrencyWaitOn    string
	GatePoll             time.Duration
	GateTimeout          time.Duration
	PromptTemplate       string
	RebasePromptTemplate string
	IncludeDrafts        bool
	WaitUntil            []string
	DispatchTimeout      time.Duration
	StateFile            string
	DryRun               bool

	// SourcePath is empty if defaults only; for stderr diagnostics.
	SourcePath string
}

// KeyError is a config parse or validation failure mapped to exit 2.
type KeyError struct {
	Key    string
	Line   int
	Reason string
}

func (e *KeyError) Error() string {
	if e == nil {
		return ""
	}
	if e.Key != "" {
		return fmt.Sprintf("key %s: %s", e.Key, e.Reason)
	}
	if e.Line > 0 {
		return fmt.Sprintf("line %d: %s", e.Line, e.Reason)
	}
	return e.Reason
}

// Defaults returns the built-in configuration.
func Defaults() Config {
	return Config{
		TitleIDPattern:       regexp.MustCompile(`[A-Z]+-[0-9]+`),
		TabLabelTemplate:     "{id}",
		HerdrBin:             "herdr",
		GHBin:                "gh",
		ConcurrencyWaitOn:    "any",
		GatePoll:             2000 * time.Millisecond,
		GateTimeout:          1800000 * time.Millisecond,
		PromptTemplate:       defaultPromptTemplate,
		RebasePromptTemplate: defaultRebasePromptTemplate,
		WaitUntil:            []string{"idle", "done"},
		DispatchTimeout:      1800000 * time.Millisecond,
		StateFile:            "~/.config/prsync/state.json",
		DryRun:               true,
	}
}

// Load applies search order, then Validate.
// explicitPath is the --config value (empty if unset).
func Load(explicitPath string) (Config, error) {
	cfg := Defaults()
	path, err := resolvePath(explicitPath)
	if err != nil {
		return Config{}, err
	}
	if path != "" {
		cfg.SourcePath = path
		kv, err := parseFile(path)
		if err != nil {
			return Config{}, err
		}
		if err := apply(&cfg, kv); err != nil {
			return Config{}, err
		}
	}
	cfg.StateFile = expandTilde(cfg.StateFile)
	cfg.GHBin = expandTilde(cfg.GHBin)
	cfg.HerdrBin = expandTilde(cfg.HerdrBin)
	if err := cfg.Validate(); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

// Validate compiles regexes and checks enums. Returns *KeyError.
func (c Config) Validate() error {
	if c.TitleIDPattern == nil {
		return &KeyError{Key: "title_id_pattern", Reason: "missing pattern"}
	}
	if c.TabLabelTemplate == "" {
		return &KeyError{Key: "tab_label_template", Reason: "must be non-empty"}
	}
	if c.HerdrBin == "" {
		return &KeyError{Key: "herdr_bin", Reason: "must be non-empty"}
	}
	if c.GHBin == "" {
		return &KeyError{Key: "gh_bin", Reason: "must be non-empty"}
	}
	if c.ConcurrencyWaitOn != "any" && c.ConcurrencyWaitOn != "managed" {
		return &KeyError{Key: "concurrency_wait_on", Reason: fmt.Sprintf("invalid value %q", c.ConcurrencyWaitOn)}
	}
	if c.GatePoll < time.Millisecond {
		return &KeyError{Key: "gate_poll_ms", Reason: "must be >= 1"}
	}
	if c.GateTimeout < time.Millisecond {
		return &KeyError{Key: "gate_timeout_ms", Reason: "must be >= 1"}
	}
	if c.DispatchTimeout < time.Millisecond {
		return &KeyError{Key: "dispatch_timeout_ms", Reason: "must be >= 1"}
	}
	if len(c.WaitUntil) == 0 {
		return &KeyError{Key: "dispatch_wait_until", Reason: "at least one token required"}
	}
	if c.StateFile == "" {
		return &KeyError{Key: "state_file", Reason: "must be non-empty"}
	}
	for _, repo := range c.Repos {
		if !repoPattern.MatchString(repo) {
			return &KeyError{Key: "repos", Reason: fmt.Sprintf("invalid repo %q", repo)}
		}
	}
	return nil
}

func parseFile(path string) (map[string]string, error) {
	f, err := os.Open(path) //nolint:gosec // path is an operator-supplied config file
	if err != nil {
		return nil, &KeyError{Reason: fmt.Sprintf("open %s: %v", path, err)}
	}
	defer f.Close() //nolint:errcheck
	return Parse(f)
}

func resolvePath(explicit string) (string, error) {
	if explicit != "" {
		info, err := os.Stat(explicit)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				return "", &KeyError{Reason: fmt.Sprintf("config file not found: %s", explicit)}
			}
			return "", &KeyError{Reason: fmt.Sprintf("stat %s: %v", explicit, err)}
		}
		if info.IsDir() {
			return "", &KeyError{Reason: fmt.Sprintf("config file not found: %s", explicit)}
		}
		return explicit, nil
	}
	if path := filepath.Join(configCWD(), "prsync.config"); fileExists(path) {
		return path, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("home dir: %w", err)
	}
	if path := filepath.Join(home, ".config", "prsync", "prsync.config"); fileExists(path) {
		return path, nil
	}
	return "", nil
}

func configCWD() string {
	if wd := os.Getenv("BUILD_WORKING_DIRECTORY"); wd != "" {
		return wd
	}
	wd, err := os.Getwd()
	if err != nil {
		return ""
	}
	return wd
}

func fileExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}

func apply(cfg *Config, kv map[string]string) error {
	for key, val := range kv {
		if err := applyKey(cfg, key, val); err != nil {
			return err
		}
	}
	return nil
}

func applyKey(cfg *Config, key, val string) error {
	switch key {
	case "repos":
		return applyRepos(cfg, val)
	case "author":
		cfg.Author = val
	case "title_id_pattern":
		return applyTitleIDPattern(cfg, val)
	case "tab_label_template":
		cfg.TabLabelTemplate = val
	case "herdr_bin":
		cfg.HerdrBin = val
	case "gh_bin":
		cfg.GHBin = val
	case "concurrency_wait_on":
		cfg.ConcurrencyWaitOn = val
	case "gate_poll_ms", "gate_timeout_ms", "dispatch_timeout_ms":
		return applyMillis(cfg, key, val)
	case "dispatch_prompt_template":
		return applyPrompt(cfg, "dispatch_prompt_template", val)
	case "rebase_prompt_template":
		return applyPrompt(cfg, "rebase_prompt_template", val)
	case "dispatch_include_drafts":
		return applyIncludeDrafts(cfg, val)
	case "dispatch_wait_until":
		return applyWaitUntil(cfg, val)
	case "state_file":
		cfg.StateFile = val
	case "dry_run":
		return applyDryRun(cfg, val)
	}
	return nil
}

func applyRepos(cfg *Config, val string) error {
	fields := strings.Fields(val)
	for _, repo := range fields {
		if !repoPattern.MatchString(repo) {
			return &KeyError{Key: "repos", Reason: fmt.Sprintf("invalid repo %q", repo)}
		}
	}
	cfg.Repos = fields
	return nil
}

func applyTitleIDPattern(cfg *Config, val string) error {
	re, err := regexp.Compile(val)
	if err != nil {
		return &KeyError{Key: "title_id_pattern", Reason: err.Error()}
	}
	cfg.TitleIDPattern = re
	return nil
}

func applyMillis(cfg *Config, key, val string) error {
	d, err := parseMillis(key, val)
	if err != nil {
		return err
	}
	switch key {
	case "gate_poll_ms":
		cfg.GatePoll = d
	case "gate_timeout_ms":
		cfg.GateTimeout = d
	case "dispatch_timeout_ms":
		cfg.DispatchTimeout = d
	}
	return nil
}

func applyPrompt(cfg *Config, key, val string) error {
	text, err := resolvePrompt(key, val)
	if err != nil {
		return err
	}
	switch key {
	case "rebase_prompt_template":
		cfg.RebasePromptTemplate = text
	default:
		cfg.PromptTemplate = text
	}
	return nil
}

func applyIncludeDrafts(cfg *Config, val string) error {
	b, err := parseBool("dispatch_include_drafts", val)
	if err != nil {
		return err
	}
	cfg.IncludeDrafts = b
	return nil
}

func applyWaitUntil(cfg *Config, val string) error {
	fields := strings.Fields(val)
	if len(fields) == 0 {
		return &KeyError{Key: "dispatch_wait_until", Reason: "at least one token required"}
	}
	cfg.WaitUntil = fields
	return nil
}

func applyDryRun(cfg *Config, val string) error {
	b, err := parseBool("dry_run", val)
	if err != nil {
		return err
	}
	cfg.DryRun = b
	return nil
}

func resolvePrompt(key, val string) (string, error) {
	if !strings.HasPrefix(val, "@") {
		return val, nil
	}
	path := expandTilde(val[1:])
	data, err := os.ReadFile(path) //nolint:gosec // path is an operator-supplied prompt file
	if err != nil {
		return "", &KeyError{Key: key, Reason: fmt.Sprintf("read %s: %v", path, err)}
	}
	return string(data), nil
}

func parseMillis(key, val string) (time.Duration, error) {
	n, err := strconv.ParseInt(val, 10, 64)
	if err != nil {
		return 0, &KeyError{Key: key, Reason: "not an integer"}
	}
	if n < 1 {
		return 0, &KeyError{Key: key, Reason: "must be >= 1"}
	}
	if n > math.MaxInt64/int64(time.Millisecond) {
		return 0, &KeyError{Key: key, Reason: "overflow"}
	}
	return time.Duration(n) * time.Millisecond, nil
}

func parseBool(key, val string) (bool, error) {
	switch val {
	case "true", "1":
		return true, nil
	case "false", "0":
		return false, nil
	default:
		return false, &KeyError{Key: key, Reason: fmt.Sprintf("invalid bool %q", val)}
	}
}

func expandTilde(path string) string {
	if path == "~" {
		if home, err := os.UserHomeDir(); err == nil {
			return home
		}
		return path
	}
	if strings.HasPrefix(path, "~/") {
		if home, err := os.UserHomeDir(); err == nil {
			return filepath.Join(home, path[2:])
		}
	}
	return path
}
