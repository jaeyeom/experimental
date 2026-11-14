package ghask

import (
	"bytes"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/pelletier/go-toml/v2"
)

// Defaults for optional config keys.
const (
	DefaultListen         = "127.0.0.1:8765"
	DefaultRateLimit      = 10
	DefaultCommandTimeout = 2 * time.Minute
)

// Reuse modes for the reuse key.
const (
	// ReusePerPR sends a later ask on the same PR to the PR's thread.
	ReusePerPR = "per-pr"
	// ReuseNone starts a new thread for every ask.
	ReuseNone = "none"
)

// Placeholders each command may use. None of them carries request text.
var (
	askPlaceholders      = []string{"file", "pr", "repo"}
	followupPlaceholders = []string{"file", "pr", "repo", "id"}
	statusPlaceholders   = []string{"id"}
)

// Config is the parsed gh-ask config.
type Config struct {
	// Listen is 127.0.0.1:<port>. Any other host is refused.
	Listen string
	// Repos is the owner/name allowlist.
	Repos []string
	// AskCommand is the argv template run for each ask that starts a
	// thread.
	AskCommand []string
	// FollowupCommand is the argv template run for a later ask on a PR
	// whose thread is still open. Required when Reuse is ReusePerPR.
	FollowupCommand []string
	// StatusCommand is the argv template run for GET /status/<id>. When
	// empty, /status answers 404.
	StatusCommand []string
	// Reuse is ReusePerPR or ReuseNone.
	Reuse string
	// IDPattern finds the id in the command's stdout. Its first group is
	// the id when it has one; otherwise the whole match is.
	IDPattern *regexp.Regexp
	// RateLimit is the number of asks allowed per hour.
	RateLimit      int
	CommandTimeout time.Duration
	// TokenFile holds the shared secret. LoadConfig defaults it to
	// "token" next to the config file.
	TokenFile string
	// StateFile holds the (repo, pr) to id map for ReusePerPR. It is not
	// a config key; the serve command sets it to DefaultStatePath.
	StateFile string
}

type fileConfig struct {
	Listen          string   `toml:"listen"`
	Repos           []string `toml:"repos"`
	AskCommand      []string `toml:"ask_command"`
	FollowupCommand []string `toml:"followup_command"`
	StatusCommand   []string `toml:"status_command"`
	Reuse           string   `toml:"reuse"`
	IDPattern       string   `toml:"id_pattern"`
	RateLimit       int      `toml:"rate_limit"`
	CommandTimeout  string   `toml:"command_timeout"`
	TokenFile       string   `toml:"token_file"`
}

// ParseConfig parses and validates TOML config. Unknown keys are errors.
func ParseConfig(data []byte) (Config, error) {
	var raw fileConfig
	dec := toml.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&raw); err != nil {
		var strict *toml.StrictMissingError
		if errors.As(err, &strict) {
			return Config{}, fmt.Errorf("parse config: %s", strict.String())
		}
		return Config{}, fmt.Errorf("parse config: %w", err)
	}
	cfg := Config{
		Listen:          raw.Listen,
		Repos:           raw.Repos,
		AskCommand:      raw.AskCommand,
		FollowupCommand: raw.FollowupCommand,
		StatusCommand:   raw.StatusCommand,
		Reuse:           raw.Reuse,
		RateLimit:       raw.RateLimit,
		CommandTimeout:  DefaultCommandTimeout,
		TokenFile:       raw.TokenFile,
	}
	if cfg.Listen == "" {
		cfg.Listen = DefaultListen
	}
	if err := CheckListenAddr(cfg.Listen); err != nil {
		return Config{}, err
	}
	if len(cfg.Repos) == 0 {
		return Config{}, errors.New("repos is empty; list the owner/name repos to allow")
	}
	for _, repo := range cfg.Repos {
		if !repoPattern.MatchString(repo) {
			return Config{}, fmt.Errorf("repos: %q is not owner/name", repo)
		}
	}
	if err := cfg.checkCommands(); err != nil {
		return Config{}, err
	}
	if raw.IDPattern == "" {
		return Config{}, errors.New("id_pattern is required")
	}
	re, err := regexp.Compile(raw.IDPattern)
	if err != nil {
		return Config{}, fmt.Errorf("id_pattern: %w", err)
	}
	cfg.IDPattern = re
	if err := cfg.applyLimits(raw); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

func (cfg *Config) checkCommands() error {
	if err := checkArgvTemplate("ask_command", cfg.AskCommand, askPlaceholders); err != nil {
		return err
	}
	switch cfg.Reuse {
	case "":
		cfg.Reuse = ReusePerPR
	case ReusePerPR, ReuseNone:
	default:
		return fmt.Errorf("reuse %q: want %q or %q", cfg.Reuse, ReusePerPR, ReuseNone)
	}
	if cfg.FollowupCommand != nil || cfg.Reuse == ReusePerPR {
		if err := checkArgvTemplate("followup_command", cfg.FollowupCommand, followupPlaceholders); err != nil {
			if len(cfg.FollowupCommand) == 0 && cfg.Reuse == ReusePerPR {
				return fmt.Errorf("%w; it is required unless reuse = %q", err, ReuseNone)
			}
			return err
		}
	}
	if cfg.StatusCommand != nil {
		if err := checkArgvTemplate("status_command", cfg.StatusCommand, statusPlaceholders); err != nil {
			return err
		}
	}
	return nil
}

func (cfg *Config) applyLimits(raw fileConfig) error {
	if cfg.RateLimit < 0 {
		return errors.New("rate_limit must be positive")
	}
	if cfg.RateLimit == 0 {
		cfg.RateLimit = DefaultRateLimit
	}
	if raw.CommandTimeout == "" {
		return nil
	}
	d, err := time.ParseDuration(raw.CommandTimeout)
	if err != nil {
		return fmt.Errorf("command_timeout: %w", err)
	}
	if d <= 0 {
		return errors.New("command_timeout must be positive")
	}
	cfg.CommandTimeout = d
	return nil
}

func checkArgvTemplate(key string, argv, allowed []string) error {
	if len(argv) == 0 {
		return fmt.Errorf("%s is empty", key)
	}
	for _, arg := range argv {
		if arg == "" {
			return fmt.Errorf("%s has an empty argument", key)
		}
	}
	vars := make(map[string]string, len(allowed))
	for _, name := range allowed {
		vars[name] = ""
	}
	if _, err := expandArgv(argv, vars); err != nil {
		return fmt.Errorf("%s: %w", key, err)
	}
	return nil
}

// CheckListenAddr accepts only 127.0.0.1:<port>.
func CheckListenAddr(addr string) error {
	host, port, err := net.SplitHostPort(addr)
	if err != nil {
		return fmt.Errorf("listen %q: %w", addr, err)
	}
	if host != "127.0.0.1" {
		return fmt.Errorf("listen %q: only 127.0.0.1:<port> is allowed", addr)
	}
	if n, err := strconv.Atoi(port); err != nil || n < 0 || n > 65535 {
		return fmt.Errorf("listen %q: bad port", addr)
	}
	return nil
}

// LoadConfig reads the config file. A relative token_file is resolved
// against the config file's directory.
func LoadConfig(path string) (Config, error) {
	data, err := os.ReadFile(path) //nolint:gosec // G304: the user's own config path
	if err != nil {
		return Config{}, fmt.Errorf("read config: %w", err)
	}
	cfg, err := ParseConfig(data)
	if err != nil {
		return Config{}, fmt.Errorf("%s: %w", path, err)
	}
	cfg.TokenFile = ResolveTokenFile(path, cfg.TokenFile)
	return cfg, nil
}

// ResolveTokenFile returns tokenFile resolved against the config file's
// directory, or "token" in that directory when tokenFile is empty.
func ResolveTokenFile(configPath, tokenFile string) string {
	dir := filepath.Dir(configPath)
	if tokenFile == "" {
		return filepath.Join(dir, "token")
	}
	if filepath.IsAbs(tokenFile) {
		return tokenFile
	}
	return filepath.Join(dir, tokenFile)
}

// DefaultConfigPath is $XDG_CONFIG_HOME/gh-ask/config.toml, or
// $HOME/.config/gh-ask/config.toml.
func DefaultConfigPath(env []string) (string, error) {
	root := lookupEnv(env, "XDG_CONFIG_HOME")
	if root == "" {
		home := lookupEnv(env, "HOME")
		if home == "" {
			return "", errors.New("HOME is not set")
		}
		root = filepath.Join(home, ".config")
	}
	return filepath.Join(root, "gh-ask", "config.toml"), nil
}

func lookupEnv(env []string, key string) string {
	value := ""
	for _, item := range env {
		if k, v, ok := strings.Cut(item, "="); ok && k == key {
			value = v
		}
	}
	return value
}
