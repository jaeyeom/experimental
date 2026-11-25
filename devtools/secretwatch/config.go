package secretwatch

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path"
	"regexp"
	"slices"
	"strings"

	"github.com/apple/pkl-go/pkl"
)

// Config is the validated secretwatch configuration.
type Config struct {
	Notify       []string
	Rules        []RuleConfig
	Allowlist    AllowlistConfig
	DisableRules []string
}

// RuleConfig configures one custom secret detection rule.
type RuleConfig struct {
	Name       string
	Pattern    string
	MinEntropy float64
}

// AllowlistConfig configures false-positive suppressions.
type AllowlistConfig struct {
	Patterns []string
	Units    []string
}

type jsonConfig struct {
	Notify       []string      `json:"notify"`
	Rules        []jsonRule    `json:"rules"`
	Allowlist    jsonAllowlist `json:"allowlist"`
	DisableRules []string      `json:"disableRules"`
}

type jsonRule struct {
	Name       string  `json:"name"`
	Pattern    string  `json:"pattern"`
	MinEntropy float64 `json:"minEntropy"`
}

type jsonAllowlist struct {
	Patterns []string `json:"patterns"`
	Units    []string `json:"units"`
}

// DefaultConfig returns a Config using only the built-in rules and no notifier.
func DefaultConfig() Config {
	return Config{}
}

// LoadConfig reads and validates a Pkl configuration file. If optional is true
// and path does not exist, DefaultConfig is returned.
func LoadConfig(ctx context.Context, cfgPath string, optional bool) (Config, error) {
	if _, err := os.Stat(cfgPath); err != nil {
		if optional && errors.Is(err, fs.ErrNotExist) {
			return DefaultConfig(), nil
		}
		return Config{}, fmt.Errorf("stat config %s: %w", cfgPath, err)
	}
	evaluator, err := pkl.NewEvaluator(
		ctx,
		pkl.PreconfiguredOptions,
		func(opts *pkl.EvaluatorOptions) {
			opts.OutputFormat = "json"
		},
	)
	if err != nil {
		return Config{}, fmt.Errorf("create pkl evaluator: %w", err)
	}
	defer func() {
		_ = evaluator.Close()
	}()

	text, err := evaluator.EvaluateOutputText(ctx, pkl.FileSource(cfgPath))
	if err != nil {
		return Config{}, fmt.Errorf("evaluate pkl config: %w", err)
	}
	var raw jsonConfig
	dec := json.NewDecoder(strings.NewReader(text))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&raw); err != nil {
		return Config{}, fmt.Errorf("decode pkl config: %w", err)
	}
	cfg := convertJSONConfig(raw)
	if err := ValidateConfig(cfg); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

func convertJSONConfig(raw jsonConfig) Config {
	cfg := Config{
		Notify:       slices.Clone(raw.Notify),
		Allowlist:    AllowlistConfig{Patterns: slices.Clone(raw.Allowlist.Patterns), Units: slices.Clone(raw.Allowlist.Units)},
		DisableRules: slices.Clone(raw.DisableRules),
	}
	for _, r := range raw.Rules {
		cfg.Rules = append(cfg.Rules, RuleConfig(r))
	}
	return cfg
}

// ValidateConfig checks rule names, regexes, allowlist globs, and notify args.
func ValidateConfig(cfg Config) error {
	for _, arg := range cfg.Notify {
		if arg == "" {
			return errors.New("notify has an empty argument")
		}
	}
	if err := validateRules(cfg.Rules); err != nil {
		return err
	}
	return validateAllowlist(cfg.Allowlist)
}

func validateRules(rules []RuleConfig) error {
	seen := make(map[string]bool, len(rules))
	for _, r := range DefaultRules() {
		seen[r.Name] = true
	}
	for _, r := range rules {
		if r.Name == "" {
			return errors.New("custom rule name is empty")
		}
		if seen[r.Name] {
			return fmt.Errorf("duplicate rule name %q", r.Name)
		}
		seen[r.Name] = true
		if r.Pattern == "" {
			return fmt.Errorf("rule %q pattern is empty", r.Name)
		}
		if r.MinEntropy < 0 {
			return fmt.Errorf("rule %q minEntropy must be non-negative", r.Name)
		}
		if _, err := regexp.Compile(r.Pattern); err != nil {
			return fmt.Errorf("rule %q pattern: %w", r.Name, err)
		}
	}
	return nil
}

func validateAllowlist(allow AllowlistConfig) error {
	for _, pat := range allow.Patterns {
		if pat == "" {
			return errors.New("allowlist pattern is empty")
		}
		if _, err := regexp.Compile(pat); err != nil {
			return fmt.Errorf("allowlist pattern %q: %w", pat, err)
		}
	}
	for _, unit := range allow.Units {
		if unit == "" {
			return errors.New("allowlist unit is empty")
		}
		if _, err := path.Match(unit, "example.service"); err != nil {
			return fmt.Errorf("allowlist unit %q: %w", unit, err)
		}
	}
	return nil
}
