package healthsnap

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"regexp"
	"slices"
	"strings"
	"time"

	"github.com/apple/pkl-go/pkl"
)

const (
	// DefaultKeepSnapshots is the number of historical snapshots retained by default.
	DefaultKeepSnapshots = 24
	// DefaultCheckTimeout is the per-check execution timeout when omitted.
	DefaultCheckTimeout = 30 * time.Second
	// DefaultNotifyTimeout is the notifier command timeout when omitted.
	DefaultNotifyTimeout = 10 * time.Second
)

// Config is the validated healthsnap configuration.
type Config struct {
	StateDir      string        `json:"stateDir"`
	KeepSnapshots int           `json:"keepSnapshots"`
	Markdown      bool          `json:"markdown"`
	Notify        NotifyConfig  `json:"notify"`
	Checks        []CheckConfig `json:"checks"`
}

// NotifyConfig configures alert command delivery and judgment routing.
type NotifyConfig struct {
	Command           []string `json:"command"`
	Recipient         string   `json:"recipient"`
	JudgmentCommand   []string `json:"judgmentCommand"`
	JudgmentRecipient string   `json:"judgmentRecipient"`
	Timeout           string   `json:"timeout"`

	timeout time.Duration
}

// TimeoutDuration returns the parsed notification timeout.
func (n NotifyConfig) TimeoutDuration() time.Duration {
	if n.timeout > 0 {
		return n.timeout
	}
	return DefaultNotifyTimeout
}

// ParseConfig configures how stdout or exit status from a command check is parsed.
type ParseConfig struct {
	Mode     ParseMode `json:"mode"`
	JSONPath string    `json:"jsonPath"`
	Regex    string    `json:"regex"`

	compiledJP JSONPath
	compiledRE *regexp.Regexp
}

// RuleConfig defines one threshold rule for a check.
type RuleConfig struct {
	ID        string   `json:"id"`
	Condition string   `json:"condition"`
	Clear     string   `json:"clear"`
	State     Severity `json:"state"`
	Repeat    string   `json:"repeat"`
	Judgment  bool     `json:"judgment"`
	Recipient string   `json:"recipient"`
	Command   []string `json:"command"`
	Message   string   `json:"message"`

	compiledExpr  Expr
	compiledClear Expr
	hasClear      bool
	repeat        time.Duration
}

// RepeatDuration returns the parsed re-alert interval (0 disables repeat alerts).
func (r RuleConfig) RepeatDuration() time.Duration {
	return r.repeat
}

// CheckConfig defines one configured health check.
type CheckConfig struct {
	ID       string            `json:"id"`
	Kind     CheckKind         `json:"kind"`
	Argv     []string          `json:"argv"`
	Dir      string            `json:"dir"`
	Env      map[string]string `json:"env"`
	Timeout  string            `json:"timeout"`
	Interval string            `json:"interval"`
	Path     string            `json:"path"`
	Metric   string            `json:"metric"`
	Parse    ParseConfig       `json:"parse"`
	Rules    []RuleConfig      `json:"rules"`
	Judgment bool              `json:"judgment"`

	timeout  time.Duration
	interval time.Duration
}

// TimeoutDuration returns the parsed check execution timeout.
func (c CheckConfig) TimeoutDuration() time.Duration {
	if c.timeout > 0 {
		return c.timeout
	}
	return DefaultCheckTimeout
}

// IntervalDuration returns the parsed minimum run interval for the check.
func (c CheckConfig) IntervalDuration() time.Duration {
	return c.interval
}

// DefaultConfig returns an empty Config with default retention and timeouts.
func DefaultConfig() Config {
	return Config{
		KeepSnapshots: DefaultKeepSnapshots,
		Notify: NotifyConfig{
			Timeout: "10s",
			timeout: DefaultNotifyTimeout,
		},
	}
}

// LoadConfig evaluates and validates a Pkl configuration file. If optional is
// true and cfgPath does not exist, DefaultConfig is returned.
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
	var cfg Config
	dec := json.NewDecoder(strings.NewReader(text))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&cfg); err != nil {
		return Config{}, fmt.Errorf("decode pkl config: %w", err)
	}
	if err := PrepareConfig(&cfg); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

// PrepareConfig applies defaults, compiles parsers and rule expressions, and
// validates cfg in place.
func PrepareConfig(cfg *Config) error {
	if cfg.KeepSnapshots == 0 {
		cfg.KeepSnapshots = DefaultKeepSnapshots
	}
	if cfg.KeepSnapshots < 0 {
		return errors.New("keepSnapshots must be positive")
	}
	if err := prepareNotify(&cfg.Notify); err != nil {
		return err
	}
	return prepareChecks(cfg.Checks)
}

func prepareNotify(n *NotifyConfig) error {
	for _, arg := range n.Command {
		if arg == "" {
			return errors.New("notify.command contains an empty argument")
		}
	}
	for _, arg := range n.JudgmentCommand {
		if arg == "" {
			return errors.New("notify.judgmentCommand contains an empty argument")
		}
	}
	if n.Timeout == "" {
		n.Timeout = "10s"
	}
	d, err := ParseDuration(n.Timeout)
	if err != nil || d <= 0 {
		return fmt.Errorf("notify.timeout %q must be a positive duration", n.Timeout)
	}
	n.timeout = d
	return nil
}

func prepareChecks(checks []CheckConfig) error {
	seenChecks := make(map[string]bool, len(checks))
	seenRules := make(map[string]bool, len(checks))
	for i := range checks {
		c := &checks[i]
		if err := prepareCheck(c, seenChecks); err != nil {
			return err
		}
		if err := prepareRules(c, seenRules); err != nil {
			return err
		}
	}
	return nil
}

func prepareCheck(c *CheckConfig, seenChecks map[string]bool) error {
	if c.ID == "" {
		return errors.New("check id is empty")
	}
	if seenChecks[c.ID] {
		return fmt.Errorf("duplicate check id %q", c.ID)
	}
	seenChecks[c.ID] = true

	if c.Kind == "" && len(c.Argv) > 0 {
		c.Kind = KindCommand
	}
	if err := parseCheckTimings(c); err != nil {
		return err
	}
	return prepareCheckKind(c)
}

func parseCheckTimings(c *CheckConfig) error {
	if c.Timeout == "" {
		c.Timeout = "30s"
	}
	to, err := ParseDuration(c.Timeout)
	if err != nil || to <= 0 {
		return fmt.Errorf("check %q timeout %q must be positive", c.ID, c.Timeout)
	}
	c.timeout = to

	if c.Interval == "" {
		c.Interval = "0s"
	}
	iv, err := ParseDuration(c.Interval)
	if err != nil {
		return fmt.Errorf("check %q interval %q: %w", c.ID, c.Interval, err)
	}
	c.interval = iv
	return nil
}

func prepareCheckKind(c *CheckConfig) error {
	switch c.Kind {
	case KindCommand:
		if len(c.Argv) == 0 || slices.Contains(c.Argv, "") {
			return fmt.Errorf("check %q requires non-empty argv", c.ID)
		}
		return prepareParser(c.ID, &c.Parse)
	case KindFileAge:
		if strings.TrimSpace(c.Path) == "" {
			return fmt.Errorf("check %q (file-age) requires path", c.ID)
		}
		return nil
	case KindHost:
		return prepareHostCheck(c)
	default:
		return fmt.Errorf("check %q has invalid kind %q", c.ID, c.Kind)
	}
}

func prepareHostCheck(c *CheckConfig) error {
	switch c.Metric {
	case "load", "load1", "load5", "load15", "load-per-cpu",
		"memory", "memory-pressure":
		return nil
	case "disk", "free-disk", "free-disk-gib", "disk-used":
		if strings.TrimSpace(c.Path) == "" {
			c.Path = "/"
		}
		return nil
	default:
		return fmt.Errorf("check %q (host) has unsupported metric %q", c.ID, c.Metric)
	}
}

func prepareParser(checkID string, p *ParseConfig) error {
	if p.Mode == "" {
		switch {
		case p.JSONPath != "":
			p.Mode = ParseJSONPath
		case p.Regex != "":
			p.Mode = ParseRegex
		default:
			p.Mode = ParseExit
		}
	}
	switch p.Mode {
	case ParseExit:
		return nil
	case ParseJSONPath:
		if p.JSONPath == "" {
			return fmt.Errorf("check %q parse.mode jsonpath requires parse.jsonPath", checkID)
		}
		jp, err := CompileJSONPath(p.JSONPath)
		if err != nil {
			return fmt.Errorf("check %q: %w", checkID, err)
		}
		p.compiledJP = jp
		return nil
	case ParseRegex:
		if p.Regex == "" {
			return fmt.Errorf("check %q parse.mode regex requires parse.regex", checkID)
		}
		re, err := regexp.Compile(p.Regex)
		if err != nil {
			return fmt.Errorf("check %q parse.regex: %w", checkID, err)
		}
		p.compiledRE = re
		return nil
	default:
		return fmt.Errorf("check %q has invalid parse.mode %q", checkID, p.Mode)
	}
}

func prepareRules(c *CheckConfig, seenRules map[string]bool) error {
	if len(c.Rules) == 0 {
		if c.Kind != KindCommand || c.Parse.Mode != ParseExit {
			return fmt.Errorf("check %q requires at least one rule", c.ID)
		}
		c.Rules = []RuleConfig{{
			ID:        c.ID,
			Condition: "exit != 0",
			State:     SeverityFail,
		}}
	}
	for i := range c.Rules {
		r := &c.Rules[i]
		if r.ID == "" {
			if len(c.Rules) > 1 {
				return fmt.Errorf("check %q has multiple rules; each rule requires an id", c.ID)
			}
			r.ID = c.ID
		}
		if seenRules[r.ID] {
			return fmt.Errorf("duplicate rule id %q", r.ID)
		}
		seenRules[r.ID] = true

		if err := prepareRule(c.ID, r); err != nil {
			return err
		}
	}
	return nil
}

func prepareRule(checkID string, r *RuleConfig) error {
	if r.State == "" {
		r.State = SeverityFail
	}
	if r.State != SeverityInfo && r.State != SeverityWarn && r.State != SeverityFail {
		return fmt.Errorf("check %q rule %q has invalid state %q (want info, warn, or fail)", checkID, r.ID, r.State)
	}
	for _, arg := range r.Command {
		if arg == "" {
			return fmt.Errorf("check %q rule %q command contains an empty argument", checkID, r.ID)
		}
	}
	compiled, err := CompileExpr(r.Condition)
	if err != nil {
		return fmt.Errorf("check %q rule %q: %w", checkID, r.ID, err)
	}
	r.compiledExpr = compiled
	if r.Clear != "" {
		compiledClear, err := CompileExpr(r.Clear)
		if err != nil {
			return fmt.Errorf("check %q rule %q clear: %w", checkID, r.ID, err)
		}
		r.compiledClear = compiledClear
		r.hasClear = true
	} else {
		r.hasClear = false
	}
	if r.Repeat == "" {
		r.Repeat = "0s"
	}
	rep, err := ParseDuration(r.Repeat)
	if err != nil {
		return fmt.Errorf("check %q rule %q repeat %q: %w", checkID, r.ID, r.Repeat, err)
	}
	r.repeat = rep
	return nil
}
