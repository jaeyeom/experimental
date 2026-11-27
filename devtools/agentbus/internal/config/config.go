// Package config reads the optional agentbus config file,
// $XDG_CONFIG_HOME/agentbus/config.toml. It holds a small TOML subset:
// `key = value` lines and # comments, where a value is a quoted string or
// a one-line list of quoted strings.
//
//	machines = ["linux2"]   # saved herdr machines to include
//	remote_timeout = "5s"   # bound on each call to another machine
//	list_liveness_max_age = "15s"  # list refreshes cached liveness older than this
//	hub = "me@hub"          # run every command on this ssh target
//	machine = "linux2"      # this machine's herdr label on the hub
//	ssh_identity_agent = "~/.ssh/agent.sock"  # ssh IdentityAgent for the hub
//	herdr_path = "~/.local/bin/herdr"  # herdr binary; wins over AGENTBUS_HERDR
//	hub_socket = "/run/user/1000/agentbus/hub.sock"  # hub.sock on the hub
//	lease_checkouts = ["/home/u/src/repo"]  # checkouts the lease guards
//	lease_check_interval = "2m"   # how often an overrun holder is checked
//	lease_checkin_repeat = "15m"  # least time between check-ins
//	lease_idle_grace = "5m"       # idle and silent this long: take over
//	lease_takeover = "log"        # "on" carries takeovers out; "log" only logs them
//	lease_ignore_procs = ["zsh", "claude"]  # adopt's process check ignores these
//	hibernate_exempt = ["ops/*"]  # names, handles, or globs hibernate never stops
//	wake_timeout = "60s"          # how long a woken agent has to get ready
//	wake_model = "sonnet"         # model a woken Claude Code agent resumes on
//	wake_model_by_name = {"ops/*" = "keep", "routine/*" = "haiku"}
//	hold_notice_after = "10m"    # one notice after a hold this long; "0s" sends none
//	gone_after = "1h"            # a raw handle gone this long: its message expires
//	hibernate_policy = "dry-run"  # "on" hibernates under memory pressure; "dry-run" only logs
//	hibernate_below = "1.5GiB"    # available memory under which the policy acts
//	hibernate_target = "3GiB"     # available memory at which it stops
//	hibernate_min_idle = "30m"    # least idle time of an agent it stops
//	hibernate_interval = "1m"     # how often it reads the available memory
//	dialog_rules = {"oauth" = 'Paste code'}  # extra dialog regex rules
//	expect_reply_senders = ["ops/reviewer"]  # senders whose messages expect a reply
package config

import (
	"errors"
	"fmt"
	"math"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"time"
)

// Config is the parsed file. The zero value is the default.
type Config struct {
	// Machines are herdr saved-machine labels or ids. Search and
	// inventory include their agents.
	Machines []string
	// RemoteTimeout bounds each call to another machine. Zero is the
	// adapter's default.
	RemoteTimeout time.Duration
	// ListLivenessMaxAge is how old list's cached liveness may get
	// before a list starts a background refresh. Zero is the default.
	ListLivenessMaxAge time.Duration
	// Hub is an ssh target. When set, commands run there instead of
	// against a local daemon. AGENTBUS_HUB overrides it.
	Hub string
	// Machine is this machine's herdr label as the hub knows it. Hub
	// mode builds the caller's herdr:<machine>/<session>/<pane> from it.
	Machine string
	// HubSocket is the hub daemon's hub.sock path on the hub. agentbus
	// install in hub mode forwards the local tunnel socket to it.
	HubSocket string
	// SSHIdentityAgent is ssh's IdentityAgent for calls to the hub.
	// Load expands a leading "~/" against HOME. Empty means ssh's own
	// agent lookup.
	SSHIdentityAgent string
	// SSHAgentSocket is SSH_AUTH_SOCK for agentbusd's remote ssh/herdr
	// calls. Load expands a leading "~/" against HOME. Empty falls back
	// to ~/.ssh/agent.sock, gpg-agent, and inherited SSH_AUTH_SOCK.
	SSHAgentSocket string
	// HerdrPath is the herdr binary. It wins over AGENTBUS_HERDR.
	// Load expands a leading "~/" against HOME.
	HerdrPath string
	// JobsPolicy is the user's jobs gate policy file. Empty is
	// jobs-policy.json beside this file. Load expands a leading "~/"
	// against HOME.
	JobsPolicy string
	// LeaseCheckouts are this machine's guarded checkouts: absolute
	// paths of git top-levels that need the lease before a change.
	LeaseCheckouts []string
	// LeaseCheckInterval is how often the daemon looks at an overrun
	// holder, and how far it extends one that is still working. Zero is
	// the default.
	LeaseCheckInterval time.Duration
	// LeaseCheckinRepeat is the least time between two check-ins to a
	// holder that keeps working. Zero is the default.
	LeaseCheckinRepeat time.Duration
	// LeaseIdleGrace is how long a checked-in holder may stay idle and
	// silent before it is taken over. Zero is the default.
	LeaseIdleGrace time.Duration
	// LeaseIgnoreProcs are the program names whose working directory in
	// a checkout does not block lease adopt. Nil means the default list;
	// a set list replaces it.
	LeaseIgnoreProcs []string
	// LeaseTakeover is lease_takeover as written when it is "log" or
	// "on", else empty. Read it through TakeoverMode.
	LeaseTakeover string
	// HibernateExempt are names, handles, or globs over them that
	// agentbus hibernate never stops.
	HibernateExempt []string
	// WakeTimeout is how long the daemon waits for a hibernated agent it
	// relaunched to get ready. Zero is the default, 60s.
	WakeTimeout time.Duration
	// WakeModel is the default model a woken Claude Code agent resumes on.
	// Empty or "keep" keeps the original model.
	WakeModel string
	// WakeModelByName overrides WakeModel for matching names, handles, or
	// globs, in file order (first match wins).
	WakeModelByName []WakeModelRule
	// HibernatePolicy is hibernate_policy as written when it is "off",
	// "dry-run", or "on", else empty. Read it through Policy.
	HibernatePolicy string
	// HibernateBelow and HibernateTarget are hibernate_below and
	// hibernate_target in bytes. Zero is unset.
	HibernateBelow  int64
	HibernateTarget int64
	// HibernateMinIdle and HibernateInterval are hibernate_min_idle and
	// hibernate_interval. Zero is the default.
	HibernateMinIdle  time.Duration
	HibernateInterval time.Duration
	// HoldNoticeAfter is how long a hold waits before one notice to the
	// sender. Nil means 10 minutes. A pointer to zero disables notices.
	HoldNoticeAfter *time.Duration
	// GoneAfter is how long a message to a raw pane handle waits after
	// its pane is gone before it expires. Nil means one hour. A pointer
	// to zero expires it at the first gone check.
	GoneAfter *time.Duration
	// VerifyDeliveryWithin is how long after delivery the daemon checks
	// a readable transcript for an auth error or no-turn. Nil means 60s.
	// A pointer to zero disables verification.
	VerifyDeliveryWithin *time.Duration
	// AuthErrorPatterns identify an assistant auth-error reply right after
	// delivery (substring-matched on synthetic/API-error turns, whole-turn
	// matched when transcript metadata is absent). Nil uses defaults.
	AuthErrorPatterns []string
	// ComposerPlaceholders are whole-line strings treated as an empty
	// composer when style info is unavailable. Nil uses defaults.
	ComposerPlaceholders []string
	// StaleComposerAfter is how long unchanged composer text holds a
	// message before notifying the sender once per tab. Nil means 10m.
	// A pointer to zero disables the notice.
	StaleComposerAfter *time.Duration
	// HibernateWarnWithin is how recent a delivery must be for
	// hibernating its agent to log a warning. Nil means 60s. A pointer
	// to zero turns the warning off.
	HibernateWarnWithin *time.Duration
	// DialogRules are extra regex rules (on top of the built-in ones)
	// for detecting a dialog or input prompt on an agent pane's screen.
	DialogRules []DialogRule
	// ExpectReplySenders are sender names, handles, or globs whose
	// delivered messages expect a reply unless already replied to.
	ExpectReplySenders []string
	// Warnings are problems that did not fail the load, such as an
	// invalid lease_takeover.
	Warnings []string
}

// DialogRule is a named regular expression rule for dialog detection.
type DialogRule struct {
	Name    string `json:"name"`
	Pattern string `json:"pattern"`
}

// WakeModelRule maps a name, handle, or glob pattern to a wake model.
type WakeModelRule struct {
	Pattern string
	Model   string
}

// ValidateWakeModel rejects an empty model or one starting with "-" to
// prevent flag injection when passed to argv.
func ValidateWakeModel(m string) error {
	if m == "" {
		return errors.New("wake model must not be empty")
	}
	if strings.HasPrefix(m, "-") {
		return errors.New("wake model must not start with -")
	}
	return nil
}

// TakeoverMode is "on" when lease_takeover is exactly "on", else "log":
// a missing or invalid key never turns takeover on.
func (c Config) TakeoverMode() string {
	if c.LeaseTakeover == "on" {
		return "on"
	}
	return "log"
}

// Policy is the memory-pressure hibernation policy with its defaults.
type Policy struct {
	// Mode is "off", "dry-run", or "on".
	Mode string
	// Below is the available memory in bytes under which the policy
	// acts, and Target the available memory at which it stops.
	Below, Target int64
	// MinIdle is how long an agent must have been idle.
	MinIdle time.Duration
	// Interval is how often the available memory is read.
	Interval time.Duration
}

// Policy defaults.
const (
	DefaultHibernateMinIdle  = 30 * time.Minute
	DefaultHibernateInterval = time.Minute
)

// Policy is the policy the daemon runs, or on a hub-mode client
// `agentbus hibernate --policy`. It is off unless
// hibernate_policy is "dry-run" or "on" and hibernate_below is set. The
// target defaults to hibernate_below.
func (c Config) Policy() Policy {
	p := Policy{
		Mode:     "off",
		Below:    c.HibernateBelow,
		Target:   max(c.HibernateTarget, c.HibernateBelow),
		MinIdle:  c.HibernateMinIdle,
		Interval: c.HibernateInterval,
	}
	if (c.HibernatePolicy == "dry-run" || c.HibernatePolicy == "on") && c.HibernateBelow > 0 {
		p.Mode = c.HibernatePolicy
	}
	if p.MinIdle <= 0 {
		p.MinIdle = DefaultHibernateMinIdle
	}
	if p.Interval <= 0 {
		p.Interval = DefaultHibernateInterval
	}
	return p
}

var errLeaseCheckouts = errors.New(`lease_checkouts must be a list of absolute paths, such as ["/home/u/src/repo"]`)

// Path is $XDG_CONFIG_HOME/agentbus/config.toml, or
// $HOME/.config/agentbus/config.toml when XDG_CONFIG_HOME is unset.
func Path(env []string) (string, error) {
	root := envGet(env, "XDG_CONFIG_HOME")
	if root == "" {
		home := envGet(env, "HOME")
		if home == "" {
			return "", errors.New("HOME is not set; the agentbus config belongs in ~/.config/agentbus")
		}
		root = filepath.Join(home, ".config")
	}
	return filepath.Join(root, "agentbus", "config.toml"), nil
}

// Load reads the config file. A missing file is the zero Config.
func Load(env []string) (Config, error) {
	path, err := Path(env)
	if err != nil {
		return Config{}, err
	}
	data, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return Config{}, nil
	}
	if err != nil {
		return Config{}, fmt.Errorf("read config: %w", err)
	}
	cfg, err := Parse(string(data))
	if err != nil {
		return Config{}, fmt.Errorf("%s: %w", path, err)
	}
	if err := cfg.expandIdentity(env); err != nil {
		return Config{}, fmt.Errorf("%s: %w", path, err)
	}
	if err := expandHome(env, "herdr_path", &cfg.HerdrPath); err != nil {
		return Config{}, fmt.Errorf("%s: %w", path, err)
	}
	if err := expandHome(env, "jobs_policy", &cfg.JobsPolicy); err != nil {
		return Config{}, fmt.Errorf("%s: %w", path, err)
	}
	if err := expandHome(env, "ssh_agent_socket", &cfg.SSHAgentSocket); err != nil {
		return Config{}, fmt.Errorf("%s: %w", path, err)
	}
	return cfg, nil
}

// expandHome rewrites a "~/..." path of key against HOME.
func expandHome(env []string, key string, path *string) error {
	if !strings.HasPrefix(*path, "~/") {
		return nil
	}
	home := envGet(env, "HOME")
	if home == "" {
		return fmt.Errorf(`HOME is not set; %s starts with "~/"`, key)
	}
	*path = filepath.Join(home, (*path)[2:])
	return nil
}

// expandIdentity rewrites a "~/..." ssh_identity_agent against HOME.
// An absolute path is left as written.
func (c *Config) expandIdentity(env []string) error {
	path := c.SSHIdentityAgent
	if !strings.HasPrefix(path, "~/") {
		return nil
	}
	home := envGet(env, "HOME")
	if home == "" {
		return errors.New(`HOME is not set; ssh_identity_agent starts with "~/"`)
	}
	c.SSHIdentityAgent = filepath.Join(home, path[2:])
	return nil
}

// Parse reads the config text.
func Parse(text string) (Config, error) {
	var (
		cfg                 Config
		section             string
		seenWakeModelByName bool
		seenDialogRules     bool
	)
	for i, rawLine := range strings.Split(text, "\n") {
		line := strings.TrimSpace(rawLine)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		if err := parseConfigLine(&cfg, line, &section, &seenWakeModelByName, &seenDialogRules); err != nil {
			return Config{}, fmt.Errorf("line %d: %w", i+1, err)
		}
	}
	return finalizeConfig(cfg)
}

func parseConfigLine(cfg *Config, line string, section *string, seenWakeModelByName, seenDialogRules *bool) error {
	if strings.HasPrefix(line, "[") {
		sec, err := parseSectionHeader(line)
		if err != nil {
			return err
		}
		switch sec {
		case "wake_model_by_name":
			if *seenWakeModelByName {
				return errors.New("duplicate wake_model_by_name")
			}
			*seenWakeModelByName = true
		case "dialog_rules":
			if *seenDialogRules {
				return errors.New("duplicate dialog_rules")
			}
			*seenDialogRules = true
		default:
			return fmt.Errorf("unknown section [%s]", sec)
		}
		*section = sec
		return nil
	}
	switch *section {
	case "wake_model_by_name":
		rule, err := parseWakeModelEntry(line)
		if err != nil {
			return err
		}
		cfg.WakeModelByName = append(cfg.WakeModelByName, rule)
		return nil
	case "dialog_rules":
		rule, err := parseDialogRuleEntry(line)
		if err != nil {
			return err
		}
		cfg.DialogRules = append(cfg.DialogRules, rule)
		return nil
	}
	return parseLine(cfg, line, seenWakeModelByName, seenDialogRules)
}

func finalizeConfig(cfg Config) (Config, error) {
	if cfg.HibernateTarget > 0 && cfg.HibernateTarget < cfg.HibernateBelow {
		return Config{}, errors.New("hibernate_target must be at least hibernate_below")
	}
	if (cfg.HibernatePolicy == "dry-run" || cfg.HibernatePolicy == "on") && cfg.HibernateBelow == 0 {
		cfg.Warnings = append(cfg.Warnings, fmt.Sprintf(`hibernate_policy = %q needs hibernate_below; using "off"`, cfg.HibernatePolicy))
	}
	return cfg, nil
}

func parseLine(cfg *Config, line string, seenWakeModelByName, seenDialogRules *bool) error {
	key, value, ok := strings.Cut(line, "=")
	if !ok {
		return errors.New("want key = value")
	}
	key = strings.TrimSpace(key)
	value = strings.TrimSpace(value)
	switch key {
	case "machines":
		return parseMachines(cfg, value)
	case "remote_timeout", "list_liveness_max_age", "lease_check_interval", "lease_checkin_repeat", "lease_idle_grace", "wake_timeout",
		"hibernate_min_idle", "hibernate_interval", "hold_notice_after", "gone_after", "verify_delivery_within", "stale_composer_after", "hibernate_warn_within":
		return parseDurationKey(cfg, key, value)
	case "auth_error_patterns", "composer_placeholders", "expect_reply_senders":
		return parsePatternListKey(cfg, key, value)
	case "dialog_rules":
		return parseUniqueKey(seenDialogRules, "dialog_rules", func() error { return parseDialogRules(cfg, value) })
	case "wake_model":
		return parseWakeModel(cfg, value)
	case "wake_model_by_name":
		return parseUniqueKey(seenWakeModelByName, "wake_model_by_name", func() error { return parseWakeModelByName(cfg, value) })
	case "hibernate_policy", "hibernate_below", "hibernate_target", "hibernate_exempt":
		return parseHibernateKey(cfg, key, value)
	case "lease_checkouts", "lease_takeover", "lease_ignore_procs":
		return parseLeaseKey(cfg, key, value)
	case "hub", "machine", "hub_socket", "ssh_identity_agent", "ssh_agent_socket", "herdr_path", "jobs_policy":
		return parseHubKey(cfg, key, value)
	default:
		return fmt.Errorf("unknown key %s", key)
	}
}

func parseUniqueKey(seen *bool, name string, parse func() error) error {
	if *seen {
		return fmt.Errorf("duplicate %s", name)
	}
	*seen = true
	return parse()
}

func parsePatternListKey(cfg *Config, key, value string) error {
	switch key {
	case "auth_error_patterns":
		return parseStringList(&cfg.AuthErrorPatterns, key, value, `["Login expired", "Please run /login"]`)
	case "composer_placeholders":
		return parseStringList(&cfg.ComposerPlaceholders, key, value, `["Once fixed, resend your message"]`)
	default:
		return parseStringList(&cfg.ExpectReplySenders, key, value, `["ops/reviewer", "ask/*"]`)
	}
}

func parseLeaseKey(cfg *Config, key, value string) error {
	switch key {
	case "lease_checkouts":
		return parseLeaseCheckouts(cfg, value)
	case "lease_takeover":
		parseLeaseTakeover(cfg, value)
		return nil
	default:
		return parseLeaseIgnoreProcs(cfg, value)
	}
}

func parseHibernateKey(cfg *Config, key, value string) error {
	switch key {
	case "hibernate_policy":
		parseHibernatePolicy(cfg, value)
		return nil
	case "hibernate_below":
		return parseSizeKey(&cfg.HibernateBelow, key, value)
	case "hibernate_target":
		return parseSizeKey(&cfg.HibernateTarget, key, value)
	default:
		return parseHibernateExempt(cfg, value)
	}
}

// parseWakeModel reads wake_model, a non-empty model string not starting with -.
func parseWakeModel(cfg *Config, value string) error {
	raw, rest, ok := parseString(value)
	if !ok || !blankOrComment(rest) {
		return errors.New(`wake_model must be a string, such as "sonnet"`)
	}
	if err := ValidateWakeModel(raw); err != nil {
		return fmt.Errorf("wake_model: %w", err)
	}
	cfg.WakeModel = raw
	return nil
}

// parseWakeModelByName reads wake_model_by_name = {"ops/*" = "keep", ...}.
func parseWakeModelByName(cfg *Config, value string) error {
	rest, ok := strings.CutPrefix(value, "{")
	if !ok {
		return errors.New(`wake_model_by_name must be an inline table, such as {"ops/*" = "keep"}`)
	}
	for {
		rest = strings.TrimSpace(rest)
		if after, done := strings.CutPrefix(rest, "}"); done {
			if !blankOrComment(after) {
				return errors.New(`wake_model_by_name must be an inline table, such as {"ops/*" = "keep"}`)
			}
			return nil
		}
		pattern, after, ok := parseTableKey(rest)
		if !ok || pattern == "" {
			return errors.New(`wake_model_by_name keys must be non-empty strings, such as "ops/*"`)
		}
		after = strings.TrimSpace(after)
		after, ok = strings.CutPrefix(after, "=")
		if !ok {
			return errors.New(`wake_model_by_name entries must be pattern = "model"`)
		}
		model, after, ok := parseString(strings.TrimSpace(after))
		if !ok {
			return errors.New(`wake_model_by_name values must be quoted strings, such as "sonnet"`)
		}
		if err := ValidateWakeModel(model); err != nil {
			return fmt.Errorf("wake_model_by_name[%q]: %w", pattern, err)
		}
		cfg.WakeModelByName = append(cfg.WakeModelByName, WakeModelRule{Pattern: pattern, Model: model})
		rest = strings.TrimSpace(after)
		if after, more := strings.CutPrefix(rest, ","); more {
			rest = after
		} else if !strings.HasPrefix(rest, "}") {
			return errors.New(`wake_model_by_name must be an inline table, such as {"ops/*" = "keep"}`)
		}
	}
}

func parseSectionHeader(line string) (string, error) {
	rest, ok := strings.CutPrefix(line, "[")
	if !ok {
		return "", errors.New("expected [section]")
	}
	sec, after, ok := strings.Cut(rest, "]")
	if !ok || !blankOrComment(after) {
		return "", errors.New("expected [section]")
	}
	sec = strings.TrimSpace(sec)
	if sec == "" {
		return "", errors.New("empty section name")
	}
	return sec, nil
}

func parseWakeModelEntry(line string) (WakeModelRule, error) {
	pattern, after, ok := parseTableKey(line)
	if !ok || pattern == "" {
		return WakeModelRule{}, errors.New(`wake_model_by_name keys must be non-empty strings, such as "ops/*"`)
	}
	after = strings.TrimSpace(after)
	after, ok = strings.CutPrefix(after, "=")
	if !ok {
		return WakeModelRule{}, errors.New("want key = value")
	}
	model, rest, ok := parseString(strings.TrimSpace(after))
	if !ok || !blankOrComment(rest) {
		return WakeModelRule{}, errors.New(`wake_model_by_name values must be quoted strings, such as "sonnet"`)
	}
	if err := ValidateWakeModel(model); err != nil {
		return WakeModelRule{}, fmt.Errorf("wake_model_by_name[%q]: %w", pattern, err)
	}
	return WakeModelRule{Pattern: pattern, Model: model}, nil
}

// ValidateDialogRule validates a dialog regex pattern.
func ValidateDialogRule(name, pattern string) error {
	if name == "" {
		return errors.New("dialog_rules rule name must not be empty")
	}
	if pattern == "" {
		return fmt.Errorf("dialog_rules[%q]: regex pattern must not be empty", name)
	}
	if _, err := regexp.Compile(pattern); err != nil {
		return fmt.Errorf("dialog_rules[%q]: invalid regex %q: %w", name, pattern, err)
	}
	return nil
}

func parseDialogRules(cfg *Config, value string) error {
	switch {
	case strings.HasPrefix(value, "["):
		return parseDialogRuleList(cfg, value)
	case strings.HasPrefix(value, "{"):
		return parseDialogRuleTable(cfg, value)
	default:
		return errors.New(`dialog_rules must be a list of regex strings or an inline table, such as {"trust" = "Do you trust"}`)
	}
}

func parseDialogRuleList(cfg *Config, value string) error {
	rest, _ := strings.CutPrefix(value, "[")
	for {
		rest = strings.TrimSpace(rest)
		if after, done := strings.CutPrefix(rest, "]"); done {
			if !blankOrComment(after) {
				return errors.New(`dialog_rules must be a list of regex strings, such as ["Do you trust"]`)
			}
			return nil
		}
		pat, after, ok := parsePatternString(rest)
		if !ok || pat == "" {
			return errors.New(`dialog_rules entries must be non-empty quoted regex strings`)
		}
		if err := ValidateDialogRule(pat, pat); err != nil {
			return err
		}
		cfg.DialogRules = append(cfg.DialogRules, DialogRule{Name: pat, Pattern: pat})
		rest = strings.TrimSpace(after)
		if after, more := strings.CutPrefix(rest, ","); more {
			rest = after
		} else if !strings.HasPrefix(rest, "]") {
			return errors.New(`dialog_rules must be a list of regex strings, such as ["Do you trust"]`)
		}
	}
}

func parseDialogRuleTable(cfg *Config, value string) error {
	rest, _ := strings.CutPrefix(value, "{")
	for {
		rest = strings.TrimSpace(rest)
		if after, done := strings.CutPrefix(rest, "}"); done {
			if !blankOrComment(after) {
				return errors.New(`dialog_rules must be an inline table, such as {"trust" = "Do you trust"}`)
			}
			return nil
		}
		name, after, ok := parseTableKey(rest)
		if !ok || name == "" {
			return errors.New(`dialog_rules keys must be non-empty strings, such as "trust"`)
		}
		after = strings.TrimSpace(after)
		after, ok = strings.CutPrefix(after, "=")
		if !ok {
			return errors.New(`dialog_rules entries must be name = "regex"`)
		}
		pat, after, ok := parsePatternString(strings.TrimSpace(after))
		if !ok {
			return errors.New(`dialog_rules values must be quoted regex strings, such as "Do you trust"`)
		}
		if err := ValidateDialogRule(name, pat); err != nil {
			return err
		}
		cfg.DialogRules = append(cfg.DialogRules, DialogRule{Name: name, Pattern: pat})
		rest = strings.TrimSpace(after)
		if after, more := strings.CutPrefix(rest, ","); more {
			rest = after
		} else if !strings.HasPrefix(rest, "}") {
			return errors.New(`dialog_rules must be an inline table, such as {"trust" = "Do you trust"}`)
		}
	}
}

func parseDialogRuleEntry(line string) (DialogRule, error) {
	name, after, ok := parseTableKey(line)
	if !ok || name == "" {
		return DialogRule{}, errors.New(`dialog_rules keys must be non-empty strings, such as "trust"`)
	}
	after = strings.TrimSpace(after)
	after, ok = strings.CutPrefix(after, "=")
	if !ok {
		return DialogRule{}, errors.New("want key = value")
	}
	pat, rest, ok := parsePatternString(strings.TrimSpace(after))
	if !ok || !blankOrComment(rest) {
		return DialogRule{}, errors.New(`dialog_rules values must be quoted regex strings, such as "Do you trust"`)
	}
	if err := ValidateDialogRule(name, pat); err != nil {
		return DialogRule{}, err
	}
	return DialogRule{Name: name, Pattern: pat}, nil
}

// parsePatternString reads a TOML single-quoted literal string ('...') or
// double-quoted basic string ("...", supporting escapes such as \\ and \").
func parsePatternString(value string) (string, string, bool) {
	if rest, ok := strings.CutPrefix(value, "'"); ok {
		end := strings.IndexByte(rest, '\'')
		if end < 0 {
			return "", "", false
		}
		return rest[:end], rest[end+1:], true
	}
	if !strings.HasPrefix(value, `"`) {
		return "", "", false
	}
	for i := 1; i < len(value); i++ {
		if value[i] == '\\' {
			i++
			continue
		}
		if value[i] == '"' {
			unquoted, err := strconv.Unquote(value[:i+1])
			if err != nil {
				return "", "", false
			}
			return unquoted, value[i+1:], true
		}
	}
	return "", "", false
}

// parseTableKey reads either a "quoted" key or a bare TOML key [A-Za-z0-9_-]+.
func parseTableKey(s string) (string, string, bool) {
	if strings.HasPrefix(s, `"`) {
		return parseString(s)
	}
	i := 0
	for i < len(s) {
		c := s[i]
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '-' {
			i++
		} else {
			break
		}
	}
	if i == 0 {
		return "", "", false
	}
	return s[:i], s[i:], true
}

// parseOptionalDuration reads a duration of at least 0s into dst. A
// missing key leaves dst nil, which means the key's default.
func parseOptionalDuration(dst **time.Duration, key, value, example string) error {
	raw, rest, ok := parseString(value)
	d, err := time.ParseDuration(raw)
	if !ok || !blankOrComment(rest) || err != nil || d < 0 {
		return fmt.Errorf("%s must be a duration of at least 0s, such as %q", key, example)
	}
	*dst = &d
	return nil
}

// parseHibernateExempt reads hibernate_exempt, a list of names,
// handles, or globs.
func parseHibernateExempt(cfg *Config, value string) error {
	exempt, ok := parseList(value)
	if !ok {
		return errors.New(`hibernate_exempt must be a list of names, handles, or globs, such as ["ops/*"]`)
	}
	cfg.HibernateExempt = exempt
	return nil
}

// parseLeaseIgnoreProcs reads lease_ignore_procs, a list of program
// names.
func parseLeaseIgnoreProcs(cfg *Config, value string) error {
	names, ok := parseList(value)
	if !ok || slices.ContainsFunc(names, func(n string) bool { return strings.Contains(n, "/") }) {
		return errors.New(`lease_ignore_procs must be a list of program names, such as ["zsh", "claude"]`)
	}
	cfg.LeaseIgnoreProcs = names
	return nil
}

func parseStringList(dst *[]string, key, value, example string) error {
	items, ok := parseList(value)
	if !ok {
		return fmt.Errorf("%s must be a list of strings, such as %s", key, example)
	}
	*dst = items
	return nil
}

// parseDurationKey reads one of the duration keys.
func parseDurationKey(cfg *Config, key, value string) error {
	switch key {
	case "remote_timeout":
		return parseDuration(&cfg.RemoteTimeout, key, value, "5s")
	case "list_liveness_max_age":
		return parseDuration(&cfg.ListLivenessMaxAge, key, value, "15s")
	case "lease_check_interval":
		return parseDuration(&cfg.LeaseCheckInterval, key, value, "2m")
	case "lease_checkin_repeat":
		return parseDuration(&cfg.LeaseCheckinRepeat, key, value, "15m")
	case "lease_idle_grace":
		return parseDuration(&cfg.LeaseIdleGrace, key, value, "5m")
	case "hibernate_min_idle":
		return parseDuration(&cfg.HibernateMinIdle, key, value, "30m")
	case "hibernate_interval":
		return parseDuration(&cfg.HibernateInterval, key, value, "1m")
	case "hold_notice_after":
		return parseOptionalDuration(&cfg.HoldNoticeAfter, key, value, "10m")
	case "gone_after":
		return parseOptionalDuration(&cfg.GoneAfter, key, value, "1h")
	case "verify_delivery_within":
		return parseOptionalDuration(&cfg.VerifyDeliveryWithin, key, value, "60s")
	case "stale_composer_after":
		return parseOptionalDuration(&cfg.StaleComposerAfter, key, value, "10m")
	case "hibernate_warn_within":
		return parseOptionalDuration(&cfg.HibernateWarnWithin, key, value, "60s")
	default:
		return parseDuration(&cfg.WakeTimeout, key, value, "60s")
	}
}

// parseDuration reads a positive duration string into dst.
func parseDuration(dst *time.Duration, key, value, example string) error {
	raw, rest, ok := parseString(value)
	d, err := time.ParseDuration(raw)
	if !ok || !blankOrComment(rest) || err != nil || d <= 0 {
		return fmt.Errorf("%s must be a positive duration, such as %q", key, example)
	}
	*dst = d
	return nil
}

// parseHibernatePolicy reads hibernate_policy, "off", "dry-run", or
// "on". Any other value is a warning and means "off".
func parseHibernatePolicy(cfg *Config, value string) {
	raw, rest, ok := parseString(value)
	if ok && blankOrComment(rest) && (raw == "off" || raw == "dry-run" || raw == "on") {
		cfg.HibernatePolicy = raw
		return
	}
	cfg.HibernatePolicy = ""
	cfg.Warnings = append(cfg.Warnings, fmt.Sprintf(`hibernate_policy = %s is not "off", "dry-run", or "on"; using "off"`, value))
}

// parseSizeKey reads a quoted size into dst.
func parseSizeKey(dst *int64, key, value string) error {
	raw, rest, ok := parseString(value)
	n, err := parseSize(raw)
	if !ok || !blankOrComment(rest) || err != nil {
		return fmt.Errorf(`%s must be a size such as "1.5GiB"`, key)
	}
	*dst = n
	return nil
}

var sizeRE = regexp.MustCompile(`(?i)^(\d+(?:\.\d+)?)\s*([kmgt]ib|b)?$`)

var sizeUnits = map[string]float64{"": 1, "b": 1, "kib": 1 << 10, "mib": 1 << 20, "gib": 1 << 30, "tib": 1 << 40}

// parseSize reads a positive byte count with an optional binary unit,
// such as "1.5GiB", "512 MiB", or "2048". Unit case is ignored.
func parseSize(s string) (int64, error) {
	m := sizeRE.FindStringSubmatch(strings.TrimSpace(s))
	if m == nil {
		return 0, fmt.Errorf("size %q: want a number and B, KiB, MiB, GiB, or TiB", s)
	}
	n, err := strconv.ParseFloat(m[1], 64)
	if err != nil {
		return 0, fmt.Errorf("size %q: %w", s, err)
	}
	unit := sizeUnits[strings.ToLower(m[2])]
	bytes := n * unit
	if bytes < 1 || bytes > math.MaxInt64/2 || (unit == 1 && bytes != math.Trunc(bytes)) {
		return 0, fmt.Errorf("size %q: out of range", s)
	}
	return int64(bytes), nil
}

// parseLeaseTakeover reads lease_takeover, "log" or "on". Any other
// value is a warning and means "log", so the daemon still starts with
// takeover off.
func parseLeaseTakeover(cfg *Config, value string) {
	raw, rest, ok := parseString(value)
	if ok && blankOrComment(rest) && (raw == "log" || raw == "on") {
		cfg.LeaseTakeover = raw
		return
	}
	cfg.LeaseTakeover = ""
	cfg.Warnings = append(cfg.Warnings, fmt.Sprintf(`lease_takeover = %s is not "log" or "on"; using "log"`, value))
}

// parseMachines reads machines, a list of saved-machine labels.
func parseMachines(cfg *Config, value string) error {
	machines, ok := parseList(value)
	if !ok {
		return errors.New("machines must be a list of strings, such as [\"linux2\"]")
	}
	for _, m := range machines {
		if m == "local" {
			return errors.New("machines must not list local; it is always included")
		}
	}
	cfg.Machines = machines
	return nil
}

// parseLeaseCheckouts reads lease_checkouts, a list of absolute paths.
func parseLeaseCheckouts(cfg *Config, value string) error {
	paths, ok := parseList(value)
	if !ok {
		return errLeaseCheckouts
	}
	for i, p := range paths {
		if !filepath.IsAbs(p) {
			return errLeaseCheckouts
		}
		paths[i] = filepath.Clean(p)
	}
	cfg.LeaseCheckouts = paths
	return nil
}

// parseHubSocket reads hub_socket, a path on the hub.
func parseHubSocket(cfg *Config, value string) error {
	path, err := parseName("hub_socket", value)
	switch {
	case err != nil:
		return err
	case !strings.HasPrefix(path, "/"):
		return errors.New("hub_socket must be an absolute path on the hub")
	case strings.Contains(path, ":"):
		// ssh -L separates the two socket paths with a colon.
		return errors.New("hub_socket must not contain a colon")
	}
	cfg.HubSocket = path
	return nil
}

// parseIdentityAgent reads ssh_identity_agent. The path is absolute
// or starts with "~/", which Load expands against HOME.
func parseIdentityAgent(cfg *Config, value string) error {
	path, err := parseName("ssh_identity_agent", value)
	if err != nil {
		return err
	}
	if !filepath.IsAbs(path) && !strings.HasPrefix(path, "~/") {
		return errors.New(`ssh_identity_agent must be an absolute path or start with "~/", such as "~/.ssh/agent.sock"`)
	}
	cfg.SSHIdentityAgent = path
	return nil
}

// parseHomePath reads key into dst: an absolute path or one starting
// with "~/", such as example.
func parseHomePath(dst *string, key, value, example string) error {
	path, err := parseName(key, value)
	if err != nil {
		return err
	}
	if !filepath.IsAbs(path) && !strings.HasPrefix(path, "~/") {
		return fmt.Errorf(`%s must be an absolute path or start with "~/", such as %q`, key, example)
	}
	*dst = path
	return nil
}

// parseHubKey reads hub, machine, hub_socket, ssh_identity_agent, and
// the paths herdr_path and jobs_policy.
func parseHubKey(cfg *Config, key, value string) error {
	if key == "jobs_policy" {
		return parseHomePath(&cfg.JobsPolicy, key, value, "~/.config/agentbus/jobs-policy.json")
	}
	if key == "herdr_path" {
		return parseHomePath(&cfg.HerdrPath, key, value, "~/.local/bin/herdr")
	}
	if key == "ssh_identity_agent" {
		return parseIdentityAgent(cfg, value)
	}
	if key == "ssh_agent_socket" {
		return parseHomePath(&cfg.SSHAgentSocket, key, value, "~/.ssh/agent.sock")
	}
	if key == "hub_socket" {
		return parseHubSocket(cfg, value)
	}
	name, err := parseName(key, value)
	if err != nil {
		return err
	}
	switch {
	case key == "hub" && strings.HasPrefix(name, "-"):
		return errors.New("hub must not start with -")
	case key == "hub":
		cfg.Hub = name
	case name == "local":
		return errors.New("machine must name this machine, not local")
	case strings.Contains(name, "/"):
		return errors.New("machine must not contain /")
	default:
		cfg.Machine = name
	}
	return nil
}

// parseName reads one non-empty string followed by an optional comment.
func parseName(key, value string) (string, error) {
	raw, rest, ok := parseString(value)
	if !ok || raw == "" || !blankOrComment(rest) {
		return "", fmt.Errorf("%s must be a non-empty string, such as \"linux2\"", key)
	}
	return raw, nil
}

// parseList reads ["a", "b"] followed by an optional comment.
// Every item is a non-empty string.
func parseList(value string) ([]string, bool) {
	rest, ok := strings.CutPrefix(value, "[")
	if !ok {
		return nil, false
	}
	out := []string{}
	for {
		rest = strings.TrimSpace(rest)
		if after, done := strings.CutPrefix(rest, "]"); done {
			return out, blankOrComment(after)
		}
		item, after, ok := parseString(rest)
		if !ok || item == "" {
			return nil, false
		}
		out = append(out, item)
		rest = strings.TrimSpace(after)
		if after, more := strings.CutPrefix(rest, ","); more {
			rest = after
		} else if !strings.HasPrefix(rest, "]") {
			return nil, false
		}
	}
}

// parseString reads one "quoted" string without escapes and returns the
// text after it.
func parseString(value string) (string, string, bool) {
	rest, ok := strings.CutPrefix(value, `"`)
	if !ok {
		return "", "", false
	}
	end := strings.IndexByte(rest, '"')
	if end < 0 || strings.ContainsRune(rest[:end], '\\') {
		return "", "", false
	}
	return rest[:end], rest[end+1:], true
}

func blankOrComment(s string) bool {
	s = strings.TrimSpace(s)
	return s == "" || strings.HasPrefix(s, "#")
}

func envGet(env []string, key string) string {
	for _, entry := range env {
		name, value, ok := strings.Cut(entry, "=")
		if ok && name == key {
			return value
		}
	}
	return ""
}

// HubSettings reads the config file when HOME/XDG_CONFIG_HOME is set
// and applies the AGENTBUS_HUB override.
func HubSettings(env []string) (Config, error) {
	var cfg Config
	if _, err := Path(env); err == nil {
		if cfg, err = Load(env); err != nil {
			return Config{}, fmt.Errorf("config: %w", err)
		}
	}
	if hub := strings.TrimSpace(envGet(env, "AGENTBUS_HUB")); hub != "" {
		if strings.HasPrefix(hub, "-") {
			return Config{}, errors.New("AGENTBUS_HUB must not start with -")
		}
		cfg.Hub = hub
	}
	return cfg, nil
}

// RefuseHubDaemon returns the standard error when a hub-mode client
// attempts to start a local agentbusd.
func RefuseHubDaemon(hub string) error {
	return fmt.Errorf("hub mode: not starting a local agentbusd; the hub is %s", hub)
}
