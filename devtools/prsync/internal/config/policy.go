package config

import (
	"fmt"
	"regexp"
	"strings"
	"time"
)

// Policy mode values for per-kind auto-dispatch.
const (
	PolicyOff    = "off"
	PolicyDryRun = "dry-run"
	PolicyAuto   = "auto"
)

// Agent backend names.
const (
	BackendHerdr   = "herdr"
	BackendCommand = "command"
)

// Lease provider names.
const (
	LeaseOff      = "off"
	LeaseFile     = "file"
	LeaseAgentbus = "agentbus"
	LeaseCommand  = "command"
)

const defaultNitReplyPromptTemplate = `Reply to the nit / non-blocking bot review threads on PR #{number} ({url}). State: {state}.

This is a reply-only dispatch: do NOT edit code, commit, or push.

Unaddressed bot nit threads:
{comments}

For each listed thread, post a concise reply on that review thread (not as a top-level PR comment) acknowledging or answering the nit, and mark the thread resolved if appropriate. Do not touch other PRs or human reviewer threads.`

const defaultNitReplyPattern = `(?i)\b(nit|nitpick|non-blocking|optional|minor)\b`

// PolicyConfig holds the configurable auto-dispatch policy, guards, nit-reply
// rules, dispatch ledger, agent backend, and lease hook settings.
type PolicyConfig struct {
	// Per-kind modes: off | dry-run | auto.
	CatchUp  string
	Rebase   string
	NitReply string
	CIFix    string
	QueueFix string

	// DispatchConflicting allows auto mode to dispatch conflicting PRs for
	// rebase / catch-up. Default false: conflicting is reported, not dispatched.
	DispatchConflicting bool

	// Guards.
	RequireIdle      bool
	MaxPerRun        int
	DailyCap         int
	SkipDrafts       bool
	HoldLabels       []string
	SkipStacked      bool
	DefaultBranches  []string
	SkipOtherMachine bool
	GuardCommand     string

	// Nit-reply rules.
	NitReplyBots           []string
	NitReplyPatterns       []*regexp.Regexp
	NitReplyPromptTemplate string

	// Dispatch ledger.
	LedgerFile string

	// Pluggable agent-messaging backend.
	AgentBackend    string
	AgentBackendCmd string

	// Pluggable lease hook.
	LeaseProvider   string
	LeaseName       string
	LeaseTimeout    time.Duration
	LeaseDir        string
	LeaseClaimCmd   string
	LeaseReleaseCmd string

	// Optional custom failure-marker template.
	FailureMarkerTemplate string
}

func defaultPolicyConfig() PolicyConfig {
	return PolicyConfig{
		CatchUp:                PolicyOff,
		Rebase:                 PolicyOff,
		NitReply:               PolicyOff,
		CIFix:                  PolicyOff,
		QueueFix:               PolicyOff,
		DispatchConflicting:    false,
		RequireIdle:            true,
		MaxPerRun:              1,
		DailyCap:               0,
		SkipDrafts:             true,
		SkipStacked:            true,
		DefaultBranches:        []string{"main", "master"},
		SkipOtherMachine:       true,
		NitReplyPatterns:       []*regexp.Regexp{regexp.MustCompile(defaultNitReplyPattern)},
		NitReplyPromptTemplate: defaultNitReplyPromptTemplate,
		LedgerFile:             "~/.config/prsync/ledger.jsonl",
		AgentBackend:           BackendHerdr,
		LeaseProvider:          LeaseOff,
		LeaseTimeout:           15 * time.Minute,
		LeaseDir:               "~/.config/prsync/leases",
	}
}

// ModeFor returns the configured mode (off | dry-run | auto) for a policy kind.
func (p PolicyConfig) ModeFor(kind string) string {
	switch kind {
	case "catch-up":
		return p.CatchUp
	case "rebase":
		return p.Rebase
	case "nit-reply":
		return p.NitReply
	case "ci-fix":
		return p.CIFix
	case "queue-fix":
		return p.QueueFix
	default:
		return PolicyOff
	}
}

func expandPolicyPaths(p *PolicyConfig) {
	p.LedgerFile = expandTilde(p.LedgerFile)
	p.LeaseDir = expandTilde(p.LeaseDir)
	p.AgentBackendCmd = expandTilde(p.AgentBackendCmd)
}

func validatePolicy(p PolicyConfig) error {
	if err := validatePolicyModes(p); err != nil {
		return err
	}
	if err := validatePolicyBackend(p); err != nil {
		return err
	}
	if err := validatePolicyLease(p); err != nil {
		return err
	}
	return validatePolicyGuards(p)
}

func validatePolicyModes(p PolicyConfig) error {
	for _, item := range []struct {
		key string
		val string
	}{
		{"policy_catch_up", p.CatchUp},
		{"policy_rebase", p.Rebase},
		{"policy_nit_reply", p.NitReply},
		{"policy_ci_fix", p.CIFix},
		{"policy_queue_fix", p.QueueFix},
	} {
		if item.val != PolicyOff && item.val != PolicyDryRun && item.val != PolicyAuto {
			return &KeyError{Key: item.key, Reason: fmt.Sprintf("invalid mode %q (want off, dry-run, or auto)", item.val)}
		}
	}
	return nil
}

func validatePolicyBackend(p PolicyConfig) error {
	if p.AgentBackend != BackendHerdr && p.AgentBackend != BackendCommand {
		return &KeyError{Key: "agent_backend", Reason: fmt.Sprintf("invalid backend %q (want herdr or command)", p.AgentBackend)}
	}
	if p.AgentBackend == BackendCommand && strings.TrimSpace(p.AgentBackendCmd) == "" {
		return &KeyError{Key: "agent_backend_cmd", Reason: "required when agent_backend=command"}
	}
	return nil
}

func validatePolicyLease(p PolicyConfig) error {
	switch p.LeaseProvider {
	case LeaseOff, LeaseFile, LeaseAgentbus, LeaseCommand:
	default:
		return &KeyError{Key: "lease_provider", Reason: fmt.Sprintf("invalid provider %q (want off, file, agentbus, or command)", p.LeaseProvider)}
	}
	if p.LeaseProvider == LeaseCommand && strings.TrimSpace(p.LeaseClaimCmd) == "" {
		return &KeyError{Key: "lease_claim_cmd", Reason: "required when lease_provider=command"}
	}
	if p.LeaseTimeout <= 0 {
		return &KeyError{Key: "lease_timeout", Reason: "must be > 0"}
	}
	return nil
}

func validatePolicyGuards(p PolicyConfig) error {
	if p.MaxPerRun < 0 {
		return &KeyError{Key: "guard_max_per_run", Reason: "must be >= 0"}
	}
	if p.DailyCap < 0 {
		return &KeyError{Key: "guard_daily_cap", Reason: "must be >= 0"}
	}
	if p.LedgerFile == "" {
		return &KeyError{Key: "dispatch_ledger_file", Reason: "must be non-empty"}
	}
	return nil
}

func applyPolicyKey(cfg *Config, key, val string) error {
	p := &cfg.Policy
	switch {
	case strings.HasPrefix(key, "policy_"):
		return applyPolicyModeKey(p, key, val)
	case strings.HasPrefix(key, "guard_"):
		return applyPolicyGuardKey(p, key, val)
	case strings.HasPrefix(key, "nit_reply_"):
		return applyPolicyNitKey(p, key, val)
	default:
		return applyPolicyInfraKey(p, key, val)
	}
}

func applyPolicyModeKey(p *PolicyConfig, key, val string) error {
	switch key {
	case "policy_catch_up":
		p.CatchUp = strings.TrimSpace(val)
	case "policy_rebase":
		p.Rebase = strings.TrimSpace(val)
	case "policy_nit_reply":
		p.NitReply = strings.TrimSpace(val)
	case "policy_ci_fix":
		p.CIFix = strings.TrimSpace(val)
	case "policy_queue_fix":
		p.QueueFix = strings.TrimSpace(val)
	case "policy_dispatch_conflicting":
		b, err := parseBool(key, val)
		p.DispatchConflicting = b
		return err
	}
	return nil
}

func applyPolicyGuardKey(p *PolicyConfig, key, val string) error {
	switch key {
	case "guard_require_idle":
		b, err := parseBool(key, val)
		p.RequireIdle = b
		return err
	case "guard_max_per_run":
		n, err := parseNonNegativeInt(key, val)
		p.MaxPerRun = n
		return err
	case "guard_daily_cap":
		n, err := parseNonNegativeInt(key, val)
		p.DailyCap = n
		return err
	case "guard_skip_drafts":
		b, err := parseBool(key, val)
		p.SkipDrafts = b
		return err
	case "guard_hold_labels":
		p.HoldLabels = splitCSV(val)
	case "guard_skip_stacked":
		b, err := parseBool(key, val)
		p.SkipStacked = b
		return err
	case "guard_default_branches":
		p.DefaultBranches = splitCSV(val)
	case "guard_skip_other_machine":
		b, err := parseBool(key, val)
		p.SkipOtherMachine = b
		return err
	case "guard_command":
		p.GuardCommand = strings.TrimSpace(val)
	}
	return nil
}

func applyPolicyNitKey(p *PolicyConfig, key, val string) error {
	switch key {
	case "nit_reply_bots":
		p.NitReplyBots = splitCSV(val)
	case "nit_reply_patterns":
		pats, err := compileCSVPatterns(key, val)
		p.NitReplyPatterns = pats
		return err
	case "nit_reply_prompt_template":
		text, err := resolvePrompt(key, val)
		if err != nil {
			return err
		}
		p.NitReplyPromptTemplate = text
	}
	return nil
}

func applyPolicyInfraKey(p *PolicyConfig, key, val string) error {
	switch key {
	case "dispatch_ledger_file":
		p.LedgerFile = strings.TrimSpace(val)
	case "agent_backend":
		p.AgentBackend = strings.TrimSpace(val)
	case "agent_backend_cmd":
		p.AgentBackendCmd = strings.TrimSpace(val)
	case "lease_provider":
		p.LeaseProvider = strings.TrimSpace(val)
	case "lease_name":
		p.LeaseName = strings.TrimSpace(val)
	case "lease_timeout":
		d, err := time.ParseDuration(strings.TrimSpace(val))
		if err != nil || d <= 0 {
			return &KeyError{Key: key, Reason: fmt.Sprintf("invalid duration %q", val)}
		}
		p.LeaseTimeout = d
	case "lease_dir":
		p.LeaseDir = strings.TrimSpace(val)
	case "lease_claim_cmd":
		p.LeaseClaimCmd = strings.TrimSpace(val)
	case "lease_release_cmd":
		p.LeaseReleaseCmd = strings.TrimSpace(val)
	case "failure_marker_template":
		p.FailureMarkerTemplate = strings.TrimSpace(val)
	}
	return nil
}

func splitCSV(val string) []string {
	if strings.TrimSpace(val) == "" {
		return nil
	}
	var out []string
	for _, part := range strings.Split(val, ",") {
		item := strings.TrimSpace(part)
		if item != "" {
			out = append(out, item)
		}
	}
	return out
}

func compileCSVPatterns(key, val string) ([]*regexp.Regexp, error) {
	items := splitCSV(val)
	if len(items) == 0 {
		return nil, nil
	}
	out := make([]*regexp.Regexp, 0, len(items))
	for _, raw := range items {
		re, err := regexp.Compile(raw)
		if err != nil {
			return nil, &KeyError{Key: key, Reason: fmt.Sprintf("invalid regex %q: %v", raw, err)}
		}
		out = append(out, re)
	}
	return out, nil
}
