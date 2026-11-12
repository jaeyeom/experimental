package config

import (
	"fmt"
	"time"

	"github.com/bmatcuk/doublestar/v4"

	pklconfig "github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl"
)

// DefaultApproveBody is the gh-approve review body when config omits body.
const DefaultApproveBody = "LGTM"

// DefaultAgentTimeout bounds an approve rule's agent command when config
// omits agent_timeout.
const DefaultAgentTimeout = 120 * time.Second

// Values of ApproveRule.Checks.
const (
	// ChecksGreen requires no failed check and every required check passed.
	ChecksGreen = "green"
	// ChecksAllowPending requires no failed check. Pending checks are allowed.
	ChecksAllowPending = "allow_pending"
)

// ApproveConfig holds gh-approve settings. Only Pkl config supports it.
type ApproveConfig struct {
	// Body is the approval body. The matching rule name is appended to it.
	Body string `yaml:"-"`
	// SkipAlreadyApproved skips a pull request the user approved at the current head.
	SkipAlreadyApproved bool `yaml:"-"`
	// Rules are ORed. A pull request that matches none is left for a human.
	Rules []ApproveRule `yaml:"-"`
}

// ApproveRule is one gh-approve rule. Its conditions are ANDed.
// An empty list or a nil limit adds no condition.
type ApproveRule struct {
	Name                 string
	Repos                []string
	PathsAllMatch        []string
	PathsNoneMatch       []string
	MaxChangedLines      *int
	MaxAddedLines        *int
	MaxFiles             *int
	PureDeletion         bool
	AuthorsAllow         []string
	AuthorsDeny          []string
	LabelsRequired       []string
	LabelsForbidden      []string
	RequiredApprovalsBy  []string
	Checks               string
	NotDraft             bool
	MaxUnresolvedThreads int
	// AgentCommand is argv, run without a shell after every other condition
	// passed. Empty adds no condition.
	AgentCommand []string
	// AgentTimeout bounds AgentCommand. Zero means DefaultAgentTimeout.
	AgentTimeout time.Duration
}

func convertApprove(src pklconfig.ApproveConfig) ApproveConfig {
	out := ApproveConfig{
		Body:                src.Body,
		SkipAlreadyApproved: src.SkipAlreadyApproved,
	}
	if out.Body == "" {
		out.Body = DefaultApproveBody
	}
	for _, rule := range src.Rules {
		converted := ApproveRule{
			Name:                 rule.Name,
			Repos:                cloneStrings(rule.Repos),
			PathsAllMatch:        cloneStrings(rule.PathsAllMatch),
			PathsNoneMatch:       cloneStrings(rule.PathsNoneMatch),
			MaxChangedLines:      copyIntPtr(rule.MaxChangedLines),
			MaxAddedLines:        copyIntPtr(rule.MaxAddedLines),
			MaxFiles:             copyIntPtr(rule.MaxFiles),
			PureDeletion:         rule.PureDeletion,
			AuthorsAllow:         cloneStrings(rule.AuthorsAllow),
			AuthorsDeny:          cloneStrings(rule.AuthorsDeny),
			LabelsRequired:       cloneStrings(rule.LabelsRequired),
			LabelsForbidden:      cloneStrings(rule.LabelsForbidden),
			RequiredApprovalsBy:  cloneStrings(rule.RequiredApprovalsBy),
			NotDraft:             rule.NotDraft,
			MaxUnresolvedThreads: rule.MaxUnresolvedThreads,
			AgentCommand:         cloneStrings(rule.AgentCommand),
			AgentTimeout:         rule.AgentTimeout.GoDuration(),
		}
		if rule.Checks != nil {
			converted.Checks = *rule.Checks
		}
		out.Rules = append(out.Rules, converted)
	}
	return out
}

func validateApprove(approve ApproveConfig) error {
	seen := make(map[string]bool, len(approve.Rules))
	for _, rule := range approve.Rules {
		if rule.Name == "" {
			return fmt.Errorf("approve rule name must not be empty")
		}
		if seen[rule.Name] {
			return fmt.Errorf("approve rule %q is defined twice", rule.Name)
		}
		seen[rule.Name] = true
		if err := validateApproveRule(rule); err != nil {
			return fmt.Errorf("approve rule %q: %w", rule.Name, err)
		}
	}
	return nil
}

func validateApproveRule(rule ApproveRule) error {
	globs := map[string][]string{
		"repos":            rule.Repos,
		"paths_all_match":  rule.PathsAllMatch,
		"paths_none_match": rule.PathsNoneMatch,
	}
	for field, patterns := range globs {
		for _, pattern := range patterns {
			if !doublestar.ValidatePattern(pattern) {
				return fmt.Errorf("%s glob %q is invalid", field, pattern)
			}
		}
	}
	switch rule.Checks {
	case "", ChecksGreen, ChecksAllowPending:
	default:
		return fmt.Errorf("checks %q is not green or allow_pending", rule.Checks)
	}
	for field, limit := range map[string]*int{
		"max_changed_lines": rule.MaxChangedLines,
		"max_added_lines":   rule.MaxAddedLines,
		"max_files":         rule.MaxFiles,
	} {
		if limit != nil && *limit < 0 {
			return fmt.Errorf("%s must be >= 0", field)
		}
	}
	if rule.MaxUnresolvedThreads < 0 {
		return fmt.Errorf("max_unresolved_threads must be >= 0")
	}
	return nil
}
