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

// Defaults of the agent_command decision cache when config omits them.
const (
	DefaultAgentCacheTTL   = 7 * 24 * time.Hour
	DefaultAgentCacheMaxMB = 50
)

// Values of ApproveRule.Checks.
const (
	// ChecksGreen requires no failed check and every required check passed.
	ChecksGreen = "green"
	// ChecksAllowPending requires no failed check. Pending checks are allowed.
	ChecksAllowPending = "allow_pending"
)

// Values of ApproveRule.Action.
const (
	// ActionApprove approves the pull request.
	ActionApprove = "approve"
	// ActionHandoff requests the rule's reviewers, then removes the user as
	// a requested reviewer. It never approves.
	ActionHandoff = "handoff"
)

// DefaultMaxMarksPerRun is how many files one gh-approve run may mark
// viewed when config omits max_marks_per_run.
const DefaultMaxMarksPerRun = 200

// ViewedConfig holds gh-approve "mark viewed" settings.
type ViewedConfig struct {
	// Enabled marks files on pull requests no approve or hand-off rule took.
	Enabled bool
	// MaxMarksPerRun is how many files one run may mark. Zero marks nothing.
	MaxMarksPerRun int
	// Rules select the files. A file is marked when any rule selects it.
	Rules []ViewedRule
}

// ViewedRule is one "mark viewed" rule. Repos filters the pull request.
// A file matches when any set selector matches.
type ViewedRule struct {
	Name string
	// Repos are owner/name or globs. Empty matches every repository.
	Repos []string
	// PathsMatch are globs. A changed file matching any of them is selected.
	PathsMatch []string
	// CodeownersNotMine selects a file whose owners include neither the
	// user nor a team the user belongs to.
	CodeownersNotMine bool
}

// ApproveConfig holds gh-approve settings. Only Pkl config supports it.
type ApproveConfig struct {
	// Body is the approval body. The matching rule name is appended to it.
	Body string `yaml:"-"`
	// SkipAlreadyApproved skips a pull request the user approved at the current head.
	SkipAlreadyApproved bool `yaml:"-"`
	// AgentCacheTTL is how long a cached agent_command decision is reused.
	AgentCacheTTL time.Duration `yaml:"-"`
	// AgentCacheMaxMB bounds the agent_command decision cache in MiB.
	AgentCacheMaxMB int `yaml:"-"`
	// Rules are ORed. A pull request that matches none is left for a human.
	Rules []ApproveRule `yaml:"-"`
}

// ApproveRule is one gh-approve rule. Its conditions are ANDed.
// An empty list or a nil limit adds no condition.
type ApproveRule struct {
	Name string
	// Action is ActionApprove or ActionHandoff.
	Action string
	// Reviewers are the logins a hand-off requests.
	Reviewers         []string
	Repos             []string
	PathsAllMatch     []string
	PathsNoneMatch    []string
	PathsAnyMatch     []string
	CodeownersCovered bool
	MaxChangedLines   *int
	MaxAddedLines     *int
	MaxFiles          *int
	PureDeletion      bool
	// DeletionsAndCommentsOnly requires a deletion and added lines that
	// are only blank or comments. Patches are fetched only when it is set.
	DeletionsAndCommentsOnly bool
	AuthorsAllow             []string
	AuthorsDeny              []string
	LabelsRequired           []string
	LabelsForbidden          []string
	RequiredApprovalsBy      []string
	Checks                   string
	NotDraft                 bool
	MaxUnresolvedThreads     int
	// AgentCommand is argv, run without a shell after every other condition
	// passed. Empty adds no condition.
	AgentCommand []string
	// AgentTimeout bounds AgentCommand. Zero means DefaultAgentTimeout.
	AgentTimeout time.Duration
	// AgentCacheDisabled runs AgentCommand on every pass, without the cache.
	AgentCacheDisabled bool
}

func convertApprove(src pklconfig.ApproveConfig) ApproveConfig {
	out := ApproveConfig{
		Body:                src.Body,
		SkipAlreadyApproved: src.SkipAlreadyApproved,
		AgentCacheTTL:       src.AgentCacheTtl.GoDuration(),
		AgentCacheMaxMB:     src.AgentCacheMaxMb,
	}
	if out.Body == "" {
		out.Body = DefaultApproveBody
	}
	if out.AgentCacheTTL <= 0 {
		out.AgentCacheTTL = DefaultAgentCacheTTL
	}
	if out.AgentCacheMaxMB <= 0 {
		out.AgentCacheMaxMB = DefaultAgentCacheMaxMB
	}
	for _, rule := range src.Rules {
		converted := ApproveRule{
			Name:                     rule.Name,
			Action:                   rule.Action,
			Reviewers:                cloneStrings(rule.Reviewers),
			Repos:                    cloneStrings(rule.Repos),
			PathsAllMatch:            cloneStrings(rule.PathsAllMatch),
			PathsNoneMatch:           cloneStrings(rule.PathsNoneMatch),
			PathsAnyMatch:            cloneStrings(rule.PathsAnyMatch),
			CodeownersCovered:        rule.CodeownersCovered,
			MaxChangedLines:          copyIntPtr(rule.MaxChangedLines),
			MaxAddedLines:            copyIntPtr(rule.MaxAddedLines),
			MaxFiles:                 copyIntPtr(rule.MaxFiles),
			PureDeletion:             rule.PureDeletion,
			DeletionsAndCommentsOnly: rule.DeletionsAndCommentsOnly,
			AuthorsAllow:             cloneStrings(rule.AuthorsAllow),
			AuthorsDeny:              cloneStrings(rule.AuthorsDeny),
			LabelsRequired:           cloneStrings(rule.LabelsRequired),
			LabelsForbidden:          cloneStrings(rule.LabelsForbidden),
			RequiredApprovalsBy:      cloneStrings(rule.RequiredApprovalsBy),
			NotDraft:                 rule.NotDraft,
			MaxUnresolvedThreads:     rule.MaxUnresolvedThreads,
			AgentCommand:             cloneStrings(rule.AgentCommand),
			AgentTimeout:             rule.AgentTimeout.GoDuration(),
			AgentCacheDisabled:       !rule.AgentCache,
		}
		if rule.Checks != nil {
			converted.Checks = *rule.Checks
		}
		out.Rules = append(out.Rules, converted)
	}
	return out
}

func convertViewed(src pklconfig.ViewedConfig) ViewedConfig {
	out := ViewedConfig{
		Enabled:        src.Enabled,
		MaxMarksPerRun: src.MaxMarksPerRun,
	}
	for _, rule := range src.Rules {
		out.Rules = append(out.Rules, ViewedRule{
			Name:              rule.Name,
			Repos:             cloneStrings(rule.Repos),
			PathsMatch:        cloneStrings(rule.PathsMatch),
			CodeownersNotMine: rule.CodeownersNotMine,
		})
	}
	return out
}

func validateViewed(viewed ViewedConfig) error {
	if viewed.MaxMarksPerRun < 0 {
		return fmt.Errorf("viewed max_marks_per_run must be >= 0")
	}
	seen := make(map[string]bool, len(viewed.Rules))
	for _, rule := range viewed.Rules {
		if rule.Name == "" {
			return fmt.Errorf("viewed rule name must not be empty")
		}
		if seen[rule.Name] {
			return fmt.Errorf("viewed rule %q is defined twice", rule.Name)
		}
		seen[rule.Name] = true
		if len(rule.PathsMatch) == 0 && !rule.CodeownersNotMine {
			return fmt.Errorf("viewed rule %q needs paths_match or codeowners_not_mine", rule.Name)
		}
		globs := map[string][]string{
			"repos":       rule.Repos,
			"paths_match": rule.PathsMatch,
		}
		for field, patterns := range globs {
			for _, pattern := range patterns {
				if !doublestar.ValidatePattern(pattern) {
					return fmt.Errorf("viewed rule %q: %s glob %q is invalid", rule.Name, field, pattern)
				}
			}
		}
	}
	return nil
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
		"paths_any_match":  rule.PathsAnyMatch,
	}
	for field, patterns := range globs {
		for _, pattern := range patterns {
			if !doublestar.ValidatePattern(pattern) {
				return fmt.Errorf("%s glob %q is invalid", field, pattern)
			}
		}
	}
	switch rule.Action {
	case ActionApprove:
		if len(rule.Reviewers) > 0 {
			return fmt.Errorf("reviewers is only allowed on a handoff rule")
		}
	case ActionHandoff:
		if len(rule.Reviewers) == 0 {
			return fmt.Errorf("reviewers is required on a handoff rule")
		}
	default:
		return fmt.Errorf("action %q is not approve or handoff", rule.Action)
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
