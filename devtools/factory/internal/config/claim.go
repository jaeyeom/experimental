package config

import (
	"fmt"
	"regexp"
	"strings"
)

const (
	// OwnershipUnassignedOrMe allows claiming unassigned items or items assigned to you (the default).
	OwnershipUnassignedOrMe = "unassigned_or_me"
	// OwnershipUnassigned allows claiming only unassigned items.
	OwnershipUnassigned = "unassigned"
	// OwnershipAny allows claiming items regardless of current assignee.
	OwnershipAny = "any"

	// InProgressBlock refuses to claim an item already marked in-progress by another (the default).
	InProgressBlock = "block"
	// InProgressAllow allows claiming an item even if already marked in-progress.
	InProgressAllow = "allow"

	// DefaultClaimLabel is the legacy GitHub claim label.
	DefaultClaimLabel = "in-progress"

	// DefaultGitHubClaimStatus is the default native GitHub issue status set on claim.
	DefaultGitHubClaimStatus = "In progress"

	// DefaultGitHubReleaseStatus is the default native GitHub issue status restored on release
	// when no non-OPEN ready_statuses entry is configured.
	DefaultGitHubReleaseStatus = "Todo"

	// GitHubStateOpen is the fallback item state for an open GitHub issue whose native status is null.
	GitHubStateOpen = "OPEN"
)

// DefaultGitHubReadyStatuses returns the default ready_statuses for a GitHub source:
// Todo, Backlog, and OPEN (for open issues with an unset/null native status).
// Todo is first so Release returns claimed items to Todo by default.
func DefaultGitHubReadyStatuses() []string {
	return []string{DefaultGitHubReleaseStatus, "Backlog", GitHubStateOpen}
}

var claimNicknamePattern = regexp.MustCompile(`^[a-z0-9][a-z0-9-]{0,31}$`)

var allowedCommentPlaceholders = map[string]struct{}{
	"agent":     {},
	"nickname":  {},
	"timestamp": {},
	"id":        {},
	"source":    {},
}

// Claim is the optional [claim] and [source.claim] configuration: start safety
// checks, ownership assignment and rules, in-progress transitions/labels, and
// claim comments. A source inherits every field from the top-level [claim] and
// overrides only the fields it sets.
type Claim struct {
	// RequireLeaf refuses skip_types and items with open children on factory start.
	RequireLeaf bool `toml:"require_leaf"`
	// RequireUnblocked refuses items with open BlockedBy blockers on factory start.
	RequireUnblocked bool `toml:"require_unblocked"`
	// SourceContainment enforces FACTORY_ITEM_SOURCE and current git repo scope.
	SourceContainment bool `toml:"source_containment"`

	// Assign takes assignee ownership on claim (Jira: Me; GitHub: @me).
	Assign bool `toml:"assign"`
	// OwnershipRule is OwnershipUnassignedOrMe (the default), OwnershipUnassigned, or OwnershipAny.
	OwnershipRule string `toml:"ownership_rule"`

	// MarkInProgress marks the issue in-progress on claim (status and/or label).
	MarkInProgress bool `toml:"mark_in_progress"`
	// InProgressRule is InProgressBlock (the default) or InProgressAllow.
	InProgressRule string `toml:"in_progress_rule"`
	// CreateLabel creates the GitHub claim label if missing.
	CreateLabel bool `toml:"create_label"`

	// Comment posts a claim comment on start.
	Comment bool `toml:"comment"`
	// Nickname is an optional machine nickname matching ^[a-z0-9][a-z0-9-]{0,31}$.
	Nickname string `toml:"nickname"`
	// CommentTemplate overrides the default claim comment format.
	CommentTemplate string `toml:"comment_template"`

	// Per-source fields (empty on top-level Config.Claim):
	// Status is the claim status (reconciles with claim_status).
	Status string `toml:"status"`
	// Transition is the Jira claim transition id or name (reconciles with claim_transition).
	Transition string `toml:"transition"`
	// Label is the claim label (reconciles with claim_label).
	Label string `toml:"label"`
}

// rawClaim is the top-level [claim] table.
type rawClaim struct {
	RequireLeaf       *bool   `toml:"require_leaf"`
	RequireUnblocked  *bool   `toml:"require_unblocked"`
	SourceContainment *bool   `toml:"source_containment"`
	Assign            *bool   `toml:"assign"`
	OwnershipRule     *string `toml:"ownership_rule"`
	MarkInProgress    *bool   `toml:"mark_in_progress"`
	InProgressRule    *string `toml:"in_progress_rule"`
	CreateLabel       *bool   `toml:"create_label"`
	Comment           *bool   `toml:"comment"`
	Nickname          *string `toml:"nickname"`
	CommentTemplate   *string `toml:"comment_template"`
}

// rawSourceClaim is the per-source [source.claim] table.
type rawSourceClaim struct {
	RequireLeaf       *bool   `toml:"require_leaf"`
	RequireUnblocked  *bool   `toml:"require_unblocked"`
	SourceContainment *bool   `toml:"source_containment"`
	Assign            *bool   `toml:"assign"`
	OwnershipRule     *string `toml:"ownership_rule"`
	MarkInProgress    *bool   `toml:"mark_in_progress"`
	InProgressRule    *string `toml:"in_progress_rule"`
	CreateLabel       *bool   `toml:"create_label"`
	Comment           *bool   `toml:"comment"`
	Nickname          *string `toml:"nickname"`
	CommentTemplate   *string `toml:"comment_template"`
	Status            *string `toml:"status"`
	Transition        *string `toml:"transition"`
	Label             *string `toml:"label"`
}

func defaultClaim() Claim {
	return Claim{
		RequireLeaf:       true,
		RequireUnblocked:  true,
		SourceContainment: true,
		Assign:            true,
		OwnershipRule:     OwnershipUnassignedOrMe,
		MarkInProgress:    true,
		InProgressRule:    InProgressBlock,
		CreateLabel:       false,
		Comment:           true,
	}
}

func (raw rawClaim) toClaim() Claim {
	return rawSourceClaim{
		RequireLeaf:       raw.RequireLeaf,
		RequireUnblocked:  raw.RequireUnblocked,
		SourceContainment: raw.SourceContainment,
		Assign:            raw.Assign,
		OwnershipRule:     raw.OwnershipRule,
		MarkInProgress:    raw.MarkInProgress,
		InProgressRule:    raw.InProgressRule,
		CreateLabel:       raw.CreateLabel,
		Comment:           raw.Comment,
		Nickname:          raw.Nickname,
		CommentTemplate:   raw.CommentTemplate,
	}.over(defaultClaim())
}

func (raw rawSourceClaim) over(base Claim) Claim {
	if raw.RequireLeaf != nil {
		base.RequireLeaf = *raw.RequireLeaf
	}
	if raw.RequireUnblocked != nil {
		base.RequireUnblocked = *raw.RequireUnblocked
	}
	if raw.SourceContainment != nil {
		base.SourceContainment = *raw.SourceContainment
	}
	if raw.Assign != nil {
		base.Assign = *raw.Assign
	}
	if raw.OwnershipRule != nil {
		base.OwnershipRule = *raw.OwnershipRule
	}
	if raw.MarkInProgress != nil {
		base.MarkInProgress = *raw.MarkInProgress
	}
	if raw.InProgressRule != nil {
		base.InProgressRule = *raw.InProgressRule
	}
	if raw.CreateLabel != nil {
		base.CreateLabel = *raw.CreateLabel
	}
	if raw.Comment != nil {
		base.Comment = *raw.Comment
	}
	if raw.Nickname != nil {
		base.Nickname = *raw.Nickname
	}
	if raw.CommentTemplate != nil {
		base.CommentTemplate = *raw.CommentTemplate
	}
	if raw.Status != nil {
		base.Status = *raw.Status
	}
	if raw.Transition != nil {
		base.Transition = *raw.Transition
	}
	if raw.Label != nil {
		base.Label = *raw.Label
	}
	return base
}

func resolveSourceClaim(src *Source, raw rawSource, top Claim) error {
	if raw.Claim != nil {
		if err := checkSourceClaimConflicts(src.Name, raw); err != nil {
			return err
		}
	}
	claim := mergeSourceClaim(src.Kind, raw, top)
	if raw.AssigneeOK != nil {
		src.AssigneeOK = *raw.AssigneeOK
		if rule, ok := ownershipRuleFromAssigneeOK(*raw.AssigneeOK); ok {
			claim.OwnershipRule = rule
		}
	} else {
		if okVal, ok := assigneeOKFromOwnershipRule(claim.OwnershipRule); ok {
			src.AssigneeOK = okVal
		} else {
			src.AssigneeOK = "me"
		}
	}
	src.Claim = claim
	src.ClaimAssign = claim.Assign
	src.ClaimStatus = claim.Status
	src.ClaimTransition = claim.Transition
	src.ClaimLabel = claim.Label
	return nil
}

func mergeSourceClaim(kind string, raw rawSource, top Claim) Claim {
	claim := top
	if kind == "github" && !hasExplicitClaimMarker(raw) {
		claim.Status = DefaultGitHubClaimStatus
	}
	if raw.ClaimAssign != nil {
		claim.Assign = *raw.ClaimAssign
	}
	if raw.ClaimStatus != nil {
		claim.Status = *raw.ClaimStatus
	}
	if raw.ClaimTransition != nil {
		claim.Transition = *raw.ClaimTransition
	}
	if raw.ClaimLabel != nil {
		claim.Label = *raw.ClaimLabel
	}
	if raw.Claim != nil {
		claim = raw.Claim.over(claim)
	}
	return claim
}

func hasExplicitClaimMarker(raw rawSource) bool {
	if raw.ClaimStatus != nil || raw.ClaimLabel != nil {
		return true
	}
	return raw.Claim != nil && (raw.Claim.Status != nil || raw.Claim.Label != nil)
}

func checkSourceClaimConflicts(name string, raw rawSource) error {
	c := raw.Claim
	switch {
	case c.Assign != nil && raw.ClaimAssign != nil:
		return fmt.Errorf("source %q: expected one of claim.assign or claim_assign, found both", name)
	case c.Status != nil && raw.ClaimStatus != nil:
		return fmt.Errorf("source %q: expected one of claim.status or claim_status, found both", name)
	case c.Transition != nil && raw.ClaimTransition != nil:
		return fmt.Errorf("source %q: expected one of claim.transition or claim_transition, found both", name)
	case c.Label != nil && raw.ClaimLabel != nil:
		return fmt.Errorf("source %q: expected one of claim.label or claim_label, found both", name)
	case c.OwnershipRule != nil && raw.AssigneeOK != nil:
		return fmt.Errorf("source %q: expected one of claim.ownership_rule or assignee_ok, found both", name)
	default:
		return nil
	}
}

func ownershipRuleFromAssigneeOK(val string) (string, bool) {
	switch val {
	case "me":
		return OwnershipUnassignedOrMe, true
	case "unassigned":
		return OwnershipUnassigned, true
	case "any":
		return OwnershipAny, true
	default:
		return "", false
	}
}

func assigneeOKFromOwnershipRule(rule string) (string, bool) {
	switch rule {
	case OwnershipUnassignedOrMe:
		return "me", true
	case OwnershipUnassigned:
		return "unassigned", true
	case OwnershipAny:
		return "any", true
	default:
		return "", false
	}
}

// ValidOwnershipRule checks a [claim] ownership_rule value: unassigned_or_me, unassigned, or any.
func ValidOwnershipRule(value string) error {
	switch value {
	case OwnershipUnassignedOrMe, OwnershipUnassigned, OwnershipAny:
		return nil
	case "":
		return fmt.Errorf("expected %s, %s, or %s, found empty", OwnershipUnassignedOrMe, OwnershipUnassigned, OwnershipAny)
	default:
		return fmt.Errorf("expected %s, %s, or %s, found %q", OwnershipUnassignedOrMe, OwnershipUnassigned, OwnershipAny, value)
	}
}

// ValidInProgressRule checks a [claim] in_progress_rule value: block or allow.
func ValidInProgressRule(value string) error {
	switch value {
	case InProgressBlock, InProgressAllow:
		return nil
	case "":
		return fmt.Errorf("expected %s or %s, found empty", InProgressBlock, InProgressAllow)
	default:
		return fmt.Errorf("expected %s or %s, found %q", InProgressBlock, InProgressAllow, value)
	}
}

// ValidNickname checks a [claim] nickname value: empty or matching ^[a-z0-9][a-z0-9-]{0,31}$.
func ValidNickname(value string) error {
	if value == "" || claimNicknamePattern.MatchString(value) {
		return nil
	}
	return fmt.Errorf("expected empty or a nickname matching ^[a-z0-9][a-z0-9-]{0,31}$, found %q", value)
}

// ValidCommentTemplate checks a [claim] comment_template value: empty or only
// containing {agent}, {nickname}, {timestamp}, {id}, and {source} placeholders.
func ValidCommentTemplate(value string) error {
	for i := 0; i < len(value); {
		switch value[i] {
		case '}':
			return fmt.Errorf("literal braces aren't supported, found %q", "}")
		case '{':
			rel := strings.IndexByte(value[i+1:], '}')
			if rel < 0 {
				return fmt.Errorf("literal braces aren't supported, found %q", value[i:])
			}
			word := value[i+1 : i+1+rel]
			if _, ok := allowedCommentPlaceholders[word]; !ok {
				return fmt.Errorf("expected one of {agent}, {nickname}, {timestamp}, {id}, {source}, found {%s}", word)
			}
			i += rel + 2
		default:
			i++
		}
	}
	return nil
}

func validClaim(where string, c Claim) error {
	if err := ValidOwnershipRule(c.OwnershipRule); err != nil {
		return fmt.Errorf("%s.ownership_rule: %w", where, err)
	}
	if err := ValidInProgressRule(c.InProgressRule); err != nil {
		return fmt.Errorf("%s.in_progress_rule: %w", where, err)
	}
	if err := ValidNickname(c.Nickname); err != nil {
		return fmt.Errorf("%s.nickname: %w", where, err)
	}
	if err := ValidCommentTemplate(c.CommentTemplate); err != nil {
		return fmt.Errorf("%s.comment_template: %w", where, err)
	}
	return nil
}
