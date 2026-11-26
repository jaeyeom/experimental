package start

import (
	"context"
	"errors"
	"fmt"
	"regexp"
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const previewTimestamp = "<timestamp>"

var claimCommentNicknameRE = regexp.MustCompile(`^Claimed by \S+ on ([a-z0-9][a-z0-9-]{0,31})(?:\s|$|\.)`)

func (s *Starter) effectiveClaim() config.Claim {
	return effectiveClaimFor(s.Cfg, s.Kind)
}

func effectiveClaimFor(cfg config.Source, kind string) config.Claim {
	if cfg.Claim == (config.Claim{}) {
		assign := true
		if kind == "jira" {
			assign = cfg.ClaimAssign
		}
		rule := config.OwnershipUnassignedOrMe
		switch cfg.AssigneeOK {
		case "unassigned":
			rule = config.OwnershipUnassigned
		case "any":
			rule = config.OwnershipAny
		}
		return config.Claim{
			RequireLeaf:       true,
			RequireUnblocked:  true,
			SourceContainment: true,
			Assign:            assign,
			OwnershipRule:     rule,
			MarkInProgress:    true,
			InProgressRule:    config.InProgressBlock,
			CreateLabel:       false,
			Comment:           false,
			Status:            cfg.ClaimStatus,
			Transition:        cfg.ClaimTransition,
			Label:             cfg.ClaimLabel,
		}
	}
	c := cfg.Claim
	if c.OwnershipRule == "" {
		c.OwnershipRule = config.OwnershipUnassignedOrMe
		switch cfg.AssigneeOK {
		case "unassigned":
			c.OwnershipRule = config.OwnershipUnassigned
		case "any":
			c.OwnershipRule = config.OwnershipAny
		}
	}
	if c.InProgressRule == "" {
		c.InProgressRule = config.InProgressBlock
	}
	if c.Status == "" {
		c.Status = cfg.ClaimStatus
	}
	if c.Transition == "" {
		c.Transition = cfg.ClaimTransition
	}
	if c.Label == "" {
		c.Label = cfg.ClaimLabel
	}
	return c
}

func (s *Starter) sourceKey() string {
	if s.Src != nil && s.Src.Name() != "" {
		return s.Src.Name()
	}
	if s.Kind != "" && s.Cfg.Name != "" {
		return s.Kind + ":" + s.Cfg.Name
	}
	return ""
}

// hasOwnClaim reports whether this machine already recorded an active or failed
// local claim for id, meaning the tracker state was set by this machine.
func (s *Starter) hasOwnClaim(ctx context.Context, id string) (bool, error) {
	if s.Store == nil || s.Hostname == "" {
		return false, nil
	}
	key := s.sourceKey()
	if key == "" {
		return false, nil
	}
	c, err := s.Store.LatestClaim(ctx, key, id)
	if errors.Is(err, source.ErrNotFound) {
		return false, nil
	}
	if err != nil {
		return false, err //nolint:wrapcheck // propagate context cancellation
	}
	if c.Machine != s.Hostname {
		return false, nil
	}
	return c.State == store.Failed || c.State.Active(), nil
}

func isInProgress(it source.Item, claim config.Claim, kind string) bool {
	return inProgressReason(it, claim, kind) != ""
}

func inProgressReason(it source.Item, claim config.Claim, _ string) string {
	if !claim.MarkInProgress {
		return ""
	}
	var parts []string
	if claim.Status != "" && it.State == claim.Status {
		parts = append(parts, fmt.Sprintf("status %q", it.State))
	}
	if claim.Label != "" && slices.Contains(it.Labels, claim.Label) {
		parts = append(parts, fmt.Sprintf("label %q", claim.Label))
	}
	return strings.Join(parts, "; ")
}

func checkOwnership(id string, it source.Item, me string, claim config.Claim) error {
	switch claim.OwnershipRule {
	case config.OwnershipAny:
		return nil
	case config.OwnershipUnassigned:
		if it.Assignee == "" {
			return nil
		}
		return fmt.Errorf("start %s: expected assignee empty, found %q; pass --force-taking-ownership to take over", id, it.Assignee)
	case config.OwnershipUnassignedOrMe, "":
		if it.Assignee == "" || it.Assignee == me {
			return nil
		}
		return fmt.Errorf("start %s: expected assignee empty or %q, found %q; pass --force-taking-ownership to take over", id, me, it.Assignee)
	default:
		return fmt.Errorf("start %s: claim.ownership_rule: expected %s, %s, or %s, found %q",
			id, config.OwnershipUnassignedOrMe, config.OwnershipUnassigned, config.OwnershipAny, claim.OwnershipRule)
	}
}

// checkSafety enforces the safety invariants that --force never bypasses:
//   - item must not be closed/done (never bypassed even by --force-taking-ownership);
//   - trust gating and tamper guards when trust.environment == "untrusted";
//   - leaf-only (skip_types and open children) when claim.require_leaf is true;
//   - unblocked when claim.require_unblocked is true;
//   - ownership_rule and in_progress_rule unless --force-taking-ownership is set
//     (or owned by this machine's own claim for in_progress_rule).
func checkSafety(id string, it source.Item, snap ready.Snapshot, cfg config.Source, kind string, opts Options, ownClaim bool) error {
	if it.Done {
		return fmt.Errorf("start %s: not ready: done: done", id)
	}
	claim := effectiveClaimFor(cfg, kind)
	if err := checkTrustForStart(id, it, snap, cfg, claim, ownClaim); err != nil {
		return err
	}
	if err := checkLeafAndBlockers(id, it, snap, cfg, kind, claim); err != nil {
		return err
	}
	if opts.ForceTakeOwnership {
		return nil
	}
	if err := checkOwnership(id, it, snap.Me, claim); err != nil {
		return err
	}
	if claim.InProgressRule != config.InProgressAllow && !ownClaim {
		if reason := inProgressReason(it, claim, kind); reason != "" {
			return fmt.Errorf("start %s: already in progress (%s); pass --force-taking-ownership to take over", id, reason)
		}
	}
	return nil
}

func checkLeafAndBlockers(id string, it source.Item, snap ready.Snapshot, cfg config.Source, kind string, claim config.Claim) error {
	if !claim.RequireLeaf && !claim.RequireUnblocked {
		return nil
	}
	res := ready.Evaluate(it, snap, cfg, kind)
	var parts []string
	for _, c := range res.Checks {
		if c.Pass {
			continue
		}
		if (c.Rule == ready.RuleType || c.Rule == ready.RuleChildren) && claim.RequireLeaf {
			parts = append(parts, formatCheck(c))
		}
		if c.Rule == ready.RuleBlockers && claim.RequireUnblocked {
			parts = append(parts, formatCheck(c))
		}
	}
	if len(parts) > 0 {
		return fmt.Errorf("start %s: not ready: %s", id, strings.Join(parts, "; "))
	}
	return nil
}

func checkNicknameComment(ctx context.Context, src source.Source, id string, claim config.Claim) error {
	if claim.Nickname == "" {
		return nil
	}
	comment, err := src.LatestClaimComment(ctx, id)
	if err != nil {
		return fmt.Errorf("start %s: latest claim comment: %w", id, err)
	}
	if comment == nil {
		return nil
	}
	other := parseClaimNickname(comment.Body)
	if other == "" || other == claim.Nickname {
		return nil
	}
	return fmt.Errorf("start %s: already claimed on machine %q (this machine is %q); pass --force-taking-ownership to take over", id, other, claim.Nickname)
}

func parseClaimNickname(body string) string {
	line, _, _ := strings.Cut(strings.TrimSpace(body), "\n")
	m := claimCommentNicknameRE.FindStringSubmatch(strings.TrimSpace(line))
	if len(m) != 2 {
		return ""
	}
	return m[1]
}

func renderClaimComment(claim config.Claim, agent, timestamp, id, sourceKey string) string {
	tmpl := claim.CommentTemplate
	if tmpl == "" {
		if claim.Nickname != "" {
			tmpl = "Claimed by {agent} on {nickname} at {timestamp}."
		} else {
			tmpl = "Claimed by {agent} at {timestamp}."
		}
	}
	return strings.NewReplacer(
		"{agent}", agent,
		"{nickname}", claim.Nickname,
		"{timestamp}", timestamp,
		"{id}", id,
		"{source}", sourceKey,
	).Replace(tmpl)
}

func takeoverWarningText(id string, it source.Item, me string, claim config.Claim, kind string) string {
	var details []string
	if it.Assignee != "" && it.Assignee != me {
		details = append(details, fmt.Sprintf("assignee %q", it.Assignee))
	} else if claim.OwnershipRule == config.OwnershipUnassigned && it.Assignee != "" {
		details = append(details, fmt.Sprintf("assignee %q", it.Assignee))
	}
	if reason := inProgressReason(it, claim, kind); reason != "" {
		details = append(details, reason)
	}
	if len(details) == 0 {
		return fmt.Sprintf("warning: --force-taking-ownership takes over %s\n", id)
	}
	return fmt.Sprintf("warning: --force-taking-ownership takes over %s (%s)\n", id, strings.Join(details, "; "))
}

func formatCheck(check ready.Check) string {
	if check.Detail == "" {
		return string(check.Rule)
	}
	return string(check.Rule) + ": " + check.Detail
}

func readyFailureForStart(it source.Item, snap ready.Snapshot, cfg config.Source, kind string, opts Options, ownClaim bool) (string, error) {
	claim := effectiveClaimFor(cfg, kind)
	evalCfg := cfg
	if opts.ForceTakeOwnership || ownClaim || claim.InProgressRule == config.InProgressAllow {
		evalCfg.Claim = claim
		evalCfg.Claim.InProgressRule = config.InProgressAllow
	}
	if opts.ForceTakeOwnership {
		evalCfg.AssigneeOK = "any"
		evalCfg.Claim.OwnershipRule = config.OwnershipAny
	}
	evalCfg = trustConfigForStart(it, evalCfg, claim, ownClaim)
	result := ready.Evaluate(it, snap, evalCfg, kind)
	if !claim.RequireLeaf {
		result = passRules(result, ready.RuleType, ready.RuleChildren)
	}
	if !claim.RequireUnblocked {
		result = passRules(result, ready.RuleBlockers)
	}
	if statusSkippedForStart(it, claim, kind, opts, ownClaim) {
		result = passRules(result, ready.RuleStatus)
	}
	if result.Ready {
		return "", nil
	}
	parts := failingChecks(result)
	if len(parts) == 0 {
		return "", fmt.Errorf("start %s: expected a failing ready check, found none", it.ID)
	}
	return "not ready: " + strings.Join(parts, "; "), nil
}

func statusSkippedForStart(it source.Item, claim config.Claim, _ string, opts Options, ownClaim bool) bool {
	if claim.Status == "" || it.State != claim.Status {
		return false
	}
	return opts.ForceTakeOwnership || ownClaim || claim.InProgressRule == config.InProgressAllow
}

func checkTrustForStart(id string, it source.Item, snap ready.Snapshot, cfg config.Source, claim config.Claim, ownClaim bool) error {
	evalCfg := trustConfigForStart(it, cfg, claim, ownClaim)
	c := ready.CheckTrust(it, snap, evalCfg)
	if !c.Pass {
		return fmt.Errorf("start %s: not ready: %s", id, formatCheck(c))
	}
	return nil
}

// trustConfigForStart allows claim.Status when re-starting this machine's own
// interrupted claim (provided untrusted_ready_statuses is not explicitly empty),
// while keeping tamper guards (e.g. locked) enforced.
func trustConfigForStart(it source.Item, cfg config.Source, claim config.Claim, ownClaim bool) config.Source {
	if cfg.Trust.Environment != config.TrustEnvUntrusted || !ownClaim || claim.Status == "" || it.State != claim.Status {
		return cfg
	}
	statuses := cfg.Trust.EffectiveUntrustedReadyStatuses()
	if len(statuses) == 0 || slices.Contains(statuses, claim.Status) {
		return cfg
	}
	out := cfg
	out.Trust.UntrustedReadyStatuses = append(slices.Clone(statuses), claim.Status)
	return out
}

func passRules(result ready.Result, rules ...ready.Rule) ready.Result {
	passed := true
	for i, check := range result.Checks {
		if slices.Contains(rules, check.Rule) {
			result.Checks[i] = ready.Check{Rule: check.Rule, Pass: true}
			continue
		}
		if !check.Pass {
			passed = false
		}
	}
	result.Ready = passed
	return result
}
