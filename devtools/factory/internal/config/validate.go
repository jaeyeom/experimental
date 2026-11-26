package config

import (
	"errors"
	"fmt"
	"net/url"
	"regexp"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/rank"
)

var repoPattern = regexp.MustCompile(`^[\w.-]+/[\w.-]+$`)

var allowedPlaceholders = map[string]struct{}{
	"id":          {},
	"title":       {},
	"url":         {},
	"source":      {},
	"base":        {},
	"parent":      {},
	"caller":      {},
	"caller_bus":  {},
	"report_back": {},
}

func validate(cfg *Config, env Env) error {
	if err := validateTop(cfg); err != nil {
		return err
	}
	if err := validateAgentRules(cfg.AgentRules); err != nil {
		return err
	}
	if err := validateLanes(cfg.Lanes, cfg.LaneOrder, env); err != nil {
		return err
	}
	seen := make(map[string]struct{}, len(cfg.Sources))
	for i := range cfg.Sources {
		if err := validateSourceMaxAge(&cfg.Sources[i]); err != nil {
			return err
		}
		if err := validateSource(&cfg.Sources[i], seen, env); err != nil {
			return err
		}
	}
	return validateLaneOverrides(cfg)
}

func validateTop(cfg *Config) error {
	if cfg.Sync.MaxAge < 0 {
		return fmt.Errorf("sync.max_age: expected a non-negative duration, found %s", cfg.Sync.MaxAge)
	}
	if err := validateGate(cfg.Gate); err != nil {
		return err
	}
	if err := ValidLeaseGate(cfg.Lease.Gate); err != nil {
		return err
	}
	if err := validLeasePrompt("lease", cfg.Lease.LeasePrompt); err != nil {
		return err
	}
	if err := validOutput("output", cfg.Output); err != nil {
		return err
	}
	if err := validClaim("claim", cfg.Claim); err != nil {
		return err
	}
	if err := validatePlaceholdersFor("defaults", "start_prompt_append", cfg.Defaults.StartPromptAppend, false); err != nil {
		return err
	}
	return validateTopWorker(cfg)
}

func validateTopWorker(cfg *Config) error {
	if !cfg.workerAgentSet {
		if err := ValidAgent(cfg.Defaults.AgentKind); err != nil {
			return fmt.Errorf(`defaults field "agent_kind": %w`, err)
		}
	}
	if !cfg.workerArgsSet {
		if err := validateArgList(`defaults field "agent_args"`, cfg.Defaults.AgentArgs); err != nil {
			return err
		}
	}
	return validWorker("worker", cfg.Worker, cfg.workerAgentSet, cfg.workerArgsSet)
}

func validateLaneOverrides(cfg *Config) error {
	for _, lane := range cfg.Lanes {
		if err := validWorker(fmt.Sprintf("lane %q worker", lane.Name), lane.Worker.Over(cfg.Worker), true, true); err != nil {
			return err
		}
		if err := validOutput(fmt.Sprintf("lane %q output", lane.Name), lane.Output.Over(cfg.Output)); err != nil {
			return err
		}
		if err := validateLaneLease(cfg, lane); err != nil {
			return err
		}
		if err := validatePlaceholdersFor(fmt.Sprintf("lane %q", lane.Name), "start_prompt_append", lane.StartPromptAppend, false); err != nil {
			return err
		}
	}
	return nil
}

func validateLaneLease(cfg *Config, lane Lane) error {
	where := fmt.Sprintf("lane %q lease", lane.Name)
	if len(cfg.Sources) == 0 {
		return validLeasePrompt(where, lane.applyLease(cfg.Lease.LeasePrompt))
	}
	for i := range cfg.Sources {
		if err := validLeasePrompt(where, lane.applyLease(cfg.Sources[i].Lease)); err != nil {
			return err
		}
	}
	return nil
}

func validateSourceMaxAge(src *Source) error {
	if src.MaxAge != nil && *src.MaxAge < 0 {
		return fmt.Errorf("source %q field \"max_age\": expected a non-negative duration, found %s", src.Name, *src.MaxAge)
	}
	return nil
}

func validateGate(gate Gate) error {
	for _, id := range gate.IgnorePanes {
		if id == "" {
			return fmt.Errorf("gate.ignore_panes: expected a non-empty pane id, found empty")
		}
	}
	for _, label := range gate.IgnoreTabs {
		if label == "" {
			return fmt.Errorf("gate.ignore_tabs: expected a non-empty tab label, found empty")
		}
	}
	for _, path := range gate.HoldFiles {
		if path == "" {
			return errors.New("gate.hold_files: expected a non-empty path, found empty")
		}
	}
	for _, label := range gate.HoldTabs {
		if label == "" {
			return errors.New("gate.hold_tabs: expected a non-empty tab label, found empty")
		}
	}
	if gate.HoldTTL != nil && *gate.HoldTTL < 0 {
		return fmt.Errorf("gate.hold_ttl: expected a non-negative duration, found %s", *gate.HoldTTL)
	}
	return validateGateBranch(gate)
}

func validateGateBranch(gate Gate) error {
	switch gate.Require {
	case "", GateOnBase, GateCleanAgainstBase:
	default:
		return InvalidGateRequire(gate.Require)
	}
	if gate.BaseBranch == "" && gate.Require != "" {
		return errors.New("gate.base_branch: expected a non-empty branch, found empty")
	}
	if gate.BaseBranch != "" && gate.Require == "" {
		return InvalidGateRequire("")
	}
	return nil
}

// ValidLeaseGate checks a [lease] gate value: auto, off, or required.
func ValidLeaseGate(value string) error {
	switch value {
	case LeaseGateAuto, LeaseGateOff, LeaseGateRequired:
		return nil
	case "":
		return fmt.Errorf("lease.gate: expected %s, %s, or %s, found empty", LeaseGateAuto, LeaseGateOff, LeaseGateRequired)
	default:
		return fmt.Errorf("lease.gate: expected %s, %s, or %s, found %q", LeaseGateAuto, LeaseGateOff, LeaseGateRequired, value)
	}
}

// ValidLeaseMode checks a [lease] mode value: off, prompt, or prompt+check.
func ValidLeaseMode(value string) error {
	switch value {
	case LeaseModeOff, LeaseModePrompt, LeaseModePromptCheck:
		return nil
	case "":
		return fmt.Errorf("expected %s, %s, or %s, found empty", LeaseModeOff, LeaseModePrompt, LeaseModePromptCheck)
	default:
		return fmt.Errorf("expected %s, %s, or %s, found %q", LeaseModeOff, LeaseModePrompt, LeaseModePromptCheck, value)
	}
}

// validLeasePrompt checks mode, claim_for, and restore. where names the table.
func validLeasePrompt(where string, p LeasePrompt) error {
	if err := ValidLeaseMode(p.Mode); err != nil {
		return fmt.Errorf("%s.mode: %w", where, err)
	}
	d, err := time.ParseDuration(p.ClaimFor)
	if err != nil || d <= 0 {
		return fmt.Errorf("%s.claim_for: expected a positive duration such as %q, found %q", where, DefaultLeaseClaimFor, p.ClaimFor)
	}
	if p.Restore && p.Mode == LeaseModeOff {
		return fmt.Errorf("%s.restore: expected mode %s or %s when restore is true, found %q", where, LeaseModePrompt, LeaseModePromptCheck, p.Mode)
	}
	return nil
}

// InvalidGateRequire reports a require value other than on_base or clean_against_base.
// An empty value is reported as empty, not as a quoted string.
func InvalidGateRequire(found string) error {
	if found == "" {
		return fmt.Errorf("gate.require: expected %s or %s, found empty", GateOnBase, GateCleanAgainstBase)
	}
	return fmt.Errorf("gate.require: expected %s or %s, found %q", GateOnBase, GateCleanAgainstBase, found)
}

func validateSource(src *Source, seen map[string]struct{}, env Env) error {
	if src.Name == "" {
		return fmt.Errorf("source %q field \"name\": expected a non-empty name, found empty", src.Name)
	}
	if _, ok := seen[src.Name]; ok {
		return fmt.Errorf("source %q field \"name\": expected a unique name, found duplicate", src.Name)
	}
	seen[src.Name] = struct{}{}

	switch src.Kind {
	case "jira":
		if err := validateJira(src); err != nil {
			return err
		}
	case "github":
		if err := validateGitHub(src); err != nil {
			return err
		}
	default:
		return fmt.Errorf("source %q field \"kind\": expected jira or github, found %q", src.Name, src.Kind)
	}
	if err := validateCwd(src, env); err != nil {
		return err
	}
	for _, check := range []func(*Source) error{
		validateStartPrompt,
		validateSourceWorkerAndOutput,
		validateSourceClaim,
		validateSourceTrust,
		validateWorkspaceRules,
		validateAssignee,
		validateParentBlocks,
		validatePriorities,
		validateRankOrder,
		validateDoctorFields,
	} {
		if err := check(src); err != nil {
			return err
		}
	}
	return validatePRTitle(src)
}

func validateDoctorFields(src *Source) error {
	if src.ExternalBlockerLookups < 0 {
		return fmt.Errorf("source %q field %q: expected a non-negative count, found %d", src.Name, src.ExternalBlockerKey, src.ExternalBlockerLookups)
	}
	for _, field := range []struct {
		name  string
		types []string
	}{
		{name: "claim_parent_types", types: src.ClaimParentTypes},
		{name: "orphan_epic_child_types", types: src.OrphanEpicChildTypes},
	} {
		for _, typ := range field.types {
			if strings.TrimSpace(typ) == "" {
				return fmt.Errorf("source %q field %q: expected a non-empty item type, found %q", src.Name, field.name, typ)
			}
		}
	}
	return nil
}

func validateRankOrder(src *Source) error {
	if src.RankOrder == nil {
		return nil
	}
	if _, err := rank.ParseOrder(src.RankOrder); err != nil {
		return fmt.Errorf("source %q field \"rank_order\": %w", src.Name, err)
	}
	return nil
}

func validatePriorities(src *Source) error {
	for name := range src.PriorityRanks {
		if name == "" {
			return fmt.Errorf("source %q field \"priority_ranks\": expected a non-empty priority name, found empty", src.Name)
		}
	}
	for _, name := range src.KnownPriorities {
		if name == "" {
			return fmt.Errorf("source %q field \"known_priorities\": expected a non-empty priority name, found empty", src.Name)
		}
	}
	return nil
}

// deferFieldPattern is a Jira field id: customfield_N or a built-in name.
var deferFieldPattern = regexp.MustCompile(`^(customfield_[0-9]+|[a-z][a-zA-Z]*)$`)

func validateJira(src *Source) error {
	if err := validateSite(src); err != nil {
		return err
	}
	if src.Email == "" {
		return fieldEmpty(src.Name, "email")
	}
	if err := validateTokenChoice(src); err != nil {
		return err
	}
	if src.DeferLabelPrefix != "" {
		return fmt.Errorf("source %q field \"defer_label_prefix\": expected it only on a github source, found it on jira; use defer_field", src.Name)
	}
	if src.PriorityField != "" {
		return fmt.Errorf("source %q field \"priority_field\": expected it only on a github source, found it on jira", src.Name)
	}
	if src.DueField != "" {
		return fmt.Errorf("source %q field \"due_field\": expected it only on a github source, found it on jira", src.Name)
	}
	if src.DeferField != "" && !deferFieldPattern.MatchString(src.DeferField) {
		return fmt.Errorf("source %q field \"defer_field\": expected a Jira field id such as customfield_12345 or duedate, found %q", src.Name, src.DeferField)
	}
	fields := []fieldValue{
		{name: "jql", value: src.JQL},
		{name: "cwd", value: src.Cwd},
		{name: "base", value: src.Base},
		{name: "start_prompt", value: src.StartPrompt},
	}
	if src.Claim.MarkInProgress {
		fields = append(fields, fieldValue{name: "claim_status", value: src.ClaimStatus})
	}
	return requireFields(src.Name, fields)
}

func validateGitHub(src *Source) error {
	if !repoPattern.MatchString(src.Repo) {
		return fmt.Errorf("source %q field \"repo\": expected owner/name, found %q", src.Name, src.Repo)
	}
	for _, field := range []fieldValue{
		{name: "priority_field", value: src.PriorityField},
		{name: "defer_field", value: src.DeferField},
		{name: "due_field", value: src.DueField},
		{name: "defer_label_prefix", value: src.DeferLabelPrefix},
	} {
		if field.value != "" && strings.TrimSpace(field.value) != field.value {
			return fmt.Errorf("source %q field %q: expected no leading or trailing spaces, found %q", src.Name, field.name, field.value)
		}
	}
	fields := []fieldValue{
		{name: "cwd", value: src.Cwd},
		{name: "base", value: src.Base},
		{name: "start_prompt", value: src.StartPrompt},
	}
	if err := requireFields(src.Name, fields); err != nil {
		return err
	}
	if src.Claim.MarkInProgress && src.ClaimStatus == "" && src.ClaimLabel == "" {
		return fmt.Errorf("source %q: expected claim_status or claim_label when claim.mark_in_progress is true, found neither", src.Name)
	}
	return nil
}

func validateSite(src *Source) error {
	parsed, err := url.Parse(src.Site)
	if err != nil || parsed.Scheme != "https" || parsed.Host == "" {
		return fmt.Errorf("source %q field \"site\": expected an https URL, found %q", src.Name, src.Site)
	}
	return nil
}

func validateTokenChoice(src *Source) error {
	switch {
	case src.TokenFile != "" && src.TokenEnv != "":
		return fmt.Errorf("source %q field \"token_file\": expected exactly one of token_file or token_env, found both", src.Name)
	case src.TokenFile == "" && src.TokenEnv == "":
		return fmt.Errorf("source %q field \"token_file\": expected exactly one of token_file or token_env, found neither", src.Name)
	default:
		return nil
	}
}

func validateCwd(src *Source, env Env) error {
	info, err := env.Stat(src.Cwd)
	if err != nil {
		return fmt.Errorf("source %q field \"cwd\": expected a directory, found %s: %w", src.Name, src.Cwd, err)
	}
	if !info.IsDir() {
		return fmt.Errorf("source %q field \"cwd\": expected a directory, found a file: %s", src.Name, src.Cwd)
	}
	return nil
}

// validateStartPrompt checks the prompt placeholders and the source's
// resolved lease rule settings.
func validateStartPrompt(src *Source) error {
	if err := validatePlaceholders(src.Name, src.StartPrompt); err != nil {
		return err
	}
	if err := validatePlaceholdersFor(fmt.Sprintf("source %q", src.Name), "start_prompt_append", src.StartPromptAppend, false); err != nil {
		return err
	}
	return validLeasePrompt(fmt.Sprintf("source %q lease", src.Name), src.Lease)
}

func validateSourceWorkerAndOutput(src *Source) error {
	if err := validWorker(fmt.Sprintf("source %q worker", src.Name), src.Worker, true, true); err != nil {
		return err
	}
	return validOutput(fmt.Sprintf("source %q output", src.Name), src.Output)
}

func validateSourceClaim(src *Source) error {
	return validClaim(fmt.Sprintf("source %q claim", src.Name), src.Claim)
}

func validatePlaceholders(name, prompt string) error {
	return validatePlaceholdersFor(fmt.Sprintf("source %q", name), "start_prompt", prompt, true)
}

func validatePlaceholdersFor(where, field, prompt string, allowDoubleBrace bool) error {
	if allowDoubleBrace {
		// {{lease_rule}}, {{trust_rule}}, and {{start_prompt_append}} are the
		// double-brace placeholders. They are checked as whole tokens; any other
		// brace shape is still refused below.
		prompt = strings.ReplaceAll(prompt, LeaseRulePlaceholder, "")
		prompt = strings.ReplaceAll(prompt, TrustRulePlaceholder, "")
		prompt = strings.ReplaceAll(prompt, StartPromptAppendPlaceholder, "")
	}
	for i := 0; i < len(prompt); {
		switch prompt[i] {
		case '}':
			return fmt.Errorf("%s field %q: literal braces aren't supported, found %q", where, field, "}")
		case '{':
			rel := strings.IndexByte(prompt[i+1:], '}')
			if rel < 0 {
				return fmt.Errorf("%s field %q: literal braces aren't supported, found %q", where, field, prompt[i:])
			}
			word := prompt[i+1 : i+1+rel]
			if _, ok := allowedPlaceholders[word]; !ok {
				if isWord(word) {
					return fmt.Errorf("%s field %q: expected one of {id}, {title}, {url}, {source}, {base}, {parent}, {caller}, {caller_bus}, {report_back}, found {%s}", where, field, word)
				}
				return fmt.Errorf("%s field %q: literal braces aren't supported, found %q", where, field, prompt[i:i+rel+2])
			}
			i += rel + 2
		default:
			i++
		}
	}
	return nil
}

func isWord(value string) bool {
	if value == "" {
		return false
	}
	for _, r := range value {
		if (r < 'a' || r > 'z') && (r < 'A' || r > 'Z') {
			return false
		}
	}
	return true
}

func validateAgentRules(rules []AgentRule) error {
	for i, rule := range rules {
		if len(rule.Labels) == 0 {
			return fmt.Errorf("agent_rule %d: expected labels or another matcher, found neither", i+1)
		}
		for _, label := range rule.Labels {
			if label == "" {
				return fmt.Errorf("agent_rule %d field \"labels\": expected a non-empty label, found empty", i+1)
			}
		}
		if err := validateArgList(fmt.Sprintf("agent_rule %d field \"agent_args\"", i+1), rule.AgentArgs); err != nil {
			return err
		}
	}
	return nil
}

func validateArgList(field string, args []string) error {
	for _, arg := range args {
		if arg == "" {
			return fmt.Errorf("%s: expected a non-empty argument, found empty", field)
		}
	}
	return nil
}

func validateWorkspaceRules(src *Source) error {
	for _, rule := range src.WorkspaceRules {
		hasParent := rule.Parent != ""
		hasLabel := rule.Label != ""
		switch {
		case hasParent && hasLabel:
			return fmt.Errorf("source %q field \"workspace_rule\": expected exactly one of parent or label, found both", src.Name)
		case !hasParent && !hasLabel:
			return fmt.Errorf("source %q field \"workspace_rule\": expected exactly one of parent or label, found neither", src.Name)
		case rule.Workspace == "":
			return fmt.Errorf("source %q field \"workspace_rule\": expected a non-empty workspace, found empty", src.Name)
		}
	}
	return nil
}

// CreateAssignee values.
const (
	// CreateAssigneeMe assigns a filed item to the authenticated user.
	CreateAssigneeMe = "me"
	// CreateAssigneeUnassigned leaves a filed item unassigned.
	CreateAssigneeUnassigned = "unassigned"
)

func validateAssignee(src *Source) error {
	switch src.AssigneeOK {
	case "me", "unassigned", "any":
	default:
		return fmt.Errorf("source %q field \"assignee_ok\": expected me, unassigned, or any, found %q", src.Name, src.AssigneeOK)
	}
	switch src.CreateAssignee {
	case CreateAssigneeMe, CreateAssigneeUnassigned:
		return nil
	default:
		return fmt.Errorf("source %q field \"create_assignee\": expected me or unassigned, found %q", src.Name, src.CreateAssignee)
	}
}

func validateParentBlocks(src *Source) error {
	if _, err := IgnoreAncestorBlocks(src.ParentBlocksChild); err != nil {
		return fmt.Errorf("source %q field \"parent_blocks_child\": %w", src.Name, err)
	}
	return nil
}

// IgnoreAncestorBlocks reports whether an ancestor's blocks link is left out
// of readiness and of the ancestor's unblock score.
// mode must be ignore or honor. An empty or unknown mode is an error.
func IgnoreAncestorBlocks(mode string) (bool, error) {
	switch mode {
	case ParentBlocksIgnore:
		return true, nil
	case ParentBlocksHonor:
		return false, nil
	case "":
		return false, errors.New("expected ignore or honor, found empty")
	default:
		return false, fmt.Errorf("expected ignore or honor, found %q", mode)
	}
}

func validatePRTitle(src *Source) error {
	if src.Kind != "jira" && src.PRTitlePattern == "" {
		return nil
	}
	if !strings.Contains(src.PRTitlePattern, "{id}") {
		return fmt.Errorf("source %q field \"pr_title_pattern\": expected a pattern containing {id}, found %q", src.Name, src.PRTitlePattern)
	}
	rewritten := strings.ReplaceAll(src.PRTitlePattern, "{id}", "X-1")
	if _, err := regexp.Compile(rewritten); err != nil {
		return fmt.Errorf("source %q field \"pr_title_pattern\": expected a valid regexp once {id} is replaced with X-1, found %q: %w", src.Name, src.PRTitlePattern, err)
	}
	return nil
}

type fieldValue struct {
	name  string
	value string
}

func requireFields(source string, fields []fieldValue) error {
	for _, field := range fields {
		if field.value == "" {
			return fieldEmpty(source, field.name)
		}
	}
	return nil
}

func fieldEmpty(source, field string) error {
	return fmt.Errorf("source %q field %q: expected a non-empty value, found empty", source, field)
}
