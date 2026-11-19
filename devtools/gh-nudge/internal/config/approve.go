package config

import (
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"time"

	"github.com/bmatcuk/doublestar/v4"

	pklconfig "github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl"
)

// DefaultApproveBody is the gh-approve review body when config omits body.
const DefaultApproveBody = "LGTM"

// DefaultAgentTimeout bounds an approve rule's agent command when config
// omits agent_timeout.
const DefaultAgentTimeout = 120 * time.Second

// DefaultFileCheckTimeout bounds a file_checks script when config omits
// file_checks_timeout.
const DefaultFileCheckTimeout = 30 * time.Second

// Built-in file_checks names.
const (
	FileCheckOnlyDeletions            = "only-deletions"
	FileCheckDeletionsAndCommentsOnly = "deletions-and-comments-only"
)

// FileCheck is one entry in ApproveRule.FileChecks: either a built-in name
// or a script argv list.
type FileCheck struct {
	Builtin string
	Argv    []string
}

// Defaults of the agent_command decision cache when config omits them.
const (
	DefaultAgentCacheTTL   = 7 * 24 * time.Hour
	DefaultAgentCacheMaxMB = 50
)

// Defaults of the patch size cap when config omits it.
const (
	DefaultPatchMaxFiles      = 300
	DefaultPatchMaxAddedLines = 5000
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
	// NeverMark are globs. A changed file matching any of them is never
	// marked, by any rule. This wins over every rule's selectors.
	NeverMark []string
	// Rules select the files. A file is marked when any rule selects it
	// and NeverMark does not.
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
	// PathsNoneMatch are globs. A changed file matching any of them is not
	// selected by this rule, even when a selector matches. Another rule
	// can still select it. except_paths in config is an alias of this.
	PathsNoneMatch []string
	// CodeownersNotMine selects a file whose owners include neither the
	// user nor a team the user belongs to.
	CodeownersNotMine bool
	// exceptPaths holds the except_paths alias until validation merges it.
	exceptPaths []string
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
	// PatchMaxFiles is the most changed files a pull request may have for
	// its patches to be fetched. Zero means DefaultPatchMaxFiles.
	PatchMaxFiles int `yaml:"-"`
	// PatchMaxAddedLines is the most added lines a pull request may have for
	// its patches to be fetched. Zero means DefaultPatchMaxAddedLines.
	PatchMaxAddedLines int `yaml:"-"`
	// MaxDuration stops a pass from starting pull requests after this limit.
	MaxDuration time.Duration `yaml:"-"`
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
	// RequestedViaTeam also runs the rule when the user is requested only
	// through a team they belong to.
	RequestedViaTeam bool
	// MineCommentsOnly requires at least one changed file owned by the user
	// or a team of the user, and every such file to change only comment or
	// blank lines.
	MineCommentsOnly bool
	// Body overrides ApproveConfig.Body for this rule. Empty uses the global
	// body. See BodyPlaceholders.
	Body string
	// FileChecks maps a glob pattern to a built-in check or script argv.
	// Matched files are judged only by their most specific pattern's check.
	FileChecks map[string]FileCheck
	// FileChecksTimeout bounds each file_checks script. Zero means
	// DefaultFileCheckTimeout.
	FileChecksTimeout time.Duration
	// MarkViewedOnPass marks files whose file_checks check returns approve
	// as viewed on the pull request.
	MarkViewedOnPass bool
}

// Placeholders an approve rule's body may contain.
const (
	PlaceholderMyFiles = "my_files"
	PlaceholderMyTeams = "my_teams"
	PlaceholderRule    = "rule"
)

var placeholderPattern = regexp.MustCompile(`\{([A-Za-z0-9_]+)\}`)

// BodyPlaceholders returns the names of the {name} placeholders in body, in
// order. A body with none is used as the global body is.
func BodyPlaceholders(body string) []string {
	var names []string
	for _, m := range placeholderPattern.FindAllStringSubmatch(body, -1) {
		names = append(names, m[1])
	}
	return names
}

func convertApprove(src pklconfig.ApproveConfig, configDir string) ApproveConfig {
	out := ApproveConfig{
		Body:                src.Body,
		SkipAlreadyApproved: src.SkipAlreadyApproved,
		AgentCacheTTL:       src.AgentCacheTtl.GoDuration(),
		AgentCacheMaxMB:     src.AgentCacheMaxMb,
		PatchMaxFiles:       src.PatchMaxFiles,
		PatchMaxAddedLines:  src.PatchMaxAddedLines,
	}
	if src.MaxDuration != nil {
		out.MaxDuration = src.MaxDuration.GoDuration()
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
	if out.PatchMaxFiles <= 0 {
		out.PatchMaxFiles = DefaultPatchMaxFiles
	}
	if out.PatchMaxAddedLines <= 0 {
		out.PatchMaxAddedLines = DefaultPatchMaxAddedLines
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
			RequestedViaTeam:         rule.RequestedViaTeam,
			MineCommentsOnly:         rule.MineCommentsOnly,
			FileChecks:               convertFileChecks(rule.FileChecks, configDir),
			FileChecksTimeout:        rule.FileChecksTimeout.GoDuration(),
			MarkViewedOnPass:         rule.MarkViewedOnPass,
		}
		if rule.Body != nil {
			converted.Body = *rule.Body
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
		NeverMark:      cloneStrings(src.NeverMark),
	}
	for _, rule := range src.Rules {
		out.Rules = append(out.Rules, ViewedRule{
			Name:              rule.Name,
			Repos:             cloneStrings(rule.Repos),
			PathsMatch:        cloneStrings(rule.PathsMatch),
			PathsNoneMatch:    cloneStrings(rule.PathsNoneMatch),
			exceptPaths:       cloneStrings(rule.ExceptPaths),
			CodeownersNotMine: rule.CodeownersNotMine,
		})
	}
	return out
}

func validateViewed(viewed *ViewedConfig) error {
	if viewed.MaxMarksPerRun < 0 {
		return fmt.Errorf("viewed max_marks_per_run must be >= 0")
	}
	for _, pattern := range viewed.NeverMark {
		if !doublestar.ValidatePattern(pattern) {
			return fmt.Errorf("viewed never_mark glob %q is invalid", pattern)
		}
	}
	seen := make(map[string]bool, len(viewed.Rules))
	for i, rule := range viewed.Rules {
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
		if len(rule.PathsNoneMatch) > 0 && len(rule.exceptPaths) > 0 {
			return fmt.Errorf("viewed rule %q sets both paths_none_match and except_paths", rule.Name)
		}
		if len(rule.PathsNoneMatch) == 0 {
			rule.PathsNoneMatch = rule.exceptPaths
		}
		rule.exceptPaths = nil
		viewed.Rules[i] = rule
		globs := map[string][]string{
			"repos":            rule.Repos,
			"paths_match":      rule.PathsMatch,
			"paths_none_match": rule.PathsNoneMatch,
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
	for _, warning := range FileChecksWarnings(approve.Rules) {
		slog.Warn(warning)
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
	if err := validateAction(rule); err != nil {
		return err
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
	if err := validateFileChecks(rule.FileChecks); err != nil {
		return err
	}
	return validateBody(rule)
}

// UncoveredLimitedBy returns the names of the rule's conditions that limit
// which files not covered by file_checks may change: paths_all_match,
// codeowners_covered and mine_comments_only. When the rule sets none, a file
// no file_checks pattern covers fails file_checks.
func (r ApproveRule) UncoveredLimitedBy() []string {
	var names []string
	if len(r.PathsAllMatch) > 0 {
		names = append(names, "paths_all_match")
	}
	if r.CodeownersCovered {
		names = append(names, "codeowners_covered")
	}
	if r.MineCommentsOnly {
		names = append(names, "mine_comments_only")
	}
	return names
}

// FileChecksWarnings returns a warning for each rule that sets file_checks
// without a condition that limits the files file_checks does not cover.
func FileChecksWarnings(rules []ApproveRule) []string {
	var out []string
	for _, rule := range rules {
		if len(rule.FileChecks) > 0 && len(rule.UncoveredLimitedBy()) == 0 {
			out = append(out, fmt.Sprintf("approve rule %q sets file_checks without paths_all_match, codeowners_covered or mine_comments_only: a changed file that no file_checks pattern covers fails the rule", rule.Name))
		}
	}
	return out
}

func validateAction(rule ApproveRule) error {
	switch rule.Action {
	case ActionApprove:
		if len(rule.Reviewers) > 0 {
			return fmt.Errorf("reviewers is only allowed on a handoff rule")
		}
	case ActionHandoff:
		if len(rule.Reviewers) == 0 {
			return fmt.Errorf("reviewers is required on a handoff rule")
		}
		if rule.RequestedViaTeam {
			return fmt.Errorf("requested_via_team is only allowed on an approve rule")
		}
		if rule.Body != "" {
			return fmt.Errorf("body is only allowed on an approve rule")
		}
	default:
		return fmt.Errorf("action %q is not approve or handoff", rule.Action)
	}
	return nil
}

func validateBody(rule ApproveRule) error {
	for _, name := range BodyPlaceholders(rule.Body) {
		switch name {
		case PlaceholderRule:
		case PlaceholderMyFiles, PlaceholderMyTeams:
			if !rule.MineCommentsOnly {
				return fmt.Errorf("body placeholder {%s} needs mine_comments_only", name)
			}
		default:
			return fmt.Errorf("unknown body placeholder {%s}", name)
		}
	}
	return nil
}

func convertFileChecks(src map[string]any, configDir string) map[string]FileCheck {
	if len(src) == 0 {
		return nil
	}
	out := make(map[string]FileCheck, len(src))
	for pattern, raw := range src {
		out[pattern] = convertFileCheckValue(raw, configDir)
	}
	return out
}

func convertFileCheckValue(raw any, configDir string) FileCheck {
	switch v := raw.(type) {
	case string:
		switch v {
		case FileCheckOnlyDeletions, FileCheckDeletionsAndCommentsOnly:
			return FileCheck{Builtin: v}
		default:
			return FileCheck{Argv: []string{resolveScriptPath(v, configDir, true)}}
		}
	case []string:
		argv := cloneStrings(v)
		if len(argv) > 0 {
			argv[0] = resolveScriptPath(argv[0], configDir, false)
		}
		return FileCheck{Argv: argv}
	case []any:
		argv := make([]string, 0, len(v))
		for _, item := range v {
			if s, ok := item.(string); ok {
				argv = append(argv, s)
			}
		}
		if len(argv) > 0 {
			argv[0] = resolveScriptPath(argv[0], configDir, false)
		}
		return FileCheck{Argv: argv}
	default:
		return FileCheck{}
	}
}

// resolveScriptPath expands a leading ~ and resolves relative paths against
// configDir. Bare command names without a slash are left for PATH lookup
// unless preferConfigDir is true and the file exists in configDir.
func resolveScriptPath(p, configDir string, preferConfigDir bool) string {
	if p == "" {
		return ""
	}
	if p == "~" || strings.HasPrefix(p, "~/") {
		if home, err := os.UserHomeDir(); err == nil {
			if p == "~" {
				return home
			}
			return filepath.Join(home, p[2:])
		}
	}
	if filepath.IsAbs(p) || configDir == "" {
		return p
	}
	if strings.ContainsRune(p, '/') || strings.ContainsRune(p, filepath.Separator) {
		return filepath.Join(configDir, p)
	}
	if preferConfigDir {
		joined := filepath.Join(configDir, p)
		if _, err := os.Stat(joined); err == nil {
			return joined
		}
	}
	return p
}

// PatternSpec ranks a file_checks glob pattern's specificity: exact paths beat
// globs, then more literal path segments, then longer literal text.
type PatternSpec struct {
	Exact           bool
	LiteralSegments int
	LiteralLen      int
}

// FileCheckSpecificity computes the specificity of a file_checks pattern.
func FileCheckSpecificity(pattern string) PatternSpec {
	exact := !hasGlobMeta(pattern)
	litSegs := 0
	for _, seg := range strings.Split(pattern, "/") {
		if seg != "" && !hasGlobMeta(seg) {
			litSegs++
		}
	}
	return PatternSpec{
		Exact:           exact,
		LiteralSegments: litSegs,
		LiteralLen:      literalTextLen(pattern),
	}
}

func hasGlobMeta(s string) bool {
	return strings.ContainsAny(s, "*?[{")
}

func literalTextLen(pattern string) int {
	n := 0
	for i := 0; i < len(pattern); i++ {
		switch pattern[i] {
		case '/', '*', '?':
		case '[':
			if end := strings.IndexByte(pattern[i+1:], ']'); end >= 0 {
				i += end + 1
			}
		case '{':
			if end := strings.IndexByte(pattern[i+1:], '}'); end >= 0 {
				i += end + 1
			}
		default:
			n++
		}
	}
	return n
}

func moreSpecific(a, b PatternSpec) bool {
	if a.Exact != b.Exact {
		return a.Exact
	}
	if a.LiteralSegments != b.LiteralSegments {
		return a.LiteralSegments > b.LiteralSegments
	}
	return a.LiteralLen > b.LiteralLen
}

// MatchFileCheck returns the most specific pattern in checks that matches
// path, along with its check.
func MatchFileCheck(checks map[string]FileCheck, path string) (string, FileCheck, bool) {
	var (
		bestPat   string
		bestCheck FileCheck
		bestSpec  PatternSpec
		found     bool
	)
	for pattern, check := range checks {
		ok, err := doublestar.Match(pattern, path)
		if err != nil || !ok {
			continue
		}
		spec := FileCheckSpecificity(pattern)
		if !found || moreSpecific(spec, bestSpec) {
			bestPat = pattern
			bestCheck = check
			bestSpec = spec
			found = true
		}
	}
	return bestPat, bestCheck, found
}

func validateFileChecks(checks map[string]FileCheck) error {
	if len(checks) == 0 {
		return nil
	}
	patterns := make([]string, 0, len(checks))
	for pattern, check := range checks {
		if pattern == "" || !doublestar.ValidatePattern(pattern) {
			return fmt.Errorf("file_checks glob %q is invalid", pattern)
		}
		switch {
		case check.Builtin == FileCheckOnlyDeletions || check.Builtin == FileCheckDeletionsAndCommentsOnly:
		case check.Builtin != "":
			return fmt.Errorf("file_checks %q: unknown built-in %q", pattern, check.Builtin)
		case len(check.Argv) == 0 || check.Argv[0] == "":
			return fmt.Errorf("file_checks %q: script must not be empty", pattern)
		}
		patterns = append(patterns, pattern)
	}
	sort.Strings(patterns)
	bySpec := make(map[PatternSpec]string, len(patterns))
	for _, pattern := range patterns {
		spec := FileCheckSpecificity(pattern)
		if prev, exists := bySpec[spec]; exists {
			return fmt.Errorf("file_checks patterns %q and %q have tied specificity", prev, pattern)
		}
		bySpec[spec] = pattern
	}
	return nil
}
