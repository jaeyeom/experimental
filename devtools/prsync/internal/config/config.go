// Package config loads the shell-sourceable prsync KEY=VALUE file.
package config

import (
	"errors"
	"fmt"
	"io/fs"
	"math"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"time"
	"unicode"
)

const branchSwitchPreamble = `Get onto this PR's branch before editing:
1. If the working tree has uncommitted changes, stash or commit them first.
2. Check out {head} (if absent locally, run ` + "`gh pr checkout {number}`" + `).
3. Fetch origin and bring {head} up to date with origin/{base} as needed.
Do not start editing until you are on {head} at its latest tip.`

const defaultPromptTemplate = branchSwitchPreamble + `

Address the unresolved review comments on PR #{number} ({url}). State: {state}.

Unaddressed threads:
{comments}

Triage each thread before editing or pushing:
- Mechanical / unambiguous (typo, nil check, rename, obvious bug): make the
  change without asking the user, and continue.
- Design decision, tradeoff, or disagreement: pause and ask the user how to
  proceed, with a recommended option. Do not edit, reply to the reviewer,
  resolve, or push for that thread until they answer.

Push after the mechanical threads are done (and after the user answers any
questions).

Reply on every listed thread once its outcome is known: right away for a
mechanical thread, and after the user answers for any other. Post the reply
on that review thread, not as a new top-level PR comment. Then mark the
thread resolved, unless the reply asks the reviewer a follow-up question.
Leave a follow-up open so the reviewer can answer. A reply that closes the
point (change made, already done, or declined) must be resolved.

After those threads are handled and pushed, re-request review only from the
human authors of the threads listed above, once all of their listed comments
are addressed:
  gh pr edit {number} --add-reviewer <login>
NEVER re-request anyone who has already approved: check
` + "`gh pr view {number} --json latestReviews`" + ` first and skip every login whose
state is APPROVED, even if you addressed their threads. Also skip bots, the
PR author, anyone already pending, and anyone who still has
unanswered threads. If nobody is left, request nobody. Do not dismiss reviews.

Do not touch other PRs.`

const defaultRebasePromptTemplate = `Rebase PR #{number} ({url}) onto origin/{base} in this working directory. State: {state}. Do not create a new worktree.

1. Check out {head}.
2. Fetch origin.
3. Rebase onto origin/{base}.
4. Resolve any conflicts.
5. Push with --force-with-lease.

Do not touch other PRs.`

const defaultCIFixPromptTemplate = branchSwitchPreamble + `

Fix the failing CI on PR #{number} ({url}). State: {state}. Stay scoped to this PR.
Do not touch unrelated failures or other PRs.

Caller hint (non-authoritative starting point; verify against live CI
state, do not trust — CI may have re-run, e.g. after a rebase, and the
failure may have changed):
{hint}

Use the repo's available CI-log / flaky-test investigation tools if any.
Do not assume a particular CI provider or build system.

1. Reproduce before touching anything. Run the failing check locally on
   this branch and confirm it actually fails before proposing a change.
2. Triage before fixing. Not every red check is a code bug:
   - a flaky or infrastructure failure → do not "fix" it; report back that
     it is not a code problem and stop.
   - a genuine failure with an unambiguous fix → fix it, verify locally,
     push.
   - a failure whose fix is a judgment call (a design tradeoff, or one
     that needs new tests written to a scope that isn't obvious) → pause
     and ask the user, with a recommended option, before editing or
     pushing.
3. Verify locally before pushing. Re-run the same check and confirm it
   now passes; never push an unverified fix.`

var repoPattern = regexp.MustCompile(`^[^/\s]+/[^/\s]+$`)

// Config is the resolved prsync configuration.
type Config struct {
	Repos            []string
	Author           string
	TitleIDPattern   *regexp.Regexp
	TabLabelTemplate string
	HerdrBin         string
	// Machines are saved herdr machines searched for PR tabs in addition
	// to the local server. Empty means local only.
	Machines []string
	// MachineTimeout bounds each herdr call to one of Machines, except
	// prompt and wait, which use DispatchTimeout.
	MachineTimeout time.Duration
	// TabMachine scopes a run to PRs whose tab is on this machine: local
	// or one of Machines. Empty means every machine.
	TabMachine string
	// Untabbed is include or exclude for PRs with no tab. Empty means
	// only the local run includes them.
	Untabbed string
	// SkipJobs are machine-wide steps (Jobs) this run does not perform.
	SkipJobs          []string
	GHBin             string
	JiraBin           string
	ConcurrencyWaitOn string
	// ConcurrencyIgnoreBlocked is true when a blocked agent does not hold
	// the gate by its status. The default, false, holds the gate for a
	// blocked agent.
	ConcurrencyIgnoreBlocked bool
	// ConcurrencyCwd is any or checkout. any counts working and blocked
	// agents anywhere; idle-tab signals still need the tab in Checkout.
	ConcurrencyCwd string
	// Checkout is the dispatch repo's checkout. It is not a file key.
	// checkout mode counts an agent only when cwd or foreground_cwd is inside it.
	Checkout             string
	GatePoll             time.Duration
	GateTimeout          time.Duration
	PromptTemplate       string
	RebasePromptTemplate string
	CIFixPromptTemplate  string
	// PromptSuffix is appended after a blank line to every rendered dispatch
	// prompt. Empty leaves prompts unchanged. It is not part of dedupe.
	PromptSuffix    string
	IncludeDrafts   bool
	WaitUntil       []string
	DispatchTimeout time.Duration
	StateFile       string
	// DedupeTTL is how long the same unaddressed comment ids stay deduped.
	// Zero expires immediately.
	DedupeTTL time.Duration
	// DedupeScans is how many later evaluations of those ids expire dedupe.
	// Zero means the scan count does not expire it.
	DedupeScans         int
	DryRun              bool
	CommentLinkMaxChars int
	CommentBodyMaxChars int
	// CommentStripPatterns are removed from each comment body before link
	// stripping and the body cap. Empty means no stripping.
	CommentStripPatterns []*regexp.Regexp
	// CIIgnoreChecks are path globs dropped from ci_state when CIOnlyChecks
	// is empty. Patterns match a CheckRun name or a StatusContext context.
	// Empty counts every rollup entry.
	CIIgnoreChecks []string
	// CIOnlyChecks, when non-empty, are the only path globs that feed
	// ci_state. They win over CIIgnoreChecks.
	CIOnlyChecks []string

	// TeamCover suggests individual reviewers for requested teams.
	// Report-only: scan never requests a reviewer itself. Default off,
	// so a scan's JSON and GitHub calls stay as they are today.
	TeamCover bool
	// TeamCoverPerTeam is how many people to suggest for one uncovered team.
	TeamCoverPerTeam int
	// TeamCoverSkipTeams are org/slug values that get no suggestion entry.
	TeamCoverSkipTeams []string
	// TeamCoverExcludeLogins are never suggested.
	TeamCoverExcludeLogins []string
	// TeamCoverSignals is the rank order. Empty ranks by login only.
	TeamCoverSignals []string
	// TeamCoverHistoryDays is the lookback for the history signal.
	TeamCoverHistoryDays int
	// TeamCoverActiveDays drops members with no org activity in this
	// many days. Zero disables the filter.
	TeamCoverActiveDays int

	// GateIgnoreProcs are patterns for an agent's helpers (MCP and
	// language servers, status-line commands). A plain pattern matches
	// as a substring and one with *, ?, or [ as a glob, against the
	// process name, the argv[0] base, or the whole command line. A
	// descendant of an idle agent that matches none of them, and is old
	// enough, is a background job that holds the gate.
	GateIgnoreProcs []string
	// GateJobMinAge is how old a descendant must be to count as a
	// background job.
	GateJobMinAge time.Duration
	// GateBuildTools are build-tool client names, matched exactly (or
	// as globs) against the process name or argv[0] base. One running
	// with its cwd in the checkout holds the gate. Empty turns the
	// signal off.
	GateBuildTools []string
	// GateBuildToolIgnore are patterns, matched like GateIgnoreProcs,
	// for build-tool processes that never count, such as a long-lived
	// build server.
	GateBuildToolIgnore []string
	// GatePendingQuestion is decide or off. decide holds a tab while a
	// decide item for its pane is answered but not done, or was opened
	// since the round started.
	GatePendingQuestion string
	// GateLease is auto, off, or required. auto and required read the
	// agentbus workdir lease for a guarded checkout; required also
	// holds the gate when the lease cannot be read.
	GateLease string
	// GateExemptPanes are pane-id globs, and GateExemptTabs tab-label
	// globs, that the gate never waits on.
	GateExemptPanes []string
	GateExemptTabs  []string

	// SourcePath is empty if defaults only; for stderr diagnostics.
	SourcePath string
}

// KeyError is a config parse or validation failure mapped to exit 2.
type KeyError struct {
	Key    string
	Line   int
	Reason string
}

func (e *KeyError) Error() string {
	if e == nil {
		return ""
	}
	if e.Key != "" {
		return fmt.Sprintf("key %s: %s", e.Key, e.Reason)
	}
	if e.Line > 0 {
		return fmt.Sprintf("line %d: %s", e.Line, e.Reason)
	}
	return e.Reason
}

// Defaults returns the built-in configuration.
func Defaults() Config {
	return Config{
		TitleIDPattern:           regexp.MustCompile(`[A-Z]+-[0-9]+`),
		TabLabelTemplate:         "{id}",
		HerdrBin:                 "herdr",
		MachineTimeout:           5 * time.Second,
		GHBin:                    "gh",
		JiraBin:                  "jira",
		ConcurrencyWaitOn:        "any",
		ConcurrencyIgnoreBlocked: false,
		ConcurrencyCwd:           "any",
		GatePoll:                 2000 * time.Millisecond,
		GateTimeout:              1800000 * time.Millisecond,
		PromptTemplate:           defaultPromptTemplate,
		RebasePromptTemplate:     defaultRebasePromptTemplate,
		CIFixPromptTemplate:      defaultCIFixPromptTemplate,
		WaitUntil:                []string{"idle", "done"},
		DispatchTimeout:          1800000 * time.Millisecond,
		StateFile:                "~/.config/prsync/state.json",
		DedupeTTL:                24 * time.Hour,
		DryRun:                   true,
		CommentLinkMaxChars:      500,
		CommentBodyMaxChars:      4000,
		TeamCoverPerTeam:         1,
		TeamCoverSignals:         []string{"codeowners", "history", "load"},
		TeamCoverHistoryDays:     90,
		GateIgnoreProcs:          slices.Clone(defaultGateIgnoreProcs),
		GateBuildTools:           []string{"bazel", "bazelisk", "make", "go", "cargo", "gradle", "npm", "pnpm"},
		GateBuildToolIgnore:      []string{"*--workspace_directory=*", "*A-server.jar*", "*--server*"},
		GatePendingQuestion:      "decide",
		GateLease:                "auto",
		GateJobMinAge:            10 * time.Second,
	}
}

// defaultGateIgnoreProcs are an agent's usual helpers: long-lived
// servers, the status-line command it reruns on each UI refresh, and
// the Claude Code sandbox's socat bridges (claude-http-<hex>.sock and
// other claude-<kind>-<hex>.sock), which live as long as the session.
var defaultGateIgnoreProcs = []string{
	"caffeinate", "gopls", "rust-analyzer", "clangd", "pyright*",
	"*language-server*", "*langserver*",
	"*mcp-server*", "*mcp_server*", "*modelcontextprotocol*", "*-mcp", "*-mcp *",
	"statusline", "status-line", "claude-hud",
	"claude-http-", "*UNIX-LISTEN:*claude-*.sock*",
}

// Load applies search order, then Validate.
// explicitPath is the --config value (empty if unset).
func Load(explicitPath string) (Config, error) {
	cfg := Defaults()
	path, err := resolvePath(explicitPath)
	if err != nil {
		return Config{}, err
	}
	if path != "" {
		cfg.SourcePath = path
		kv, err := parseFile(path)
		if err != nil {
			return Config{}, err
		}
		if err := apply(&cfg, kv); err != nil {
			return Config{}, err
		}
	}
	cfg.StateFile = expandTilde(cfg.StateFile)
	cfg.GHBin = expandTilde(cfg.GHBin)
	cfg.HerdrBin = expandTilde(cfg.HerdrBin)
	cfg.JiraBin = expandTilde(cfg.JiraBin)
	if err := cfg.Validate(); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

// Validate compiles regexes and checks enums. Returns *KeyError.
func (c Config) Validate() error {
	if c.TitleIDPattern == nil {
		return &KeyError{Key: "title_id_pattern", Reason: "missing pattern"}
	}
	if c.TabLabelTemplate == "" {
		return &KeyError{Key: "tab_label_template", Reason: "must be non-empty"}
	}
	if c.HerdrBin == "" {
		return &KeyError{Key: "herdr_bin", Reason: "must be non-empty"}
	}
	if c.GHBin == "" {
		return &KeyError{Key: "gh_bin", Reason: "must be non-empty"}
	}
	if c.JiraBin == "" {
		return &KeyError{Key: "jira_bin", Reason: "must be non-empty"}
	}
	if err := validateConcurrency(c); err != nil {
		return err
	}
	if err := validateScope(c); err != nil {
		return err
	}
	if c.GatePoll < time.Millisecond {
		return &KeyError{Key: "gate_poll_ms", Reason: "must be >= 1"}
	}
	if c.GateTimeout < time.Millisecond {
		return &KeyError{Key: "gate_timeout_ms", Reason: "must be >= 1"}
	}
	if c.DispatchTimeout < time.Millisecond {
		return &KeyError{Key: "dispatch_timeout_ms", Reason: "must be >= 1"}
	}
	if len(c.WaitUntil) == 0 {
		return &KeyError{Key: "dispatch_wait_until", Reason: "at least one token required"}
	}
	if c.StateFile == "" {
		return &KeyError{Key: "state_file", Reason: "must be non-empty"}
	}
	for _, repo := range c.Repos {
		if !repoPattern.MatchString(repo) {
			return &KeyError{Key: "repos", Reason: fmt.Sprintf("invalid repo %q", repo)}
		}
	}
	return nil
}

func validateConcurrency(c Config) error {
	if c.ConcurrencyWaitOn != "any" && c.ConcurrencyWaitOn != "managed" {
		return &KeyError{Key: "concurrency_wait_on", Reason: fmt.Sprintf("invalid value %q", c.ConcurrencyWaitOn)}
	}
	if c.ConcurrencyCwd != "any" && c.ConcurrencyCwd != "checkout" {
		return &KeyError{Key: "concurrency_cwd", Reason: fmt.Sprintf("invalid value %q", c.ConcurrencyCwd)}
	}
	if c.GatePendingQuestion != "decide" && c.GatePendingQuestion != "off" {
		return &KeyError{Key: "gate_pending_question", Reason: fmt.Sprintf("invalid value %q", c.GatePendingQuestion)}
	}
	if c.GateLease != "auto" && c.GateLease != "off" && c.GateLease != "required" {
		return &KeyError{Key: "gate_lease", Reason: fmt.Sprintf("invalid value %q", c.GateLease)}
	}
	return nil
}

func applyGateSignal(cfg *Config, key, val string) error {
	switch key {
	case "gate_pending_question":
		cfg.GatePendingQuestion = strings.TrimSpace(val)
		return nil
	case "gate_lease":
		cfg.GateLease = strings.TrimSpace(val)
		return nil
	case "gate_job_min_age":
		d, err := time.ParseDuration(strings.TrimSpace(val))
		if err != nil || d < 0 {
			return &KeyError{Key: key, Reason: fmt.Sprintf("invalid duration %q", val)}
		}
		cfg.GateJobMinAge = d
		return nil
	}
	patterns, err := splitGlobs(key, val)
	if err != nil {
		return err
	}
	switch key {
	case "gate_ignore_procs":
		cfg.GateIgnoreProcs = patterns
	case "gate_build_tools":
		cfg.GateBuildTools = patterns
	case "gate_build_tool_ignore":
		cfg.GateBuildToolIgnore = patterns
	case "gate_exempt_panes":
		cfg.GateExemptPanes = patterns
	case "gate_exempt_tabs":
		cfg.GateExemptTabs = patterns
	}
	return nil
}

func parseFile(path string) (map[string]string, error) {
	f, err := os.Open(path) //nolint:gosec // path is an operator-supplied config file
	if err != nil {
		return nil, &KeyError{Reason: fmt.Sprintf("open %s: %v", path, err)}
	}
	defer f.Close() //nolint:errcheck
	return Parse(f)
}

func resolvePath(explicit string) (string, error) {
	if explicit != "" {
		info, err := os.Stat(explicit)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				return "", &KeyError{Reason: fmt.Sprintf("config file not found: %s", explicit)}
			}
			return "", &KeyError{Reason: fmt.Sprintf("stat %s: %v", explicit, err)}
		}
		if info.IsDir() {
			return "", &KeyError{Reason: fmt.Sprintf("config file not found: %s", explicit)}
		}
		return explicit, nil
	}
	if path := filepath.Join(configCWD(), "prsync.config"); fileExists(path) {
		return path, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("home dir: %w", err)
	}
	if path := filepath.Join(home, ".config", "prsync", "prsync.config"); fileExists(path) {
		return path, nil
	}
	return "", nil
}

func configCWD() string {
	if wd := os.Getenv("BUILD_WORKING_DIRECTORY"); wd != "" {
		return wd
	}
	wd, err := os.Getwd()
	if err != nil {
		return ""
	}
	return wd
}

func fileExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}

func apply(cfg *Config, kv map[string]string) error {
	for key, val := range kv {
		if err := applyKey(cfg, key, val); err != nil {
			return err
		}
	}
	return nil
}

func applyKey(cfg *Config, key, val string) error {
	switch key {
	case "repos":
		return applyRepos(cfg, val)
	case "author":
		cfg.Author = val
	case "title_id_pattern":
		return applyTitleIDPattern(cfg, val)
	case "tab_label_template":
		cfg.TabLabelTemplate = val
	case "herdr_bin", "gh_bin", "jira_bin":
		applyBin(cfg, key, val)
	case "concurrency_wait_on", "concurrency_ignore_blocked", "concurrency_cwd":
		return applyConcurrency(cfg, key, val)
	case "gate_poll_ms", "gate_timeout_ms", "dispatch_timeout_ms", "machine_timeout_ms":
		return applyMillis(cfg, key, val)
	case "dispatch_prompt_template", "rebase_prompt_template", "ci_fix_prompt_template":
		return applyPrompt(cfg, key, val)
	case "dispatch_include_drafts":
		return applyIncludeDrafts(cfg, val)
	case "dispatch_wait_until":
		return applyWaitUntil(cfg, val)
	case "state_file":
		cfg.StateFile = val
	case "dedupe_ttl", "dedupe_scans":
		return applyDedupe(cfg, key, val)
	case "dry_run":
		return applyDryRun(cfg, val)
	default:
		return applyOtherKey(cfg, key, val)
	}
	return nil
}

func applyConcurrency(cfg *Config, key, val string) error {
	switch key {
	case "concurrency_wait_on":
		cfg.ConcurrencyWaitOn = val
	case "concurrency_ignore_blocked":
		return applyIgnoreBlocked(cfg, val)
	case "concurrency_cwd":
		cfg.ConcurrencyCwd = val
	}
	return nil
}

func applyBin(cfg *Config, key, val string) {
	switch key {
	case "herdr_bin":
		cfg.HerdrBin = val
	case "gh_bin":
		cfg.GHBin = val
	case "jira_bin":
		cfg.JiraBin = val
	}
}

func applyOtherKey(cfg *Config, key, val string) error {
	switch key {
	case "machines":
		return applyMachines(cfg, val)
	case "tab_machine", "untabbed", "skip_jobs":
		applyScopeKey(cfg, key, val)
		return nil
	case "dispatch_prompt_suffix":
		cfg.PromptSuffix = val
		return nil
	case "comment_link_max_chars", "comment_body_max_chars", "comment_strip_patterns":
		return applyCommentKey(cfg, key, val)
	case "ci_ignore_checks", "ci_only_checks":
		return applyCheckGlobs(cfg, key, val)
	case "gate_ignore_procs", "gate_build_tools", "gate_build_tool_ignore", "gate_pending_question",
		"gate_lease", "gate_job_min_age", "gate_exempt_panes", "gate_exempt_tabs":
		return applyGateSignal(cfg, key, val)
	case "team_cover", "team_cover_per_team", "team_cover_skip_teams",
		"team_cover_exclude_logins", "team_cover_signals",
		"team_cover_history_days", "team_cover_active_days":
		return applyTeamCover(cfg, key, val)
	default:
		return nil
	}
}

func applyTeamCover(cfg *Config, key, val string) error {
	switch key {
	case "team_cover":
		return applyTeamCoverEnabled(cfg, val)
	case "team_cover_per_team":
		return applyTeamCoverPerTeam(cfg, val)
	case "team_cover_skip_teams":
		return applyTeamCoverSkip(cfg, val)
	case "team_cover_exclude_logins":
		return applyTeamCoverExclude(cfg, val)
	case "team_cover_signals":
		return applyTeamCoverSignals(cfg, val)
	case "team_cover_history_days":
		return applyTeamCoverDays(&cfg.TeamCoverHistoryDays, key, val)
	case "team_cover_active_days":
		return applyTeamCoverDays(&cfg.TeamCoverActiveDays, key, val)
	default:
		return nil
	}
}

func applyTeamCoverEnabled(cfg *Config, val string) error {
	b, err := parseBool("team_cover", val)
	if err != nil {
		return err
	}
	cfg.TeamCover = b
	return nil
}

func applyTeamCoverPerTeam(cfg *Config, val string) error {
	n, err := parseNonNegativeInt("team_cover_per_team", val)
	if err != nil {
		return err
	}
	if n < 1 {
		return &KeyError{Key: "team_cover_per_team", Reason: "must be >= 1"}
	}
	cfg.TeamCoverPerTeam = n
	return nil
}

func applyTeamCoverSkip(cfg *Config, val string) error {
	teams, err := parseSkipTeams(val)
	if err != nil {
		return err
	}
	cfg.TeamCoverSkipTeams = teams
	return nil
}

func parseSkipTeams(val string) ([]string, error) {
	if strings.TrimSpace(val) == "" {
		return nil, nil
	}
	var out []string
	for _, part := range strings.Split(val, ",") {
		team := strings.TrimSpace(part)
		if team == "" {
			continue
		}
		if !repoPattern.MatchString(team) {
			return nil, &KeyError{Key: "team_cover_skip_teams", Reason: fmt.Sprintf("invalid team %q", team)}
		}
		out = append(out, team)
	}
	return out, nil
}

func applyTeamCoverExclude(cfg *Config, val string) error {
	logins, err := parseExcludeLogins(val)
	if err != nil {
		return err
	}
	cfg.TeamCoverExcludeLogins = logins
	return nil
}

func parseExcludeLogins(val string) ([]string, error) {
	if strings.TrimSpace(val) == "" {
		return nil, nil
	}
	if strings.HasPrefix(val, "@") {
		text, err := resolvePrompt("team_cover_exclude_logins", val)
		if err != nil {
			return nil, err
		}
		return loginLines(text), nil
	}
	var out []string
	for _, part := range strings.Split(val, ",") {
		login := strings.TrimSpace(part)
		if login == "" {
			continue
		}
		out = append(out, login)
	}
	return out, nil
}

func loginLines(text string) []string {
	var out []string
	for _, line := range strings.Split(text, "\n") {
		login := strings.TrimSpace(line)
		if login == "" || strings.HasPrefix(login, "#") {
			continue
		}
		out = append(out, login)
	}
	return out
}

func applyTeamCoverSignals(cfg *Config, val string) error {
	signals, err := parseTeamCoverSignals(val)
	if err != nil {
		return err
	}
	cfg.TeamCoverSignals = signals
	return nil
}

func parseTeamCoverSignals(val string) ([]string, error) {
	if strings.TrimSpace(val) == "" {
		return []string{}, nil
	}
	allowed := map[string]struct{}{
		"codeowners": {},
		"history":    {},
		"load":       {},
	}
	var out []string
	seen := map[string]struct{}{}
	for _, part := range strings.Split(val, ",") {
		sig := strings.TrimSpace(part)
		if sig == "" {
			continue
		}
		if _, ok := allowed[sig]; !ok {
			return nil, &KeyError{Key: "team_cover_signals", Reason: fmt.Sprintf("unknown signal %q", sig)}
		}
		if _, ok := seen[sig]; ok {
			continue
		}
		seen[sig] = struct{}{}
		out = append(out, sig)
	}
	if out == nil {
		return []string{}, nil
	}
	return out, nil
}

func applyTeamCoverDays(dst *int, key, val string) error {
	n, err := parseNonNegativeInt(key, val)
	if err != nil {
		return err
	}
	*dst = n
	return nil
}

func applyCheckGlobs(cfg *Config, key, val string) error {
	patterns, err := splitGlobs(key, val)
	if err != nil {
		return err
	}
	switch key {
	case "ci_ignore_checks":
		cfg.CIIgnoreChecks = patterns
	case "ci_only_checks":
		cfg.CIOnlyChecks = patterns
	}
	return nil
}

func splitGlobs(key, val string) ([]string, error) {
	if strings.TrimSpace(val) == "" {
		return nil, nil
	}
	parts := strings.Split(val, ",")
	out := make([]string, 0, len(parts))
	for _, part := range parts {
		pattern := strings.TrimSpace(part)
		if pattern == "" {
			continue
		}
		if _, err := path.Match(pattern, ""); err != nil {
			return nil, &KeyError{Key: key, Reason: fmt.Sprintf("invalid glob %q", pattern)}
		}
		out = append(out, pattern)
	}
	if len(out) == 0 {
		return nil, nil
	}
	return out, nil
}

func applyCommentKey(cfg *Config, key, val string) error {
	if key == "comment_strip_patterns" {
		return applyStripPatterns(cfg, val)
	}
	return applyCommentLimit(cfg, key, val)
}

func applyRepos(cfg *Config, val string) error {
	fields := strings.Fields(val)
	for _, repo := range fields {
		if !repoPattern.MatchString(repo) {
			return &KeyError{Key: "repos", Reason: fmt.Sprintf("invalid repo %q", repo)}
		}
	}
	cfg.Repos = fields
	return nil
}

func applyTitleIDPattern(cfg *Config, val string) error {
	re, err := regexp.Compile(val)
	if err != nil {
		return &KeyError{Key: "title_id_pattern", Reason: err.Error()}
	}
	cfg.TitleIDPattern = re
	return nil
}

func applyMillis(cfg *Config, key, val string) error {
	d, err := parseMillis(key, val)
	if err != nil {
		return err
	}
	switch key {
	case "gate_poll_ms":
		cfg.GatePoll = d
	case "gate_timeout_ms":
		cfg.GateTimeout = d
	case "dispatch_timeout_ms":
		cfg.DispatchTimeout = d
	case "machine_timeout_ms":
		cfg.MachineTimeout = d
	}
	return nil
}

// applyMachines reads a space- or comma-separated list of saved herdr
// machines. "local" is reserved for the local server.
func applyMachines(cfg *Config, val string) error {
	fields := strings.FieldsFunc(val, func(r rune) bool { return r == ',' || unicode.IsSpace(r) })
	out := make([]string, 0, len(fields))
	for _, machine := range fields {
		if machine == "local" {
			return &KeyError{Key: "machines", Reason: `"local" is the local server; list only other machines`}
		}
		if !slices.Contains(out, machine) {
			out = append(out, machine)
		}
	}
	cfg.Machines = out
	return nil
}

func applyPrompt(cfg *Config, key, val string) error {
	text, err := resolvePrompt(key, val)
	if err != nil {
		return err
	}
	switch key {
	case "rebase_prompt_template":
		cfg.RebasePromptTemplate = text
	case "ci_fix_prompt_template":
		cfg.CIFixPromptTemplate = text
	default:
		cfg.PromptTemplate = text
	}
	return nil
}

func applyIgnoreBlocked(cfg *Config, val string) error {
	b, err := parseBool("concurrency_ignore_blocked", val)
	if err != nil {
		return err
	}
	cfg.ConcurrencyIgnoreBlocked = b
	return nil
}

func applyIncludeDrafts(cfg *Config, val string) error {
	b, err := parseBool("dispatch_include_drafts", val)
	if err != nil {
		return err
	}
	cfg.IncludeDrafts = b
	return nil
}

func applyWaitUntil(cfg *Config, val string) error {
	fields := strings.Fields(val)
	if len(fields) == 0 {
		return &KeyError{Key: "dispatch_wait_until", Reason: "at least one token required"}
	}
	cfg.WaitUntil = fields
	return nil
}

func applyDedupe(cfg *Config, key, val string) error {
	switch key {
	case "dedupe_ttl":
		d, err := time.ParseDuration(val)
		if err != nil {
			return &KeyError{Key: key, Reason: "not a duration"}
		}
		if d < 0 {
			return &KeyError{Key: key, Reason: "must be >= 0"}
		}
		cfg.DedupeTTL = d
	case "dedupe_scans":
		n, err := parseNonNegativeInt(key, val)
		if err != nil {
			return err
		}
		cfg.DedupeScans = n
	}
	return nil
}

func applyDryRun(cfg *Config, val string) error {
	b, err := parseBool("dry_run", val)
	if err != nil {
		return err
	}
	cfg.DryRun = b
	return nil
}

func applyStripPatterns(cfg *Config, val string) error {
	text, err := resolvePrompt("comment_strip_patterns", val)
	if err != nil {
		return err
	}
	patterns, err := compileStripPatterns(text)
	if err != nil {
		return err
	}
	cfg.CommentStripPatterns = patterns
	return nil
}

func compileStripPatterns(text string) ([]*regexp.Regexp, error) {
	var out []*regexp.Regexp
	for i, line := range strings.Split(text, "\n") {
		n := i + 1
		trimmed := strings.TrimSpace(line)
		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}
		re, err := regexp.Compile(trimmed)
		if err != nil {
			return nil, &KeyError{
				Key:    "comment_strip_patterns",
				Line:   n,
				Reason: fmt.Sprintf("line %d: %s", n, err.Error()),
			}
		}
		out = append(out, re)
	}
	return out, nil
}

func applyCommentLimit(cfg *Config, key, val string) error {
	n, err := parseNonNegativeInt(key, val)
	if err != nil {
		return err
	}
	switch key {
	case "comment_link_max_chars":
		cfg.CommentLinkMaxChars = n
	case "comment_body_max_chars":
		cfg.CommentBodyMaxChars = n
	}
	return nil
}

func resolvePrompt(key, val string) (string, error) {
	if !strings.HasPrefix(val, "@") {
		return val, nil
	}
	path := expandTilde(val[1:])
	data, err := os.ReadFile(path) //nolint:gosec // path is an operator-supplied prompt file
	if err != nil {
		return "", &KeyError{Key: key, Reason: fmt.Sprintf("read %s: %v", path, err)}
	}
	return string(data), nil
}

func parseMillis(key, val string) (time.Duration, error) {
	n, err := strconv.ParseInt(val, 10, 64)
	if err != nil {
		return 0, &KeyError{Key: key, Reason: "not an integer"}
	}
	if n < 1 {
		return 0, &KeyError{Key: key, Reason: "must be >= 1"}
	}
	if n > math.MaxInt64/int64(time.Millisecond) {
		return 0, &KeyError{Key: key, Reason: "overflow"}
	}
	return time.Duration(n) * time.Millisecond, nil
}

func parseNonNegativeInt(key, val string) (int, error) {
	n, err := strconv.Atoi(val)
	if err != nil {
		return 0, &KeyError{Key: key, Reason: "not an integer"}
	}
	if n < 0 {
		return 0, &KeyError{Key: key, Reason: "must be >= 0"}
	}
	return n, nil
}

func parseBool(key, val string) (bool, error) {
	switch val {
	case "true", "1":
		return true, nil
	case "false", "0":
		return false, nil
	default:
		return false, &KeyError{Key: key, Reason: fmt.Sprintf("invalid bool %q", val)}
	}
}

func expandTilde(path string) string {
	if path == "~" {
		if home, err := os.UserHomeDir(); err == nil {
			return home
		}
		return path
	}
	if strings.HasPrefix(path, "~/") {
		if home, err := os.UserHomeDir(); err == nil {
			return filepath.Join(home, path[2:])
		}
	}
	return path
}
