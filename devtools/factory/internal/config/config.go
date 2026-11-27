// Package config loads and validates factory.toml.
// OSEnv is the only production reader of environment variables.
package config

import (
	"bytes"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/pelletier/go-toml/v2"
)

// DefaultPRTitlePattern is the default pull request title pattern for Jira
// sources and unconfigured Jira claims.
const DefaultPRTitlePattern = `\[{id}\]`

const defaultPRTitlePattern = DefaultPRTitlePattern

// ErrSourceNotFound is returned when a source key is not present in Config.Sources.
var ErrSourceNotFound = errors.New("expected a configured source, found none")

const (
	// ParentBlocksIgnore does not let an ancestor's blocks link gate its
	// descendant or count toward the ancestor's unblock score.
	ParentBlocksIgnore = "ignore"
	// ParentBlocksHonor keeps that link in readiness and in the unblock score.
	ParentBlocksHonor = "honor"
)

// Config is a loaded factory.toml.
type Config struct {
	Defaults   Defaults    `toml:"defaults"`
	Worker     Worker      `toml:"worker"`
	Sync       Sync        `toml:"sync"`
	Gate       Gate        `toml:"gate"`
	Lease      Lease       `toml:"lease"`
	Output     Output      `toml:"output"`
	Claim      Claim       `toml:"claim"`
	AgentRules []AgentRule `toml:"agent_rule"` // First matching rule replaces Defaults.AgentArgs.
	Sources    []Source    `toml:"source"`
	// Lanes are the [[lane]] entries. Empty means one implicit local lane:
	// every command then behaves as it did before lanes existed.
	Lanes []Lane `toml:"lane"`
	// LaneOrder is the [lanes] table.
	LaneOrder    LaneOrder `toml:"lanes"`
	Path         string    `toml:"-"` // File actually loaded.
	CallerPaneID string    `toml:"-"` // HERDR_PANE_ID, read once by Load.
	// ItemSource is FACTORY_ITEM_SOURCE, the kind:name source of the item
	// the calling worker was started on. Empty when unset. factory file
	// files only into it.
	ItemSource string `toml:"-"`
	// NoColor is true when NO_COLOR is set to a non-empty value.
	// stats --color auto then prints no escape codes.
	NoColor bool `toml:"-"`
	// GHBin is the gh binary every factory call runs: the top-level gh_bin
	// key, else GH_BIN, else DefaultGHBin. Load resolves it and copies it
	// onto every Source.
	GHBin          string `toml:"-"`
	workerAgentSet bool   `toml:"-"`
	workerArgsSet  bool   `toml:"-"`
}

// ItemSourceEnv is set by factory start in each worker's tab to the item's
// source, kind:name.
const ItemSourceEnv = "FACTORY_ITEM_SOURCE"

// OutputModeEnv is set by factory start in each worker's environment to the
// effective [output] mode ("pr" or "merge").
const OutputModeEnv = "FACTORY_OUTPUT_MODE"

// DefaultGHBin is the gh binary when neither gh_bin nor GH_BIN is set.
const DefaultGHBin = "gh"

// Lane is one [[lane]]: a herdr machine with its own checkout, worker cap,
// and the item labels routed to it.
type Lane struct {
	Name string `toml:"name"`
	// Machine is a herdr saved machine label or id. Empty is the local herdr server.
	Machine string `toml:"machine"`
	// Checkout is the directory the worker starts in, on Machine.
	Checkout string `toml:"checkout"`
	// MaxWorkers caps the active claims recorded for this lane.
	MaxWorkers int `toml:"max_workers"`
	// MaxActive caps every active claim on the lane, handed off or not.
	// 0 (unset) means no cap beyond max_in_flight.
	MaxActive int `toml:"max_active"`
	// Labels route an item that carries any of them to this lane only.
	Labels []string `toml:"labels"`
	// Worker is [lane.worker]. Its set fields override the source's.
	Worker LaneWorker `toml:"worker"`
	// Lease is [lane.lease]. Its set fields override the source's.
	Lease LeasePrompt `toml:"lease"`
	// Output is [lane.output]. Its set fields override the source's.
	Output Output `toml:"output"`
	// Exec runs a plain command on a remote lane's machine, such as
	// ["ssh", "mac1"]. factory appends the command, each argument
	// single-quoted for the remote shell. verify uses it for checkout checks.
	// Empty means verify cannot read that lane's checkout.
	Exec                  []string `toml:"exec"`
	StartPromptAppend     string   `toml:"-"`
	leaseRestore          *bool    `toml:"-"`
	outputPushCmdSet      bool     `toml:"-"`
	startPromptAppendFile bool     `toml:"-"`
	startPromptAppendSet  bool     `toml:"-"`
}

func (l Lane) applyLease(base LeasePrompt) LeasePrompt {
	out := l.Lease.Over(base)
	if l.leaseRestore != nil {
		out.Restore = *l.leaseRestore
	}
	return out
}

func (l Lane) applyOutput(base Output) Output {
	out := l.Output.Over(base)
	if l.outputPushCmdSet {
		out.PushCmd = l.Output.PushCmd
	}
	return out
}

// Remote reports whether the lane runs on a herdr saved machine.
func (l Lane) Remote() bool {
	return l.Machine != ""
}

// LaneOrder is the [lanes] table.
type LaneOrder struct {
	// Default is the lane preference order for an item with no lane label.
	Default []string `toml:"default"`
}

// Sync is the optional [sync] table.
type Sync struct {
	// MaxAge is how old a source may be before next, show, priorities, and doctor sync it.
	// Zero, the default, means those commands do not auto-sync.
	MaxAge time.Duration `toml:"max_age"`
}

const (
	// GateOnBase requires HEAD to be on Gate.BaseBranch.
	GateOnBase = "on_base"
	// GateCleanAgainstBase requires that HEAD has no commits that are not on origin/Gate.BaseBranch.
	GateCleanAgainstBase = "clean_against_base"
)

const (
	// LeaseGateAuto reads the agentbus lease when it can, and falls back
	// to the herdr and git checks, with a note, when it cannot.
	LeaseGateAuto = "auto"
	// LeaseGateOff never reads the lease.
	LeaseGateOff = "off"
	// LeaseGateRequired treats a lease that cannot be read as busy.
	LeaseGateRequired = "required"
	// LeaseGateQueue allows a lease held by another agent (the started agent
	// queues behind the holder through the lease queue) and skips the
	// checkout git checks while another agent holds or is releasing the
	// lease, while still refusing paused and needs-adopt. Free or self-held
	// leases still run checkout checks, and unreadable leases fall back to
	// the herdr and git checks with a note, as in auto.
	LeaseGateQueue = "queue"
	// LeaseGateQueueRequired is LeaseGateQueue, and treats a lease that
	// cannot be read as busy, as in required.
	LeaseGateQueueRequired = "queue-required"
)

const (
	// LeaseModeOff leaves the start prompt as written and never runs agentbus at start.
	LeaseModeOff = "off"
	// LeaseModePrompt puts the lease rule in the start prompt.
	LeaseModePrompt = "prompt"
	// LeaseModePromptCheck is LeaseModePrompt, and start also reads and
	// records the lease just before it launches the agent.
	LeaseModePromptCheck = "prompt+check"
	// DefaultLeaseClaimFor is the claim_for written into the lease rule.
	DefaultLeaseClaimFor = "45m"
	// LeaseRulePlaceholder in start_prompt places the lease rule there
	// instead of at the top.
	LeaseRulePlaceholder = "{{lease_rule}}"
	// StartPromptAppendPlaceholder in start_prompt places the
	// start_prompt_append lines there instead of at the top.
	StartPromptAppendPlaceholder = "{{start_prompt_append}}"
)

// Lease is the optional [lease] table: how factory uses the agentbus
// workdir lease of a checkout.
type Lease struct {
	// Gate is LeaseGateAuto (the default), LeaseGateOff, LeaseGateRequired,
	// LeaseGateQueue, or LeaseGateQueueRequired.
	// It is top-level only: a checkout may be shared by several sources.
	Gate string `toml:"gate"`
	// LeasePrompt is the start-prompt rule. A source and a lane may override it.
	LeasePrompt
}

// LeasePrompt is the start-prompt part of [lease]. It is allowed at the
// top level, per source ([source.lease]), and per lane ([lane.lease]).
// A lane overrides its source, and a source overrides the top level.
type LeasePrompt struct {
	// Mode is LeaseModeOff (the default), LeaseModePrompt, or LeaseModePromptCheck.
	// On a lane, empty means the source's mode.
	Mode string `toml:"mode"`
	// ClaimFor is the --for duration written into the rule, such as "45m".
	// On a lane, empty means the source's value.
	ClaimFor string `toml:"claim_for"`
	// Restore includes agentbus lease restore alongside claim when true.
	Restore bool `toml:"restore"`
}

// Over returns base with every field set in p replacing base's.
func (p LeasePrompt) Over(base LeasePrompt) LeasePrompt {
	if p.Mode != "" {
		base.Mode = p.Mode
		if p.Mode == LeaseModeOff && !p.Restore {
			base.Restore = false
		}
	}
	if p.ClaimFor != "" {
		base.ClaimFor = p.ClaimFor
	}
	if p.Restore {
		base.Restore = true
	}
	return base
}

// Gate is the optional [gate] table.
// Listed panes and tab labels are left out of the working-agent check.
// BaseBranch, when set, also checks the checkout against that branch.
// HoldFiles and HoldTabs block a start while a long job is between turns.
type Gate struct {
	IgnorePanes []string       `toml:"ignore_panes"` // Pane ids the start gate skips.
	IgnoreTabs  []string       `toml:"ignore_tabs"`  // Tab labels the start gate skips.
	BaseBranch  string         `toml:"base_branch"`  // Unset means no branch check.
	Require     string         `toml:"require"`      // GateOnBase or GateCleanAgainstBase when BaseBranch is set.
	HoldFiles   []string       `toml:"hold_files"`   // Paths that hold while they exist. Empty uses DefaultHoldFile.
	HoldTabs    []string       `toml:"hold_tabs"`    // Tab labels that hold until the tab is missing or done.
	HoldTTL     *time.Duration `toml:"-"`            // Nil means no age check. A stale hold still refuses.
	// DefaultHoldFile is ~/.local/state/factory/hold when HoldFiles is empty.
	// Load sets it. XDG_STATE_HOME is not consulted.
	DefaultHoldFile string `toml:"-"`
}

// ExternalLookups maps each Jira source key (jira:name) to its
// external_blocker_lookups bound, for sync. Other kinds are left out: their
// syncs fetch no blockers outside the mirror.
func (c *Config) ExternalLookups() map[string]int {
	out := make(map[string]int)
	for i := range c.Sources {
		if c.Sources[i].Kind == "jira" {
			out["jira:"+c.Sources[i].Name] = c.Sources[i].ExternalBlockerLookups
		}
	}
	return out
}

// PRTitlePattern returns the pr_title_pattern for a source key (kind:name).
func (c *Config) PRTitlePattern(sourceKey string) (string, error) {
	if c == nil {
		return "", errors.New("pr_title_pattern: expected a config, found nil")
	}
	if sourceKey == "" {
		return "", errors.New("pr_title_pattern: expected a non-empty source, found empty")
	}
	for i := range c.Sources {
		src := c.Sources[i]
		if src.Kind == "" || src.Name == "" {
			return "", fmt.Errorf("source: expected non-empty kind and name, found kind %q name %q", src.Kind, src.Name)
		}
		if src.Kind+":"+src.Name != sourceKey {
			continue
		}
		if src.Kind != "github" && src.PRTitlePattern == "" {
			return "", fmt.Errorf("source %s: expected a pr_title_pattern, found empty", sourceKey)
		}
		return src.PRTitlePattern, nil
	}
	return "", fmt.Errorf("source %s: %w", sourceKey, ErrSourceNotFound)
}

// SourceCheckouts returns each source cwd once, in source order.
// An empty cwd is an error. The method does not check that the directory exists.
func (c *Config) SourceCheckouts() ([]string, error) {
	if c == nil {
		return nil, errors.New("config: expected a config, found nil")
	}
	out := make([]string, 0, len(c.Sources))
	seen := make(map[string]struct{}, len(c.Sources))
	for i := range c.Sources {
		cwd := c.Sources[i].Cwd
		if cwd == "" {
			return nil, fmt.Errorf("source %q: expected a cwd, found empty", c.Sources[i].Name)
		}
		if _, ok := seen[cwd]; ok {
			continue
		}
		seen[cwd] = struct{}{}
		out = append(out, cwd)
	}
	if len(out) == 0 {
		return nil, errors.New("config: expected at least one source checkout, found none")
	}
	return out, nil
}

// ActiveHoldFiles returns the hold files the gate watches.
// A non-empty HoldFiles list is that list. Otherwise it is DefaultHoldFile
// when Load set one. A nil result means no file is checked.
func (g Gate) ActiveHoldFiles() []string {
	if len(g.HoldFiles) > 0 {
		out := make([]string, len(g.HoldFiles))
		copy(out, g.HoldFiles)
		return out
	}
	if g.DefaultHoldFile == "" {
		return nil
	}
	return []string{g.DefaultHoldFile}
}

// EffectiveMaxAge returns this source's max_age when it is set, otherwise top.
func (s Source) EffectiveMaxAge(top time.Duration) time.Duration {
	if s.MaxAge != nil {
		return *s.MaxAge
	}
	return top
}

// Defaults are the built-in values, overridden by the [defaults] table.
// A source inherits ExcludeLabels, HumanLabels, and SkipTypes when it does not set them.
// Lists replace; they are not merged.
type Defaults struct {
	MaxInFlight      int           `toml:"max_in_flight"`      // Default 3.
	MinStartInterval time.Duration `toml:"min_start_interval"` // Default 10m.
	FullSyncEvery    time.Duration `toml:"full_sync_every"`    // Default 24h.
	StallAfter       time.Duration `toml:"stall_after"`        // Default 2h.
	ExcludeLabels    []string      `toml:"exclude_labels"`     // Default needs-human, blocked-external.
	HumanLabels      []string      `toml:"human_labels"`       // Default needs-human. Not every exclude label.
	SkipTypes        []string      `toml:"skip_types"`         // Default Epic.
	AgentKind        string        `toml:"agent_kind"`         // Default claude.
	AgentArgs        []string      `toml:"agent_args"`         // After -- on herdr agent start. Empty means none.
	DryRun           bool          `toml:"dry_run"`            // Default true.
	Caller           string        `toml:"caller"`             // Optional {caller} name. Empty means unset.
	// WakeBeforePrompt is WakeAuto (the default) or WakeOff. Auto runs
	// `agentbus wake --if-hibernated` before each prompt when agentbus is installed.
	WakeBeforePrompt string `toml:"wake_before_prompt"`
	// AgentReadyTimeout bounds the wait for a new agent to be idle before
	// its prompt. Default 60s.
	AgentReadyTimeout time.Duration `toml:"agent_ready_timeout"`
	// AgentReadySettle is the pause after that idle, so the agent's input
	// box accepts the prompt. Default 1s. 0 means no pause.
	AgentReadySettle time.Duration `toml:"agent_ready_settle"`
	// AgentBusyTimeout bounds the retries of herdr agent start while the new
	// pane's shell is still busy (agent_pane_busy). Default 20s.
	AgentBusyTimeout time.Duration `toml:"agent_busy_timeout"`
	// StartPromptAppend holds standing lines (or @path before expand)
	// placed after the lease rule and before start_prompt.
	StartPromptAppend     string `toml:"-"`
	startPromptAppendFile bool   `toml:"-"`
}

// wake_before_prompt values.
const (
	WakeAuto = "auto"
	WakeOff  = "off"
)

// Source is one Jira or GitHub backlog.
type Source struct {
	Name string `toml:"name"`
	Kind string `toml:"kind"` // "jira" or "github".

	Cwd                   string `toml:"cwd"`
	Base                  string `toml:"base"`
	Workspace             string `toml:"workspace"`
	StartPrompt           string `toml:"start_prompt"`
	StartPromptAppend     string `toml:"-"`
	startPromptAppendFile bool   `toml:"-"`
	startPromptAppendSet  bool   `toml:"-"`

	RequireLabels []string `toml:"require_labels"`
	ExcludeLabels []string `toml:"exclude_labels"`
	// HumanLabels selects factory next --human. Omitted inherits Defaults.HumanLabels.
	// An explicit empty list stays empty.
	HumanLabels []string `toml:"human_labels"`
	SkipTypes   []string `toml:"skip_types"`
	// OrphanIgnore lists item ids that doctor does not report as orphans.
	// An absent or empty list reports every open item with no parent.
	OrphanIgnore []string `toml:"orphan_ignore"`
	// OrphanIgnoreLabels lists labels that silence the same warning.
	// Matching is case-insensitive. These lists are not inherited from defaults.
	OrphanIgnoreLabels []string `toml:"orphan_ignore_labels"`
	// ClaimParentTypes are item types doctor's dropped-claim check skips:
	// such an item is in claim_status because its children are, and has no
	// tab or pull request of its own. Case-insensitive. Default Epic, Story.
	ClaimParentTypes []string `toml:"claim_parent_types"`
	// OrphanEpicChildTypes are the item types that can sit under an epic.
	// An orphan of another type gets doctor's link, move, or ignore options
	// instead of "set its parent epic". Case-insensitive. Default Story,
	// Task, Bug. An explicit empty list turns the type rule off.
	OrphanEpicChildTypes []string `toml:"orphan_epic_child_types"`
	// ExternalBlockerLookups bounds how many blockers outside the mirror a
	// sync of a Jira source fetches by key. Default 100. Zero fetches none.
	// The old key doctor_blocker_lookups is still accepted.
	ExternalBlockerLookups int `toml:"external_blocker_lookups"`
	// ExternalBlockerKey is the config key ExternalBlockerLookups came from,
	// for error messages: external_blocker_lookups unless only the old key
	// was set.
	ExternalBlockerKey string   `toml:"-"`
	ParentPriority     []string `toml:"parent_priority"`
	RankOrder          []string `toml:"rank_order"` // Nil keeps priority, unblocks, parent_rank, created.
	ParentAllowlist    []string `toml:"parent_allowlist"`

	AssigneeOK string `toml:"assignee_ok"` // "me", "unassigned", or "any". Default "me".
	// CreateAssignee is who factory file assigns a new item to:
	// CreateAssigneeMe (the default) or CreateAssigneeUnassigned.
	CreateAssignee string `toml:"create_assignee"`
	// ParentBlocksChild is ParentBlocksIgnore or ParentBlocksHonor.
	// Load sets ParentBlocksIgnore when the key is absent.
	// Both modes warn. Ignore does not gate the descendant or count the unblock.
	ParentBlocksChild string          `toml:"parent_blocks_child"`
	FinishOnMerge     bool            `toml:"finish_on_merge"`
	WorkspaceRules    []WorkspaceRule `toml:"workspace_rule"`

	Site      string `toml:"site"`
	Email     string `toml:"email"`
	TokenFile string `toml:"token_file"`
	TokenEnv  string `toml:"token_env"`
	JQL       string `toml:"jql"`

	ReadyStatuses []string `toml:"ready_statuses"`
	ClaimStatus   string   `toml:"claim_status"`
	// ClaimTransition is a Jira transition id or name. Empty means unset.
	// Claim uses it when more than one transition lands in ClaimStatus
	// and the transition's own name does not select one.
	ClaimTransition string `toml:"claim_transition"`
	FinishStatus    string `toml:"finish_status"`

	ClaimAssign    bool   `toml:"claim_assign"` // Jira default true.
	BlocksLinkType string `toml:"blocks_link_type"`
	PRTitlePattern string `toml:"pr_title_pattern"`
	// PRTemplate is the pull request template verify reads.
	// A relative path is inside Cwd. Empty means unset.
	PRTemplate string `toml:"pr_template"`
	// DeferField is a Jira field id (such as customfield_12345) or a GitHub
	// date issue field name (default "Start date") that holds the item's
	// not-before date. Empty disables reading a deferral field.
	DeferField string `toml:"defer_field"`
	// DeferLabelPrefix, such as "not-before:", makes a GitHub label
	// PREFIX + YYYY-MM-DD defer the item to that date. GitHub only.
	DeferLabelPrefix string `toml:"defer_label_prefix"`
	// PriorityField is the GitHub single-select issue field name read for
	// item priority and written by SetPriority when priority_labels is empty.
	// Defaults to "Priority" on GitHub sources. Empty disables it.
	PriorityField string `toml:"priority_field"`
	// DueField is the GitHub date issue field name read for item due date
	// (takes precedence over milestone.dueOn when set). Defaults to
	// "Target date" on GitHub sources. Empty disables it.
	DueField        string `toml:"due_field"`
	DefaultParent   string `toml:"default_parent"`
	CreateProject   string `toml:"create_project"`
	CreateIssueType string `toml:"create_issue_type"`
	// CreateSubtaskIssueType is the issue type file uses under a standard
	// (non-epic) Jira parent. Empty means the project's single subtask type.
	CreateSubtaskIssueType string `toml:"create_subtask_issue_type"`

	Token string `toml:"-"` // Set by ResolveToken, not by Load.

	// GHBin is Config.GHBin, copied by Load so code that holds only a
	// Source runs the same gh binary.
	GHBin string `toml:"-"`

	// readToken is set by Load for a Jira source. ResolveToken calls it once.
	readToken func(*Source) error

	Repo           string         `toml:"repo"`
	ClaimLabel     string         `toml:"claim_label"`
	PriorityLabels map[string]int `toml:"priority_labels"`

	// MaxAge overrides Sync.MaxAge for this source.
	// Nil uses the top-level value. A pointer to zero disables auto-sync here only.
	MaxAge *time.Duration `toml:"-"`

	// Worker is the resolved worker configuration for this source: [source.worker]
	// over the top-level [worker]. Load resolves it. A lane may override it.
	Worker Worker `toml:"worker"`

	// Lease is the start-prompt lease rule for this source: [source.lease]
	// over the top-level [lease]. Load resolves it. A lane may override it.
	Lease LeasePrompt `toml:"lease"`

	// Output is the resolved output mode for this source: [source.output]
	// over the top-level [output]. Load resolves it. A lane may override it.
	Output Output `toml:"output"`

	// Claim is the resolved claim configuration for this source: [source.claim]
	// and legacy claim_*/assignee_ok keys over the top-level [claim]. Load resolves it.
	Claim Claim `toml:"claim"`

	// Trust is the [source.trust] classification and untrusted-author gating config.
	Trust Trust `toml:"trust"`

	// PriorityRanks maps a Jira priority name to a rank.
	// A name here overrides the built-in Highest..Lowest map.
	PriorityRanks map[string]int `toml:"priority_ranks"`
	// KnownPriorities are Jira priority names that stay at rank 2 and do not warn.
	// A name that is also in PriorityRanks uses that rank.
	KnownPriorities []string `toml:"known_priorities"`
}

// WorkspaceRule maps one parent id or one label to a herdr workspace.
type WorkspaceRule struct {
	Parent    string `toml:"parent"`
	Label     string `toml:"label"`
	Workspace string `toml:"workspace"`
}

// AgentRule replaces Defaults.AgentArgs when an item label matches.
// Labels is an any-of match. The first matching rule wins, and its
// AgentArgs replace the defaults instead of appending to them.
type AgentRule struct {
	// Name is an optional label for the rule in claims and factory stats.
	// Empty uses the first label.
	Name      string   `toml:"name"`
	Labels    []string `toml:"labels"`
	AgentArgs []string `toml:"agent_args"`
}

// Env is the filesystem and process environment Load reads.
// Every function is required. Tests pass fakes. Production uses OSEnv.
type Env struct {
	LookupEnv func(string) (string, bool)
	Getwd     func() (string, error)
	HomeDir   func() (string, error)
	ReadFile  func(string) ([]byte, error)
	Stat      func(string) (fs.FileInfo, error)
	// EvalSymlinks resolves symbolic links in a path. Nil leaves paths as
	// they are.
	EvalSymlinks func(string) (string, error)
}

// OSEnv returns the process environment.
// It is the only production reader of environment variables.
func OSEnv() Env {
	return Env{
		LookupEnv: os.LookupEnv,
		Getwd:     os.Getwd,
		HomeDir:   os.UserHomeDir,
		ReadFile:  os.ReadFile,
		Stat:      os.Stat,
		// Resolves a symlinked checkout, such as macOS /tmp, for SourceForDir.
		EvalSymlinks: filepath.EvalSymlinks,
	}
}

// Load reads factory.toml and validates it.
// explicitPath, when set, is the only file tried; a missing file is an error.
// Otherwise Load tries ./factory.toml, then <git-root>/factory.toml when inside
// a git repository, then $XDG_CONFIG_HOME/factory/factory.toml. An unset
// XDG_CONFIG_HOME means ~/.config. When none of those files exist, the error is
// "error: no config found; looked in: <list>".
// A Jira source must set exactly one of token_file and token_env.
// Load does not read that file or variable. Call ResolveToken when a
// command contacts the source.
func Load(explicitPath string, env Env) (*Config, error) {
	if err := env.complete(); err != nil {
		return nil, err
	}
	path, err := resolvePath(explicitPath, env)
	if err != nil {
		return nil, err
	}
	data, err := env.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read config %s: %w", path, err)
	}
	raw, err := decodeRaw(data)
	if err != nil {
		return nil, err
	}
	cfg, err := raw.toConfig()
	if err != nil {
		return nil, err
	}
	cfg.Path = path
	if err := expand(cfg, env); err != nil {
		return nil, err
	}
	if err := validate(cfg, env); err != nil {
		return nil, err
	}
	pane, err := readCallerPane(env)
	if err != nil {
		return nil, err
	}
	cfg.CallerPaneID = pane
	itemSource, err := readItemSource(env)
	if err != nil {
		return nil, err
	}
	cfg.ItemSource = itemSource
	noColor, _ := env.LookupEnv("NO_COLOR")
	cfg.NoColor = noColor != ""
	ghBin, err := resolveGHBin(raw.GHBin, env)
	if err != nil {
		return nil, err
	}
	cfg.GHBin = ghBin
	for i := range cfg.Sources {
		cfg.Sources[i].GHBin = ghBin
	}
	prepareTokens(cfg, env)
	return cfg, nil
}

// readCallerPane reads HERDR_PANE_ID once.
// An unset variable is an empty id. A set but empty variable is an error.
func readCallerPane(env Env) (string, error) {
	value, ok := env.LookupEnv("HERDR_PANE_ID")
	if !ok {
		return "", nil
	}
	if value == "" {
		return "", errors.New("HERDR_PANE_ID: expected a non-empty pane id, found empty")
	}
	return value, nil
}

// readItemSource reads FACTORY_ITEM_SOURCE once. Unset is empty. A set
// value must be kind:name.
func readItemSource(env Env) (string, error) {
	value, ok := env.LookupEnv(ItemSourceEnv)
	if !ok {
		return "", nil
	}
	kind, name, found := strings.Cut(value, ":")
	if !found || kind == "" || name == "" {
		return "", fmt.Errorf("%s: expected kind:name, found %q", ItemSourceEnv, value)
	}
	return value, nil
}

// resolveGHBin picks the gh binary: the gh_bin key, else GH_BIN, else
// DefaultGHBin. An empty key or a set but empty GH_BIN is an error.
// A leading ~/ expands to the home directory.
func resolveGHBin(key *string, env Env) (string, error) {
	if key != nil {
		if *key == "" {
			return "", errors.New("gh_bin: expected a non-empty binary name or path, found empty")
		}
		bin, err := expandUserPath(*key, env)
		if err != nil {
			return "", fmt.Errorf("gh_bin: %w", err)
		}
		return bin, nil
	}
	value, ok := env.LookupEnv("GH_BIN")
	if !ok {
		return DefaultGHBin, nil
	}
	if value == "" {
		return "", errors.New("GH_BIN: expected a non-empty binary name or path, found empty")
	}
	bin, err := expandUserPath(value, env)
	if err != nil {
		return "", fmt.Errorf("GH_BIN: %w", err)
	}
	return bin, nil
}

func (e Env) complete() error {
	switch {
	case e.LookupEnv == nil:
		return errors.New("config env: expected LookupEnv, found nil")
	case e.Getwd == nil:
		return errors.New("config env: expected Getwd, found nil")
	case e.HomeDir == nil:
		return errors.New("config env: expected HomeDir, found nil")
	case e.ReadFile == nil:
		return errors.New("config env: expected ReadFile, found nil")
	case e.Stat == nil:
		return errors.New("config env: expected Stat, found nil")
	default:
		return nil
	}
}

// GitRoot walks upward from dir using env.Stat to find the nearest ancestor
// containing a .git directory (normal clone) or regular file (git worktree).
// It does not execute git.
func GitRoot(dir string, env Env) (string, bool, error) {
	if env.Stat == nil {
		return "", false, errors.New("config env: expected Stat, found nil")
	}
	if dir == "" {
		return "", false, errors.New("git root: expected a non-empty directory, found empty")
	}
	cur := filepath.Clean(dir)
	for {
		gitPath := filepath.Join(cur, ".git")
		info, err := env.Stat(gitPath)
		if err == nil {
			if info.IsDir() || info.Mode().IsRegular() {
				return cur, true, nil
			}
			return "", false, fmt.Errorf("git marker %s: expected directory or regular file, found %s", gitPath, info.Mode().Type())
		}
		if !errors.Is(err, fs.ErrNotExist) {
			return "", false, fmt.Errorf("git marker %s: %w", gitPath, err)
		}
		parent := filepath.Dir(cur)
		if parent == cur {
			return "", false, nil
		}
		cur = parent
	}
}

func resolvePath(explicit string, env Env) (string, error) {
	if explicit != "" {
		ok, err := regularFile(explicit, env)
		if err != nil {
			return "", err
		}
		if !ok {
			return "", fmt.Errorf("config file not found: %s", explicit)
		}
		return explicit, nil
	}
	wd, err := env.Getwd()
	if err != nil {
		return "", fmt.Errorf("working directory: %w", err)
	}
	local := filepath.Join(wd, "factory.toml")
	looked := []string{local}
	ok, err := regularFile(local, env)
	if err != nil {
		return "", err
	}
	if ok {
		return local, nil
	}
	root, inGit, err := GitRoot(wd, env)
	if err != nil {
		return "", err
	}
	if inGit {
		repoConfig := filepath.Join(root, "factory.toml")
		if repoConfig != local {
			looked = append(looked, repoConfig)
			ok, err = regularFile(repoConfig, env)
			if err != nil {
				return "", err
			}
			if ok {
				return repoConfig, nil
			}
		}
	}
	home, err := configHome(env)
	if err != nil {
		return "", err
	}
	xdgPath := filepath.Join(home, "factory", "factory.toml")
	looked = append(looked, xdgPath)
	ok, err = regularFile(xdgPath, env)
	if err != nil {
		return "", err
	}
	if ok {
		return xdgPath, nil
	}
	return "", fmt.Errorf("error: no config found; looked in: %s", strings.Join(looked, ", "))
}

func configHome(env Env) (string, error) {
	xdg, ok := env.LookupEnv("XDG_CONFIG_HOME")
	if ok {
		if xdg == "" {
			return "", errors.New("XDG_CONFIG_HOME: expected a non-empty path, found empty")
		}
		return xdg, nil
	}
	home, err := env.HomeDir()
	if err != nil {
		return "", fmt.Errorf("home directory: %w", err)
	}
	if home == "" {
		return "", errors.New("home directory: expected a non-empty path, found empty")
	}
	return filepath.Join(home, ".config"), nil
}

// GlobalConfigPath returns $XDG_CONFIG_HOME/factory/factory.toml, or
// ~/.config/factory/factory.toml when XDG_CONFIG_HOME is unset.
func GlobalConfigPath(env Env) (string, error) {
	if env.LookupEnv == nil {
		return "", errors.New("config env: expected LookupEnv, found nil")
	}
	if env.HomeDir == nil {
		return "", errors.New("config env: expected HomeDir, found nil")
	}
	home, err := configHome(env)
	if err != nil {
		return "", err
	}
	return filepath.Join(home, "factory", "factory.toml"), nil
}

// ExpandHome expands a leading "~" or "~/" using env.HomeDir.
func ExpandHome(path string, env Env) (string, error) {
	if env.HomeDir == nil {
		return "", errors.New("config env: expected HomeDir, found nil")
	}
	return expandUserPath(path, env)
}

// ContractHome rewrites path under env.HomeDir to "~" or "~/<rel>".
// Paths outside the home directory are returned cleaned.
func ContractHome(path string, env Env) (string, error) {
	if env.HomeDir == nil {
		return "", errors.New("config env: expected HomeDir, found nil")
	}
	home, err := env.HomeDir()
	if err != nil {
		return "", fmt.Errorf("home directory: %w", err)
	}
	if home == "" {
		return "", errors.New("home directory: expected a non-empty path, found empty")
	}
	cleanPath := filepath.Clean(path)
	cleanHome := filepath.Clean(home)
	if short, ok := relativeToHome(cleanHome, cleanPath); ok {
		return short, nil
	}
	if env.EvalSymlinks != nil {
		if short, ok := relativeToHome(realPath(cleanHome, env), realPath(cleanPath, env)); ok {
			return short, nil
		}
	}
	return cleanPath, nil
}

func relativeToHome(home, path string) (string, bool) {
	rel, err := filepath.Rel(home, path)
	if err != nil {
		return "", false
	}
	if rel == "." {
		return "~", true
	}
	if rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return "", false
	}
	return "~/" + filepath.ToSlash(rel), true
}

func regularFile(path string, env Env) (bool, error) {
	info, err := env.Stat(path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return false, nil
		}
		return false, fmt.Errorf("config path %s: %w", path, err)
	}
	if info.IsDir() {
		return false, fmt.Errorf("config path %s is a directory", path)
	}
	return true, nil
}

// raw types keep enough presence information to tell an omitted key from a
// zero value. A nil pointer means the key was absent. A nil slice means the
// key was absent; an empty slice means the key was set to an empty list.
type rawConfig struct {
	GHBin      *string     `toml:"gh_bin"`
	Defaults   rawDefaults `toml:"defaults"`
	Worker     rawWorker   `toml:"worker"`
	Sync       rawSync     `toml:"sync"`
	Gate       rawGate     `toml:"gate"`
	Lease      rawLease    `toml:"lease"`
	Output     rawOutput   `toml:"output"`
	Claim      rawClaim    `toml:"claim"`
	AgentRules []AgentRule `toml:"agent_rule"`
	Sources    []rawSource `toml:"source"`
	Lanes      []rawLane   `toml:"lane"`
	LaneOrder  *rawLanes   `toml:"lanes"`
}

type rawLane struct {
	Name              *string         `toml:"name"`
	Machine           *string         `toml:"machine"`
	Checkout          *string         `toml:"checkout"`
	MaxWorkers        *int            `toml:"max_workers"`
	MaxActive         *int            `toml:"max_active"`
	Labels            []string        `toml:"labels"`
	Worker            *rawLaneWorker  `toml:"worker"`
	Lease             *rawLeasePrompt `toml:"lease"`
	Output            *rawOutput      `toml:"output"`
	Exec              []string        `toml:"exec"`
	StartPromptAppend any             `toml:"start_prompt_append"`
}

type rawLanes struct {
	Default []string `toml:"default"`
}

type rawSync struct {
	MaxAge *string `toml:"max_age"`
}

type rawGate struct {
	IgnorePanes []string `toml:"ignore_panes"`
	IgnoreTabs  []string `toml:"ignore_tabs"`
	BaseBranch  string   `toml:"base_branch"`
	Require     string   `toml:"require"`
	HoldFiles   []string `toml:"hold_files"`
	HoldTabs    []string `toml:"hold_tabs"`
	HoldTTL     *string  `toml:"hold_ttl"`
}

type rawLease struct {
	Gate     *string `toml:"gate"`
	Mode     *string `toml:"mode"`
	ClaimFor *string `toml:"claim_for"`
	Restore  *bool   `toml:"restore"`
}

// rawLeasePrompt is [source.lease] or [lane.lease]. It has no gate key.
type rawLeasePrompt struct {
	Mode     *string `toml:"mode"`
	ClaimFor *string `toml:"claim_for"`
	Restore  *bool   `toml:"restore"`
}

func (raw rawLease) toLease() Lease {
	out := Lease{
		Gate:        LeaseGateAuto,
		LeasePrompt: LeasePrompt{Mode: LeaseModeOff, ClaimFor: DefaultLeaseClaimFor},
	}
	if raw.Gate != nil {
		out.Gate = *raw.Gate
	}
	out.LeasePrompt = rawLeasePrompt{Mode: raw.Mode, ClaimFor: raw.ClaimFor, Restore: raw.Restore}.over(out.LeasePrompt)
	return out
}

// over returns base with every key present in raw replacing base's.
// A key set to an empty string stays empty, so validation reports it.
func (raw rawLeasePrompt) over(base LeasePrompt) LeasePrompt {
	if raw.Mode != nil {
		base.Mode = *raw.Mode
		if *raw.Mode == LeaseModeOff && raw.Restore == nil {
			base.Restore = false
		}
	}
	if raw.ClaimFor != nil {
		base.ClaimFor = *raw.ClaimFor
	}
	if raw.Restore != nil {
		base.Restore = *raw.Restore
	}
	return base
}

type rawDefaults struct {
	MaxInFlight      *int     `toml:"max_in_flight"`
	MinStartInterval *string  `toml:"min_start_interval"`
	FullSyncEvery    *string  `toml:"full_sync_every"`
	StallAfter       *string  `toml:"stall_after"`
	ExcludeLabels    []string `toml:"exclude_labels"`
	HumanLabels      []string `toml:"human_labels"`
	SkipTypes        []string `toml:"skip_types"`
	AgentKind        *string  `toml:"agent_kind"`
	AgentArgs        []string `toml:"agent_args"`
	DryRun           *bool    `toml:"dry_run"`
	Caller           *string  `toml:"caller"`
	WakeBeforePrompt *string  `toml:"wake_before_prompt"`

	AgentReadyTimeout *string `toml:"agent_ready_timeout"`
	AgentReadySettle  *string `toml:"agent_ready_settle"`
	AgentBusyTimeout  *string `toml:"agent_busy_timeout"`
	StartPromptAppend any     `toml:"start_prompt_append"`
}

type rawSource struct {
	Name               *string         `toml:"name"`
	Kind               *string         `toml:"kind"`
	Cwd                *string         `toml:"cwd"`
	Base               *string         `toml:"base"`
	Workspace          *string         `toml:"workspace"`
	StartPrompt        *string         `toml:"start_prompt"`
	StartPromptAppend  any             `toml:"start_prompt_append"`
	RequireLabels      []string        `toml:"require_labels"`
	ExcludeLabels      []string        `toml:"exclude_labels"`
	HumanLabels        []string        `toml:"human_labels"`
	SkipTypes          []string        `toml:"skip_types"`
	OrphanIgnore       []string        `toml:"orphan_ignore"`
	OrphanIgnoreLabels []string        `toml:"orphan_ignore_labels"`
	ClaimParentTypes   []string        `toml:"claim_parent_types"`
	OrphanEpicTypes    []string        `toml:"orphan_epic_child_types"`
	BlockerLookups     *int            `toml:"external_blocker_lookups"`
	OldBlockerLookups  *int            `toml:"doctor_blocker_lookups"`
	ParentPriority     []string        `toml:"parent_priority"`
	RankOrder          []string        `toml:"rank_order"`
	ParentAllowlist    []string        `toml:"parent_allowlist"`
	AssigneeOK         *string         `toml:"assignee_ok"`
	CreateAssignee     *string         `toml:"create_assignee"`
	ParentBlocksChild  *string         `toml:"parent_blocks_child"`
	FinishOnMerge      *bool           `toml:"finish_on_merge"`
	WorkspaceRules     []WorkspaceRule `toml:"workspace_rule"`

	Site                   *string  `toml:"site"`
	Email                  *string  `toml:"email"`
	TokenFile              *string  `toml:"token_file"`
	TokenEnv               *string  `toml:"token_env"`
	JQL                    *string  `toml:"jql"`
	ReadyStatuses          []string `toml:"ready_statuses"`
	ClaimStatus            *string  `toml:"claim_status"`
	ClaimTransition        *string  `toml:"claim_transition"`
	FinishStatus           *string  `toml:"finish_status"`
	ClaimAssign            *bool    `toml:"claim_assign"`
	BlocksLinkType         *string  `toml:"blocks_link_type"`
	PRTitlePattern         *string  `toml:"pr_title_pattern"`
	PRTemplate             *string  `toml:"pr_template"`
	DeferField             *string  `toml:"defer_field"`
	DeferLabelPrefix       *string  `toml:"defer_label_prefix"`
	PriorityField          *string  `toml:"priority_field"`
	DueField               *string  `toml:"due_field"`
	DefaultParent          *string  `toml:"default_parent"`
	CreateProject          *string  `toml:"create_project"`
	CreateIssueType        *string  `toml:"create_issue_type"`
	CreateSubtaskIssueType *string  `toml:"create_subtask_issue_type"`

	Repo            *string         `toml:"repo"`
	ClaimLabel      *string         `toml:"claim_label"`
	PriorityLabels  map[string]int  `toml:"priority_labels"`
	PriorityRanks   map[string]int  `toml:"priority_ranks"`
	KnownPriorities []string        `toml:"known_priorities"`
	MaxAge          *string         `toml:"max_age"`
	Worker          *rawWorker      `toml:"worker"`
	Lease           *rawLeasePrompt `toml:"lease"`
	Output          *rawOutput      `toml:"output"`
	Claim           *rawSourceClaim `toml:"claim"`
	Trust           *rawTrust       `toml:"trust"`
}

func decodeRaw(data []byte) (rawConfig, error) {
	dec := toml.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	var raw rawConfig
	if err := dec.Decode(&raw); err != nil {
		return rawConfig{}, decodeError(err)
	}
	return raw, nil
}

func decodeError(err error) error {
	var strict *toml.StrictMissingError
	if !errors.As(err, &strict) {
		return fmt.Errorf("decode config: %w", err)
	}
	names := make([]string, 0, len(strict.Errors))
	for i := range strict.Errors {
		key := strings.Join(strict.Errors[i].Key(), ".")
		if key == "" {
			key = strict.Errors[i].Error()
		}
		names = append(names, key)
	}
	return fmt.Errorf("unknown key %s", strings.Join(names, ", "))
}

func (raw rawConfig) toConfig() (*Config, error) {
	defaults, err := raw.Defaults.toDefaults()
	if err != nil {
		return nil, err
	}
	workerCfg, err := raw.Worker.resolveTop(&defaults, raw.Defaults)
	if err != nil {
		return nil, err
	}
	holdTTL, err := parseOptionalDuration("gate.hold_ttl", raw.Gate.HoldTTL)
	if err != nil {
		return nil, err
	}
	syncCfg, err := raw.Sync.toSync()
	if err != nil {
		return nil, err
	}
	outputCfg, err := raw.Output.toOutput()
	if err != nil {
		return nil, err
	}
	cfg := &Config{
		Defaults: defaults,
		Worker:   workerCfg,
		Sync:     syncCfg,
		Gate: Gate{
			IgnorePanes: copyStrings(raw.Gate.IgnorePanes),
			IgnoreTabs:  copyStrings(raw.Gate.IgnoreTabs),
			BaseBranch:  raw.Gate.BaseBranch,
			Require:     raw.Gate.Require,
			HoldFiles:   copyStrings(raw.Gate.HoldFiles),
			HoldTabs:    copyStrings(raw.Gate.HoldTabs),
			HoldTTL:     holdTTL,
		},
		Lease:          raw.Lease.toLease(),
		Output:         outputCfg,
		Claim:          raw.Claim.toClaim(),
		AgentRules:     copyAgentRules(raw.AgentRules),
		Sources:        make([]Source, len(raw.Sources)),
		workerAgentSet: raw.Worker.Agent != nil,
		workerArgsSet:  raw.Worker.AgentArgs != nil,
	}
	lanes, order, err := raw.toLanes()
	if err != nil {
		return nil, err
	}
	cfg.Lanes = lanes
	cfg.LaneOrder = order
	if err := raw.fillSources(cfg); err != nil {
		return nil, err
	}
	return cfg, nil
}

func (raw rawConfig) fillSources(cfg *Config) error {
	for i := range raw.Sources {
		src, err := raw.Sources[i].toSource(cfg.Defaults)
		if err != nil {
			return err
		}
		src.Worker = cfg.Worker.clone()
		if raw.Sources[i].Worker != nil {
			src.Worker = raw.Sources[i].Worker.over(src.Worker)
		}
		src.Lease = cfg.Lease.LeasePrompt
		if raw.Sources[i].Lease != nil {
			src.Lease = raw.Sources[i].Lease.over(src.Lease)
		}
		src.Output = cfg.Output
		if raw.Sources[i].Output != nil {
			src.Output = raw.Sources[i].Output.over(src.Output)
		}
		if err := resolveSourceClaim(&src, raw.Sources[i], cfg.Claim); err != nil {
			return err
		}
		cfg.Sources[i] = src
	}
	return nil
}

func (raw rawConfig) toLanes() ([]Lane, LaneOrder, error) {
	if raw.LaneOrder != nil && len(raw.Lanes) == 0 {
		return nil, LaneOrder{}, errors.New("lanes: expected at least one [[lane]], found none")
	}
	var lanes []Lane
	for i, item := range raw.Lanes {
		lane, err := item.toLane(i)
		if err != nil {
			return nil, LaneOrder{}, err
		}
		lanes = append(lanes, lane)
	}
	var order LaneOrder
	if raw.LaneOrder != nil {
		order.Default = copyStrings(raw.LaneOrder.Default)
	}
	return lanes, order, nil
}

func (item rawLane) toLane(index int) (Lane, error) {
	if item.MaxWorkers == nil {
		name := str(item.Name)
		if name == "" {
			name = fmt.Sprintf("#%d", index+1)
		}
		return Lane{}, fmt.Errorf("lane %q field \"max_workers\": expected a positive integer, found none", name)
	}
	lane := Lane{
		Name:       str(item.Name),
		Machine:    str(item.Machine),
		Checkout:   str(item.Checkout),
		MaxWorkers: *item.MaxWorkers,
		Labels:     copyStrings(item.Labels),
		Exec:       copyStrings(item.Exec),
	}
	if item.MaxActive != nil {
		if *item.MaxActive < 1 {
			return Lane{}, fmt.Errorf("lane %q field \"max_active\": expected a positive integer or no key, found %d", lane.Name, *item.MaxActive)
		}
		lane.MaxActive = *item.MaxActive
	}
	return item.applyOverrides(lane)
}

func (item rawLane) applyOverrides(lane Lane) (Lane, error) {
	if item.Worker != nil {
		w, err := item.Worker.toLaneWorker(lane.Name)
		if err != nil {
			return Lane{}, err
		}
		lane.Worker = w
	}
	if item.Lease != nil {
		if err := laneLeaseSet(lane.Name, *item.Lease); err != nil {
			return Lane{}, err
		}
		lane.Lease = item.Lease.over(LeasePrompt{})
		if item.Lease.Restore != nil {
			v := *item.Lease.Restore
			lane.leaseRestore = &v
		}
	}
	if item.Output != nil {
		if err := laneOutputSet(lane.Name, *item.Output); err != nil {
			return Lane{}, err
		}
		lane.Output = item.Output.over(Output{})
		lane.outputPushCmdSet = item.Output.PushCmd != nil
	}
	if item.StartPromptAppend != nil {
		val, isFile, err := parseStartPromptAppend(fmt.Sprintf("lane %q", lane.Name), item.StartPromptAppend)
		if err != nil {
			return Lane{}, err
		}
		lane.StartPromptAppend = val
		lane.startPromptAppendFile = isFile
		lane.startPromptAppendSet = true
	}
	return lane, nil
}

// laneLeaseSet rejects a [lane.lease] key set to an empty string. On a
// lane, empty means "inherit", so an explicit empty value would be silent.
func laneLeaseSet(name string, raw rawLeasePrompt) error {
	if raw.Mode != nil && *raw.Mode == "" {
		return fmt.Errorf("lane %q lease.mode: expected %s, %s, or %s, found empty", name, LeaseModeOff, LeaseModePrompt, LeaseModePromptCheck)
	}
	if raw.ClaimFor != nil && *raw.ClaimFor == "" {
		return fmt.Errorf("lane %q lease.claim_for: expected a positive duration, found empty", name)
	}
	return nil
}

func (raw rawSync) toSync() (Sync, error) {
	if raw.MaxAge == nil {
		return Sync{}, nil
	}
	parsed, err := time.ParseDuration(*raw.MaxAge)
	if err != nil {
		return Sync{}, fmt.Errorf("sync.max_age: expected a duration, found %q: %w", *raw.MaxAge, err)
	}
	return Sync{MaxAge: parsed}, nil
}

func (raw rawDefaults) toDefaults() (Defaults, error) {
	d := Defaults{
		MaxInFlight:   3,
		ExcludeLabels: []string{"needs-human", "blocked-external"},
		HumanLabels:   []string{"needs-human"},
		SkipTypes:     []string{"Epic"},
		AgentKind:     "claude",
		DryRun:        true,
	}
	var err error
	if raw.MaxInFlight != nil {
		d.MaxInFlight = *raw.MaxInFlight
	}
	d.MinStartInterval, err = parseDurationField("min_start_interval", raw.MinStartInterval, 10*time.Minute)
	if err != nil {
		return Defaults{}, err
	}
	d.FullSyncEvery, err = parseDurationField("full_sync_every", raw.FullSyncEvery, 24*time.Hour)
	if err != nil {
		return Defaults{}, err
	}
	d.StallAfter, err = parseDurationField("stall_after", raw.StallAfter, 2*time.Hour)
	if err != nil {
		return Defaults{}, err
	}
	raw.applyFields(&d)
	d.WakeBeforePrompt, err = parseWake(raw.WakeBeforePrompt)
	if err != nil {
		return Defaults{}, err
	}
	if err := raw.startTimings(&d); err != nil {
		return Defaults{}, err
	}
	if d.StartPromptAppend, d.startPromptAppendFile, err = parseStartPromptAppend("defaults", raw.StartPromptAppend); err != nil {
		return Defaults{}, err
	}
	return d, nil
}

func (raw rawDefaults) applyFields(d *Defaults) {
	if raw.ExcludeLabels != nil {
		d.ExcludeLabels = copyStrings(raw.ExcludeLabels)
	}
	if raw.HumanLabels != nil {
		d.HumanLabels = copyStrings(raw.HumanLabels)
	}
	if raw.SkipTypes != nil {
		d.SkipTypes = copyStrings(raw.SkipTypes)
	}
	if raw.AgentKind != nil {
		d.AgentKind = *raw.AgentKind
	}
	if raw.AgentArgs != nil {
		d.AgentArgs = copyStrings(raw.AgentArgs)
	}
	if raw.DryRun != nil {
		d.DryRun = *raw.DryRun
	}
	if raw.Caller != nil && strings.TrimSpace(*raw.Caller) != "" {
		d.Caller = *raw.Caller
	}
}

func parseStartPromptAppend(where string, raw any) (string, bool, error) {
	if raw == nil {
		return "", false, nil
	}
	switch v := raw.(type) {
	case string:
		if !strings.HasPrefix(v, "@") {
			return "", false, fmt.Errorf("%s field \"start_prompt_append\": expected a list of strings or @path, found %q", where, v)
		}
		if strings.TrimPrefix(v, "@") == "" {
			return "", false, fmt.Errorf("%s field \"start_prompt_append\": expected a file path after @, found empty", where)
		}
		return v, true, nil
	case []any:
		lines := make([]string, len(v))
		for i, item := range v {
			s, ok := item.(string)
			if !ok {
				return "", false, fmt.Errorf("%s field \"start_prompt_append\": expected a list of strings or @path, found element %d of type %T", where, i+1, item)
			}
			lines[i] = s
		}
		return strings.Join(lines, "\n"), false, nil
	default:
		return "", false, fmt.Errorf("%s field \"start_prompt_append\": expected a list of strings or @path, found %T", where, raw)
	}
}

// startTimings reads the agent ready and busy waits. The two timeouts must
// be positive. The settle pause may be zero.
func (raw rawDefaults) startTimings(d *Defaults) error {
	var err error
	if d.AgentReadyTimeout, err = parseDurationField("agent_ready_timeout", raw.AgentReadyTimeout, 60*time.Second); err != nil {
		return err
	}
	if d.AgentReadySettle, err = parseDurationField("agent_ready_settle", raw.AgentReadySettle, time.Second); err != nil {
		return err
	}
	if d.AgentBusyTimeout, err = parseDurationField("agent_busy_timeout", raw.AgentBusyTimeout, 20*time.Second); err != nil {
		return err
	}
	switch {
	case d.AgentReadyTimeout <= 0:
		return fmt.Errorf("defaults field \"agent_ready_timeout\": expected a positive duration, found %s", d.AgentReadyTimeout)
	case d.AgentBusyTimeout <= 0:
		return fmt.Errorf("defaults field \"agent_busy_timeout\": expected a positive duration, found %s", d.AgentBusyTimeout)
	case d.AgentReadySettle < 0:
		return fmt.Errorf("defaults field \"agent_ready_settle\": expected a non-negative duration, found %s", d.AgentReadySettle)
	}
	return nil
}

// parseWake reads defaults.wake_before_prompt. Absent is WakeAuto.
func parseWake(value *string) (string, error) {
	if value == nil {
		return WakeAuto, nil
	}
	switch *value {
	case WakeAuto, WakeOff:
		return *value, nil
	default:
		return "", fmt.Errorf("defaults.wake_before_prompt: expected %q or %q, found %q", WakeAuto, WakeOff, *value)
	}
}

func parseDurationField(field string, value *string, fallback time.Duration) (time.Duration, error) {
	if value == nil {
		return fallback, nil
	}
	parsed, err := time.ParseDuration(*value)
	if err != nil {
		return 0, fmt.Errorf("defaults field %q: expected a duration, found %q: %w", field, *value, err)
	}
	return parsed, nil
}

func parseOptionalDuration(field string, value *string) (*time.Duration, error) {
	if value == nil {
		return nil, nil
	}
	parsed, err := time.ParseDuration(*value)
	if err != nil {
		return nil, fmt.Errorf("%s: expected a duration, found %q: %w", field, *value, err)
	}
	return &parsed, nil
}

// blockerLookups reads external_blocker_lookups, or the old key
// doctor_blocker_lookups. Setting both is an error. Default 100.
func (raw rawSource) applyListDefaults(src *Source, defaults Defaults) {
	src.ExcludeLabels = inheritStrings(raw.ExcludeLabels, defaults.ExcludeLabels)
	src.HumanLabels = inheritStrings(raw.HumanLabels, defaults.HumanLabels)
	src.SkipTypes = inheritStrings(raw.SkipTypes, defaults.SkipTypes)
	if raw.OrphanIgnore != nil {
		src.OrphanIgnore = copyStrings(raw.OrphanIgnore)
	}
	if raw.OrphanIgnoreLabels != nil {
		src.OrphanIgnoreLabels = copyStrings(raw.OrphanIgnoreLabels)
	}
	src.ClaimParentTypes = inheritStrings(raw.ClaimParentTypes, []string{"Epic", "Story"})
	src.OrphanEpicChildTypes = inheritStrings(raw.OrphanEpicTypes, []string{"Story", "Task", "Bug"})
}

func inheritStrings(raw, fallback []string) []string {
	if raw != nil {
		return copyStrings(raw)
	}
	return copyStrings(fallback)
}

func (raw rawSource) applyStartPromptAppend(src *Source, defaults Defaults) error {
	if raw.StartPromptAppend == nil {
		src.StartPromptAppend = defaults.StartPromptAppend
		src.startPromptAppendFile = defaults.startPromptAppendFile
		return nil
	}
	val, isFile, err := parseStartPromptAppend(fmt.Sprintf("source %q", src.Name), raw.StartPromptAppend)
	if err != nil {
		return err
	}
	src.StartPromptAppend = val
	src.startPromptAppendFile = isFile
	src.startPromptAppendSet = true
	return nil
}

func (raw rawSource) blockerLookups() (int, string, error) {
	const key, old = "external_blocker_lookups", "doctor_blocker_lookups"
	switch {
	case raw.BlockerLookups != nil && raw.OldBlockerLookups != nil:
		return 0, key, fmt.Errorf("source %q: expected one of %s or %s (the old name), found both", str(raw.Name), key, old)
	case raw.BlockerLookups != nil:
		return *raw.BlockerLookups, key, nil
	case raw.OldBlockerLookups != nil:
		return *raw.OldBlockerLookups, old, nil
	default:
		return 100, key, nil
	}
}

func (raw rawSource) toSource(defaults Defaults) (Source, error) {
	src := Source{
		Name:             str(raw.Name),
		Kind:             str(raw.Kind),
		Cwd:              str(raw.Cwd),
		Base:             str(raw.Base),
		Workspace:        str(raw.Workspace),
		StartPrompt:      str(raw.StartPrompt),
		RequireLabels:    copyStrings(raw.RequireLabels),
		ParentPriority:   copyStrings(raw.ParentPriority),
		RankOrder:        copyStrings(raw.RankOrder),
		ParentAllowlist:  copyStrings(raw.ParentAllowlist),
		FinishOnMerge:    boolValue(raw.FinishOnMerge),
		WorkspaceRules:   copyRules(raw.WorkspaceRules),
		Site:             str(raw.Site),
		Email:            str(raw.Email),
		TokenFile:        str(raw.TokenFile),
		TokenEnv:         str(raw.TokenEnv),
		JQL:              str(raw.JQL),
		ReadyStatuses:    copyStrings(raw.ReadyStatuses),
		FinishStatus:     str(raw.FinishStatus),
		DefaultParent:    str(raw.DefaultParent),
		CreateProject:    str(raw.CreateProject),
		Repo:             str(raw.Repo),
		PriorityLabels:   copyPriorities(raw.PriorityLabels),
		PriorityRanks:    copyPriorities(raw.PriorityRanks),
		KnownPriorities:  copyStrings(raw.KnownPriorities),
		PRTemplate:       str(raw.PRTemplate),
		DeferField:       str(raw.DeferField),
		DeferLabelPrefix: str(raw.DeferLabelPrefix),
		PriorityField:    str(raw.PriorityField),
		DueField:         str(raw.DueField),
	}
	if err := raw.applyStartPromptAppend(&src, defaults); err != nil {
		return Source{}, err
	}
	raw.applyListDefaults(&src, defaults)
	lookups, key, err := raw.blockerLookups()
	if err != nil {
		return Source{}, err
	}
	src.ExternalBlockerLookups = lookups
	src.ExternalBlockerKey = key
	src.CreateAssignee = CreateAssigneeMe
	if raw.CreateAssignee != nil {
		src.CreateAssignee = *raw.CreateAssignee
	}
	src.ParentBlocksChild = ParentBlocksIgnore
	if raw.ParentBlocksChild != nil {
		src.ParentBlocksChild = *raw.ParentBlocksChild
	}
	switch src.Kind {
	case "jira":
		applyJiraDefaults(&src, raw)
	case "github":
		applyGitHubDefaults(&src, raw)
	}
	if err := resolveSourceTrust(&src, raw.Trust); err != nil {
		return Source{}, err
	}
	maxAge, err := parseSourceMaxAge(src.Name, raw.MaxAge)
	if err != nil {
		return Source{}, err
	}
	src.MaxAge = maxAge
	return src, nil
}

func parseSourceMaxAge(name string, raw *string) (*time.Duration, error) {
	if raw == nil {
		return nil, nil
	}
	parsed, err := time.ParseDuration(*raw)
	if err != nil {
		return nil, fmt.Errorf("source %q field \"max_age\": expected a duration, found %q: %w", name, *raw, err)
	}
	return &parsed, nil
}

func applyJiraDefaults(src *Source, raw rawSource) {
	src.BlocksLinkType = "Blocks"
	if raw.BlocksLinkType != nil {
		src.BlocksLinkType = *raw.BlocksLinkType
	}
	src.PRTitlePattern = defaultPRTitlePattern
	if raw.PRTitlePattern != nil {
		src.PRTitlePattern = *raw.PRTitlePattern
	}
	src.CreateIssueType = "Task"
	if raw.CreateIssueType != nil {
		src.CreateIssueType = *raw.CreateIssueType
	}
	if raw.CreateSubtaskIssueType != nil {
		src.CreateSubtaskIssueType = *raw.CreateSubtaskIssueType
	}
}

// Default GitHub native issue field names (matching GitHub's starter fields).
const (
	DefaultGitHubPriorityField = "Priority"
	DefaultGitHubDeferField    = "Start date"
	DefaultGitHubDueField      = "Target date"
)

func applyGitHubDefaults(src *Source, raw rawSource) {
	if raw.ReadyStatuses == nil {
		src.ReadyStatuses = DefaultGitHubReadyStatuses()
	}
	if raw.PriorityField == nil {
		src.PriorityField = DefaultGitHubPriorityField
	}
	if raw.DeferField == nil {
		src.DeferField = DefaultGitHubDeferField
	}
	if raw.DueField == nil {
		src.DueField = DefaultGitHubDueField
	}
	if src.Workspace == "" {
		_, name, ok := strings.Cut(src.Repo, "/")
		if ok && name != "" && !strings.Contains(name, "/") {
			src.Workspace = name
		}
	}
	if raw.BlocksLinkType != nil {
		src.BlocksLinkType = *raw.BlocksLinkType
	}
	if raw.PRTitlePattern != nil {
		src.PRTitlePattern = *raw.PRTitlePattern
	}
	if raw.CreateIssueType != nil {
		src.CreateIssueType = *raw.CreateIssueType
	}
	if raw.CreateSubtaskIssueType != nil {
		src.CreateSubtaskIssueType = *raw.CreateSubtaskIssueType
	}
}

func expand(cfg *Config, env Env) error {
	if err := expandGate(&cfg.Gate, env); err != nil {
		return err
	}
	if cfg.Defaults.startPromptAppendFile {
		if err := expandAppendPrompt("defaults", &cfg.Defaults.StartPromptAppend, env); err != nil {
			return err
		}
		cfg.Defaults.startPromptAppendFile = false
	}
	for i := range cfg.Lanes {
		if cfg.Lanes[i].startPromptAppendFile {
			if err := expandAppendPrompt(fmt.Sprintf("lane %q", cfg.Lanes[i].Name), &cfg.Lanes[i].StartPromptAppend, env); err != nil {
				return err
			}
			cfg.Lanes[i].startPromptAppendFile = false
		}
		// A remote checkout is a path on that machine. Its ~ is not ours.
		if cfg.Lanes[i].Remote() {
			continue
		}
		checkout, err := expandUserPath(cfg.Lanes[i].Checkout, env)
		if err != nil {
			return fmt.Errorf("lane %q field \"checkout\": %w", cfg.Lanes[i].Name, err)
		}
		cfg.Lanes[i].Checkout = checkout
	}
	for i := range cfg.Sources {
		if !cfg.Sources[i].startPromptAppendSet {
			cfg.Sources[i].StartPromptAppend = cfg.Defaults.StartPromptAppend
			cfg.Sources[i].startPromptAppendFile = false
		}
		if err := expandSource(&cfg.Sources[i], env); err != nil {
			return err
		}
	}
	return nil
}

func expandGate(gate *Gate, env Env) error {
	for i, path := range gate.HoldFiles {
		expanded, err := expandUserPath(path, env)
		if err != nil {
			return fmt.Errorf("gate.hold_files: %w", err)
		}
		gate.HoldFiles[i] = expanded
	}
	if len(gate.HoldFiles) > 0 {
		return nil
	}
	home, err := env.HomeDir()
	if err != nil {
		return fmt.Errorf("gate.hold_files: home directory: %w", err)
	}
	if home == "" {
		return errors.New("gate.hold_files: home directory: expected a non-empty path, found empty")
	}
	gate.DefaultHoldFile = filepath.Join(home, ".local", "state", "factory", "hold")
	return nil
}

func expandSource(src *Source, env Env) error {
	cwd, err := expandUserPath(src.Cwd, env)
	if err != nil {
		return fmt.Errorf("source %q field \"cwd\": %w", src.Name, err)
	}
	src.Cwd = cwd
	tokenFile, err := expandUserPath(src.TokenFile, env)
	if err != nil {
		return fmt.Errorf("source %q field \"token_file\": %w", src.Name, err)
	}
	src.TokenFile = tokenFile
	return expandPrompt(src, env)
}

func expandPrompt(src *Source, env Env) error {
	if src.startPromptAppendFile {
		if err := expandAppendPrompt(fmt.Sprintf("source %q", src.Name), &src.StartPromptAppend, env); err != nil {
			return err
		}
		src.startPromptAppendFile = false
	}
	if !strings.HasPrefix(src.StartPrompt, "@") {
		return nil
	}
	path, err := expandUserPath(strings.TrimPrefix(src.StartPrompt, "@"), env)
	if err != nil {
		return fmt.Errorf("source %q field \"start_prompt\": %w", src.Name, err)
	}
	if path == "" {
		return fmt.Errorf("source %q field \"start_prompt\": expected a file path after @, found empty", src.Name)
	}
	data, err := env.ReadFile(path)
	if err != nil {
		return fmt.Errorf("source %q field \"start_prompt\": expected to read %s: %w", src.Name, path, err)
	}
	src.StartPrompt = string(data)
	return nil
}

func expandAppendPrompt(where string, target *string, env Env) error {
	path, err := expandUserPath(strings.TrimPrefix(*target, "@"), env)
	if err != nil {
		return fmt.Errorf("%s field \"start_prompt_append\": %w", where, err)
	}
	if path == "" {
		return fmt.Errorf("%s field \"start_prompt_append\": expected a file path after @, found empty", where)
	}
	data, err := env.ReadFile(path)
	if err != nil {
		return fmt.Errorf("%s field \"start_prompt_append\": expected to read %s: %w", where, path, err)
	}
	*target = strings.TrimRight(string(data), "\r\n")
	return nil
}

// expandUserPath expands a leading ~/ using Env.HomeDir.
// Other shapes, including a ~/ in the middle of the path, are returned as-is.
func expandUserPath(path string, env Env) (string, error) {
	if path != "~" && !strings.HasPrefix(path, "~/") {
		return path, nil
	}
	home, err := env.HomeDir()
	if err != nil {
		return "", fmt.Errorf("home directory: %w", err)
	}
	if home == "" {
		return "", errors.New("home directory: expected a non-empty path, found empty")
	}
	if path == "~" {
		return home, nil
	}
	return filepath.Join(home, path[2:]), nil
}

// prepareTokens arms each Jira source to read its credential on ResolveToken.
// It does not read the file or the environment variable.
func prepareTokens(cfg *Config, env Env) {
	read := tokenReader(env)
	for i := range cfg.Sources {
		if cfg.Sources[i].Kind != "jira" {
			continue
		}
		cfg.Sources[i].readToken = read
	}
}

func tokenReader(env Env) func(*Source) error {
	return func(src *Source) error {
		return loadToken(src, env)
	}
}

// ResolveToken reads the Jira API token from token_file or token_env.
// Load checks that exactly one of those fields is set and does not read it.
// Call this when a command contacts the source. A second call does not read again.
// A source that Load did not arm, including GitHub, is a no-op.
func (s *Source) ResolveToken() error {
	if s == nil {
		return errors.New("source: expected a source, found nil")
	}
	if s.readToken == nil {
		return nil
	}
	if err := s.readToken(s); err != nil {
		return err
	}
	s.readToken = nil
	return nil
}

func loadToken(src *Source, env Env) error {
	if src.TokenFile != "" {
		return readTokenFile(src, env)
	}
	return readTokenEnv(src, env)
}

func readTokenFile(src *Source, env Env) error {
	info, err := env.Stat(src.TokenFile)
	if err != nil {
		return fmt.Errorf("source %q field \"token_file\": expected to read %s: %w", src.Name, src.TokenFile, err)
	}
	if info.Mode().Perm()&0o077 != 0 {
		return fmt.Errorf("source %q field \"token_file\": %s is accessible by group or others; chmod 600 %s", src.Name, src.TokenFile, src.TokenFile)
	}
	data, err := env.ReadFile(src.TokenFile)
	if err != nil {
		return fmt.Errorf("source %q field \"token_file\": expected to read %s: %w", src.Name, src.TokenFile, err)
	}
	src.Token = strings.TrimSpace(string(data))
	return nil
}

func readTokenEnv(src *Source, env Env) error {
	value, ok := env.LookupEnv(src.TokenEnv)
	if !ok {
		return fmt.Errorf("source %q field \"token_env\": expected environment variable %s to be set, found unset", src.Name, src.TokenEnv)
	}
	src.Token = value
	return nil
}

func str(value *string) string {
	if value == nil {
		return ""
	}
	return *value
}

func boolValue(value *bool) bool {
	if value == nil {
		return false
	}
	return *value
}

func copyStrings(in []string) []string {
	if in == nil {
		return nil
	}
	out := make([]string, len(in))
	copy(out, in)
	return out
}

func copyPriorities(in map[string]int) map[string]int {
	out := make(map[string]int, len(in))
	for key, value := range in {
		out[key] = value
	}
	return out
}

func copyRules(in []WorkspaceRule) []WorkspaceRule {
	if in == nil {
		return nil
	}
	out := make([]WorkspaceRule, len(in))
	copy(out, in)
	return out
}

func copyAgentRules(in []AgentRule) []AgentRule {
	if in == nil {
		return nil
	}
	out := make([]AgentRule, len(in))
	for i, rule := range in {
		out[i] = AgentRule{
			Name:      rule.Name,
			Labels:    copyStrings(rule.Labels),
			AgentArgs: copyStrings(rule.AgentArgs),
		}
	}
	return out
}
