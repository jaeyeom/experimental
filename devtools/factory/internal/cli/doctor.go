package cli

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"os"
	"slices"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/doctor"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/remote"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/toolcli"
	"github.com/jaeyeom/experimental/devtools/factory/internal/verify"
)

func newDoctorCmd(ctx context.Context) *cobra.Command {
	var asJSON bool
	var noSync bool
	var offline bool
	var scope scopeFlags
	cmd := &cobra.Command{
		Use:           "doctor",
		Short:         "Run CLI diagnostics and report dependency-graph and claim problems",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("doctor: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runDoctor(ctx, cmd, asJSON, noSync, offline, scope)
		},
	}
	cmd.Flags().BoolVar(&asJSON, "json", false, "print the diagnostics as JSON")
	cmd.Flags().BoolVar(&noSync, "no-sync", false, "do not refresh a stale mirror")
	cmd.Flags().BoolVar(&offline, "offline", false, "skip network-dependent checks")
	addScopeFlags(cmd, &scope)
	return cmd
}

func runDoctor(ctx context.Context, cmd *cobra.Command, asJSON, noSync, offline bool, scope scopeFlags) (err error) {
	if ctx == nil {
		return errors.New("doctor: expected a context, found nil")
	}
	if err := scope.check("doctor"); err != nil {
		return err
	}

	tool := factoryTool()
	cfg, cfgErr := loadConfig(cmd)
	configureDoctorTool(tool, cfg, cfgErr)

	st, storeCheck := openDoctorStore(ctx, cmd)
	if st != nil {
		defer func() {
			if cerr := st.Close(); cerr != nil {
				err = errors.Join(err, fmt.Errorf("close store: %w", cerr))
			}
		}()
	}

	extra, scopeErr := buildDoctorExtras(ctx, cmd, st, storeCheck, cfg, cfgErr, scope, asJSON, noSync, offline)
	if scopeErr != nil {
		return scopeErr
	}

	code, runErr := tool.RunDoctor(ctx, offline, asJSON, extra, cmd.OutOrStdout())
	if runErr != nil {
		return exitStatusError{code: code, err: runErr}
	}
	if code != 0 {
		return silentExitError{code: code}
	}
	return nil
}

func configureDoctorTool(tool *toolcli.Tool, cfg *config.Config, cfgErr error) {
	tool.CheckConfig = func() error { return cfgErr }
	if cfgErr == nil && cfg != nil {
		tool.ConfigPath = func() (string, bool) { return cfg.Path, cfg.Path != "" }
		if cfg.GHBin != "" {
			tool.RequiredCommands = []string{"herdr", cfg.GHBin, "git"}
		}
	}
}

func buildDoctorExtras(ctx context.Context, cmd *cobra.Command, st *store.Store, storeCheck toolcli.CheckResult, cfg *config.Config, cfgErr error, scope scopeFlags, asJSON, noSync, offline bool) ([]toolcli.CheckResult, error) {
	extra := []toolcli.CheckResult{storeCheck, checkSourcesDoctor(ctx, cfg, cfgErr, offline)}
	extra = append(extra, checkGitHubDoctor(ctx, cfg, cfgErr, offline)...)
	if cfgErr != nil || cfg == nil || st == nil {
		return extra, nil
	}
	only, scopeErr := scope.resolve(cmd, cfg)
	if scopeErr != nil {
		return nil, scopeErr
	}
	cfgs, srcErr := sourcesFor(cfg, only)
	if srcErr != nil {
		return nil, srcErr
	}
	if cfg.Sync.MaxAge > 0 {
		if err := writeMirrorAge(ctx, cmd, st, cfgs, cfg.Sync.MaxAge, asJSON); err != nil {
			return nil, err
		}
	}
	return append(extra, collectDomainDoctorChecks(ctx, cmd, st, cfg, only, noSync || offline)...), nil
}

func openDoctorStore(ctx context.Context, cmd *cobra.Command) (*store.Store, toolcli.CheckResult) {
	dbPath, err := databasePath(cmd)
	if err != nil {
		return nil, toolcli.CheckResult{
			Name:   "store",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "check XDG_STATE_HOME or --db",
		}
	}
	st, err := store.Open(ctx, dbPath)
	if err != nil {
		return nil, toolcli.CheckResult{
			Name:   "store",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "ensure the store directory is writable",
		}
	}
	return st, toolcli.CheckResult{
		Name:   "store",
		Status: toolcli.StatusOK,
		Detail: dbPath,
	}
}

func checkSourcesDoctor(ctx context.Context, cfg *config.Config, cfgErr error, offline bool) toolcli.CheckResult {
	if offline {
		return toolcli.CheckResult{
			Name:   "sources",
			Status: toolcli.StatusWarn,
			Detail: "skipped (--offline)",
			Hint:   "run without --offline to check source reachability",
		}
	}
	if cfgErr != nil || cfg == nil {
		return toolcli.CheckResult{
			Name:   "sources",
			Status: toolcli.StatusWarn,
			Detail: "skipped (config unavailable)",
			Hint:   "fix config to check source reachability",
		}
	}
	if len(cfg.Sources) == 0 {
		return toolcli.CheckResult{
			Name:   "sources",
			Status: toolcli.StatusOK,
			Detail: "no sources configured",
		}
	}
	return verifyConfiguredSources(ctx, cfg)
}

func verifyConfiguredSources(ctx context.Context, cfg *config.Config) toolcli.CheckResult {
	var checked []string
	ghChecked := false
	for i := range cfg.Sources {
		src := cfg.Sources[i]
		checked = append(checked, src.Kind+":"+src.Name)
		switch src.Kind {
		case "github":
			if !ghChecked {
				ghChecked = true
				if res, failed := checkGHAuth(ctx, cfg.GHBin); failed {
					return res
				}
			}
		case "jira":
			if cfg.Sync.MaxAge > 0 {
				if res, failed := checkJiraToken(src); failed {
					return res
				}
			}
		}
	}
	return toolcli.CheckResult{
		Name:   "sources",
		Status: toolcli.StatusOK,
		Detail: strings.Join(checked, ", "),
	}
}

func checkGHAuth(ctx context.Context, ghBin string) (toolcli.CheckResult, bool) {
	if ghBin == "" {
		ghBin = config.DefaultGHBin
	}
	exec := newCommandExec()
	if exec == nil {
		return toolcli.CheckResult{
			Name:   "sources",
			Status: toolcli.StatusFail,
			Detail: "expected an executor, found nil",
		}, true
	}
	res, err := exec.Execute(ctx, executor.ToolConfig{
		Command: ghBin,
		Args:    []string{"auth", "status"},
	})
	if err != nil {
		return toolcli.CheckResult{
			Name:   "sources",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("%s auth status failed: %v", ghBin, err),
			Hint:   fmt.Sprintf("run `%s auth login`", ghBin),
		}, true
	}
	if res == nil || res.ExitCode != 0 {
		msg := "non-zero exit"
		if res != nil {
			if s := strings.TrimSpace(res.Stderr); s != "" {
				msg = s
			} else {
				msg = fmt.Sprintf("exit %d", res.ExitCode)
			}
		}
		return toolcli.CheckResult{
			Name:   "sources",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("%s auth status failed: %s", ghBin, msg),
			Hint:   fmt.Sprintf("run `%s auth login`", ghBin),
		}, true
	}
	return toolcli.CheckResult{}, false
}

// checkGitHubDoctor verifies the gh CLI version and, when online, checks
// native GitHub issue status and issue field availability for every configured
// GitHub source, warning when legacy label configs can be removed.
func checkGitHubDoctor(ctx context.Context, cfg *config.Config, cfgErr error, offline bool) []toolcli.CheckResult {
	if cfgErr != nil || cfg == nil {
		return nil
	}
	var ghSources []config.Source
	for i := range cfg.Sources {
		if cfg.Sources[i].Kind == "github" {
			ghSources = append(ghSources, cfg.Sources[i])
		}
	}
	if len(ghSources) == 0 {
		return nil
	}
	ghBin := cfg.GHBin
	if ghBin == "" {
		ghBin = config.DefaultGHBin
	}
	exec := newCommandExec()
	if exec == nil {
		return []toolcli.CheckResult{{
			Name:   "github-status",
			Status: toolcli.StatusFail,
			Detail: "expected an executor, found nil",
		}}
	}
	client, err := ghClient(exec, ghBin)
	if err != nil {
		return []toolcli.CheckResult{{
			Name:   "github-status",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
		}}
	}
	ver, err := client.Version(ctx)
	if err != nil {
		return []toolcli.CheckResult{{
			Name:   "github-status",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("%s --version failed: %v", ghBin, err),
			Hint:   fmt.Sprintf("install or upgrade %s (>= %s)", ghBin, gh.MinGHVersion),
		}}
	}
	if !ver.AtLeast(gh.MinGHVersion) {
		return []toolcli.CheckResult{{
			Name:   "github-status",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("%s version %s is older than %s", ghBin, ver, gh.MinGHVersion),
			Hint:   fmt.Sprintf("upgrade %s to >= %s", ghBin, gh.MinGHVersion),
		}}
	}
	if offline {
		return []toolcli.CheckResult{{
			Name:   "github-status",
			Status: toolcli.StatusWarn,
			Detail: fmt.Sprintf("gh %s; repo status and field checks skipped (--offline)", ver),
			Hint:   "run without --offline to check repository issue status and field support",
		}}
	}
	return []toolcli.CheckResult{
		checkGitHubRepoStatuses(ctx, client, ver, ghSources),
		checkGitHubRepoFields(ctx, client, ghSources),
	}
}

type repoStatusProbe struct {
	statuses []gh.PlanningStatus
	names    []string
	err      error
}

type ghStatusSummary struct {
	failures       []string
	failHints      []string
	warnings       []string
	warnHints      []string
	supportedRepos []string
	seenSupported  map[string]struct{}
}

func checkGitHubRepoStatuses(ctx context.Context, client *gh.Client, ver gh.SemVer, sources []config.Source) toolcli.CheckResult {
	probes := probeRepoStatuses(ctx, client, sources)
	summary := ghStatusSummary{seenSupported: make(map[string]struct{})}
	for _, src := range sources {
		summary.evaluate(src, probes[src.Repo])
	}
	return summary.result(ver)
}

func probeRepoStatuses(ctx context.Context, client *gh.Client, sources []config.Source) map[string]repoStatusProbe {
	probes := make(map[string]repoStatusProbe, len(sources))
	for _, src := range sources {
		if _, ok := probes[src.Repo]; ok {
			continue
		}
		statuses, err := client.PlanningStatuses(ctx, src.Repo)
		names := make([]string, len(statuses))
		for i, st := range statuses {
			names[i] = st.Name
		}
		probes[src.Repo] = repoStatusProbe{statuses: statuses, names: names, err: err}
	}
	return probes
}

func (s *ghStatusSummary) evaluate(src config.Source, probe repoStatusProbe) {
	if probe.err != nil || len(probe.statuses) == 0 {
		if src.Claim.MarkInProgress && src.ClaimStatus != "" {
			reason := "no planning statuses returned"
			if probe.err != nil {
				reason = probe.err.Error()
			}
			s.failures = append(s.failures, fmt.Sprintf("github:%s (%s): native issue statuses unavailable (%s)", src.Name, src.Repo, reason))
			s.failHints = append(s.failHints, fmt.Sprintf("enable GitHub issue statuses on %s, or set claim_label = %q on source %q", src.Repo, config.DefaultClaimLabel, src.Name))
		}
		return
	}
	if _, ok := s.seenSupported[src.Repo]; !ok {
		s.seenSupported[src.Repo] = struct{}{}
		s.supportedRepos = append(s.supportedRepos, src.Repo)
	}
	if src.Claim.MarkInProgress && src.ClaimStatus != "" && !slices.Contains(probe.names, src.ClaimStatus) {
		s.failures = append(s.failures, fmt.Sprintf("github:%s (%s): claim_status %q not in repository statuses (%s)", src.Name, src.Repo, src.ClaimStatus, strings.Join(probe.names, ", ")))
		s.failHints = append(s.failHints, fmt.Sprintf("set claim_status on source %q to one of: %s", src.Name, strings.Join(probe.names, ", ")))
	}
	if src.ClaimLabel != "" {
		s.warnings = append(s.warnings, fmt.Sprintf("github:%s (%s) uses claim_label %q, but native issue statuses are available", src.Name, src.Repo, src.ClaimLabel))
		s.warnHints = append(s.warnHints, fmt.Sprintf("remove claim_label from source %q in factory.toml to use native issue statuses", src.Name))
	}
}

func (s *ghStatusSummary) result(ver gh.SemVer) toolcli.CheckResult {
	switch {
	case len(s.failures) > 0:
		return toolcli.CheckResult{
			Name:   "github-status",
			Status: toolcli.StatusFail,
			Detail: strings.Join(s.failures, "; "),
			Hint:   strings.Join(s.failHints, "; "),
		}
	case len(s.warnings) > 0:
		return toolcli.CheckResult{
			Name:   "github-status",
			Status: toolcli.StatusWarn,
			Detail: strings.Join(s.warnings, "; "),
			Hint:   strings.Join(s.warnHints, "; "),
		}
	case len(s.supportedRepos) == 0:
		return toolcli.CheckResult{
			Name:   "github-status",
			Status: toolcli.StatusOK,
			Detail: fmt.Sprintf("gh %s; label-based claiming configured", ver),
		}
	default:
		return toolcli.CheckResult{
			Name:   "github-status",
			Status: toolcli.StatusOK,
			Detail: fmt.Sprintf("gh %s; native issue statuses supported (%s)", ver, strings.Join(s.supportedRepos, ", ")),
		}
	}
}

type repoFieldsProbe struct {
	singleSelect map[string]bool
	dates        map[string]bool
	err          error
}

type ghFieldsSummary struct {
	warnings  []string
	warnHints []string
	available []string
	seenRepo  map[string]struct{}
}

func checkGitHubRepoFields(ctx context.Context, client *gh.Client, sources []config.Source) toolcli.CheckResult {
	probes := probeRepoFields(ctx, client, sources)
	summary := ghFieldsSummary{seenRepo: make(map[string]struct{})}
	for _, src := range sources {
		summary.evaluate(src, probes[src.Repo])
	}
	return summary.result()
}

func probeRepoFields(ctx context.Context, client *gh.Client, sources []config.Source) map[string]repoFieldsProbe {
	probes := make(map[string]repoFieldsProbe, len(sources))
	for _, src := range sources {
		if _, ok := probes[src.Repo]; ok {
			continue
		}
		fields, err := client.IssueFields(ctx, src.Repo)
		ss := map[string]bool{}
		dates := map[string]bool{}
		for _, f := range fields {
			switch f.TypeName {
			case gh.IssueFieldTypeSingleSelect:
				ss[f.Name] = true
			case gh.IssueFieldTypeDate:
				dates[f.Name] = true
			}
		}
		probes[src.Repo] = repoFieldsProbe{singleSelect: ss, dates: dates, err: err}
	}
	return probes
}

func (s *ghFieldsSummary) evaluate(src config.Source, probe repoFieldsProbe) {
	if probe.err != nil {
		return
	}
	var matched []string
	if src.PriorityField != "" && probe.singleSelect[src.PriorityField] {
		matched = append(matched, src.PriorityField)
		if len(src.PriorityLabels) > 0 {
			s.warnings = append(s.warnings, fmt.Sprintf("github:%s (%s) uses priority_labels, but native issue field %q is available", src.Name, src.Repo, src.PriorityField))
			s.warnHints = append(s.warnHints, fmt.Sprintf("remove priority_labels from source %q in factory.toml to use the native %q field", src.Name, src.PriorityField))
		}
	}
	if src.DeferField != "" && probe.dates[src.DeferField] {
		matched = append(matched, src.DeferField)
		if src.DeferLabelPrefix != "" {
			s.warnings = append(s.warnings, fmt.Sprintf("github:%s (%s) uses defer_label_prefix %q, but native issue field %q is available", src.Name, src.Repo, src.DeferLabelPrefix, src.DeferField))
			s.warnHints = append(s.warnHints, fmt.Sprintf("remove defer_label_prefix from source %q in factory.toml to use the native %q field", src.Name, src.DeferField))
		}
	}
	if src.DueField != "" && probe.dates[src.DueField] {
		matched = append(matched, src.DueField)
	}
	if len(matched) > 0 {
		if _, ok := s.seenRepo[src.Repo]; !ok {
			s.seenRepo[src.Repo] = struct{}{}
			s.available = append(s.available, fmt.Sprintf("%s: %s", src.Repo, strings.Join(matched, ", ")))
		}
	}
}

func (s *ghFieldsSummary) result() toolcli.CheckResult {
	if len(s.warnings) > 0 {
		return toolcli.CheckResult{
			Name:   "github-fields",
			Status: toolcli.StatusWarn,
			Detail: strings.Join(s.warnings, "; "),
			Hint:   strings.Join(s.warnHints, "; "),
		}
	}
	if len(s.available) == 0 {
		return toolcli.CheckResult{
			Name:   "github-fields",
			Status: toolcli.StatusOK,
			Detail: "no native issue fields configured on repositories",
		}
	}
	return toolcli.CheckResult{
		Name:   "github-fields",
		Status: toolcli.StatusOK,
		Detail: fmt.Sprintf("native issue fields available (%s)", strings.Join(s.available, "; ")),
	}
}

func checkJiraToken(src config.Source) (toolcli.CheckResult, bool) {
	if src.TokenFile != "" {
		if _, err := os.Stat(src.TokenFile); err != nil {
			return toolcli.CheckResult{
				Name:   "sources",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("jira:%s token_file: %v", src.Name, err),
				Hint:   "create the token file with chmod 600",
			}, true
		}
		return toolcli.CheckResult{}, false
	}
	if src.TokenEnv != "" {
		val, ok := config.OSEnv().LookupEnv(src.TokenEnv)
		if !ok || strings.TrimSpace(val) == "" {
			return toolcli.CheckResult{
				Name:   "sources",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("jira:%s token_env %s is unset or empty", src.Name, src.TokenEnv),
				Hint:   fmt.Sprintf("export %s", src.TokenEnv),
			}, true
		}
	}
	return toolcli.CheckResult{}, false
}

func collectDomainDoctorChecks(ctx context.Context, cmd *cobra.Command, st *store.Store, cfg *config.Config, only string, noSync bool) []toolcli.CheckResult {
	if err := refreshBeforeRead(ctx, cmd, cfg, st, only, noSync); err != nil {
		return []toolcli.CheckResult{{
			Name:   "sync",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
		}}
	}
	findings, err := collectDoctor(ctx, st, cfg, only)
	if err != nil {
		return []toolcli.CheckResult{{
			Name:   "work-items",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "check herdr and store state",
		}}
	}
	slices.SortFunc(findings, func(a, b doctor.Finding) int {
		if c := cmp.Compare(a.Check, b.Check); c != 0 {
			return c
		}
		return cmp.Compare(a.Message, b.Message)
	})
	out := make([]toolcli.CheckResult, 0, len(findings))
	for _, f := range findings {
		status := toolcli.StatusWarn
		if f.Severity == doctor.SeverityInfo {
			status = toolcli.StatusOK
		}
		out = append(out, toolcli.CheckResult{
			Name:   f.Check,
			Status: status,
			Detail: f.Message,
			Hint:   f.Suggestion,
		})
	}
	return out
}

func collectDoctor(ctx context.Context, st *store.Store, cfg *config.Config, only string) ([]doctor.Finding, error) {
	srcs, err := sourcesFor(cfg, only)
	if err != nil {
		return nil, err
	}
	claims, err := st.ActiveClaims(ctx)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	finished, err := st.FinishedItems(ctx)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	lanes, err := doctorLanes(ctx, cfg)
	if err != nil {
		return nil, err
	}
	now := time.Now().UTC()
	var all []doctor.Finding
	for i := range srcs {
		part, err := doctorSource(ctx, st, cfg.Defaults.StallAfter, srcs[i], claims, finished, lanes, now)
		if err != nil {
			return nil, err
		}
		all = append(all, part...)
	}
	return all, nil
}

func doctorSource(ctx context.Context, st *store.Store, stall time.Duration, src config.Source, claims []store.Claim, finished []store.Finished, remote []doctorLane, now time.Time) ([]doctor.Finding, error) {
	key, err := sourceKey(src.Kind, src.Name)
	if err != nil {
		return nil, err
	}
	if err := checkKind(key, src.Kind); err != nil {
		return nil, err
	}
	snap, err := loadSnapshot(ctx, st, src)
	if err != nil {
		return nil, err
	}
	snap, lanes := laneSnapshot(snap, remote)
	blockers, err := storedBlockers(ctx, st, src, key)
	if err != nil {
		return nil, err
	}
	return doctor.Check(doctor.Input{ //nolint:wrapcheck // internal factory error already carries context
		Kind:          src.Kind,
		SourceKey:     key,
		Config:        src,
		Snapshot:      snap,
		Claims:        claims,
		Finished:      finished,
		Now:           now,
		StallAfter:    stall,
		Blockers:      blockers,
		Lanes:         lanes,
		LandedCommits: landedCommitsForDoctor(ctx, src, key, snap, claims, remote),
	})
}

func landedCommitsForDoctor(ctx context.Context, src config.Source, key string, snap ready.Snapshot, claims []store.Claim, lanes []doctorLane) map[string]bool {
	exec := newCommandExec()
	if exec == nil {
		return nil
	}
	activeByID, candidates := doctorMergeCandidates(src, key, snap, claims)
	if len(candidates) == 0 {
		return nil
	}
	laneByName := make(map[string]config.Lane, len(lanes))
	for _, dl := range lanes {
		laneByName[dl.lane.Name] = dl.lane
	}
	out := make(map[string]bool)
	for id := range candidates {
		if _, ok := snap.Items[id]; !ok {
			continue
		}
		claim, hasClaim := activeByID[id]
		if doctorItemLanded(ctx, exec, src, id, claim, hasClaim, laneByName) {
			out[id] = true
		}
	}
	return out
}

func doctorMergeCandidates(src config.Source, key string, snap ready.Snapshot, claims []store.Claim) (map[string]store.Claim, map[string]struct{}) {
	activeByID := make(map[string]store.Claim)
	candidates := make(map[string]struct{})
	for _, claim := range claims {
		if claim.Source != key || !claim.State.Active() {
			continue
		}
		if _, ok := activeByID[claim.ID]; !ok {
			activeByID[claim.ID] = claim
		}
		if claim.State == store.Started {
			candidates[claim.ID] = struct{}{}
		}
	}
	if src.ClaimStatus != "" {
		for id, it := range snap.Items {
			if it.State == src.ClaimStatus {
				candidates[id] = struct{}{}
			}
		}
	}
	return activeByID, candidates
}

func doctorItemLanded(ctx context.Context, exec executor.Executor, src config.Source, id string, claim store.Claim, hasClaim bool, laneByName map[string]config.Lane) bool {
	var lane config.Lane
	if hasClaim && claim.Lane != "" {
		lane = laneByName[claim.Lane]
	}
	effCfg := src
	if lane.Name != "" {
		effCfg = config.LaneSource(src, lane)
	}
	if verify.EffectiveOutputMode(hasClaim, claim, effCfg) != config.OutputModeMerge {
		return false
	}
	dir := effCfg.Cwd
	if hasClaim && claim.Worktree != "" {
		dir = claim.Worktree
	}
	if dir == "" || effCfg.Base == "" {
		return false
	}
	gitExec := exec
	if lane.Remote() {
		rexec, err := remote.New(exec, lane.Exec)
		if err != nil {
			return false
		}
		gitExec = rexec
	}
	_, reason, err := verify.FindLinearCommit(gitExec)(ctx, dir, effCfg.Base, id, effCfg.PRTitlePattern)
	return err == nil && reason == ""
}

// doctorLane is one configured lane. tabs is filled for a remote lane only:
// a local lane's tabs come from each source's snapshot.
type doctorLane struct {
	lane config.Lane
	tabs doctor.LaneTabs
}

// doctorLanes lists each remote lane machine's workspaces and tabs once.
// A machine that does not answer is recorded as unreachable, not an error.
func doctorLanes(ctx context.Context, cfg *config.Config) ([]doctorLane, error) {
	if !cfg.HasLanes() {
		return nil, nil
	}
	exec := newCommandExec()
	if exec == nil {
		return nil, errors.New("doctor lanes: expected an executor, found nil")
	}
	out := make([]doctorLane, 0, len(cfg.Lanes))
	for _, lane := range cfg.Lanes {
		view := doctorLane{lane: lane}
		if lane.Remote() {
			tabs, err := remoteLaneTabs(ctx, herdr.NewClient(exec, "herdr").OnMachine(lane.Machine))
			if err != nil {
				return nil, fmt.Errorf("lane %s: %w", lane.Name, err)
			}
			view.tabs = tabs
		}
		out = append(out, view)
	}
	return out, nil
}

func remoteLaneTabs(ctx context.Context, client *herdr.Client) (doctor.LaneTabs, error) {
	ws, err := client.Workspaces(ctx)
	if err != nil {
		return unreachableLane(ctx, client, err)
	}
	tabs, err := client.Tabs(ctx)
	if err != nil {
		return unreachableLane(ctx, client, err)
	}
	return doctor.LaneTabs{Workspaces: ws, Tabs: tabs}, nil
}

func unreachableLane(ctx context.Context, client *herdr.Client, err error) (doctor.LaneTabs, error) {
	if !herdr.Unreachable(err) {
		return doctor.LaneTabs{}, err
	}
	return doctor.LaneTabs{Unreachable: client.WhyUnreachable(ctx, err)}, nil
}

// laneSnapshot adds every reachable remote lane's workspaces and tabs to snap,
// as tick does, and returns each lane's own tabs for claims recorded on it.
func laneSnapshot(snap ready.Snapshot, lanes []doctorLane) (ready.Snapshot, map[string]doctor.LaneTabs) {
	if len(lanes) == 0 {
		return snap, nil
	}
	local := doctor.LaneTabs{Workspaces: snap.Workspaces, Tabs: snap.Tabs}
	out := make(map[string]doctor.LaneTabs, len(lanes))
	for _, view := range lanes {
		if !view.lane.Remote() {
			out[view.lane.Name] = local
			continue
		}
		out[view.lane.Name] = view.tabs
		if view.tabs.Unreachable == "" {
			snap.Workspaces = append(slices.Clone(snap.Workspaces), view.tabs.Workspaces...)
			snap.Tabs = append(slices.Clone(snap.Tabs), view.tabs.Tabs...)
		}
	}
	return snap, out
}

// storedBlockers converts the external blocker records the last sync of a
// Jira source stored into doctor's lookups. Doctor fetches nothing itself.
func storedBlockers(ctx context.Context, st *store.Store, src config.Source, key string) (map[string]doctor.BlockerLookup, error) {
	if src.Kind != "jira" {
		return nil, nil
	}
	recs, err := st.ExternalBlockers(ctx, key)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	out := make(map[string]doctor.BlockerLookup, len(recs))
	for id, rec := range recs {
		switch rec.Status {
		case store.ExternalFound:
			out[id] = doctor.BlockerLookup{Status: doctor.BlockerFound, Item: source.Item{
				Source: key, ID: id, Title: rec.Title, State: rec.State, Done: rec.Done, Assignee: rec.Assignee,
			}}
		case store.ExternalNotFound:
			out[id] = doctor.BlockerLookup{Status: doctor.BlockerNotFound}
		default:
			out[id] = doctor.BlockerLookup{Status: doctor.BlockerUnchecked, Reason: rec.Reason}
		}
	}
	return out, nil
}
