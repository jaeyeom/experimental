package orgctl

import (
	"fmt"
	"slices"
	"strings"
	"time"
)

// DefaultExpiring is how soon a cron counts as attention.
const DefaultExpiring = 48 * time.Hour

// PlanItem kinds.
const (
	PlanKindRemote   = "remote"
	PlanKindTab      = "tab"
	PlanKindAgentbus = "agentbus"
	PlanKindWhen     = "when"
	PlanKindCwd      = "cwd"
	PlanKindModel    = "model"
	PlanKindCron     = "cron"
	PlanKindBrief    = "brief"
	PlanKindRunner   = "runner"
	PlanKindPolicy   = "policy"
	PlanKindAgentsMD = "agents-md"
	PlanKindUserStep = "user-step"
)

// PlanItem statuses.
const (
	PlanStatusOK           = "ok"
	PlanStatusMissing      = "missing"
	PlanStatusExpiring     = "expiring"
	PlanStatusUnlisted     = "unlisted"
	PlanStatusStale        = "stale"
	PlanStatusUnverifiable = "unverifiable"
	PlanStatusChanged      = "changed"
	PlanStatusEdited       = "edited"
	PlanStatusUnknown      = "unknown"
	PlanStatusUnmanaged    = "unmanaged"
	PlanStatusUserStep     = "user-step"
)

var validPlanKinds = []string{
	PlanKindRemote,
	PlanKindTab,
	PlanKindAgentbus,
	PlanKindWhen,
	PlanKindCwd,
	PlanKindModel,
	PlanKindCron,
	PlanKindBrief,
	PlanKindRunner,
	PlanKindPolicy,
	PlanKindAgentsMD,
	PlanKindUserStep,
}

// ValidPlanKinds returns the supported --only filter kinds in deterministic order.
func ValidPlanKinds() []string {
	return slices.Clone(validPlanKinds)
}

// ValidatePlanKinds checks that every kind in only is a known plan kind.
func ValidatePlanKinds(only []string) error {
	for _, kind := range only {
		if !slices.Contains(validPlanKinds, kind) {
			return fmt.Errorf("unknown --only kind %q (valid: %s)", kind, strings.Join(validPlanKinds, ", "))
		}
	}
	return nil
}

// PlanItem is one structured item produced by plan.
type PlanItem struct {
	Role     string     `json:"role,omitempty"`
	Machine  string     `json:"machine,omitempty"`
	Kind     string     `json:"kind"`
	ID       string     `json:"id,omitempty"`
	Status   string     `json:"status"`
	Schedule string     `json:"schedule,omitempty"`
	Expiry   *time.Time `json:"expiry,omitempty"`
	Reason   string     `json:"reason,omitempty"`

	line    string
	changed bool
}

// Live is the snapshot plan compares with a spec.
// A nil KnownModels list skips the unknown-model check.
type Live struct {
	Local          string
	HerdrNotRead   bool
	NamesNotRead   bool
	Tabs           []LiveTab
	Agents         []LiveAgent
	Names          []LiveName
	Crons          []LiveCron
	CronRead       []string
	PublishedCrons map[string]PublishedCrons
	Briefs         map[string]string
	AgentsMD       map[string]string
	KnownModels    []string
	// MissingCwd lists expanded role directories that were not found.
	MissingCwd []string
	// GOOS picks the runner unit path. See [RunnerUnit].
	GOOS string
	// Found records whether a runner unit or the policy file exists,
	// keyed by expanded path. A path that was not checked is absent.
	Found map[string]bool
	Now   time.Time
}

// LiveTab is a tab label in a workspace.
// Panes lists the herdr pane ids in the tab.
type LiveTab struct {
	Workspace string
	Label     string
	Panes     []string
}

// LiveAgent is one running agent.
// Pane is the herdr pane id. Import joins an agentbus name to it.
type LiveAgent struct {
	Name      string
	Cwd       string
	Model     string
	SessionID string
	Kind      string
	Workspace string
	Tab       string
	Machine   string
	Pane      string
}

// LiveName is one agentbus name.
// SessionID and Pane come from a herdr:<server>/<session>/<pane> handle.
// Liveness is the agentbus value, such as alive, unreachable, or gone.
type LiveName struct {
	Name      string
	When      string
	Server    string
	SessionID string
	Liveness  string
	Pane      string
}

// LiveCron is one durable job read from a role cwd.
type LiveCron struct {
	Cwd                string
	ID                 string
	Cron               string
	Prompt             string
	ExpiresAt          time.Time
	CreatedBySessionID string
}

// PlanOptions filters plan and sets the expiring and stale thresholds.
// A non-positive Expiring uses DefaultExpiring.
// A non-positive CronStale uses DefaultCronStale.
type PlanOptions struct {
	Machine   string
	Role      string
	Expiring  time.Duration
	Only      []string
	CronStale time.Duration
}

// Plan diffs spec against live. Diff lines come first, then USER STEP lines.
// The command that prints this never writes.
func Plan(spec Spec, live Live, opts PlanOptions) []string {
	return PlanLines(PlanItems(spec, live, opts))
}

// PlanItems diffs spec against live and returns structured items, filtered by opts.
func PlanItems(spec Spec, live Live, opts PlanOptions) []PlanItem {
	if opts.Expiring <= 0 {
		opts.Expiring = DefaultExpiring
	}
	if opts.CronStale <= 0 {
		opts.CronStale = DefaultCronStale
	}
	if live.Now.IsZero() {
		live.Now = time.Now().UTC()
	}
	names := selectedRoles(spec, opts)
	var items []PlanItem
	var steps []PlanItem
	for _, name := range names {
		role := spec.Roles[name]
		items = append(items, roleItems(name, role, live, opts.Expiring, opts.CronStale)...)
		steps = append(steps, userStepItems(name, role)...)
	}
	items = append(items, jobsItems(spec.Jobs, live, opts)...)
	if opts.Role == "" {
		items = append(items, agentsMDItems(live)...)
	}
	items = append(items, steps...)
	if len(opts.Only) == 0 {
		return items
	}
	only := make(map[string]bool, len(opts.Only))
	for _, k := range opts.Only {
		only[k] = true
	}
	filtered := make([]PlanItem, 0, len(items))
	for _, it := range items {
		if only[it.Kind] {
			filtered = append(filtered, it)
		}
	}
	return filtered
}

// PlanLines extracts the human-readable output lines from PlanItems.
func PlanLines(items []PlanItem) []string {
	var lines []string
	for _, it := range items {
		if it.line != "" {
			lines = append(lines, it.line)
		}
	}
	return lines
}

// PlanItemsChanged reports whether any item represents a diff or unverifiable state.
// Statuses ok, unmanaged, and user-step do not count as changes.
func PlanItemsChanged(items []PlanItem) bool {
	for _, it := range items {
		if it.changed {
			return true
		}
	}
	return false
}

func agentsMDItems(live Live) []PlanItem {
	var items []PlanItem
	for _, path := range mapKeys(live.AgentsMD) {
		switch live.AgentsMD[path] {
		case BriefEdited:
			items = append(items, PlanItem{
				Machine: live.Local, Kind: PlanKindAgentsMD, ID: path,
				Status: PlanStatusEdited, Reason: "edited in place",
				line: "~ agents-md " + path + " edited in place", changed: true,
			})
		case BriefMissing:
			items = append(items, PlanItem{
				Machine: live.Local, Kind: PlanKindAgentsMD, ID: path,
				Status: PlanStatusMissing, Reason: "missing",
				line: "+ agents-md " + path, changed: true,
			})
		case BriefOK:
			items = append(items, PlanItem{
				Machine: live.Local, Kind: PlanKindAgentsMD, ID: path,
				Status: PlanStatusOK,
			})
		}
	}
	return items
}

// RunnerUnit is the unit file a runner is installed as: a launchd agent
// on darwin and a systemd user unit elsewhere.
func RunnerUnit(name, goos string) string {
	if goos == "darwin" {
		return "~/Library/LaunchAgents/" + name + ".plist"
	}
	return "~/.config/systemd/user/" + name + ".service"
}

// jobsItems reports missing runner units, then a missing policy file.
// The policy is checked only when a selected runner is on this machine.
// A --role filter selects no runners.
func jobsItems(jobs *Jobs, live Live, opts PlanOptions) []PlanItem {
	if jobs == nil || opts.Role != "" {
		return nil
	}
	var items []PlanItem
	local := false
	for _, name := range mapKeys(jobs.Runners) {
		runner := jobs.Runners[name]
		if opts.Machine != "" && runner.Machine != opts.Machine {
			continue
		}
		if runner.Machine != live.Local {
			items = append(items, PlanItem{
				Machine: runner.Machine, Kind: PlanKindRunner, ID: name,
				Status: PlanStatusUnverifiable, Reason: "remote: not read",
				line: "? runner " + name + " remote: not read", changed: true,
			})
			continue
		}
		local = true
		unit := RunnerUnit(name, live.GOOS)
		items = append(items, fileItem(PlanKindRunner, name, "runner "+name+" unit", unit, live))
	}
	if local {
		items = append(items, fileItem(PlanKindPolicy, jobs.Policy, "jobs policy", jobs.Policy, live))
	}
	return items
}

func fileItem(kind, id, label, path string, live Live) PlanItem {
	found, checked := live.Found[expandOrKeep(path)]
	switch {
	case !checked:
		return PlanItem{
			Machine: live.Local, Kind: kind, ID: id,
			Status: PlanStatusUnverifiable, Reason: "not checked",
			line: "? " + label + " not checked", changed: true,
		}
	case !found:
		return PlanItem{
			Machine: live.Local, Kind: kind, ID: id,
			Status: PlanStatusMissing, Reason: "missing " + path,
			line: "+ " + label + " " + path, changed: true,
		}
	default:
		return PlanItem{
			Machine: live.Local, Kind: kind, ID: id,
			Status: PlanStatusOK,
		}
	}
}

// expandOrKeep expands a leading ~. A home lookup error keeps path.
func expandOrKeep(path string) string {
	expanded, err := ExpandPath(path)
	if err != nil {
		return path
	}
	return expanded
}

// PlanChanged reports whether lines contain a diff marker.
// USER STEP lines are not changes.
func PlanChanged(lines []string) bool {
	for _, line := range lines {
		if line == "" || strings.HasPrefix(line, shellTabPrefix) {
			continue
		}
		switch line[0] {
		case '+', '~', '!', '?':
			return true
		}
	}
	return false
}

func selectedRoles(spec Spec, opts PlanOptions) []string {
	names := make([]string, 0, len(spec.Roles))
	for name, role := range spec.Roles {
		if opts.Role != "" && name != opts.Role {
			continue
		}
		if opts.Machine != "" && role.Machine != opts.Machine {
			continue
		}
		names = append(names, name)
	}
	slices.Sort(names)
	return names
}

func roleItems(name string, role Role, live Live, window, staleThreshold time.Duration) []PlanItem {
	if role.Machine != live.Local {
		return []PlanItem{{
			Role: name, Machine: role.Machine, Kind: PlanKindRemote,
			Status: PlanStatusUnverifiable, Reason: "remote: not read",
			line: "? " + name + " remote: not read", changed: true,
		}}
	}
	var items []PlanItem
	items = append(items, presenceItems(name, role, live)...)
	if cwdMissing(role, live) {
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindCwd,
			Status: PlanStatusUnverifiable, Reason: "cwd not found",
			line: "? " + name + " cwd not found", changed: true,
		})
	} else {
		items = append(items, cronItems(name, role, live, window, staleThreshold)...)
	}
	switch live.Briefs[name] {
	case BriefEdited:
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindBrief, ID: role.Brief.File,
			Status: PlanStatusEdited, Reason: "brief edited in place",
			line: "! " + name + " brief edited in place", changed: true,
		})
	case BriefUnmanaged:
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindBrief, ID: role.Brief.File,
			Status: PlanStatusUnmanaged, Reason: "brief unmanaged",
			line: "i " + name + " brief unmanaged", changed: false,
		})
	case BriefMissing:
		if role.Brief.File != "" {
			items = append(items, PlanItem{
				Role: name, Machine: role.Machine, Kind: PlanKindBrief, ID: role.Brief.File,
				Status: PlanStatusMissing, Reason: "brief missing",
				line: "+ " + name + " brief missing", changed: true,
			})
		}
	case BriefOK:
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindBrief, ID: role.Brief.File,
			Status: PlanStatusOK,
		})
	}
	if modelUnknown(role, live) {
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindModel, ID: role.Model,
			Status: PlanStatusUnknown, Reason: fmt.Sprintf("model %q unknown", role.Model),
			line: "! " + name + " model unknown", changed: true,
		})
	}
	return items
}

func presenceItems(name string, role Role, live Live) []PlanItem {
	var items []PlanItem
	items = append(items, tabItem(name, role, live))
	items = append(items, agentbusItems(name, role, live)...)
	if live.HerdrNotRead {
		return items
	}
	agent, ok := liveAgent(live, name)
	if !ok {
		return items
	}
	return append(items, agentRuntimeItems(name, role, agent)...)
}

func tabItem(name string, role Role, live Live) PlanItem {
	tabID := role.Workspace + "/" + role.Tab
	switch {
	case live.HerdrNotRead:
		return PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindTab, ID: tabID,
			Status: PlanStatusUnverifiable, Reason: "herdr: not read",
			line: "? " + name + " herdr: not read", changed: true,
		}
	case !tabPresent(live, role):
		return PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindTab, ID: tabID,
			Status: PlanStatusMissing, Reason: "tab " + tabID + " missing",
			line: "+ " + name + " tab " + tabID, changed: true,
		}
	default:
		return PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindTab, ID: tabID,
			Status: PlanStatusOK,
		}
	}
}

func agentbusItems(name string, role Role, live Live) []PlanItem {
	if live.NamesNotRead {
		return []PlanItem{{
			Role: name, Machine: role.Machine, Kind: PlanKindAgentbus,
			Status: PlanStatusUnverifiable, Reason: "agentbus: not read",
			line: "? " + name + " agentbus: not read", changed: true,
		}}
	}
	when, ok := liveWhen(live, name)
	if !ok {
		return []PlanItem{{
			Role: name, Machine: role.Machine, Kind: PlanKindAgentbus,
			Status: PlanStatusMissing, Reason: "agentbus name not registered",
			line: "+ " + name + " agentbus", changed: true,
		}}
	}
	items := []PlanItem{{
		Role: name, Machine: role.Machine, Kind: PlanKindAgentbus,
		Status: PlanStatusOK,
	}}
	if when != role.When {
		return append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindWhen,
			Status: PlanStatusChanged, Reason: fmt.Sprintf("when %q != spec %q", when, role.When),
			line: "~ " + name + " when", changed: true,
		})
	}
	return append(items, PlanItem{
		Role: name, Machine: role.Machine, Kind: PlanKindWhen,
		Status: PlanStatusOK,
	})
}

func agentRuntimeItems(name string, role Role, agent LiveAgent) []PlanItem {
	var items []PlanItem
	if agent.Cwd != "" {
		if agent.Cwd != roleDir(role) {
			items = append(items, PlanItem{
				Role: name, Machine: role.Machine, Kind: PlanKindCwd,
				Status: PlanStatusChanged, Reason: fmt.Sprintf("cwd %q != spec %q", agent.Cwd, roleDir(role)),
				line: "~ " + name + " cwd", changed: true,
			})
		} else {
			items = append(items, PlanItem{
				Role: name, Machine: role.Machine, Kind: PlanKindCwd,
				Status: PlanStatusOK,
			})
		}
	}
	if agent.Model != "" && role.Model != "" {
		if agent.Model != role.Model {
			items = append(items, PlanItem{
				Role: name, Machine: role.Machine, Kind: PlanKindModel, ID: role.Model,
				Status: PlanStatusChanged, Reason: fmt.Sprintf("model %q != spec %q", agent.Model, role.Model),
				line: "~ " + name + " model", changed: true,
			})
		} else {
			items = append(items, PlanItem{
				Role: name, Machine: role.Machine, Kind: PlanKindModel, ID: role.Model,
				Status: PlanStatusOK,
			})
		}
	}
	return items
}

func cronItems(name string, role Role, live Live, window, staleThreshold time.Duration) []PlanItem {
	if pub, ok := live.PublishedCrons[name]; ok {
		return publishedCronItems(name, role, pub, live.Now, window, staleThreshold)
	}
	var items []PlanItem
	dir := roleDir(role)
	read := cronWasRead(live, dir)
	for _, schedule := range role.Schedules {
		items = append(items, diskCronItem(name, role.Machine, dir, schedule, live, read, window))
	}
	return items
}

func publishedCronItems(name string, role Role, pub PublishedCrons, now time.Time, window, staleThreshold time.Duration) []PlanItem {
	var items []PlanItem
	if pub.ReadError != "" {
		if len(role.Schedules) == 0 {
			return []PlanItem{{
				Role: name, Machine: role.Machine, Kind: PlanKindCron,
				Status: PlanStatusUnverifiable, Reason: pub.ReadError,
				line: "? " + name + " cron unverifiable", changed: true,
			}}
		}
		for _, s := range role.Schedules {
			items = append(items, PlanItem{
				Role: name, Machine: role.Machine, Kind: PlanKindCron,
				ID: s.ID, Schedule: s.Cron,
				Status: PlanStatusUnverifiable, Reason: pub.ReadError,
				line: "? " + name + " cron " + s.ID + " unverifiable", changed: true,
			})
		}
		return items
	}
	if !pub.PublishedAt.IsZero() && now.Sub(pub.PublishedAt) > staleThreshold {
		age := now.Sub(pub.PublishedAt).Round(time.Second)
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindCron,
			Status: PlanStatusStale,
			Reason: fmt.Sprintf("published crons are %s old (threshold %s)", age, staleThreshold),
			line:   "! " + name + " cron stale", changed: true,
		})
	}

	matchedPub, matchedSpec := matchPublishedCrons(role.Schedules, pub.Crons)
	for i, s := range role.Schedules {
		j := matchedSpec[i]
		if j < 0 {
			items = append(items, PlanItem{
				Role: name, Machine: role.Machine, Kind: PlanKindCron,
				ID: s.ID, Schedule: s.Cron,
				Status: PlanStatusMissing, Reason: "not in published crons",
				line: "+ " + name + " cron " + s.ID, changed: true,
			})
			continue
		}
		pc := pub.Crons[j]
		exp := expiryPtr(pc.Expiry)
		if expiringSoon(pc.Expiry, now, window) {
			items = append(items, PlanItem{
				Role: name, Machine: role.Machine, Kind: PlanKindCron,
				ID: s.ID, Schedule: s.Cron, Expiry: exp,
				Status: PlanStatusExpiring, Reason: "within expiring window",
				line: "! " + name + " cron " + s.ID + " expiring", changed: true,
			})
			continue
		}
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindCron,
			ID: s.ID, Schedule: s.Cron, Expiry: exp,
			Status: PlanStatusOK,
		})
	}
	for j, pc := range pub.Crons {
		if matchedPub[j] {
			continue
		}
		label := pc.ID
		if label == "" {
			label = pc.Schedule
		}
		if label == "" {
			label = "(unnamed)"
		}
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindCron,
			ID: pc.ID, Schedule: pc.Schedule, Expiry: expiryPtr(pc.Expiry),
			Status: PlanStatusUnlisted, Reason: "published but not in spec",
			line: "! " + name + " cron " + label + " unlisted", changed: true,
		})
	}
	return items
}

func matchPublishedCrons(schedules []Schedule, published []PublishedCron) ([]bool, []int) {
	matchedPub := make([]bool, len(published))
	matchedSpec := make([]int, len(schedules))
	for i := range matchedSpec {
		matchedSpec[i] = -1
	}
	for i, s := range schedules {
		if j := findPublishedByID(s, published, matchedPub); j >= 0 {
			matchedPub[j] = true
			matchedSpec[i] = j
		}
	}
	for i, s := range schedules {
		if matchedSpec[i] >= 0 || s.Cron == "" {
			continue
		}
		if j := findPublishedByExpr(s, published, matchedPub); j >= 0 {
			matchedPub[j] = true
			matchedSpec[i] = j
		}
	}
	return matchedPub, matchedSpec
}

func findPublishedByID(s Schedule, published []PublishedCron, used []bool) int {
	for j, pc := range published {
		if !used[j] && pc.ID == s.ID && (s.Cron == "" || pc.Schedule == s.Cron) {
			return j
		}
	}
	return -1
}

func findPublishedByExpr(s Schedule, published []PublishedCron, used []bool) int {
	var candidates []int
	for j, pc := range published {
		if !used[j] && pc.Schedule == s.Cron {
			candidates = append(candidates, j)
		}
	}
	if len(candidates) == 0 {
		return -1
	}
	if len(candidates) > 1 && s.Prompt != "" {
		want := promptPrefix(s.Prompt)
		for _, j := range candidates {
			if published[j].Prompt != "" && promptPrefix(published[j].Prompt) == want {
				return j
			}
		}
	}
	return candidates[0]
}

func diskCronItem(name, machine, cwd string, schedule Schedule, live Live, read bool, window time.Duration) PlanItem {
	owner := nameSession(live, name)
	if !read || owner == "" {
		reason := "cron state not read"
		if read && owner == "" {
			reason = "no live agentbus session"
		}
		return PlanItem{
			Role: name, Machine: machine, Kind: PlanKindCron,
			ID: schedule.ID, Schedule: schedule.Cron,
			Status: PlanStatusUnverifiable, Reason: reason,
			line: "? " + name + " cron " + schedule.ID + " unverifiable", changed: true,
		}
	}
	job, hit := matchCron(live, cwd, schedule, owner)
	switch hit {
	case cronOwned:
		exp := expiryPtr(job.ExpiresAt)
		if expiringSoon(job.ExpiresAt, live.Now, window) {
			return PlanItem{
				Role: name, Machine: machine, Kind: PlanKindCron,
				ID: schedule.ID, Schedule: schedule.Cron, Expiry: exp,
				Status: PlanStatusExpiring, Reason: "within expiring window",
				line: "! " + name + " cron " + schedule.ID + " expiring", changed: true,
			}
		}
		return PlanItem{
			Role: name, Machine: machine, Kind: PlanKindCron,
			ID: schedule.ID, Schedule: schedule.Cron, Expiry: exp,
			Status: PlanStatusOK,
		}
	case cronOther:
		return PlanItem{
			Role: name, Machine: machine, Kind: PlanKindCron,
			ID: schedule.ID, Schedule: schedule.Cron, Expiry: expiryPtr(job.ExpiresAt),
			Status: PlanStatusOK, Reason: "owned by another session",
		}
	default:
		if schedule.Durable {
			return PlanItem{
				Role: name, Machine: machine, Kind: PlanKindCron,
				ID: schedule.ID, Schedule: schedule.Cron,
				Status: PlanStatusMissing, Reason: "durable cron missing",
				line: "+ " + name + " cron " + schedule.ID, changed: true,
			}
		}
		return PlanItem{
			Role: name, Machine: machine, Kind: PlanKindCron,
			ID: schedule.ID, Schedule: schedule.Cron,
			Status: PlanStatusUnverifiable, Reason: "session-only cron not published",
			line: "? " + name + " cron " + schedule.ID + " unverifiable", changed: true,
		}
	}
}

func expiryPtr(t time.Time) *time.Time {
	if t.IsZero() {
		return nil
	}
	u := t.UTC()
	return &u
}

func userStepItems(name string, role Role) []PlanItem {
	var items []PlanItem
	for _, item := range role.Needs.Permissions {
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindUserStep, ID: item,
			Status: PlanStatusUserStep, Reason: "permission " + item,
			line: "USER STEP: " + name + " permission " + item,
		})
	}
	for _, item := range role.Needs.Secrets {
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindUserStep, ID: item,
			Status: PlanStatusUserStep, Reason: "secret " + item,
			line: "USER STEP: " + name + " secret " + item,
		})
	}
	for _, item := range role.Needs.Services {
		items = append(items, PlanItem{
			Role: name, Machine: role.Machine, Kind: PlanKindUserStep, ID: item,
			Status: PlanStatusUserStep, Reason: "service " + item,
			line: "USER STEP: " + name + " service " + item,
		})
	}
	return items
}

func modelUnknown(role Role, live Live) bool {
	if role.Model == "" || live.KnownModels == nil {
		return false
	}
	for _, model := range live.KnownModels {
		if model == role.Model {
			return false
		}
	}
	return true
}

func tabPresent(live Live, role Role) bool {
	for _, tab := range live.Tabs {
		if tab.Workspace == role.Workspace && tab.Label == role.Tab {
			return true
		}
	}
	return false
}

func liveWhen(live Live, name string) (string, bool) {
	for _, item := range live.Names {
		if item.Name == name {
			return item.When, true
		}
	}
	return "", false
}

func liveAgent(live Live, name string) (LiveAgent, bool) {
	for _, agent := range live.Agents {
		if agent.Name == name {
			return agent, true
		}
	}
	return LiveAgent{}, false
}

// cronPromptPrefix is the prompt length stored on a live job.
const cronPromptPrefix = 80

type cronHit int

const (
	cronNone cronHit = iota
	cronOwned
	cronOther
)

// matchCron pairs a spec schedule with a live job by owner session and
// cron expression. The spec id is not compared to the live id. Two jobs
// with the same cron are split by the prompt prefix.
func matchCron(live Live, cwd string, schedule Schedule, owner string) (LiveCron, cronHit) {
	same := cronsWithExpr(live, cwd, schedule.Cron)
	picked, ok := pickCron(same, schedule.Prompt)
	if !ok {
		return LiveCron{}, cronNone
	}
	if owner != "" && picked.CreatedBySessionID == owner {
		return picked, cronOwned
	}
	if picked.CreatedBySessionID != "" && picked.CreatedBySessionID != owner {
		return picked, cronOther
	}
	return LiveCron{}, cronNone
}

func cronsWithExpr(live Live, cwd, expr string) []LiveCron {
	if expr == "" {
		return nil
	}
	var out []LiveCron
	seen := map[string]bool{}
	for _, cron := range live.Crons {
		if cron.Cwd != cwd || cron.Cron != expr || seen[cronKey(cron)] {
			continue
		}
		seen[cronKey(cron)] = true
		out = append(out, cron)
	}
	return out
}

func pickCron(jobs []LiveCron, prompt string) (LiveCron, bool) {
	if len(jobs) == 0 {
		return LiveCron{}, false
	}
	if len(jobs) == 1 {
		return jobs[0], true
	}
	want := promptPrefix(prompt)
	var found LiveCron
	n := 0
	for _, job := range jobs {
		if job.Prompt == want {
			found = job
			n++
		}
	}
	if n != 1 {
		return LiveCron{}, false
	}
	return found, true
}

func promptPrefix(s string) string {
	r := []rune(s)
	if len(r) <= cronPromptPrefix {
		return s
	}
	return string(r[:cronPromptPrefix])
}

// nameSession is the session in the role's agentbus handle.
// Plan and drift both use it for job ownership.
func nameSession(live Live, name string) string {
	for _, item := range live.Names {
		if item.Name == name {
			return item.SessionID
		}
	}
	return ""
}

func cwdMissing(role Role, live Live) bool {
	dir := roleDir(role)
	for _, item := range live.MissingCwd {
		if item == dir {
			return true
		}
	}
	return false
}

func cronWasRead(live Live, cwd string) bool {
	for _, item := range live.CronRead {
		if item == cwd {
			return true
		}
	}
	return false
}

func expiringSoon(at, now time.Time, window time.Duration) bool {
	if at.IsZero() {
		return false
	}
	return !at.After(now.Add(window))
}
