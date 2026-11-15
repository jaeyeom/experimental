package orgctl

import (
	"slices"
	"time"
)

// DefaultExpiring is how soon a cron counts as attention.
const DefaultExpiring = 48 * time.Hour

// Live is the snapshot plan compares with a spec.
// A nil KnownModels list skips the unknown-model check.
type Live struct {
	Local        string
	HerdrNotRead bool
	NamesNotRead bool
	Tabs         []LiveTab
	Agents       []LiveAgent
	Names        []LiveName
	Crons        []LiveCron
	CronRead     []string
	Briefs       map[string]string
	KnownModels  []string
	// MissingCwd lists expanded role directories that were not found.
	MissingCwd []string
	Now        time.Time
}

// LiveTab is a tab label in a workspace.
type LiveTab struct {
	Workspace string
	Label     string
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

// PlanOptions filters plan and sets the expiring window.
// A non-positive Expiring uses DefaultExpiring.
type PlanOptions struct {
	Machine  string
	Role     string
	Expiring time.Duration
}

// Plan diffs spec against live. Diff lines come first, then USER STEP lines.
// The command that prints this never writes.
func Plan(spec Spec, live Live, opts PlanOptions) []string {
	if opts.Expiring <= 0 {
		opts.Expiring = DefaultExpiring
	}
	names := selectedRoles(spec, opts)
	var lines []string
	var steps []string
	for _, name := range names {
		role := spec.Roles[name]
		lines = append(lines, roleDiff(name, role, live, opts.Expiring)...)
		steps = append(steps, userSteps(name, role)...)
	}
	return append(lines, steps...)
}

// PlanChanged reports whether lines contain a diff marker.
// USER STEP lines are not changes.
func PlanChanged(lines []string) bool {
	for _, line := range lines {
		if line == "" {
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

func roleDiff(name string, role Role, live Live, window time.Duration) []string {
	if role.Machine != live.Local {
		return []string{"? " + name + " remote: not read"}
	}
	var lines []string
	lines = append(lines, presenceLines(name, role, live)...)
	if cwdMissing(role, live) {
		lines = append(lines, "? "+name+" cwd not found")
	} else {
		lines = append(lines, cronLines(name, role, live, window)...)
	}
	switch live.Briefs[name] {
	case BriefEdited:
		lines = append(lines, "! "+name+" brief edited in place")
	case BriefUnmanaged:
		lines = append(lines, "i "+name+" brief unmanaged")
	case BriefMissing:
		if role.Brief.File != "" {
			lines = append(lines, "+ "+name+" brief missing")
		}
	}
	if modelUnknown(role, live) {
		lines = append(lines, "! "+name+" model unknown")
	}
	return lines
}

func presenceLines(name string, role Role, live Live) []string {
	var lines []string
	if live.HerdrNotRead {
		lines = append(lines, "? "+name+" herdr: not read")
	} else if !tabPresent(live, role) {
		lines = append(lines, "+ "+name+" tab "+role.Workspace+"/"+role.Tab)
	}
	if live.NamesNotRead {
		lines = append(lines, "? "+name+" agentbus: not read")
	} else if when, ok := liveWhen(live, name); !ok {
		lines = append(lines, "+ "+name+" agentbus")
	} else if when != role.When {
		lines = append(lines, "~ "+name+" when")
	}
	if live.HerdrNotRead {
		return lines
	}
	agent, ok := liveAgent(live, name)
	if !ok {
		return lines
	}
	if agent.Cwd != "" && agent.Cwd != roleDir(role) {
		lines = append(lines, "~ "+name+" cwd")
	}
	if agent.Model != "" && role.Model != "" && agent.Model != role.Model {
		lines = append(lines, "~ "+name+" model")
	}
	return lines
}

func cronLines(name string, role Role, live Live, window time.Duration) []string {
	var lines []string
	dir := roleDir(role)
	read := cronWasRead(live, dir)
	for _, schedule := range role.Schedules {
		if line := cronLine(name, dir, schedule, live, read, window); line != "" {
			lines = append(lines, line)
		}
	}
	return lines
}

func cronLine(name, cwd string, schedule Schedule, live Live, read bool, window time.Duration) string {
	owner := nameSession(live, name)
	if !read || owner == "" {
		return "? " + name + " cron " + schedule.ID + " unverifiable"
	}
	job, hit := matchCron(live, cwd, schedule, owner)
	switch hit {
	case cronOwned:
		if expiringSoon(job.ExpiresAt, live.Now, window) {
			return "! " + name + " cron " + schedule.ID + " expiring"
		}
		return ""
	case cronOther:
		return ""
	default:
		if schedule.Durable {
			return "+ " + name + " cron " + schedule.ID
		}
		return "? " + name + " cron " + schedule.ID + " unverifiable"
	}
}

func userSteps(name string, role Role) []string {
	var lines []string
	for _, item := range role.Needs.Permissions {
		lines = append(lines, "USER STEP: "+name+" permission "+item)
	}
	for _, item := range role.Needs.Secrets {
		lines = append(lines, "USER STEP: "+name+" secret "+item)
	}
	for _, item := range role.Needs.Services {
		lines = append(lines, "USER STEP: "+name+" service "+item)
	}
	return lines
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
