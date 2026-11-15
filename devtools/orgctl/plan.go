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
	Now          time.Time
}

// LiveTab is a tab label in a workspace.
type LiveTab struct {
	Workspace string
	Label     string
}

// LiveAgent is one running agent.
type LiveAgent struct {
	Name      string
	Cwd       string
	Model     string
	SessionID string
	Kind      string
	Workspace string
	Tab       string
	Machine   string
}

// LiveName is one agentbus name.
type LiveName struct {
	Name   string
	When   string
	Server string
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
	lines = append(lines, cronLines(name, role, live, window)...)
	if live.Briefs[name] == BriefEdited {
		lines = append(lines, "! "+name+" brief edited in place")
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
	if agent.Cwd != "" && agent.Cwd != role.Cwd {
		lines = append(lines, "~ "+name+" cwd")
	}
	if agent.Model != "" && role.Model != "" && agent.Model != role.Model {
		lines = append(lines, "~ "+name+" model")
	}
	return lines
}

func cronLines(name string, role Role, live Live, window time.Duration) []string {
	var lines []string
	read := cronWasRead(live, role.Cwd)
	for _, schedule := range role.Schedules {
		if line := cronLine(name, role.Cwd, schedule, live, read, window); line != "" {
			lines = append(lines, line)
		}
	}
	return lines
}

func cronLine(name, cwd string, schedule Schedule, live Live, read bool, window time.Duration) string {
	if !read {
		return "? " + name + " cron " + schedule.ID + " unverifiable"
	}
	job, found := findCron(live, cwd, schedule.ID)
	if !found {
		if schedule.Durable {
			return "+ " + name + " cron " + schedule.ID
		}
		return "? " + name + " cron " + schedule.ID + " unverifiable"
	}
	if expiringSoon(job.ExpiresAt, live.Now, window) {
		return "! " + name + " cron " + schedule.ID + " expiring"
	}
	return ""
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

func findCron(live Live, cwd, id string) (LiveCron, bool) {
	for _, cron := range live.Crons {
		if cron.Cwd == cwd && cron.ID == id {
			return cron, true
		}
	}
	return LiveCron{}, false
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
