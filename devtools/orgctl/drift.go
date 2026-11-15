package orgctl

import (
	"path/filepath"
	"slices"
	"strings"
)

// Drift reports live tabs, names, and crons that the spec does not declare.
// It also reports a cron created by another role's session, a when mismatch,
// and a brief edited in place. Each finding is followed by a suggestion.
// Nothing is written.
func Drift(spec Spec, live Live) []string {
	var lines []string
	lines = append(lines, extraTabs(spec, live)...)
	lines = append(lines, extraNames(spec, live)...)
	lines = append(lines, extraCrons(spec, live)...)
	lines = append(lines, misplacedCrons(spec, live)...)
	lines = append(lines, whenDrift(spec, live)...)
	lines = append(lines, briefDrift(spec, live)...)
	return lines
}

func extraTabs(spec Spec, live Live) []string {
	if live.HerdrNotRead {
		return nil
	}
	tabs := make([]LiveTab, 0, len(live.Tabs))
	for _, tab := range live.Tabs {
		if roleOwnsTab(spec, tab) || poolTab(tab, spec, live) {
			continue
		}
		tabs = append(tabs, tab)
	}
	slices.SortFunc(tabs, func(a, b LiveTab) int {
		if a.Workspace != b.Workspace {
			return strings.Compare(a.Workspace, b.Workspace)
		}
		return strings.Compare(a.Label, b.Label)
	})
	var lines []string
	for _, tab := range tabs {
		lines = append(lines, "+ tab "+tab.Workspace+"/"+tab.Label)
		lines = append(lines, "USER: close tab "+tab.Label+" in "+tab.Workspace)
	}
	return lines
}

func extraNames(spec Spec, live Live) []string {
	if live.NamesNotRead {
		return nil
	}
	var names, gone []string
	for _, item := range live.Names {
		if item.Liveness == "gone" {
			gone = append(gone, item.Name)
			continue
		}
		if _, ok := spec.Roles[item.Name]; ok || matchesPool(item.Name, spec, live) {
			continue
		}
		names = append(names, item.Name)
	}
	slices.Sort(names)
	slices.Sort(gone)
	var lines []string
	for _, name := range names {
		lines = append(lines, "+ name "+name)
		lines = append(lines, nameStub(name))
	}
	for _, name := range gone {
		lines = append(lines, "gone (stale registration): "+name)
		lines = append(lines, "USER: agentbus release "+name)
	}
	return lines
}

func extraCrons(spec Spec, live Live) []string {
	crons := make([]LiveCron, 0, len(live.Crons))
	seen := map[string]bool{}
	for _, cron := range live.Crons {
		key := cronKey(cron)
		if seen[key] || declaredCron(spec, cron) {
			continue
		}
		seen[key] = true
		crons = append(crons, cron)
	}
	slices.SortFunc(crons, func(a, b LiveCron) int {
		if a.Cwd != b.Cwd {
			return strings.Compare(a.Cwd, b.Cwd)
		}
		return strings.Compare(a.ID, b.ID)
	})
	var lines []string
	for _, cron := range crons {
		lines = append(lines, "+ cron "+cron.ID)
		lines = append(lines, cronStub(cron))
	}
	return lines
}

func misplacedCrons(spec Spec, live Live) []string {
	names := roleNames(spec)
	var lines []string
	seen := map[string]bool{}
	for _, name := range names {
		role := spec.Roles[name]
		owner := roleSession(live, name)
		for _, schedule := range role.Schedules {
			cron, hit := matchCron(live, roleDir(role), schedule, owner)
			if hit != cronOther || seen[cronKey(cron)] || jobOwned(spec, live, cron) {
				continue
			}
			seen[cronKey(cron)] = true
			target := sessionRole(spec, live, cron.CreatedBySessionID)
			if target == "" {
				target = name
			}
			lines = append(lines, "! cron "+schedule.ID+" misplaced: move to role "+target)
			lines = append(lines, "USER: recreate cron in role "+target)
		}
	}
	return lines
}

func cronKey(cron LiveCron) string {
	return cron.Cwd + "\x00" + cron.ID
}

func jobOwned(spec Spec, live Live, job LiveCron) bool {
	for _, name := range roleNames(spec) {
		role := spec.Roles[name]
		if roleDir(role) != job.Cwd {
			continue
		}
		owner := roleSession(live, name)
		for _, schedule := range role.Schedules {
			got, hit := matchCron(live, job.Cwd, schedule, owner)
			if hit == cronOwned && got.Cwd == job.Cwd && got.ID == job.ID {
				return true
			}
		}
	}
	return false
}

func whenDrift(spec Spec, live Live) []string {
	if live.NamesNotRead {
		return nil
	}
	var lines []string
	for _, name := range roleNames(spec) {
		when, ok := liveWhen(live, name)
		if !ok || when == spec.Roles[name].When {
			continue
		}
		lines = append(lines, "~ "+name+" when")
		lines = append(lines, whenStub(when))
	}
	return lines
}

func briefDrift(spec Spec, live Live) []string {
	var lines []string
	for _, name := range roleNames(spec) {
		if live.Briefs[name] != BriefEdited {
			continue
		}
		lines = append(lines, "! "+name+" brief edited in place")
		lines = append(lines, "USER: review brief for "+name)
	}
	return lines
}

func roleNames(spec Spec) []string {
	names := make([]string, 0, len(spec.Roles))
	for name := range spec.Roles {
		names = append(names, name)
	}
	slices.Sort(names)
	return names
}

func roleOwnsTab(spec Spec, tab LiveTab) bool {
	for _, role := range spec.Roles {
		if role.Workspace == tab.Workspace && role.Tab == tab.Label {
			return true
		}
	}
	return false
}

func declaredCron(spec Spec, cron LiveCron) bool {
	for _, role := range spec.Roles {
		if roleDir(role) != cron.Cwd {
			continue
		}
		for _, schedule := range role.Schedules {
			if schedule.Cron != "" && schedule.Cron == cron.Cron {
				return true
			}
		}
	}
	return false
}

func roleSession(live Live, name string) string {
	agent, ok := liveAgent(live, name)
	if !ok {
		return ""
	}
	return agent.SessionID
}

func sessionRole(spec Spec, live Live, session string) string {
	if session == "" {
		return ""
	}
	for _, name := range roleNames(spec) {
		if roleSession(live, name) == session {
			return name
		}
	}
	return ""
}

func matchesPool(name string, spec Spec, live Live) bool {
	for key, pool := range spec.Pools {
		if pool.NamePattern == "" && pool.TabPattern == "" {
			if name == key || strings.HasPrefix(name, key+"-") || strings.HasPrefix(name, key+"/") || agentInWorkspace(live, name, pool.Workspace) {
				return true
			}
			continue
		}
		if pool.NamePattern == "" || !globMatch(pool.NamePattern, name) {
			continue
		}
		if pool.TabPattern == "" || nameTabMatches(live, name, pool) {
			return true
		}
	}
	return false
}

func poolTab(tab LiveTab, spec Spec, live Live) bool {
	for _, pool := range spec.Pools {
		if tab.Workspace != pool.Workspace {
			continue
		}
		if pool.NamePattern == "" && pool.TabPattern == "" {
			return true
		}
		if pool.TabPattern == "" || !globMatch(pool.TabPattern, tab.Label) {
			continue
		}
		if pool.NamePattern != "" && !tabHasName(live, tab, pool.NamePattern) {
			continue
		}
		return true
	}
	return false
}

func nameTabMatches(live Live, name string, pool Pool) bool {
	for _, agent := range live.Agents {
		if agent.Name == name && agent.Workspace == pool.Workspace && globMatch(pool.TabPattern, agent.Tab) {
			return true
		}
	}
	return false
}

func tabHasName(live Live, tab LiveTab, pattern string) bool {
	for _, agent := range live.Agents {
		if agent.Workspace == tab.Workspace && agent.Tab == tab.Label && globMatch(pattern, agent.Name) {
			return true
		}
	}
	return false
}

func agentInWorkspace(live Live, name, workspace string) bool {
	for _, agent := range live.Agents {
		if agent.Name == name && agent.Workspace == workspace {
			return true
		}
	}
	return false
}

func globMatch(pattern, value string) bool {
	ok, err := filepath.Match(pattern, value)
	return err == nil && ok
}

func nameStub(name string) string {
	return "module drift.stub\n\nname = " + pklQuote(name) + "\n"
}

func cronStub(cron LiveCron) string {
	return "module drift.stub\n\nid = " + pklQuote(cron.ID) +
		"\ncron = " + pklQuote(cron.Cron) +
		"\ncwd = " + pklQuote(cron.Cwd) + "\n"
}

func whenStub(when string) string {
	return "module drift.stub\n\n`when` = " + pklQuote(when) + "\n"
}

func pklQuote(s string) string {
	var b strings.Builder
	b.WriteByte('"')
	for _, r := range s {
		switch r {
		case '\\', '"':
			b.WriteByte('\\')
			b.WriteRune(r)
		case '\n':
			b.WriteString(`\n`)
		case '\r':
			b.WriteString(`\r`)
		default:
			b.WriteRune(r)
		}
	}
	b.WriteByte('"')
	return b.String()
}
