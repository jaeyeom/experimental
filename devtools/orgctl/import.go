package orgctl

import (
	"slices"
	"strings"
)

// Import renders a Pkl stub that amends amendsPath.
// reportsTo, decides, brief and needs are TODO comments.
// Pool candidates are comments. The stub does not write a file.
func Import(live Live, amendsPath string) string {
	roles := importRoles(live)
	var b strings.Builder
	b.WriteString("amends " + pklQuote(amendsPath) + "\n\n")
	b.WriteString("// TODO: org name\n")
	b.WriteString("org = \"TODO\"\n\n")
	writeMachines(&b, roles)
	b.WriteString("\nrules {}\n\n")
	writeImportRoles(&b, roles)
	b.WriteByte('\n')
	writePoolComments(&b, live)
	return b.String()
}

type importRole struct {
	name      string
	machine   string
	workspace string
	tab       string
	cwd       string
	when      string
	agent     string
	schedules []LiveCron
}

func importRoles(live Live) []importRole {
	agents := make(map[string]LiveAgent, len(live.Agents))
	for _, agent := range live.Agents {
		agents[agent.Name] = agent
	}
	seen := map[string]bool{}
	var roles []importRole
	for _, item := range live.Names {
		if item.Name == "" || seen[item.Name] {
			continue
		}
		seen[item.Name] = true
		agent := agents[item.Name]
		machine := agent.Machine
		if machine == "" {
			machine = item.Server
		}
		if machine == "" {
			machine = "local"
		}
		kind := agent.Kind
		if kind == "" {
			kind = "unknown"
		}
		workspace := agent.Workspace
		if workspace == "" {
			workspace = "TODO"
		}
		tab := agent.Tab
		if tab == "" {
			tab = "TODO"
		}
		cwd := agent.Cwd
		if cwd == "" {
			cwd = "TODO"
		}
		roles = append(roles, importRole{
			name: item.Name, machine: machine, workspace: workspace,
			tab: tab, cwd: cwd, when: item.When, agent: kind,
			schedules: cronsFor(live, cwd),
		})
	}
	slices.SortFunc(roles, func(a, b importRole) int {
		return strings.Compare(a.name, b.name)
	})
	return roles
}

func cronsFor(live Live, cwd string) []LiveCron {
	var jobs []LiveCron
	for _, cron := range live.Crons {
		if cron.Cwd == cwd && cron.ID != "" {
			jobs = append(jobs, cron)
		}
	}
	slices.SortFunc(jobs, func(a, b LiveCron) int {
		return strings.Compare(a.ID, b.ID)
	})
	return jobs
}

func writeMachines(b *strings.Builder, roles []importRole) {
	seen := map[string]bool{}
	var machines []string
	for _, role := range roles {
		if role.machine == "" || seen[role.machine] {
			continue
		}
		seen[role.machine] = true
		machines = append(machines, role.machine)
	}
	if len(machines) == 0 {
		machines = []string{"local"}
	}
	slices.Sort(machines)
	b.WriteString("machines {\n")
	for _, machine := range machines {
		b.WriteString("  [" + pklQuote(machine) + "] {\n")
		b.WriteString("    // TODO: host\n")
		b.WriteString("    host = " + pklQuote(machine) + "\n")
		b.WriteString("    herdrHandle = " + pklQuote(machine) + "\n")
		b.WriteString("    // TODO: timezone\n")
		b.WriteString("    timezone = \"UTC\"\n")
		b.WriteString("  }\n")
	}
	b.WriteString("}\n")
}

func writeImportRoles(b *strings.Builder, roles []importRole) {
	b.WriteString("roles {\n")
	for _, role := range roles {
		b.WriteString("  [" + pklQuote(role.name) + "] {\n")
		b.WriteString("    machine = " + pklQuote(role.machine) + "\n")
		b.WriteString("    workspace = " + pklQuote(role.workspace) + "\n")
		b.WriteString("    tab = " + pklQuote(role.tab) + "\n")
		b.WriteString("    cwd = " + pklQuote(role.cwd) + "\n")
		if role.when != "" {
			b.WriteString("    `when` = " + pklQuote(role.when) + "\n")
		}
		if role.agent == "unknown" {
			b.WriteString("    // TODO: agent\n")
		}
		b.WriteString("    agent = " + pklQuote(role.agent) + "\n")
		b.WriteString("    // TODO: reportsTo\n")
		b.WriteString("    // TODO: decides\n")
		b.WriteString("    // TODO: brief\n")
		b.WriteString("    // TODO: needs\n")
		if len(role.schedules) > 0 {
			b.WriteString("    schedules {\n")
			for _, schedule := range role.schedules {
				cron := schedule.Cron
				if cron == "" {
					cron = "* * * * *"
				}
				prompt := schedule.Prompt
				if prompt == "" {
					prompt = "TODO"
				}
				b.WriteString("      new {\n")
				b.WriteString("        id = " + pklQuote(schedule.ID) + "\n")
				b.WriteString("        cron = " + pklQuote(cron) + "\n")
				b.WriteString("        prompt = " + pklQuote(prompt) + "\n")
				b.WriteString("      }\n")
			}
			b.WriteString("    }\n")
		}
		b.WriteString("  }\n")
	}
	b.WriteString("}\n")
}

func writePoolComments(b *strings.Builder, live Live) {
	type candidate struct {
		workspace string
		prefix    string
		pattern   string
	}
	counts := map[candidate]int{}
	for _, tab := range live.Tabs {
		prefix, pattern, ok := tabPattern(tab.Label)
		if !ok || tab.Workspace == "" {
			continue
		}
		counts[candidate{tab.Workspace, prefix, pattern}]++
	}
	var found []candidate
	for item, n := range counts {
		if n >= 2 {
			found = append(found, item)
		}
	}
	slices.SortFunc(found, func(a, b candidate) int {
		if a.workspace != b.workspace {
			return strings.Compare(a.workspace, b.workspace)
		}
		return strings.Compare(a.pattern, b.pattern)
	})
	for _, item := range found {
		b.WriteString("// TODO: pool candidate " + pklQuote(item.prefix) +
			" in workspace " + pklQuote(item.workspace) +
			" tabPattern = " + pklQuote(item.pattern) + "\n")
	}
}

func tabPattern(label string) (prefix, pattern string, ok bool) {
	for _, sep := range []string{"-", "/"} {
		i := strings.Index(label, sep)
		if i <= 0 {
			continue
		}
		prefix = label[:i]
		return prefix, prefix + sep + "*", true
	}
	return "", "", false
}
