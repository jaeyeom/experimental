package orgctl

import (
	"fmt"
	"slices"
	"strings"
	"unicode"
)

// importPromptLimit is the longest prompt copied into the stub.
// A longer prompt is read from prompts/<id>.txt.
const importPromptLimit = 80

// placeholderCwd is the cwd written when a name has no directory.
// check accepts a path that starts with ~/.
const placeholderCwd = "~/TODO"

// RenderedImport is a Pkl stub plus prompt files it reads.
// Files are keyed by a path relative to the stub.
type RenderedImport struct {
	Stub  string
	Files map[string]string
}

// Import renders a Pkl stub that amends amendsPath.
// reportsTo, decides, brief and needs are TODO comments.
// Pool candidates are comments. The stub does not write a file.
func Import(live Live, amendsPath string) string {
	return RenderImport(live, amendsPath).Stub
}

// RenderImport renders the stub and any prompt files a long job needs.
func RenderImport(live Live, amendsPath string) RenderedImport {
	roles, gone := importRoles(live)
	var unowned []LiveCron
	files := map[string]string{}
	roles, unowned = attachJobs(roles, live.Crons, files)
	var b strings.Builder
	b.WriteString("amends " + pklQuote(amendsPath) + "\n\n")
	b.WriteString("// TODO: org name\n")
	b.WriteString("org = \"TODO\"\n\n")
	writeMachines(&b, roles)
	b.WriteString("\nrules {}\n\n")
	writeScope(&b, roles)
	writeImportRoles(&b, roles)
	b.WriteByte('\n')
	writePoolComments(&b, live)
	writeGone(&b, gone)
	writeUnowned(&b, unowned)
	return RenderedImport{Stub: b.String(), Files: files}
}

type importRole struct {
	name      string
	machine   string
	workspace string
	tab       string
	cwd       string
	when      string
	agent     string
	session   string
	schedules []importSchedule
}

type importSchedule struct {
	id     string
	cron   string
	prompt string
}

func importRoles(live Live) ([]importRole, []string) {
	byName := map[string]LiveAgent{}
	for _, agent := range live.Agents {
		if agent.Name != "" {
			if _, ok := byName[agent.Name]; !ok {
				byName[agent.Name] = agent
			}
		}
	}
	seen := map[string]bool{}
	var roles []importRole
	var gone []string
	for _, item := range live.Names {
		if item.Name == "" || seen[item.Name] {
			continue
		}
		seen[item.Name] = true
		if staleLiveness(item.Liveness) {
			gone = append(gone, item.Name)
			continue
		}
		agent, _ := agentFor(item, live, byName)
		roles = append(roles, roleFrom(item, agent))
	}
	slices.SortFunc(roles, func(a, b importRole) int {
		return strings.Compare(a.name, b.name)
	})
	slices.Sort(gone)
	uniquifyTabs(roles)
	return roles, gone
}

func agentFor(item LiveName, live Live, byName map[string]LiveAgent) (LiveAgent, bool) {
	if item.Pane != "" {
		return paneAgent(live, item.Pane)
	}
	agent, ok := byName[item.Name]
	return agent, ok
}

func roleFrom(item LiveName, agent LiveAgent) importRole {
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
		cwd = placeholderCwd
	}
	return importRole{
		name: item.Name, machine: machine, workspace: workspace,
		tab: tab, cwd: cwd, when: item.When, agent: kind, session: item.SessionID,
	}
}

func uniquifyTabs(roles []importRole) {
	seen := map[string]bool{}
	for i := range roles {
		key := roles[i].workspace + "\t" + roles[i].tab
		if !seen[key] {
			seen[key] = true
			continue
		}
		base := roles[i].tab
		if strings.HasPrefix(base, "TODO") {
			base = "TODO"
		}
		for n := 1; ; n++ {
			candidate := fmt.Sprintf("%s-%d", base, n)
			key = roles[i].workspace + "\t" + candidate
			if !seen[key] {
				roles[i].tab = candidate
				seen[key] = true
				break
			}
		}
	}
}

func attachJobs(roles []importRole, crons []LiveCron, files map[string]string) ([]importRole, []LiveCron) {
	bySession := map[string]int{}
	for i, role := range roles {
		if role.session != "" {
			if _, ok := bySession[role.session]; !ok {
				bySession[role.session] = i
			}
		}
	}
	owned := map[int][]LiveCron{}
	var unowned []LiveCron
	for _, cron := range crons {
		idx, ok := bySession[cron.CreatedBySessionID]
		if cron.CreatedBySessionID == "" || !ok {
			unowned = append(unowned, cron)
			continue
		}
		owned[idx] = append(owned[idx], cron)
	}
	for idx, jobs := range owned {
		sortCrons(jobs)
		roles[idx].schedules = slugJobs(jobs, files)
	}
	sortCrons(unowned)
	return roles, unowned
}

func sortCrons(jobs []LiveCron) {
	slices.SortFunc(jobs, func(a, b LiveCron) int {
		if a.Cron != b.Cron {
			return strings.Compare(a.Cron, b.Cron)
		}
		return strings.Compare(a.Prompt, b.Prompt)
	})
}

func slugJobs(jobs []LiveCron, files map[string]string) []importSchedule {
	used := map[string]bool{}
	out := make([]importSchedule, 0, len(jobs))
	for _, job := range jobs {
		id := uniqueSlug(promptSlug(job.Prompt), used)
		cron := job.Cron
		if cron == "" {
			cron = "* * * * *"
		}
		text := job.Prompt
		if text == "" {
			text = "TODO"
		}
		expr := pklQuote(text)
		if len([]rune(text)) > importPromptLimit {
			path := "prompts/" + id + ".txt"
			files[path] = text
			expr = "read(" + pklQuote(path) + ").text"
		}
		out = append(out, importSchedule{id: id, cron: cron, prompt: expr})
	}
	return out
}

func uniqueSlug(base string, used map[string]bool) string {
	if base == "" {
		base = "job"
	}
	id := base
	for n := 2; used[id]; n++ {
		id = fmt.Sprintf("%s-%d", base, n)
	}
	used[id] = true
	return id
}

func promptSlug(prompt string) string {
	var words []string
	var cur strings.Builder
	flush := func() {
		if cur.Len() == 0 || len(words) >= 4 {
			cur.Reset()
			return
		}
		words = append(words, cur.String())
		cur.Reset()
	}
	for _, r := range strings.ToLower(prompt) {
		if unicode.IsLetter(r) || unicode.IsDigit(r) {
			if len(words) < 4 {
				cur.WriteRune(r)
			}
			continue
		}
		flush()
	}
	flush()
	if len(words) == 0 {
		return "job"
	}
	slug := strings.Join(words, "-")
	if runes := []rune(slug); len(runes) > 40 {
		slug = strings.TrimRight(string(runes[:40]), "-")
	}
	if slug == "" {
		return "job"
	}
	return slug
}

func writeGone(b *strings.Builder, names []string) {
	for _, name := range names {
		b.WriteString("// gone: " + name + "\n")
	}
}

func writeUnowned(b *strings.Builder, jobs []LiveCron) {
	for _, job := range jobs {
		prefix := strings.ReplaceAll(promptPrefix(job.Prompt), "\n", " ")
		prefix = strings.ReplaceAll(prefix, "\r", " ")
		b.WriteString("// unowned cron: " + job.Cron + " " + prefix + "\n")
	}
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

func writeScope(b *strings.Builder, roles []importRole) {
	seen := map[string]bool{}
	var workspaces []string
	for _, role := range roles {
		if role.workspace == "" || seen[role.workspace] {
			continue
		}
		seen[role.workspace] = true
		workspaces = append(workspaces, role.workspace)
	}
	if len(workspaces) == 0 {
		return
	}
	slices.Sort(workspaces)
	b.WriteString("scope {\n")
	b.WriteString("  workspaces {\n")
	for _, workspace := range workspaces {
		b.WriteString("    " + pklQuote(workspace) + "\n")
	}
	b.WriteString("  }\n")
	b.WriteString("}\n\n")
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
				b.WriteString("      new {\n")
				b.WriteString("        id = " + pklQuote(schedule.id) + "\n")
				b.WriteString("        cron = " + pklQuote(schedule.cron) + "\n")
				b.WriteString("        prompt = " + schedule.prompt + "\n")
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
