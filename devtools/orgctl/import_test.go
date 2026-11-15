package orgctl

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestImportEmitsScope(t *testing.T) {
	live := Live{
		Names: []LiveName{{Name: "b/two"}, {Name: "a/one"}},
		Agents: []LiveAgent{
			{Name: "b/two", Workspace: "misc", Tab: "t", Cwd: "/b", Kind: "agent", Machine: "local"},
			{Name: "a/one", Workspace: "org-a", Tab: "t", Cwd: "/a", Kind: "agent", Machine: "local"},
		},
	}
	stub := Import(live, "Org.pkl")
	want := "scope {\n  workspaces {\n    \"misc\"\n    \"org-a\"\n  }\n}\n"
	if !strings.Contains(stub, want) {
		t.Fatalf("stub missing scope\n%s", stub)
	}
}

func TestImportJoinsPaneAndJobs(t *testing.T) {
	live := Live{
		Names: []LiveName{
			{Name: "desk/one", SessionID: "sess-1", Server: "local", Pane: "p1", Liveness: "alive"},
			{Name: "desk/two", SessionID: "sess-2", Server: "local", Pane: "p2", Liveness: "alive"},
			{Name: "desk/three", SessionID: "sess-3", Server: "local", Pane: "p3", Liveness: "alive"},
			{Name: "old/gone", SessionID: "sess-9", Server: "local", Pane: "p9", Liveness: "gone"},
		},
		Agents: []LiveAgent{
			{Name: "agent-a", Pane: "p1", Workspace: "org-a", Tab: "one", Cwd: "/work/one", Kind: "grok", Machine: "local"},
			{Name: "agent-b", Pane: "p2", Workspace: "org-a", Tab: "two", Cwd: "/work/two", Kind: "grok", Machine: "local"},
			{Name: "agent-c", Pane: "p3", Workspace: "org-b", Tab: "three", Cwd: "/work/three", Kind: "grok", Machine: "local"},
			{Name: "agent-old", Pane: "p9", Workspace: "org-a", Tab: "old", Cwd: "/work/old", Kind: "grok"},
		},
		Crons: []LiveCron{
			{CreatedBySessionID: "sess-1", Cron: "0 9 * * 1-5", Prompt: "Read the overnight notes."},
			{CreatedBySessionID: "sess-2", Cron: "15 8 * * *", Prompt: "File the receipts."},
			{CreatedBySessionID: "sess-1", Cron: "0 18 * * 1-5", Prompt: "Close the desk."},
			{CreatedBySessionID: "sess-unknown", Cron: "30 7 * * *", Prompt: "Unowned sweep of the queue."},
		},
	}
	amends := filepath.Join(orgctlRoot(t), "pkl", "Org.pkl")
	stub := Import(live, amends)
	if strings.Contains(stub, "old/gone") && !strings.Contains(stub, "// gone: old/gone") {
		t.Fatalf("gone name imported as a role\n%s", stub)
	}
	if strings.Contains(stub, `["old/gone"]`) {
		t.Fatalf("gone role emitted\n%s", stub)
	}
	if !strings.Contains(stub, "// gone: old/gone") {
		t.Fatalf("missing gone comment\n%s", stub)
	}
	if !strings.Contains(stub, "// unowned cron: 30 7 * * * Unowned sweep of the queue.") {
		t.Fatalf("missing unowned job\n%s", stub)
	}
	path := filepath.Join(t.TempDir(), "org.pkl")
	if err := os.WriteFile(path, []byte(stub), 0o600); err != nil {
		t.Fatal(err)
	}
	stdout, stderr, code := evalPkl(t, path)
	if code != 0 {
		t.Fatalf("pkl eval exit %d\n%s\n%s", code, stderr, stub)
	}
	spec, err := Decode(stdout)
	if err != nil {
		t.Fatal(err)
	}
	if len(spec.Roles) != 3 {
		t.Fatalf("roles = %d\n%s", len(spec.Roles), stub)
	}
	one := spec.Roles["desk/one"]
	if one.Workspace != "org-a" || one.Tab != "one" || one.Cwd != "/work/one" {
		t.Fatalf("desk/one = %+v", one)
	}
	two := spec.Roles["desk/two"]
	if two.Workspace != "org-a" || two.Tab != "two" || two.Cwd != "/work/two" {
		t.Fatalf("desk/two = %+v", two)
	}
	three := spec.Roles["desk/three"]
	if three.Workspace != "org-b" || three.Tab != "three" || three.Cwd != "/work/three" {
		t.Fatalf("desk/three = %+v", three)
	}
	got := map[string]string{}
	for _, schedule := range one.Schedules {
		got[schedule.ID] = schedule.Cron + "\n" + schedule.Prompt
	}
	if got["read-the-overnight-notes"] != "0 9 * * 1-5\nRead the overnight notes." || got["close-the-desk"] != "0 18 * * 1-5\nClose the desk." || len(one.Schedules) != 2 {
		t.Fatalf("desk/one schedules = %+v", one.Schedules)
	}
	if len(two.Schedules) != 1 || two.Schedules[0].ID != "file-the-receipts" || two.Schedules[0].Prompt != "File the receipts." {
		t.Fatalf("desk/two schedules = %+v", two.Schedules)
	}
	if len(three.Schedules) != 0 {
		t.Fatalf("desk/three schedules = %+v", three.Schedules)
	}
}

func TestImportUniquePlaceholders(t *testing.T) {
	live := Live{Names: []LiveName{{Name: "a/one", Server: "local"}, {Name: "b/two", Server: "local"}}}
	amends := filepath.Join(orgctlRoot(t), "pkl", "Org.pkl")
	stub := Import(live, amends)
	path := filepath.Join(t.TempDir(), "org.pkl")
	if err := os.WriteFile(path, []byte(stub), 0o600); err != nil {
		t.Fatal(err)
	}
	stdout, stderr, code := evalPkl(t, path)
	if code != 0 {
		t.Fatalf("pkl eval exit %d\n%s\n%s", code, stderr, stub)
	}
	spec, err := Decode(stdout)
	if err != nil {
		t.Fatal(err)
	}
	if spec.Roles["a/one"].Tab != "TODO" || spec.Roles["b/two"].Tab != "TODO-1" {
		t.Fatalf("tabs = %q %q\n%s", spec.Roles["a/one"].Tab, spec.Roles["b/two"].Tab, stub)
	}
}

func TestImportDeadNameIsComment(t *testing.T) {
	live := Live{
		Names: []LiveName{
			{Name: "desk/one", Liveness: "alive", Server: "local"},
			{Name: "old/dead", Liveness: "dead", Server: "local"},
			{Name: "old/away", Liveness: "unreachable", Server: "local"},
		},
		Agents: []LiveAgent{{
			Name: "desk/one", Workspace: "hq", Tab: "one", Cwd: "/work", Kind: "grok", Machine: "local",
		}},
	}
	stub := Import(live, "Org.pkl")
	if strings.Contains(stub, `["old/dead"]`) {
		t.Fatalf("dead name imported as a role\n%s", stub)
	}
	if !strings.Contains(stub, "// gone: old/dead") {
		t.Fatalf("missing dead comment\n%s", stub)
	}
	if !strings.Contains(stub, `["desk/one"]`) || !strings.Contains(stub, `["old/away"]`) {
		t.Fatalf("live names missing\n%s", stub)
	}
}

func TestImportOutputPassesCheck(t *testing.T) {
	live := Live{Names: []LiveName{{Name: "a/one", Server: "local"}}}
	amends := filepath.Join(orgctlRoot(t), "pkl", "Org.pkl")
	stub := Import(live, amends)
	if !strings.Contains(stub, `cwd = "~/TODO"`) {
		t.Fatalf("cwd placeholder\n%s", stub)
	}
	path := filepath.Join(t.TempDir(), "org.pkl")
	if err := os.WriteFile(path, []byte(stub), 0o600); err != nil {
		t.Fatal(err)
	}
	stdout, stderr, code := evalPkl(t, path)
	if code != 0 {
		t.Fatalf("pkl eval exit %d\n%s\n%s", code, stderr, stub)
	}
	spec, err := Decode(stdout)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := spec.CheckDecides(nil); err != nil {
		t.Fatal(err)
	}
	if err := spec.CheckCwd(); err != nil {
		t.Fatal(err)
	}
	if err := spec.CheckPatterns(); err != nil {
		t.Fatal(err)
	}
	if spec.Roles["a/one"].Cwd != "~/TODO" {
		t.Fatalf("cwd = %q", spec.Roles["a/one"].Cwd)
	}
}

func TestImportLongPrompt(t *testing.T) {
	prompt := strings.Repeat("word ", 30) + "tail"
	live := Live{
		Names: []LiveName{{Name: "desk/one", SessionID: "sess-1", Server: "local"}},
		Agents: []LiveAgent{{
			Name: "desk/one", Workspace: "org-a", Tab: "one", Cwd: "/work/one", Kind: "grok", Machine: "local",
		}},
		Crons: []LiveCron{{CreatedBySessionID: "sess-1", Cron: "0 9 * * *", Prompt: prompt}},
	}
	rendered := RenderImport(live, "Org.pkl")
	if strings.Contains(rendered.Stub, prompt) || !strings.Contains(rendered.Stub, `prompt = read("prompts/word-word-word-word.txt")`) {
		t.Fatalf("stub =\n%s", rendered.Stub)
	}
	if rendered.Files["prompts/word-word-word-word.txt"] != prompt {
		t.Fatalf("files = %#v", rendered.Files)
	}
}

func TestImportEvaluates(t *testing.T) {
	live := Live{
		Names: []LiveName{{
			Name: "coord/lead", When: "mornings", Server: "local", SessionID: "sess-lead",
		}},
		Agents: []LiveAgent{{
			Name: "coord/lead", Machine: "local", Workspace: "hq", Tab: "lead",
			Cwd: "/work/lead", Kind: "coordinator",
		}},
		Crons: []LiveCron{{
			Cwd: "/work/lead", ID: "overnight", Cron: "0 9 * * 1-5",
			Prompt: "Read the overnight notes.", CreatedBySessionID: "sess-lead",
		}},
		Tabs: []LiveTab{
			{Workspace: "workers", Label: "hands-1"},
			{Workspace: "workers", Label: "hands-2"},
		},
	}
	amends := filepath.Join(orgctlRoot(t), "pkl", "Org.pkl")
	stub := Import(live, amends)
	for _, todo := range []string{"reportsTo", "decides", "brief", "needs", "pool candidate", `tabPattern = "hands-*"`} {
		if !strings.Contains(stub, "TODO") || !strings.Contains(stub, todo) {
			t.Errorf("stub missing TODO %s\n%s", todo, stub)
		}
	}
	path := filepath.Join(t.TempDir(), "org.pkl")
	if err := os.WriteFile(path, []byte(stub), 0o600); err != nil {
		t.Fatal(err)
	}
	stdout, stderr, code := evalPkl(t, path)
	if code != 0 {
		t.Fatalf("pkl eval exit %d\n%s\n%s", code, stderr, stub)
	}
	spec, err := Decode(stdout)
	if err != nil {
		t.Fatal(err)
	}
	role := spec.Roles["coord/lead"]
	if role.Machine != "local" || role.Workspace != "hq" || role.Tab != "lead" || role.Cwd != "/work/lead" {
		t.Fatalf("role = %+v", role)
	}
	if role.When != "mornings" || role.Agent != "coordinator" {
		t.Fatalf("role = %+v", role)
	}
	if len(role.Schedules) != 1 || role.Schedules[0].ID != "read-the-overnight-notes" || role.Schedules[0].Cron != "0 9 * * 1-5" || role.Schedules[0].Prompt != "Read the overnight notes." {
		t.Fatalf("schedules = %+v", role.Schedules)
	}
	if _, err := spec.CheckDecides(nil); err != nil {
		t.Fatal(err)
	}
	if spec.Scope == nil || len(spec.Scope.Workspaces) != 1 || spec.Scope.Workspaces[0] != "hq" {
		t.Fatalf("scope = %+v", spec.Scope)
	}
}
