package orgctl

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestImportEvaluates(t *testing.T) {
	live := Live{
		Names: []LiveName{{Name: "coord/lead", When: "mornings", Server: "local"}},
		Agents: []LiveAgent{{
			Name: "coord/lead", Machine: "local", Workspace: "hq", Tab: "lead",
			Cwd: "/work/lead", Kind: "coordinator",
		}},
		Crons: []LiveCron{{
			Cwd: "/work/lead", ID: "overnight", Cron: "0 9 * * 1-5",
			Prompt: "Read the overnight notes.",
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
	if len(role.Schedules) != 1 || role.Schedules[0].ID != "overnight" || role.Schedules[0].Prompt != "Read the overnight notes." {
		t.Fatalf("schedules = %+v", role.Schedules)
	}
	if _, err := spec.CheckDecides(nil); err != nil {
		t.Fatal(err)
	}
}
