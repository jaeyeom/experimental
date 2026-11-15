package orgctl

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestDriftReportsLiveExtras(t *testing.T) {
	spec := Spec{
		Roles: map[string]Role{
			"coord/lead": {
				Machine: "north", Workspace: "hq", Tab: "lead",
				Cwd: "/work/lead", When: "mornings",
				Schedules: []Schedule{{ID: "overnight", Cron: "0 9 * * 1-5", Durable: true}},
				Brief:     Brief{File: "briefs/lead.md"},
			},
			"review/desk": {
				Machine: "north", Workspace: "hq", Tab: "desk",
				Cwd: "/work/review",
			},
		},
		Pools: map[string]Pool{"hands": {Workspace: "workers"}},
	}
	live := Live{
		Tabs: []LiveTab{
			{Workspace: "hq", Label: "lead"},
			{Workspace: "hq", Label: "desk"},
			{Workspace: "hq", Label: "stray"},
			{Workspace: "workers", Label: "hands-2"},
		},
		Names: []LiveName{
			{Name: "coord/lead", When: "evenings"},
			{Name: "review/desk"},
			{Name: "extra/one"},
			{Name: "hands-1"},
		},
		Agents: []LiveAgent{
			{Name: "coord/lead", SessionID: "sess-lead"},
			{Name: "review/desk", SessionID: "sess-review"},
		},
		Crons: []LiveCron{
			{Cwd: "/work/lead", ID: "overnight", Cron: "0 9 * * 1-5", CreatedBySessionID: "sess-review"},
			{Cwd: "/work/other", ID: "stray", Cron: "15 8 * * *"},
		},
		Briefs: map[string]string{"coord/lead": BriefEdited},
	}
	lines := Drift(spec, live)
	got := strings.Join(lines, "\n")
	for _, want := range []string{
		"+ tab hq/stray",
		"USER: close tab stray in hq",
		"+ name extra/one",
		"+ cron stray",
		"! cron overnight misplaced: move to role review/desk",
		"USER: recreate cron in role review/desk",
		"~ coord/lead when",
		"! coord/lead brief edited in place",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q\n%s", want, got)
		}
	}
	if strings.Contains(got, "hands") {
		t.Fatalf("pool item reported:\n%s", got)
	}
	if !PlanChanged(lines) {
		t.Fatal("drift should count as changes")
	}
	stubs := 0
	for _, line := range lines {
		if !strings.HasPrefix(line, "module ") {
			continue
		}
		stubs++
		path := filepath.Join(t.TempDir(), "stub.pkl")
		if err := os.WriteFile(path, []byte(line), 0o600); err != nil {
			t.Fatal(err)
		}
		_, stderr, code := evalPkl(t, path)
		if code != 0 {
			t.Fatalf("pkl eval exit %d\n%s\n%s", code, line, stderr)
		}
	}
	if stubs == 0 {
		t.Fatal("no pkl stub")
	}
}

func TestDriftClean(t *testing.T) {
	spec := Spec{Roles: map[string]Role{
		"coord/lead": {
			Workspace: "hq", Tab: "lead", Cwd: "/work/lead", When: "mornings",
		},
	}}
	live := Live{
		Tabs:  []LiveTab{{Workspace: "hq", Label: "lead"}},
		Names: []LiveName{{Name: "coord/lead", When: "mornings"}},
		Briefs: map[string]string{
			"coord/lead": BriefOK,
		},
	}
	lines := Drift(spec, live)
	if len(lines) != 0 || PlanChanged(lines) {
		t.Fatalf("clean drift = %#v", lines)
	}
}
