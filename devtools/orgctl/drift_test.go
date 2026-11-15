package orgctl

import (
	"fmt"
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
			{Cwd: "/work/lead", ID: "aa11bb22", Cron: "0 9 * * 1-5", CreatedBySessionID: "sess-review"},
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
	if strings.Contains(got, "hands") || strings.Contains(got, "+ cron aa11bb22") {
		t.Fatalf("pool item or declared cron reported:\n%s", got)
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

func TestDriftPoolPatterns(t *testing.T) {
	spec := Spec{
		Roles: map[string]Role{
			"desk/lead": {Workspace: "mixed", Tab: "lead", When: "mornings"},
		},
		Pools: map[string]Pool{
			"adhoc": {Workspace: "mixed", NamePattern: "adhoc/*"},
		},
	}
	live := Live{
		Tabs: []LiveTab{{Workspace: "mixed", Label: "lead"}},
		Names: []LiveName{
			{Name: "desk/lead", When: "mornings"},
			{Name: "adhoc/one"},
			{Name: "adhoc/two"},
			{Name: "stray/name"},
		},
	}
	got := strings.Join(Drift(spec, live), "\n")
	if !strings.Contains(got, "+ name stray/name") {
		t.Fatalf("missing stray\n%s", got)
	}
	if strings.Contains(got, "adhoc/") || strings.Contains(got, "+ name desk/lead") {
		t.Fatalf("pool worker or role reported\n%s", got)
	}

	role := spec
	role.Roles = map[string]Role{
		"adhoc/lead": {Workspace: "mixed", Tab: "lead", When: "mornings"},
	}
	roleLive := Live{
		Tabs:  []LiveTab{{Workspace: "mixed", Label: "lead"}},
		Names: []LiveName{{Name: "adhoc/lead", When: "evenings"}},
	}
	got = strings.Join(Drift(role, roleLive), "\n")
	if !strings.Contains(got, "~ adhoc/lead when") || strings.Contains(got, "+ name adhoc/lead") {
		t.Fatalf("role should win\n%s", got)
	}

	open := Spec{Pools: map[string]Pool{"hands": {Workspace: "workers"}}}
	openLive := Live{Tabs: []LiveTab{
		{Workspace: "workers", Label: "other"},
		{Workspace: "hq", Label: "stray"},
	}}
	got = strings.Join(Drift(open, openLive), "\n")
	if strings.Contains(got, "workers/other") || !strings.Contains(got, "+ tab hq/stray") {
		t.Fatalf("unpatterned workspace =\n%s", got)
	}

	both := Spec{Pools: map[string]Pool{
		"adhoc": {Workspace: "mixed", NamePattern: "adhoc/*", TabPattern: "adhoc-*"},
	}}
	bothLive := Live{
		Names: []LiveName{{Name: "adhoc/one"}, {Name: "adhoc/two"}},
		Agents: []LiveAgent{
			{Name: "adhoc/one", Workspace: "mixed", Tab: "adhoc-1"},
			{Name: "adhoc/two", Workspace: "mixed", Tab: "other"},
		},
		Tabs: []LiveTab{
			{Workspace: "mixed", Label: "adhoc-1"},
			{Workspace: "mixed", Label: "other"},
		},
	}
	got = strings.Join(Drift(both, bothLive), "\n")
	if strings.Contains(got, "adhoc/one") || strings.Contains(got, "adhoc-1") {
		t.Fatalf("matched worker reported\n%s", got)
	}
	if !strings.Contains(got, "+ name adhoc/two") || !strings.Contains(got, "+ tab mixed/other") {
		t.Fatalf("unmatched worker missing\n%s", got)
	}
}

func TestDriftReportsSharedCwdOnce(t *testing.T) {
	const cwd = "/work/shared"
	roles := map[string]Role{}
	var agents []LiveAgent
	for i := range 10 {
		name := fmt.Sprintf("desk/%02d", i)
		role := Role{Cwd: cwd}
		if i < 3 {
			role.Schedules = []Schedule{{
				ID: fmt.Sprintf("job-%d", i), Cron: fmt.Sprintf("%d * * * *", i), Durable: true,
			}}
			agents = append(agents, LiveAgent{Name: name, SessionID: fmt.Sprintf("sess-%d", i)})
		}
		roles[name] = role
	}
	var jobs []LiveCron
	for i := range 3 {
		job := LiveCron{
			Cwd: cwd, ID: fmt.Sprintf("aaaa000%d", i), Cron: fmt.Sprintf("%d * * * *", i),
			CreatedBySessionID: fmt.Sprintf("sess-%d", i),
		}
		jobs = append(jobs, job, job)
	}
	for i := range 2 {
		job := LiveCron{Cwd: cwd, ID: fmt.Sprintf("bbbb000%d", i), Cron: fmt.Sprintf("%d * * * *", i+10)}
		jobs = append(jobs, job, job)
	}
	lines := Drift(Spec{Roles: roles}, Live{Crons: jobs, Agents: agents})
	got := strings.Join(lines, "\n")
	count := strings.Count(got, "\n+ cron ")
	if strings.HasPrefix(got, "+ cron ") {
		count++
	}
	if count != 2 || strings.Contains(got, "misplaced") || strings.Contains(got, "+ cron aaaa") {
		t.Fatalf("+ cron count = %d\n%s", count, got)
	}

	roles["desk/00"] = Role{Cwd: cwd, Schedules: []Schedule{{ID: "night", Cron: "0 9 * * *", Durable: true}}}
	moved := LiveCron{
		Cwd: cwd, ID: "cccc0001", Cron: "0 9 * * *", CreatedBySessionID: "sess-9",
	}
	agents = append(agents, LiveAgent{Name: "desk/09", SessionID: "sess-9"})
	lines = Drift(Spec{Roles: roles}, Live{Crons: []LiveCron{moved, moved}, Agents: agents})
	got = strings.Join(lines, "\n")
	if strings.Count(got, "misplaced") != 1 || strings.Contains(got, "+ cron ") {
		t.Fatalf("misplaced =\n%s", got)
	}
}

func TestDriftSkipsPoolWorkspace(t *testing.T) {
	spec := Spec{Pools: map[string]Pool{"hands": {Workspace: "workers"}}}
	live := Live{
		Tabs: []LiveTab{
			{Workspace: "workers", Label: "a"},
			{Workspace: "workers", Label: "b"},
			{Workspace: "workers", Label: "c"},
			{Workspace: "misc", Label: "stray"},
		},
		Names:  []LiveName{{Name: "worker/one"}, {Name: "other/name"}},
		Agents: []LiveAgent{{Name: "worker/one", Workspace: "workers", Tab: "a"}},
	}
	got := strings.Join(Drift(spec, live), "\n")
	want := "+ tab misc/stray\nUSER: close tab stray in misc\n+ name other/name\nmodule drift.stub\n\nname = \"other/name\"\n"
	if got != want {
		t.Fatalf("drift =\n%s\nwant\n%s", got, want)
	}
}

func TestCheckPatterns(t *testing.T) {
	err := (Spec{Pools: map[string]Pool{"hands": {NamePattern: "["}}}).CheckPatterns()
	if err == nil || err.Error() != `pool hands: namePattern "[": syntax error in pattern` {
		t.Fatalf("err = %v", err)
	}
	if err := (Spec{Pools: map[string]Pool{"hands": {NamePattern: "adhoc/*"}}}).CheckPatterns(); err != nil {
		t.Fatal(err)
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
