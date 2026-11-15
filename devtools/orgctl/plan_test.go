package orgctl

import (
	"strings"
	"testing"
	"time"
)

func TestPlanMarkers(t *testing.T) {
	now := time.Date(2026, 11, 14, 0, 0, 0, 0, time.UTC)
	spec := planSpec()
	live := Live{
		Local: "north",
		Now:   now,
		Tabs:  []LiveTab{{Workspace: "hq", Label: "desk"}},
		Agents: []LiveAgent{{
			Name: "changed/one", Cwd: "/work/other", Model: "desk-small",
		}},
		Names: []LiveName{{Name: "changed/one", When: "evenings", SessionID: "sess-changed"}},
		Crons: []LiveCron{{
			Cwd: "/work/changed", ID: "aabbccdd", Cron: "0 8 * * *",
			CreatedBySessionID: "sess-changed", ExpiresAt: now.Add(24 * time.Hour),
		}},
		CronRead:    []string{"/work/missing", "/work/changed"},
		Briefs:      map[string]string{"changed/one": BriefEdited},
		KnownModels: []string{"desk-small"},
	}
	got := strings.Join(Plan(spec, live, PlanOptions{}), "\n")
	want := strings.Join([]string{
		"~ changed/one when",
		"~ changed/one cwd",
		"~ changed/one model",
		"! changed/one cron daily expiring",
		"! changed/one brief edited in place",
		"! changed/one model unknown",
		"+ missing/one tab hq/lead",
		"+ missing/one agentbus",
		"? missing/one cron overnight unverifiable",
		"? missing/one cron ping unverifiable",
		"! missing/one model unknown",
		"? watch/night remote: not read",
		"USER STEP: missing/one permission notes.read",
		"USER STEP: missing/one secret MAIL_TOKEN",
		"USER STEP: missing/one service mail",
	}, "\n")
	if got != want {
		t.Fatalf("plan =\n%s\nwant\n%s", got, want)
	}
	if !PlanChanged(Plan(spec, live, PlanOptions{})) {
		t.Fatal("markers should count as changes")
	}
}

func TestPlanUnmanagedBrief(t *testing.T) {
	spec := Spec{Roles: map[string]Role{
		"desk/clerk": {Machine: "local", Workspace: "front", Tab: "clerk", Cwd: "/work/desk"},
	}}
	live := Live{
		Local:    "local",
		Tabs:     []LiveTab{{Workspace: "front", Label: "clerk"}},
		Names:    []LiveName{{Name: "desk/clerk"}},
		Agents:   []LiveAgent{{Name: "desk/clerk", Cwd: "/work/desk"}},
		CronRead: []string{"/work/desk"},
		Briefs:   map[string]string{"desk/clerk": BriefUnmanaged},
	}
	got := Plan(spec, live, PlanOptions{})
	if strings.Join(got, "\n") != "i desk/clerk brief unmanaged" {
		t.Fatalf("plan =\n%s", strings.Join(got, "\n"))
	}
	if PlanChanged(got) {
		t.Fatal("an unmanaged brief is not a change")
	}
}

func TestPlanFilters(t *testing.T) {
	spec := planSpec()
	live := Live{Local: "north", CronRead: []string{"/work/missing", "/work/changed"}}
	byRole := Plan(spec, live, PlanOptions{Role: "watch/night"})
	if strings.Join(byRole, "\n") != "? watch/night remote: not read" {
		t.Fatalf("role filter =\n%s", strings.Join(byRole, "\n"))
	}
	byMachine := Plan(spec, live, PlanOptions{Machine: "south"})
	if strings.Join(byMachine, "\n") != "? watch/night remote: not read" {
		t.Fatalf("machine filter =\n%s", strings.Join(byMachine, "\n"))
	}
}

func TestPlanUnreadableAndQuiet(t *testing.T) {
	now := time.Date(2026, 11, 14, 0, 0, 0, 0, time.UTC)
	spec := Spec{Roles: map[string]Role{
		"coord/lead": {
			Machine: "north", Workspace: "hq", Tab: "lead",
			Cwd: "/work/lead", When: "mornings", Model: "desk-large",
			Schedules: []Schedule{{ID: "daily", Cron: "0 8 * * *", Durable: true}},
			Needs:     Needs{Permissions: []string{"notes.read"}},
		},
	}}
	unread := Live{
		Local: "north", HerdrNotRead: true, NamesNotRead: true, Now: now,
	}
	got := strings.Join(Plan(spec, unread, PlanOptions{}), "\n")
	want := "? coord/lead herdr: not read\n? coord/lead agentbus: not read\n? coord/lead cron daily unverifiable\nUSER STEP: coord/lead permission notes.read"
	if got != want {
		t.Fatalf("unread =\n%s\nwant\n%s", got, want)
	}

	quiet := Live{
		Local: "north",
		Now:   now,
		Tabs:  []LiveTab{{Workspace: "hq", Label: "lead"}},
		Agents: []LiveAgent{{
			Name: "coord/lead", Cwd: "/work/lead", Model: "desk-large",
		}},
		Names: []LiveName{{Name: "coord/lead", When: "mornings", SessionID: "sess-lead"}},
		Crons: []LiveCron{{
			Cwd: "/work/lead", ID: "99887766", Cron: "0 8 * * *",
			CreatedBySessionID: "sess-lead", ExpiresAt: now.Add(10 * 24 * time.Hour),
		}},
		CronRead:    []string{"/work/lead"},
		Briefs:      map[string]string{"coord/lead": BriefOK},
		KnownModels: []string{"desk-large"},
	}
	lines := Plan(spec, quiet, PlanOptions{})
	joined := strings.Join(lines, "\n")
	if joined != "USER STEP: coord/lead permission notes.read" {
		t.Fatalf("quiet =\n%s", joined)
	}
	if PlanChanged(lines) {
		t.Fatal("user steps are not changes")
	}
	soon := quiet
	soon.Crons = []LiveCron{{
		Cwd: "/work/lead", ID: "11223344", Cron: "0 8 * * *",
		CreatedBySessionID: "sess-lead", ExpiresAt: now.Add(48 * time.Hour),
	}}
	if !strings.Contains(strings.Join(Plan(spec, soon, PlanOptions{}), "\n"), "! coord/lead cron daily expiring") {
		t.Fatal("cron at the window edge should be attention")
	}
}

func TestPlanCronMatchByOwner(t *testing.T) {
	now := time.Date(2026, 11, 14, 0, 0, 0, 0, time.UTC)
	far := now.Add(10 * 24 * time.Hour)
	const (
		session = "sess-owner"
		expr    = "0 9 * * 1-5"
		promptA = "Read the overnight notes and file them."
		promptB = "Check the mailbox and leave a summary."
	)
	spec := Spec{Roles: map[string]Role{
		"desk/clerk": {
			Machine: "local", Workspace: "front", Tab: "clerk", Cwd: "/work/desk",
			Schedules: []Schedule{{
				ID: "notes", Cron: expr, Prompt: promptA, Durable: true,
			}},
		},
	}}
	live := Live{
		Local:    "local",
		Now:      now,
		Tabs:     []LiveTab{{Workspace: "front", Label: "clerk"}},
		Names:    []LiveName{{Name: "desk/clerk", SessionID: session}},
		Agents:   []LiveAgent{{Name: "desk/clerk", Cwd: "/work/desk"}},
		CronRead: []string{"/work/desk"},
	}
	job := func(id, owner, prompt, cronExpr string) LiveCron {
		return LiveCron{
			Cwd: "/work/desk", ID: id, Cron: cronExpr, Prompt: prompt,
			CreatedBySessionID: owner, ExpiresAt: far,
		}
	}
	lines := func(crons []LiveCron, names []LiveName) string {
		t.Helper()
		next := live
		next.Crons = crons
		if names != nil {
			next.Names = names
		}
		return strings.Join(Plan(spec, next, PlanOptions{}), "\n")
	}

	owned := lines([]LiveCron{job("1111aaaa", session, promptA, expr)}, nil)
	if owned != "" {
		t.Fatalf("owned job =\n%s", owned)
	}
	renewed := lines([]LiveCron{job("2222bbbb", session, promptA, expr)}, nil)
	if renewed != "" {
		t.Fatalf("renewed job =\n%s", renewed)
	}
	missing := lines(nil, nil)
	if missing != "+ desk/clerk cron notes" {
		t.Fatalf("missing =\n%s", missing)
	}
	other := lines([]LiveCron{job("3333cccc", "sess-other", promptA, expr)}, nil)
	if strings.Contains(other, "+") || strings.Contains(other, "cron notes") {
		t.Fatalf("other session =\n%s", other)
	}
	sameID := lines([]LiveCron{job("notes", session, promptA, "1 2 * * *")}, nil)
	if sameID != "+ desk/clerk cron notes" {
		t.Fatalf("same id, other cron =\n%s", sameID)
	}
	noHandle := lines(
		[]LiveCron{job("4444dddd", session, promptA, expr)},
		[]LiveName{{Name: "desk/clerk"}},
	)
	if noHandle != "? desk/clerk cron notes unverifiable" {
		t.Fatalf("no handle =\n%s", noHandle)
	}

	two := spec
	two.Roles["desk/clerk"] = Role{
		Machine: "local", Workspace: "front", Tab: "clerk", Cwd: "/work/desk",
		Schedules: []Schedule{
			{ID: "notes", Cron: expr, Prompt: promptA, Durable: true},
			{ID: "mail", Cron: expr, Prompt: promptB, Durable: true},
		},
	}
	got := strings.Join(Plan(two, Live{
		Local: "local", Now: now,
		Tabs:     []LiveTab{{Workspace: "front", Label: "clerk"}},
		Names:    []LiveName{{Name: "desk/clerk", SessionID: session}},
		Agents:   []LiveAgent{{Name: "desk/clerk", Cwd: "/work/desk"}},
		CronRead: []string{"/work/desk"},
		Crons: []LiveCron{
			job("5555eeee", session, promptA, expr),
			job("6666ffff", session, promptB, expr),
		},
	}, PlanOptions{}), "\n")
	if got != "" {
		t.Fatalf("prompt prefix =\n%s", got)
	}
}

func planSpec() Spec {
	return Spec{
		Machines: map[string]Machine{
			"north": {Host: "north.example"},
			"south": {Host: "south.example"},
		},
		Roles: map[string]Role{
			"missing/one": {
				Machine: "north", Workspace: "hq", Tab: "lead",
				Cwd: "/work/missing", When: "mornings", Model: "desk-large",
				Schedules: []Schedule{
					{ID: "overnight", Cron: "0 9 * * 1-5", Durable: true},
					{ID: "ping", Cron: "* * * * *", Durable: false},
				},
				Needs: Needs{
					Permissions: []string{"notes.read"},
					Secrets:     []string{"MAIL_TOKEN"},
					Services:    []string{"mail"},
				},
			},
			"changed/one": {
				Machine: "north", Workspace: "hq", Tab: "desk",
				Cwd: "/work/changed", When: "mornings", Model: "desk-large",
				Schedules: []Schedule{{ID: "daily", Cron: "0 8 * * *", Durable: true}},
			},
			"watch/night": {
				Machine: "south", Workspace: "hq", Tab: "night", Cwd: "/work/watch",
			},
		},
	}
}
