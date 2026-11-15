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
		Names: []LiveName{{Name: "changed/one", When: "evenings"}},
		Crons: []LiveCron{{
			Cwd: "/work/changed", ID: "daily", ExpiresAt: now.Add(24 * time.Hour),
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
		"+ missing/one cron overnight",
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
			Schedules: []Schedule{{ID: "daily", Durable: true}},
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
		Names:       []LiveName{{Name: "coord/lead", When: "mornings"}},
		Crons:       []LiveCron{{Cwd: "/work/lead", ID: "daily", ExpiresAt: now.Add(10 * 24 * time.Hour)}},
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
	soon.Crons = []LiveCron{{Cwd: "/work/lead", ID: "daily", ExpiresAt: now.Add(48 * time.Hour)}}
	if !strings.Contains(strings.Join(Plan(spec, soon, PlanOptions{}), "\n"), "! coord/lead cron daily expiring") {
		t.Fatal("cron at the window edge should be attention")
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
