package orgctl

import (
	"encoding/json"
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

func TestPlanMissingBriefAndCwd(t *testing.T) {
	spec := Spec{Roles: map[string]Role{
		"desk/clerk": {
			Machine: "local", Workspace: "front", Tab: "clerk", Cwd: "/work/desk",
			Brief: Brief{File: "BRIEF.md", Text: "hello"},
		},
	}}
	live := Live{
		Local:    "local",
		Tabs:     []LiveTab{{Workspace: "front", Label: "clerk"}},
		Names:    []LiveName{{Name: "desk/clerk", SessionID: "sess"}},
		Agents:   []LiveAgent{{Name: "desk/clerk", Cwd: "/work/desk"}},
		CronRead: []string{"/work/desk"},
		Briefs:   map[string]string{"desk/clerk": BriefMissing},
	}
	got := strings.Join(Plan(spec, live, PlanOptions{}), "\n")
	if got != "+ desk/clerk brief missing" {
		t.Fatalf("missing brief =\n%s", got)
	}
	if !PlanChanged(Plan(spec, live, PlanOptions{})) {
		t.Fatal("a missing brief is a change")
	}

	gone := spec
	role := gone.Roles["desk/clerk"]
	role.Cwd = "/no/such"
	role.Brief = Brief{}
	role.Schedules = []Schedule{{ID: "daily", Cron: "0 8 * * *", Durable: true}}
	gone.Roles["desk/clerk"] = role
	missing := Live{
		Local:      "local",
		Tabs:       []LiveTab{{Workspace: "front", Label: "clerk"}},
		Names:      []LiveName{{Name: "desk/clerk", SessionID: "sess"}},
		Agents:     []LiveAgent{{Name: "desk/clerk", Cwd: "/no/such"}},
		MissingCwd: []string{"/no/such"},
	}
	got = strings.Join(Plan(gone, missing, PlanOptions{}), "\n")
	if got != "? desk/clerk cwd not found" {
		t.Fatalf("missing cwd =\n%s", got)
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

func jobsSpec() Spec {
	return Spec{
		Machines: map[string]Machine{"north": {}, "south": {}},
		Jobs: &Jobs{
			Policy:      "~/jobs/policy.json",
			AnomalyRole: "coord/lead",
			Runners: map[string]Runner{
				"r-north": {Machine: "north", Kinds: []string{"merge"}},
				"r-other": {Machine: "north", Kinds: []string{"rebase"}},
				"r-south": {Machine: "south", Kinds: []string{"merge"}},
			},
		},
	}
}

func TestPlanJobs(t *testing.T) {
	t.Setenv("HOME", "/home/test")
	spec := jobsSpec()
	unit := func(name string) string { return "/home/test/.config/systemd/user/" + name + ".service" }

	missing := Live{Local: "north", Found: map[string]bool{
		unit("r-north"): false, unit("r-other"): true, "/home/test/jobs/policy.json": false,
	}}
	got := Plan(spec, missing, PlanOptions{})
	want := strings.Join([]string{
		"+ runner r-north unit ~/.config/systemd/user/r-north.service",
		"? runner r-south remote: not read",
		"+ jobs policy ~/jobs/policy.json",
	}, "\n")
	if strings.Join(got, "\n") != want {
		t.Fatalf("plan =\n%s\nwant\n%s", strings.Join(got, "\n"), want)
	}
	if !PlanChanged(got) {
		t.Fatal("a missing unit is a change")
	}

	present := Live{Local: "north", Found: map[string]bool{
		unit("r-north"): true, unit("r-other"): true, "/home/test/jobs/policy.json": true,
	}}
	got = Plan(spec, present, PlanOptions{Machine: "north"})
	if len(got) != 0 {
		t.Fatalf("all present: plan =\n%s", strings.Join(got, "\n"))
	}

	unchecked := Live{Local: "north"}
	got = Plan(spec, unchecked, PlanOptions{Machine: "north"})
	want = strings.Join([]string{
		"? runner r-north unit not checked",
		"? runner r-other unit not checked",
		"? jobs policy not checked",
	}, "\n")
	if strings.Join(got, "\n") != want {
		t.Fatalf("unchecked: plan =\n%s\nwant\n%s", strings.Join(got, "\n"), want)
	}
}

func TestPlanJobsFilters(t *testing.T) {
	spec := jobsSpec()
	live := Live{Local: "north"}
	if got := Plan(spec, live, PlanOptions{Role: "coord/lead"}); len(got) != 0 {
		t.Errorf("--role: plan =\n%s", strings.Join(got, "\n"))
	}
	got := strings.Join(Plan(spec, live, PlanOptions{Machine: "south"}), "\n")
	if got != "? runner r-south remote: not read" {
		t.Errorf("--machine south: plan =\n%s", got)
	}
	if got := Plan(Spec{}, live, PlanOptions{}); len(got) != 0 {
		t.Errorf("no jobs: plan =\n%s", strings.Join(got, "\n"))
	}
}

func TestRunnerUnit(t *testing.T) {
	if got := RunnerUnit("jobs-runner", "linux"); got != "~/.config/systemd/user/jobs-runner.service" {
		t.Errorf("linux = %q", got)
	}
	if got := RunnerUnit("jobs-runner", "darwin"); got != "~/Library/LaunchAgents/jobs-runner.plist" {
		t.Errorf("darwin = %q", got)
	}
}

func TestPlanJSONRoundTripAllKindsAndStatuses(t *testing.T) {
	now := time.Date(2026, 11, 27, 5, 0, 0, 0, time.UTC)
	far := now.Add(10 * 24 * time.Hour)
	soon := now.Add(12 * time.Hour)

	spec := Spec{
		Machines: map[string]Machine{"north": {}, "south": {}},
		Jobs: &Jobs{
			Policy: "~/jobs/policy.json",
			Runners: map[string]Runner{
				"r-ok":      {Machine: "north"},
				"r-missing": {Machine: "north"},
				"r-remote":  {Machine: "south"},
			},
		},
		Roles: map[string]Role{
			"ok/role": {
				Machine: "north", Workspace: "hq", Tab: "ok", Cwd: "/work/ok",
				When: "mornings", Model: "desk-small",
				Brief:     Brief{File: "BRIEF.md", Text: "hello"},
				Schedules: []Schedule{{ID: "daily", Cron: "0 8 * * *", Durable: false}},
			},
			"drift/role": {
				Machine: "north", Workspace: "hq", Tab: "missing-tab", Cwd: "/work/drift",
				When: "mornings", Model: "desk-large",
				Brief: Brief{File: "BRIEF.md", Text: "hello"},
				Schedules: []Schedule{
					{ID: "soon", Cron: "0 8 * * *", Durable: false},
					{ID: "absent", Cron: "0 9 * * *", Durable: false},
				},
				Needs: Needs{Permissions: []string{"notes.read"}},
			},
			"unmanaged/role": {
				Machine: "north", Workspace: "hq", Tab: "unmanaged", Cwd: "/work/unmanaged",
				Brief: Brief{File: "BRIEF.md"},
				Schedules: []Schedule{
					{ID: "blind", Cron: "0 10 * * *", Durable: false},
				},
			},
			"missing/brief": {
				Machine: "north", Workspace: "hq", Tab: "ok", Cwd: "/work/missing-brief",
				Brief: Brief{File: "BRIEF.md", Text: "hello"},
			},
			"remote/role": {
				Machine: "south", Workspace: "hq", Tab: "night", Cwd: "/work/remote",
			},
		},
	}

	t.Setenv("HOME", "/home/test")
	live := Live{
		Local:       "north",
		Now:         now,
		KnownModels: []string{"desk-small"},
		Tabs: []LiveTab{
			{Workspace: "hq", Label: "ok"},
			{Workspace: "hq", Label: "unmanaged"},
		},
		Agents: []LiveAgent{
			{Name: "ok/role", Cwd: "/work/ok", Model: "desk-small"},
			{Name: "drift/role", Cwd: "/work/other", Model: "desk-small"},
		},
		Names: []LiveName{
			{Name: "ok/role", When: "mornings", SessionID: "s-ok"},
			{Name: "drift/role", When: "evenings", SessionID: "s-drift"},
		},
		CronRead: []string{"/work/ok", "/work/drift", "/work/unmanaged", "/work/missing-brief"},
		PublishedCrons: map[string]PublishedCrons{
			"ok/role": {
				Role:        "ok/role",
				PublishedAt: now.Add(-1 * time.Hour),
				Crons: []PublishedCron{
					{ID: "c-ok", Schedule: "0 8 * * *", Created: now.Add(-2 * time.Hour), Expiry: far},
				},
			},
			"drift/role": {
				Role:        "drift/role",
				PublishedAt: now.Add(-48 * time.Hour),
				Crons: []PublishedCron{
					{ID: "c-soon", Schedule: "0 8 * * *", Created: now.Add(-6 * 24 * time.Hour), Expiry: soon},
					{ID: "c-extra", Schedule: "*/5 * * * *", Created: now.Add(-1 * time.Hour), Expiry: far},
				},
			},
		},
		Briefs: map[string]string{
			"ok/role":        BriefOK,
			"drift/role":     BriefEdited,
			"unmanaged/role": BriefUnmanaged,
			"missing/brief":  BriefMissing,
		},
		AgentsMD: map[string]string{
			"AGENTS.md":           BriefOK,
			"roles/ops/AGENTS.md": BriefEdited,
		},
		Found: map[string]bool{
			expandOrKeep(RunnerUnit("r-ok", "")):      true,
			expandOrKeep(RunnerUnit("r-missing", "")): false,
			expandOrKeep("~/jobs/policy.json"):        true,
		},
	}

	items := PlanItems(spec, live, PlanOptions{})
	raw, err := json.Marshal(items)
	if err != nil {
		t.Fatalf("json.Marshal: %v", err)
	}
	var decoded []PlanItem
	if err := json.Unmarshal(raw, &decoded); err != nil {
		t.Fatalf("json.Unmarshal: %v", err)
	}
	if len(decoded) != len(items) {
		t.Fatalf("round-trip len = %d, want %d", len(decoded), len(items))
	}

	seenKinds := map[string]bool{}
	seenStatuses := map[string]bool{}
	for _, it := range decoded {
		seenKinds[it.Kind] = true
		seenStatuses[it.Status] = true
	}
	for _, wantKind := range ValidPlanKinds() {
		if !seenKinds[wantKind] {
			t.Errorf("missing kind %q in JSON output: %+v", wantKind, decoded)
		}
	}
	for _, wantStatus := range []string{
		PlanStatusOK,
		PlanStatusMissing,
		PlanStatusExpiring,
		PlanStatusUnlisted,
		PlanStatusStale,
		PlanStatusUnverifiable,
		PlanStatusChanged,
		PlanStatusEdited,
		PlanStatusUnknown,
		PlanStatusUnmanaged,
		PlanStatusUserStep,
	} {
		if !seenStatuses[wantStatus] {
			t.Errorf("missing status %q in JSON output: %+v", wantStatus, decoded)
		}
	}

	// Verify expiry survives round-trip on ok/expiring cron items.
	var foundSoon bool
	for _, it := range decoded {
		if it.Role == "drift/role" && it.Kind == PlanKindCron && it.ID == "soon" {
			foundSoon = true
			if it.Status != PlanStatusExpiring || it.Schedule != "0 8 * * *" || it.Expiry == nil || !it.Expiry.Equal(soon) {
				t.Fatalf("expiring item = %+v", it)
			}
		}
	}
	if !foundSoon {
		t.Fatal("missing expiring cron item in decoded output")
	}
}

func TestPlanOnlyFilterAndPublishedCrons(t *testing.T) {
	now := time.Date(2026, 11, 27, 5, 0, 0, 0, time.UTC)
	far := now.Add(10 * 24 * time.Hour)
	soon := now.Add(12 * time.Hour)

	spec := Spec{
		Roles: map[string]Role{
			"desk/clerk": {
				Machine: "local", Workspace: "front", Tab: "clerk", Cwd: "/work/desk",
				When: "mornings",
				Schedules: []Schedule{
					{ID: "notes", Cron: "0 9 * * 1-5", Prompt: "Read overnight notes", Durable: false},
					{ID: "mail", Cron: "0 9 * * 1-5", Prompt: "Check mailbox", Durable: false},
					{ID: "nightly", Cron: "0 23 * * *", Durable: false},
				},
				Needs: Needs{Secrets: []string{"API_KEY"}},
			},
			"desk/unpub": {
				Machine: "local", Workspace: "front", Tab: "unpub", Cwd: "/work/unpub",
				Schedules: []Schedule{{ID: "ping", Cron: "* * * * *", Durable: false}},
			},
		},
	}

	// Tab is missing and when differs, but with --only cron those are hidden.
	live := Live{
		Local: "local",
		Now:   now,
		Names: []LiveName{{Name: "desk/clerk", When: "evenings"}, {Name: "desk/unpub"}},
		PublishedCrons: map[string]PublishedCrons{
			"desk/clerk": {
				Role:        "desk/clerk",
				PublishedAt: now.Add(-30 * time.Hour), // stale (> 24h)
				Crons: []PublishedCron{
					{ID: "1111aaaa", Schedule: "0 9 * * 1-5", Prompt: "Read overnight notes", Expiry: far},
					{ID: "2222bbbb", Schedule: "0 9 * * 1-5", Prompt: "Check mailbox", Expiry: soon},
					{ID: "3333cccc", Schedule: "*/10 * * * *", Expiry: far},
				},
			},
		},
	}

	items := PlanItems(spec, live, PlanOptions{Only: []string{PlanKindCron}})
	for _, it := range items {
		if it.Kind != PlanKindCron {
			t.Fatalf("--only cron leaked kind %q: %+v", it.Kind, it)
		}
	}
	lines := Plan(spec, live, PlanOptions{Only: []string{PlanKindCron}})
	joined := strings.Join(lines, "\n")
	for _, forbidden := range []string{"tab", "when", "USER STEP", "API_KEY"} {
		if strings.Contains(joined, forbidden) {
			t.Errorf("--only cron output unexpectedly contained %q:\n%s", forbidden, joined)
		}
	}
	for _, want := range []string{
		"! desk/clerk cron stale",
		"! desk/clerk cron mail expiring",
		"+ desk/clerk cron nightly",
		"! desk/clerk cron 3333cccc unlisted",
		"? desk/unpub cron ping unverifiable",
	} {
		if !strings.Contains(joined, want) {
			t.Errorf("cron output missing %q:\n%s", want, joined)
		}
	}

	// If we only look at a role whose published cron is fresh and ok, --only cron exits clean even when tab is missing.
	cleanCronSpec := Spec{
		Roles: map[string]Role{
			"desk/clerk": {
				Machine: "local", Workspace: "front", Tab: "clerk", Cwd: "/work/desk",
				Schedules: []Schedule{{ID: "notes", Cron: "0 9 * * 1-5", Durable: false}},
			},
		},
	}
	cleanCronLive := Live{
		Local: "local",
		Now:   now,
		PublishedCrons: map[string]PublishedCrons{
			"desk/clerk": {
				Role:        "desk/clerk",
				PublishedAt: now.Add(-1 * time.Hour),
				Crons:       []PublishedCron{{ID: "1111aaaa", Schedule: "0 9 * * 1-5", Expiry: far}},
			},
		},
	}
	allItems := PlanItems(cleanCronSpec, cleanCronLive, PlanOptions{})
	if !PlanItemsChanged(allItems) {
		t.Fatal("unfiltered plan should report missing tab/agentbus")
	}
	cronOnly := PlanItems(cleanCronSpec, cleanCronLive, PlanOptions{Only: []string{PlanKindCron}})
	if PlanItemsChanged(cronOnly) {
		t.Fatalf("--only cron should not report changes when only tab/agentbus differ: %+v", cronOnly)
	}
	if len(Plan(cleanCronSpec, cleanCronLive, PlanOptions{Only: []string{PlanKindCron}})) != 0 {
		t.Fatal("text plan with --only cron should be empty when cron is ok")
	}
}
