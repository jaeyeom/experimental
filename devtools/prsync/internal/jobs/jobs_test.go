package jobs

import (
	"context"
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"slices"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

var update *bool

func TestMain(m *testing.M) {
	update = flag.Bool("update", false, "rewrite golden files")
	os.Exit(m.Run())
}

func strPtr(s string) *string { return &s }

// behindWithComment is a pull request that needs a comment job and a
// rebase job, so each golden file shows an agent job and a cli job.
func behindWithComment(number int) scan.PR {
	return scan.PR{
		Repo:             "acme/widgets",
		Number:           number,
		State:            "open",
		HeadSHA:          "abc123",
		MergeStateStatus: "BEHIND",
		Mergeable:        "MERGEABLE",
		CIState:          "passing",
		Unaddressed:      true,
		BlockingComments: []scan.Comment{{CommentID: "c1"}, {CommentID: "c2"}},
	}
}

func jobsConfig() config.Config {
	cfg := config.Defaults()
	cfg.Machines = []string{"linux2"}
	cfg.JobMachines = map[string]string{config.LocalMachine: "tiger", "linux2": "turtle"}
	cfg.JobLanes = map[string]string{"needs-linux": "linux2"}
	return cfg
}

func TestBuildGoldenAffinity(t *testing.T) {
	t.Parallel()

	tabbed := behindWithComment(1)
	tabbed.Tab = &scan.Tab{TabID: "w1:t1", PaneID: strPtr("w1:p1"), Machine: "linux2"}
	tabbed.Labels = []string{"needs-linux"}

	laned := behindWithComment(2)
	laned.Labels = []string{"bug", "needs-linux"}
	laned.Tab = &scan.Tab{TabID: "w1:t2"} // a tab with no agent is not an agent tab

	owned := behindWithComment(3)

	anywhere := behindWithComment(4)

	owners := Owners{"acme/widgets#3": config.LocalMachine}
	for _, tc := range []struct {
		name string
		pr   scan.PR
	}{
		{"tab", tabbed},
		{"lane", laned},
		{"owner", owned},
		{"any", anywhere},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			doc := scan.Document{Repos: []string{"acme/widgets"}, PRs: []scan.PR{tc.pr}}
			checkGolden(t, "affinity-"+tc.name+".json", Build(doc, owners, jobsConfig()))
		})
	}
}

func TestBuildKindsAndSkips(t *testing.T) {
	t.Parallel()

	all := scan.PR{
		Repo: "acme/widgets", Number: 5, State: "open", HeadSHA: "abc123",
		Tab:              &scan.Tab{TabID: "w1:t5", PaneID: strPtr("w1:p5")},
		Actions:          []scan.Action{{Name: scan.ActionMergeQueueFix}},
		Unaddressed:      true,
		BlockingComments: []scan.Comment{{CommentID: "c9"}},
		CIState:          "failing",
		Mergeable:        "CONFLICTING",
	}
	draft := behindWithComment(6)
	draft.IsDraft = true
	unknown := behindWithComment(7)
	unknown.TabUnknown = scan.TabUnknownMachineUnreachable
	closed := behindWithComment(8)
	closed.State = "closed"
	clean := scan.PR{Repo: "acme/widgets", Number: 9, State: "open", MergeStateStatus: "CLEAN", CIState: "passing"}

	doc := scan.Document{PRs: []scan.PR{all, draft, unknown, closed, clean}}
	got := Build(doc, Owners{}, jobsConfig())
	var kinds []string
	for _, s := range got.Jobs {
		if s.Subject != "acme/widgets#5" {
			t.Fatalf("job for %s, want only #5: %+v", s.Subject, got.Jobs)
		}
		kinds = append(kinds, s.Kind+"/"+s.Executor)
	}
	want := []string{"merge-queue-fix/tab", "comment/tab", "ci-fix/tab", "rebase/cli"}
	if !slices.Equal(kinds, want) {
		t.Fatalf("kinds = %v, want %v", kinds, want)
	}
	if got.Jobs[0].Machine != "tiger" {
		t.Fatalf("local tab machine = %q, want tiger (job_machines)", got.Jobs[0].Machine)
	}

	cfg := jobsConfig()
	cfg.IncludeDrafts = true
	if got := Build(scan.Document{PRs: []scan.PR{draft}}, Owners{}, cfg); len(got.Jobs) != 2 {
		t.Fatalf("include_drafts jobs = %+v", got.Jobs)
	}
}

func TestBuildEmptyIsAnEmptyList(t *testing.T) {
	t.Parallel()

	raw, err := json.Marshal(Build(scan.Document{}, nil, config.Defaults()))
	if err != nil {
		t.Fatal(err)
	}
	if string(raw) != `{"jobs":[]}` {
		t.Fatalf("json = %s", raw)
	}
}

func TestOwnersNote(t *testing.T) {
	t.Parallel()

	owners := Owners{
		"acme/widgets#1": "linux2",
		"acme/widgets#2": "linux2",
		"acme/other#3":   "linux2",
	}
	doc := scan.Document{
		Repos: []string{"acme/widgets"},
		PRs: []scan.PR{
			{Repo: "acme/widgets", Number: 1, State: "open"},
			{Repo: "acme/widgets", Number: 4, State: "open", Tab: &scan.Tab{PaneID: strPtr("p4")}},
		},
	}
	owners.Note(doc)
	want := Owners{"acme/widgets#1": "linux2", "acme/widgets#4": config.LocalMachine, "acme/other#3": "linux2"}
	if len(owners) != len(want) {
		t.Fatalf("owners = %v, want %v", owners, want)
	}
	for k, v := range want {
		if owners[k] != v {
			t.Fatalf("owners = %v, want %v", owners, want)
		}
	}

	path := filepath.Join(t.TempDir(), OwnersFile)
	if err := owners.Save(path); err != nil {
		t.Fatal(err)
	}
	back, err := LoadOwners(path)
	if err != nil || back["acme/widgets#4"] != config.LocalMachine {
		t.Fatalf("LoadOwners = %v, %v", back, err)
	}
	missing, err := LoadOwners(filepath.Join(t.TempDir(), "none.json"))
	if err != nil || len(missing) != 0 {
		t.Fatalf("missing = %v, %v", missing, err)
	}
}

func TestQueueArgs(t *testing.T) {
	t.Parallel()

	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("agentbus", true)
	mock.ExpectCommandWithArgs("agentbus", "jobs", "show", "j-7", "--json").
		WillSucceed(`{"id":"j-7","kind":"rebase","subject":"acme/widgets#1","head":"abc123","needs":{"machine":"turtle","affinity":"tab","executor":"cli"}}`, 0).Build()
	mock.ExpectCommandWithArgs("agentbus", "jobs", "enqueue", "--kind", "conflict", "--subject", "acme/widgets#1",
		"--head", "abc123", "--machine", "turtle", "--affinity", "tab", "--executor", "tab", "--json").
		WillSucceed(`{"id":"j-8","state":"ready"}`, 0).Build()
	mock.ExpectCommandWithArgs("agentbus", "jobs", "done", "j-7", "--runner", "r1", "--note", "ok").WillSucceed("", 0).Build()
	mock.ExpectCommandWithArgs("agentbus", "jobs", "fail", "j-7", "--exit", "75", "--note", "no").WillSucceed("", 0).Build()
	mock.ExpectCommandWithArgs("agentbus", "jobs", "show", "j-9", "--json").WillFail("no job j-9", 1).Build()

	q := Queue{Exec: mock, Bin: "agentbus"}
	ctx := context.Background()
	j, err := q.Show(ctx, "j-7")
	if err != nil || j.Kind != "rebase" || j.Needs.Machine != "turtle" || j.Head != "abc123" {
		t.Fatalf("Show = %+v, %v", j, err)
	}
	id, err := q.Enqueue(ctx, Spec{Kind: KindConflict, Subject: j.Subject, Head: j.Head, Machine: "turtle", Affinity: AffinityTab, Executor: ExecutorTab})
	if err != nil || id != "j-8" {
		t.Fatalf("Enqueue = %q, %v", id, err)
	}
	if err := q.Done(ctx, "j-7", "r1", "ok"); err != nil {
		t.Fatal(err)
	}
	code := 75
	if err := q.Fail(ctx, "j-7", "", &code, "no"); err != nil {
		t.Fatal(err)
	}
	if _, err := q.Show(ctx, "j-9"); err == nil {
		t.Fatal("Show(j-9) error = nil")
	}
}

func checkGolden(t *testing.T, name string, doc Document) {
	t.Helper()
	got, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	got = append(got, '\n')
	path := filepath.Join("testdata", name)
	if *update {
		if err := os.WriteFile(path, got, 0o600); err != nil {
			t.Fatal(err)
		}
	}
	want, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if string(got) != string(want) {
		t.Fatalf("%s:\n got %s\nwant %s", name, got, want)
	}
}
