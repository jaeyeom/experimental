package decide

import (
	"context"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

const (
	fakePanes      = `{"result":{"panes":[{"pane_id":"w3:p7","tab_id":"w3:t2","workspace_id":"w3"},{"pane_id":"w3:p8","tab_id":"w3:t4","workspace_id":"w3"}]}}`
	fakeTabs       = `{"result":{"tabs":[{"tab_id":"w3:t2","label":"review","workspace_id":"w3"},{"tab_id":"w3:t4","label":"build","workspace_id":"w3"}]}}`
	fakeWorkspaces = `{"result":{"workspaces":[{"workspace_id":"w3","label":"experimental"}]}}`
)

func fakeHerdrMachine(f *herdrtest.Fake, machine string) {
	f.Write(machine+".panes", fakePanes)
	f.Write(machine+".tabs", fakeTabs)
	f.Write(machine+".workspaces", fakeWorkspaces)
}

func resolveOne(t *testing.T, it *Item, local string) OwnerLocation {
	t.Helper()
	got := LocationResolver{}.Resolve(context.Background(), []OwnerLocation{OwnerPane(it, local)})
	if len(got) != 1 {
		t.Fatalf("resolved %d locations", len(got))
	}
	return got[0]
}

func TestOwnerLocationLivePane(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "local")
	loc := resolveOne(t, &Item{NotifyPane: "w3:p7"}, "")
	want := OwnerLocation{Workspace: "experimental", Tab: "review", Pane: "w3:p7", State: LocationLive, WorkspaceID: "w3", TabID: "w3:t2"}
	if loc != want {
		t.Fatalf("location = %+v, want %+v", loc, want)
	}
	if got := loc.Text(); got != "experimental / review (w3:p7)" {
		t.Fatalf("text = %q", got)
	}
	if got := OwnerAt("review-load", loc); got != "review-load at experimental / review (w3:p7)" {
		t.Fatalf("owner line = %q", got)
	}
}

func TestOwnerLocationClosedPane(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "local")
	loc := resolveOne(t, &Item{NotifyPane: "w3:p9"}, "")
	if loc.State != LocationClosed || loc.Pane != "w3:p9" {
		t.Fatalf("location = %+v", loc)
	}
	if got := loc.Text(); got != "(w3:p9) — tab closed" {
		t.Fatalf("text = %q", got)
	}
}

func TestOwnerLocationNoPaneIsUnknown(t *testing.T) {
	f := herdrtest.Install(t)
	loc := resolveOne(t, &Item{}, "")
	if loc.State != LocationUnknown || loc.Pane != "" {
		t.Fatalf("location = %+v", loc)
	}
	if got := loc.Text(); got != "unknown" {
		t.Fatalf("text = %q", got)
	}
	if argv := f.Argv(); len(argv) != 0 {
		t.Fatalf("herdr ran %q", argv)
	}
}

func TestOwnerLocationBusNameIsNotResolved(t *testing.T) {
	f := herdrtest.Install(t)
	loc := resolveOne(t, &Item{NotifyPane: BusPanePrefix + "ada"}, "")
	if loc.State != LocationUnknown || loc.Pane != BusPanePrefix+"ada" {
		t.Fatalf("location = %+v", loc)
	}
	if argv := f.Argv(); len(argv) != 0 {
		t.Fatalf("herdr ran %q", argv)
	}
}

func TestOwnerLocationOtherMachine(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "mac")
	loc := resolveOne(t, &Item{NotifyPane: "w3:p7", NotifyMachine: "mac"}, "hub")
	if loc.State != LocationLive || loc.Machine != "mac" {
		t.Fatalf("location = %+v", loc)
	}
	if got := loc.Text(); got != "mac: experimental / review (w3:p7)" {
		t.Fatalf("text = %q", got)
	}
	for _, argv := range f.Argv() {
		if !strings.HasPrefix(argv, "--machine mac ") {
			t.Fatalf("herdr call %q is not on mac", argv)
		}
	}
}

func TestOwnerLocationThisMachineByLabel(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "local")
	loc := resolveOne(t, &Item{NotifyPane: "w3:p7", NotifyMachine: "hub"}, "hub")
	if loc.State != LocationLive || loc.Machine != "" {
		t.Fatalf("location = %+v", loc)
	}
	for _, argv := range f.Argv() {
		if strings.HasPrefix(argv, "--machine") {
			t.Fatalf("herdr call %q went to another machine", argv)
		}
	}
}

func TestOwnerLocationUnreachableMachine(t *testing.T) {
	f := herdrtest.Install(t)
	f.Write("mac.down", "")
	loc := resolveOne(t, &Item{NotifyPane: "w3:p7", NotifyMachine: "mac"}, "")
	if loc.State != LocationUnreachable || loc.Machine != "mac" || loc.Pane != "w3:p7" {
		t.Fatalf("location = %+v", loc)
	}
	if got := loc.Text(); got != "mac: (w3:p7) (unreachable)" {
		t.Fatalf("text = %q", got)
	}
}

func TestOwnerLocationHerdrFailureShowsPaneOnly(t *testing.T) {
	f := herdrtest.Install(t)
	f.Write("local.down", "")
	loc := resolveOne(t, &Item{NotifyPane: "w3:p7"}, "")
	if loc.State != LocationUnknown || loc.Pane != "w3:p7" {
		t.Fatalf("location = %+v", loc)
	}
	if got := loc.Text(); got != "(w3:p7)" {
		t.Fatalf("text = %q", got)
	}
}

func TestOwnerLocationListsOncePerMachine(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "local")
	fakeHerdrMachine(f, "mac")
	items := []Item{
		{NotifyPane: "w3:p7"},
		{NotifyPane: "w3:p8"},
		{NotifyPane: "w3:p7", NotifyMachine: "mac"},
		{NotifyPane: "w3:p8", NotifyMachine: "mac"},
	}
	locs := make([]OwnerLocation, len(items))
	for i := range items {
		locs[i] = OwnerPane(&items[i], "")
	}
	got := LocationResolver{}.Resolve(context.Background(), locs)
	if got[1].Tab != "build" || got[3].Tab != "build" || got[2].Machine != "mac" {
		t.Fatalf("locations = %+v", got)
	}
	argv := f.Argv()
	for _, want := range []string{
		"pane list", "tab list", "workspace list",
		"--machine mac pane list", "--machine mac tab list", "--machine mac workspace list",
	} {
		if n := countCalls(argv, want); n != 1 {
			t.Fatalf("%q ran %d times in %q", want, n, argv)
		}
	}
}

func countCalls(argv []string, want string) int {
	n := 0
	for _, call := range argv {
		if call == want {
			n++
		}
	}
	return n
}

func TestOwnerLocationPendingShowsPaneID(t *testing.T) {
	loc := OwnerPane(&Item{NotifyPane: "w3:p7", NotifyMachine: "mac"}, "")
	if got := loc.Text(); got != "mac: (w3:p7)" {
		t.Fatalf("text = %q", got)
	}
	if loc.State != LocationUnknown {
		t.Fatalf("state = %q", loc.State)
	}
}
