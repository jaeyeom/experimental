package decide

import (
	"strings"
	"testing"
)

func TestAssignSetsWorkerAndAppendsEvent(t *testing.T) {
	led, _ := openTemp(t)
	item := reassignSeed(t, led)
	got, err := led.Assign(item.ID, Assign{Worker: "bea", By: "cara"})
	if err != nil {
		t.Fatal(err)
	}
	if got.Owner != "ada" || got.Worker != "bea" || got.WorkerNotifyPane != "agentbus:bea" || got.WorkerNotifyMachine != "" {
		t.Fatalf("assign = owner %q worker %q pane %q machine %q", got.Owner, got.Worker, got.WorkerNotifyPane, got.WorkerNotifyMachine)
	}
	if got.NotifyPane != "p1" || got.State != StateAnswered {
		t.Fatalf("owner routing changed: pane %q state %s", got.NotifyPane, got.State)
	}
	last := got.Messages[len(got.Messages)-1]
	const want = "assigned: bea is working on it (by cara)"
	if last.Author != "system" || last.By != "cara" || last.Text != want {
		t.Fatalf("event = %+v, want %q", last, want)
	}
	stored, err := led.Get(item.ID)
	if err != nil || stored.Worker != "bea" || stored.WorkerNotifyPane != "agentbus:bea" {
		t.Fatalf("stored = %+v err %v", stored, err)
	}
}

func TestAssignPaneKeepsItsMachineAndReplacesTheWorker(t *testing.T) {
	led, _ := openTemp(t)
	item := reassignSeed(t, led)
	if _, err := led.Assign(item.ID, Assign{Worker: "bea"}); err != nil {
		t.Fatal(err)
	}
	got, err := led.Assign(item.ID, Assign{Worker: "cid", Pane: "p7", Machine: "mac"})
	if err != nil {
		t.Fatal(err)
	}
	if got.Worker != "cid" || got.WorkerNotifyPane != "p7" || got.WorkerNotifyMachine != "mac" {
		t.Fatalf("assign = worker %q pane %q machine %q", got.Worker, got.WorkerNotifyPane, got.WorkerNotifyMachine)
	}
	const want = "assigned: cid is working on it, was bea"
	if last := got.Messages[len(got.Messages)-1]; last.Text != want {
		t.Fatalf("event = %q, want %q", last.Text, want)
	}
}

func TestAssignClearRemovesWorkerAndAppendsEvent(t *testing.T) {
	led, _ := openTemp(t)
	item := reassignSeed(t, led)
	if _, err := led.Assign(item.ID, Assign{Worker: "bea", Pane: "p7", Machine: "mac"}); err != nil {
		t.Fatal(err)
	}
	got, err := led.Assign(item.ID, Assign{Clear: true, By: "cara"})
	if err != nil {
		t.Fatal(err)
	}
	if got.Worker != "" || got.WorkerNotifyPane != "" || got.WorkerNotifyMachine != "" {
		t.Fatalf("clear = worker %q pane %q machine %q", got.Worker, got.WorkerNotifyPane, got.WorkerNotifyMachine)
	}
	const want = "unassigned: bea is no longer working on it (by cara)"
	if last := got.Messages[len(got.Messages)-1]; last.Author != "system" || last.Text != want {
		t.Fatalf("event = %+v, want %q", last, want)
	}
	n := len(got.Messages)
	again, err := led.Assign(item.ID, Assign{Clear: true})
	if err != nil || len(again.Messages) != n {
		t.Fatalf("second clear = %d messages err %v, want %d", len(again.Messages), err, n)
	}
}

func TestAssignRefusesEmptyWorkerAndClosedItems(t *testing.T) {
	led, _ := openTemp(t)
	item := reassignSeed(t, led)
	if _, err := led.Assign(item.ID, Assign{Worker: "  "}); err == nil {
		t.Fatal("empty worker was accepted")
	}
	if _, err := led.Done(item.ID, "finished"); err != nil {
		t.Fatal(err)
	}
	_, err := led.Assign(item.ID, Assign{Worker: "bea"})
	if err == nil || !strings.Contains(err.Error(), "done") {
		t.Fatalf("assign done = %v", err)
	}
}

func TestRouteWarningOnlyForAnotherWorker(t *testing.T) {
	it := &Item{Owner: "ada"}
	if got := RouteWarning(it); got != "" {
		t.Fatalf("no worker warning = %q", got)
	}
	it.Worker = "ada"
	if got := RouteWarning(it); got != "" {
		t.Fatalf("owner as worker warning = %q", got)
	}
	it.Worker = "bea"
	const want = "This reply goes to ada (owner); bea is working on it."
	if got := RouteWarning(it); got != want {
		t.Fatalf("warning = %q, want %q", got, want)
	}
}

func TestReplyTargetsFollowTheRoute(t *testing.T) {
	cfg := Config{NotifyPane: "cfg-pane"}
	it := &Item{Owner: "ada", NotifyPane: "p1", NotifyMachine: "m1"}
	owner := ReplyTarget{Name: "ada", Pane: "p1", Machine: "m1"}
	worker := ReplyTarget{Name: "bea", Pane: "agentbus:bea"}
	if got := ReplyTargets(it, cfg, ""); len(got) != 1 || got[0] != owner {
		t.Fatalf("no worker = %+v", got)
	}
	it.Worker, it.WorkerNotifyPane = "bea", "agentbus:bea"
	cases := map[string][]ReplyTarget{
		"":          {owner},
		RouteOwner:  {owner},
		RouteWorker: {worker},
		RouteBoth:   {owner, worker},
	}
	for route, want := range cases {
		got := ReplyTargets(it, cfg, route)
		if len(got) != len(want) {
			t.Fatalf("route %q = %+v, want %+v", route, got, want)
		}
		for i := range want {
			if got[i] != want[i] {
				t.Fatalf("route %q = %+v, want %+v", route, got, want)
			}
		}
	}
	it.NotifyClosed = true
	if got := ReplyTargets(it, cfg, RouteBoth); len(got) != 1 || got[0] != worker {
		t.Fatalf("closed owner, both = %+v", got)
	}
	if got := ReplyTargets(it, cfg, RouteOwner); len(got) != 0 {
		t.Fatalf("closed owner, owner = %+v", got)
	}
}

func TestCheckRoute(t *testing.T) {
	for _, ok := range []string{RouteOwner, RouteWorker, RouteBoth} {
		if err := CheckRoute(ok); err != nil {
			t.Fatalf("CheckRoute(%q) = %v", ok, err)
		}
	}
	if err := CheckRoute("alpha"); err == nil {
		t.Fatal("CheckRoute accepted alpha")
	}
}
