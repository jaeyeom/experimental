package herdr

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

// installFakeHerdr puts a fake herdr first on PATH. local and m2 both
// list one idle claude agent in pane w2:p3 with an empty composer.
func installFakeHerdr(t *testing.T) *herdrtest.Fake {
	t.Helper()
	f := herdrtest.Install(t)
	for _, m := range []string{"local", "m2"} {
		f.Write(m+".agents", agentList)
		f.Write(m+".screen", claudeScreen("❯ "))
	}
	return f
}

func TestRemoteCallsUseMachine(t *testing.T) {
	f := installFakeHerdr(t)
	ad := &Adapter{Timeout: 10 * time.Second}
	ctx := context.Background()

	if _, err := ad.Deliver(ctx, "herdr:m2/default/w2:p3", "hello"); err != nil {
		t.Fatal(err)
	}
	if got := f.Argv(); len(got) != 1 || got[0] != "--machine m2 agent prompt w2:p3 hello" {
		t.Fatalf("remote deliver argv = %q", got)
	}
	if _, err := ad.Deliver(ctx, "herdr:local/default/w2:p3", "hello"); err != nil {
		t.Fatal(err)
	}
	if got := f.Argv(); len(got) != 1 || got[0] != "agent prompt w2:p3 hello" {
		t.Fatalf("local deliver argv = %q", got)
	}

	state, err := ad.State(ctx, "herdr:m2/default/w2:p3")
	if err != nil || state != adapter.StateIdle {
		t.Fatalf("remote state = %q, %v", state, err)
	}
	if got := f.Argv(); len(got) != 1 || got[0] != "--machine m2 agent list" {
		t.Fatalf("remote state argv = %q", got)
	}
	gone, err := ad.State(ctx, "herdr:m2/default/w9:p9")
	if err != nil || gone != adapter.StateGone {
		t.Fatalf("missing remote pane = %q, %v", gone, err)
	}
	f.Argv()

	empty, _, err := ad.ComposerEmpty(ctx, "herdr:m2/default/w2:p3")
	if err != nil || !empty {
		t.Fatalf("remote composer = %v, %v", empty, err)
	}
	want := []string{"--machine m2 agent list", "--machine m2 agent read w2:p3 --source visible"}
	if got := f.Argv(); strings.Join(got, "|") != strings.Join(want, "|") {
		t.Fatalf("remote composer argv = %q, want %q", got, want)
	}
}

func TestUnreachableMachineCauses(t *testing.T) {
	tests := []struct {
		name  string
		files []string
		cause string
	}{
		{"connection refused", []string{"m2.down"}, remotecall.CauseHostUnreachable},
		{"publickey", []string{"m2.locked"}, remotecall.CauseAuthLocked},
		{"hang, host answers", []string{"m2.hang"}, remotecall.CauseAuthLocked},
		{"hang, host down", []string{"m2.hang", "m2.status-down"}, remotecall.CauseHostUnreachable},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			f := installFakeHerdr(t)
			for _, name := range tt.files {
				f.Write(name, "")
			}
			ad := &Adapter{Timeout: 300 * time.Millisecond}
			start := time.Now()
			_, err := ad.State(context.Background(), "herdr:m2/default/w2:p3")
			if took := time.Since(start); took > 5*time.Second {
				t.Fatalf("State took %s", took)
			}
			ue := remotecall.AsUnreachable(err)
			if ue == nil || ue.Cause != tt.cause || ue.Machine != "m2" {
				t.Fatalf("State error = %v, want cause %q", err, tt.cause)
			}
			if _, err := ad.Deliver(context.Background(), "herdr:m2/default/w2:p3", "hi"); remotecall.AsUnreachable(err) == nil {
				t.Fatalf("Deliver error = %v, want unreachable", err)
			}
		})
	}
}

func TestReachableMachineErrorIsHandledAsToday(t *testing.T) {
	f := installFakeHerdr(t)
	f.Write("m2.prompt-error", "")
	ad := &Adapter{Timeout: 10 * time.Second}
	result, err := ad.Deliver(context.Background(), "herdr:m2/default/w2:p3", "hi")
	if err != nil || result != adapter.ResultFailed {
		t.Fatalf("Deliver = %q, %v; want failed, nil", result, err)
	}
}

func TestInventoryAddsConfiguredMachines(t *testing.T) {
	f := installFakeHerdr(t)
	ad := &Adapter{Machines: []string{"m2"}, Timeout: 10 * time.Second}
	agents, err := ad.Inventory(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(agents) != 2 || agents[0].Handle != "herdr:local/default/w2:p3" || agents[1].Handle != "herdr:m2/default/w2:p3" || agents[1].TabLabel != "m2-tab" {
		t.Fatalf("inventory = %+v", agents)
	}

	f.Write("m2.down", "")
	agents, err = ad.Inventory(context.Background())
	var warn *adapter.InventoryWarning
	if !errors.As(err, &warn) || len(warn.Lines) != 1 || warn.Lines[0] != "machine m2 unreachable: host unreachable" {
		t.Fatalf("inventory error = %v", err)
	}
	if len(agents) != 1 || agents[0].Handle != "herdr:local/default/w2:p3" {
		t.Fatalf("inventory with m2 down = %+v", agents)
	}
}

func TestMachineStatusUp(t *testing.T) {
	cases := []struct {
		name, stdout, stderr string
		want                 bool
	}{
		{"reachable", `[{"id":"x","label":"linux","status":"reachable","error":null}]`, "", true},
		{"auth status on a live host", `[{"label":"m2","status":"auth_required","error":"locked"}]`, "", true},
		{"unreachable", `[{"label":"m2","status":"unreachable","error":"connect failed"}]`, "", false},
		{"disabled", `[{"label":"m2","status":"disabled","error":null}]`, "", false},
		{"empty status", `[{"label":"m2","status":""}]`, "", false},
		{"unparsed text with a down marker", "machine m2 is offline", "", false},
		{"unparsed text without a marker", "ok", "", true},
	}
	for _, tc := range cases {
		if got := machineStatusUp(tc.stdout, tc.stderr); got != tc.want {
			t.Errorf("%s: machineStatusUp = %v, want %v", tc.name, got, tc.want)
		}
	}
}
