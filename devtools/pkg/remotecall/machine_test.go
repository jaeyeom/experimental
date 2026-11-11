package remotecall

import (
	"context"
	"slices"
	"testing"
	"time"
)

func TestOnHerdrMachinePrependsTheMachine(t *testing.T) {
	var got []string
	run := func(_ context.Context, args []string) (string, string, int, error) {
		got = args
		return "ok", "", 0, nil
	}
	stdout, _, code, err := OnHerdrMachine(context.Background(), run, nil, "m2", time.Second, []string{"agent", "prompt", "p3", "hi"})
	if err != nil || code != 0 || stdout != "ok" {
		t.Fatalf("OnHerdrMachine = %q, %d, %v", stdout, code, err)
	}
	if want := []string{"--machine", "m2", "agent", "prompt", "p3", "hi"}; !slices.Equal(got, want) {
		t.Fatalf("args = %q, want %q", got, want)
	}
}

func TestOnHerdrMachineClassifiesAnUnreachableMachine(t *testing.T) {
	down := func(context.Context, []string) (string, string, int, error) {
		return "", "ssh: connect to host m2 port 22: Connection refused", 255, nil
	}
	_, _, _, err := OnHerdrMachine(context.Background(), down, nil, "m2", time.Second, []string{"agent", "prompt"})
	if ue := AsUnreachable(err); ue == nil || ue.Machine != "m2" || ue.Cause != CauseHostUnreachable {
		t.Fatalf("err = %v, want m2 host unreachable", err)
	}
}

func TestOnHerdrMachineKillsAHangAtTheDeadline(t *testing.T) {
	hang := func(ctx context.Context, _ []string) (string, string, int, error) {
		<-ctx.Done()
		return "", "", -1, nil
	}
	up := func(context.Context) bool { return true }
	start := time.Now()
	_, _, _, err := OnHerdrMachine(context.Background(), hang, up, "m2", 50*time.Millisecond, []string{"agent", "prompt"})
	if time.Since(start) > 2*time.Second {
		t.Fatal("OnHerdrMachine did not stop at the deadline")
	}
	if ue := AsUnreachable(err); ue == nil || ue.Cause != CauseAuthLocked {
		t.Fatalf("err = %v, want authentication locked", err)
	}
}

func TestOnHerdrMachineKeepsARealHerdrError(t *testing.T) {
	fail := func(context.Context, []string) (string, string, int, error) {
		return "", `{"error":{"code":"pane_not_found"}}`, 1, nil
	}
	up := func(context.Context) bool { return true }
	_, stderr, code, err := OnHerdrMachine(context.Background(), fail, up, "m2", time.Second, []string{"agent", "prompt"})
	if err != nil || code != 1 || stderr == "" {
		t.Fatalf("OnHerdrMachine = %q, %d, %v", stderr, code, err)
	}
}

func TestHerdrProbeAsksMachineStatus(t *testing.T) {
	var got []string
	run := func(_ context.Context, args []string) (string, string, int, error) {
		got = args
		return `[{"label":"m2","status":"unreachable"}]`, "", 0, nil
	}
	if HerdrProbe(run, "m2")(context.Background()) {
		t.Fatal("probe reported an unreachable machine as up")
	}
	if want := []string{"machine", "status", "m2", "--json"}; !slices.Equal(got, want) {
		t.Fatalf("args = %q, want %q", got, want)
	}
}
