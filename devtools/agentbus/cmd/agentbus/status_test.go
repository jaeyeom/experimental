package main

import (
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

type reportJSON struct {
	ID          string `json:"id"`
	State       string `json:"state"`
	HeldReason  string `json:"held_reason"`  //nolint:tagliatelle // status --json shape
	StoredAt    string `json:"stored_at"`    //nolint:tagliatelle // status --json shape
	DeliveredAt string `json:"delivered_at"` //nolint:tagliatelle // status --json shape
	Error       string `json:"error"`
}

// decodeStatuses reads one status object per line.
func decodeStatuses(t *testing.T, stdout string) []reportJSON {
	t.Helper()
	var out []reportJSON
	for _, line := range strings.Split(strings.TrimSuffix(stdout, "\n"), "\n") {
		var got reportJSON
		dec := json.NewDecoder(strings.NewReader(line))
		dec.DisallowUnknownFields()
		if err := dec.Decode(&got); err != nil {
			t.Fatalf("line is not status JSON: %v\n%s", err, stdout)
		}
		out = append(out, got)
	}
	return out
}

func mustTime(t *testing.T, field, value string) {
	t.Helper()
	if _, err := time.Parse(time.RFC3339, value); err != nil {
		t.Fatalf("%s %q is not RFC3339: %v", field, value, err)
	}
}

func TestStatusJSONHeldThenDelivered(t *testing.T) {
	env, ad := heldWorld(t, nil)
	ad.DeliverErr = &remotecall.UnreachableError{Machine: "local", Cause: "host unreachable"}
	code, stdout, stderr := runCmd([]string{"send", heldReviewer, "held"}, env("p1"))
	if code != 3 {
		t.Fatalf("send = %d, stderr %q", code, stderr)
	}
	id := firstID(t, stdout)

	stdout, _ = mustCmd(t, []string{"status", "--json", id}, env(""))
	got := decodeStatuses(t, stdout)
	if len(got) != 1 || got[0].ID != id || got[0].State != "held" || got[0].HeldReason != "transport unreachable" || got[0].DeliveredAt != "" || got[0].Error != "" {
		t.Fatalf("held status = %s", stdout)
	}
	mustTime(t, "stored_at", got[0].StoredAt)
	if strings.Contains(stdout, `"delivered_at"`) || strings.Contains(stdout, `"error"`) {
		t.Fatalf("fields that do not apply are not omitted: %s", stdout)
	}

	ad.DeliverErr = nil
	stdout, _ = mustCmd(t, []string{"status", "--json", id}, env(""))
	got = decodeStatuses(t, stdout)
	if len(got) != 1 || got[0].State != "delivered" || got[0].HeldReason != "" {
		t.Fatalf("delivered status = %s", stdout)
	}
	mustTime(t, "delivered_at", got[0].DeliveredAt)
}

func TestStatusJSONComposerNotEmpty(t *testing.T) {
	env, ad := heldWorld(t, nil)
	ad.Composer = func(string) (bool, string, error) { return false, adapter.HoldDraft, nil }
	id := firstID(t, mustOut(t, []string{"send", heldReviewer, "draft"}, env("p1")))
	stdout, _ := mustCmd(t, []string{"status", "--json", id}, env(""))
	got := decodeStatuses(t, stdout)
	if len(got) != 1 || got[0].State != "held" || got[0].HeldReason != "composer busy" {
		t.Fatalf("status = %s", stdout)
	}
}

func TestStatusJSONSeveralIDsInOneCall(t *testing.T) {
	env, _ := heldWorld(t, nil)
	first := firstID(t, mustOut(t, []string{"send", heldReviewer, "one"}, env("p1")))
	second := firstID(t, mustOut(t, []string{"send", heldReviewer, "two"}, env("p1")))
	stdout, _ := mustCmd(t, []string{"status", "--json", first, "m-unknown", second}, env(""))
	got := decodeStatuses(t, stdout)
	if len(got) != 3 || got[0].ID != first || got[1] != (reportJSON{ID: "m-unknown", State: "unknown"}) || got[2].ID != second {
		t.Fatalf("status = %s", stdout)
	}
	for _, s := range []reportJSON{got[0], got[2]} {
		if s.State == "unknown" || s.StoredAt == "" {
			t.Fatalf("status = %s", stdout)
		}
	}
}

func TestStatusUsage(t *testing.T) {
	for _, args := range [][]string{
		{"status"},
		{"status", "--json"},
		{"status", "m-1", "m-2"},
		{"status", "--bogus", "m-1"},
	} {
		if code, _, _ := runCmd(args, nil); code != 2 {
			t.Errorf("%q = %d, want 2", args, code)
		}
	}
}

func TestStatusTextIsUnchanged(t *testing.T) {
	env, _ := heldWorld(t, nil)
	id := firstID(t, mustOut(t, []string{"send", heldReviewer, "text"}, env("p1")))
	stdout, _ := mustCmd(t, []string{"status", id}, env(""))
	if !strings.HasPrefix(stdout, "id: "+id+"\nstate: ") || !strings.Contains(stdout, "\nwaited: ") {
		t.Fatalf("status = %q", stdout)
	}
}

func TestStatusJSONHubModeAnswersAPendingOutboxEntryLocally(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	id := pinClientID(t, idA)
	if code, _, stderr := s.run(t, s.env, "", "send", hubTarget, "queued here"); code != 0 {
		t.Fatalf("send = %d, stderr %q", code, stderr)
	}
	code, stdout, stderr := s.run(t, s.env, "", "status", "--json", id)
	if code != 0 {
		t.Fatalf("status = %d, stderr %q", code, stderr)
	}
	got := decodeStatuses(t, stdout)
	if len(got) != 1 || got[0].ID != id || got[0].State != "held" || got[0].HeldReason != "transport unreachable" {
		t.Fatalf("status = %s", stdout)
	}
	mustTime(t, "stored_at", got[0].StoredAt)
}

func TestStatusJSONHubModeAsksTheHubForAForwardedMessage(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	id := pinClientID(t, idA)
	if code, _, stderr := s.run(t, s.env, "", "send", hubTarget, "forwarded"); code != 0 {
		t.Fatalf("send = %d, stderr %q", code, stderr)
	}
	s.startHubDaemon(t)
	if code, _, stderr := s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	code, stdout, stderr := s.run(t, s.env, "", "status", "--json", id, "m-unknown")
	if code != 0 {
		t.Fatalf("status = %d, stderr %q", code, stderr)
	}
	got := decodeStatuses(t, stdout)
	if len(got) != 2 || got[0].ID != id || got[0].State == "unknown" || got[1] != (reportJSON{ID: "m-unknown", State: "unknown"}) {
		t.Fatalf("status = %s", stdout)
	}
}
